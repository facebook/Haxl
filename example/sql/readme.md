# Solving the "N+1 Selects Problem" with Haxl

The so-called “[N+1 selects problem](http://ocharles.org.uk/blog/posts/2014-03-24-queries-in-loops-without-a-care-in-the-world.html)” is characterized by a set of queries in a loop. To ape the example from Ollie Charles:

```haskell
getAllUsernames = do
  userIds <- getAllUserIds
  for userIds $ \userId -> do
    getUsernameById userId
```

The `IO` version of this code would perform one data fetch for `getAllUserIds`, then another for each call to `getUsernameById`; assuming each one is implemented with something like the SQL `select` statement, that means “N+1 selects”.

But Haxl does not suffer from this problem. Using *this very code*, the Haxl implementation will perform *exactly two* data fetches: one to `getAllUserIds` and one with all the `getUsernameById` calls batched together.

First, a dash of boilerplate:

```haskell
{-# LANGUAGE DeriveDataTypeable,
             GADTs,
             MultiParamTypeClasses,
             StandaloneDeriving,
             TypeFamilies #-}

import Data.Typeable
import Haxl.Core
```

## The Request Type

First we make a data type with a constructor for each type of request.

```haskell
data UserReq a where
  GetAllIds   :: UserReq [Id]
  GetNameById :: Id -> UserReq Name
  deriving (Typeable)

type Id = Int
type Name = String

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
   hashWithSalt s GetAllIds       = hashWithSalt s (0::Int)
   hashWithSalt s (GetNameById a) = hashWithSalt s (1::Int, a)

deriving instance Show (UserReq a)
instance ShowP UserReq where showp = show
```

This type is parameterized so that each request can indicate which type of result it returns. It is `Typeable` so that Haxl can safely store requests to multiple data sources at once, as well as `Eq` and `Hashable` for caching and `Show` for debug output.

## Making a Data Source from a Request Type

Now we make this an instance of Haxl’s `StateKey` and `DataSource` classes. `StateKey` lets us associate a data source with its global state, to be initialized once. (We won’t take advantage of this here.)

```haskell
instance StateKey UserReq where
  data State UserReq = UserState {}
```

Every data source needs to tell Haxl its name, by giving an instance for
the `DataSourceName` class:

```haskell
instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"
```

Next, `DataSource` lets us specify how a set of blocked requests are to be fetched. It is parameterized by the type of a *user environment* of cross–data source global data, as well as a request type that is an instance of `StateKey`. It is defined as follows:

```haskell
class (DataSourceName, StateKey req, ShowP req) => DataSource u req where
  fetch
    :: State req           -- Data source state.
    -> Flags               -- Flags, containing tracing verbosity level, etc.
    -> u                   -- User environment for cross–data source globals.
    -> [BlockedFetch req]  -- Set of blocked fetches to perform.
    -> PerformFetch        -- An action to perform the fetching.
```

We are mainly concerned with implementing the `fetch` method, and in this case we can ignore many of its parameters, which are to support more complex data sources than ours. The key point is that Haxl gives us a list of *all* the requests that are currently waiting to be fetched, which means we can batch them together however we please.

Taking a look at the definition of `BlockedFetch` informs us how to implement the `fetch` method:

```haskell
data BlockedFetch r = forall a. BlockedFetch (r a) (ResultVar a)
type ResultVar a = MVar (Either SomeException a)
```

Here we have that a `BlockedFetch` consists of a pair of a request (of type `r a`) and a `MVar` containing `Either SomeException` (if fetching failed) or the result of the request. The role of `fetch` is to fill these `MVar`s.

## Implementing the `fetch` Method

Now, a data source can fetch data in one of two ways:

  * **Synchronously:** the fetching operation is an `IO ()` that fetches all the
    data and then returns.

  * **Asynchronously:** we can do something else while the data is being
    fetched. The fetching operation takes an `IO ()` as an argument, which is
    the operation to perform while the data is being fetched.

These are represented by the constructors of the `PerformFetch` type that `fetch` returns:

```haskell
data PerformFetch
  = SyncFetch  (IO ())
  | AsyncFetch (IO () -> IO ())
```

We will use `SyncFetch` here for simplicity. (Haxl also includes `syncFetch` and `asyncFetch` helper functions for implementing common `fetch` patterns.) Now, in the implementation of `fetch`, assuming we have some function `sql` for running SQL queries, we can do something like this:

```haskell
-- We have no user environment, so we use ().
type Haxl = GenHaxl ()

instance DataSource u UserReq where
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do

    unless (null allIdVars) $ do
      allIds <- sql "select id from ids"
      mapM_ (\r -> putSuccess r allIds) allIdVars

    unless (null ids) $ do
      names <- sql $ unwords
        [ "select name from names where"
        , intercalate " or " $ map ("id = " ++) idStrings
        , "order by find_in_set(id, '" ++ intercalate "," idStrings ++ "')"
        ]
      mapM_ (uncurry putSuccess) (zip vars names)

    where
    allIdVars :: [ResultVar [Id]]
    allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]

    idStrings :: [String]
    idStrings = map show ids

    ids :: [Id]
    vars :: [ResultVar Name]
    (ids, vars) = unzip
      [(userId, r) | BlockedFetch (GetNameById userId) r <- blockedFetches]
```

## Tying it All Together

All that remains to make the original example work is to define `getAllUserIds` and `getUserById` using Haxl’s `dataFetch` function.

```haskell
getAllUserIds :: Haxl [Id]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: Id -> Haxl Name
getUsernameById userId = dataFetch (GetNameById userId)
```

`dataFetch` simply takes a request to a data source and returns a
`GenHaxl` action to fetch it concurrently with others.

```haskell
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u a
```

Like magic, the naïve code that *looks* like it will do N+1 fetches will now do just two.

```haskell
getAllUsernames :: Haxl [Name]
getAllUsernames = do
  userIds <- getAllUserIds       -- Round 1
  for userIds $ \userId -> do    -- Round 2
    getUsernameById userId
```

The only change is that its type signature is now `Haxl` instead of `IO`, and at the top level we have to place a call to `runHaxl`:

```haskell
main :: IO ()
main = do

  -- Initialize Haxl state.
  let stateStore = stateSet UserState{} stateEmpty

  -- Initialize Haxl environment.
  env0 <- initEnv stateStore ()

  -- Run action.
  names <- runHaxl env0 getAllUsernames

  print names
```
