# An example data source for accessing the Facebook Graph API

The [Facebook Graph
API](https://developers.facebook.com/docs/graph-api) allows
third-party applications to access Facebook data for users that have
explicitly indicated that they want the app to be able to access their
data.

We're going to build a Haxl data source for the Facebook Graph API, by
wrapping the existing [fb](http://hackage.haskell.org/package/fb)
package in a Haxl data source API.  Once we've done this, Haxl will
transparently

 * Perform multiple requests to the API concurrently,
   and concurrently with requests to other data sources.

 * Cache previous requests, so that different parts of our code can
   request the same data without having to worry about whether it gets
   fetched twice.

A data source consists of two parts:

 * The *data source API*, which allows the data source to be
   initialized.  In our example, this is the module [FB.DataSource](FB/DataSource.hs).

 * The *user API*, which exports a set of data-fetching functions in the `Haxl` monad.  In our example, this is the module [FB](FB.hs).

## The Data Source API: `FB.DataSource`

First, let's look at the data source API.  The most important part of
a data source is the set of requests that it supports.  A data source
must define its requests as a GADT:

```haskell
data FacebookReq a where
   GetObject      :: Id -> FacebookReq Object
   GetUser        :: UserId -> FacebookReq User
   GetUserFriends :: UserId -> FacebookReq [Friend]
  deriving Typeable
```

We have three requests: retrieve an arbitrary object, retrieve a user,
and retrive a user's friends.  In reality there are a lot more request
types that we could add here, but these will suffice for the example.

Note that the `FacebookReq` type has a type parameter: this is the
result type of the request.  Each of our requests instantiates this by
the appropriate return type: `Object` for `GetObject`, `User` for
`GetUser`, and `[Friend]` for `GetUserFriends`.

There is some necessary boilerplate that goes along with a data source:

```haskell
deriving instance Eq (FacebookReq a)
deriving instance Show (FacebookReq a)

instance Show1 FacebookReq where show1 = show

instance Hashable (FacebookReq a) where
  hashWithSalt s (GetObject (Id id))      = hashWithSalt s (0::Int,id)
  hashWithSalt s (GetUser (Id id))        = hashWithSalt s (1::Int,id)
  hashWithSalt s (GetUserFriends (Id id)) = hashWithSalt s (2::Int,id)
```

Requests are required to be instances of various classes, so that the
Haxl framework can store them in a cache, and print them out.

Next, a data source can have associated state.  Haxl keeps track of
each data source's state, and provides the state when data needs to be
fetched, as we'll see in a moment.  The state for a data source is
defined by giving an instance of the `StateKey` class:

```haskell
instance StateKey FacebookReq where
  data State FacebookReq =
    FacebookState
       { credentials :: Credentials
       , userAccessToken :: UserAccessToken
       , manager :: Manager
       , numThreads :: Int
       }
```

The `StateKey` class has an associated data type `State`,
parameterised by the request type (here `FacebookReq`).  For the
Facebook data source, we need several things:

 * The app credentials and an access token.  These are the keys
   required to access the Facebook API, and will be passed in when we
   initialize the data source.

 * The `Manager`; this comes from `Network.HTTP.Client`, and it
   maintains a set of open connections to `HTTP` servers.

 * `numThreads`, which says how many threads we will use in our data
   source to fetch data concurrently.

A data source should provide a way to initialize its state.  To
initialize our data source we need to create a new `Manager`, and
store the credentials and other info in the `FacebookState` record:

```haskell
initGlobalState
  :: Int
  -> Credentials
  -> UserAccessToken
  -> IO (State FacebookReq)

initGlobalState threads creds token = do
  manager <- newManager tlsManagerSettings
  return FacebookState
    { credentials = creds
    , manager = manager
    , userAccessToken = token
    , numThreads = threads
    }
```

Next, we give instances for the `DataSourceName` and `DataSource` classes:

```haskell
instance DataSourceName FacebookReq where
  dataSourceName _ = "Facebook"

instance DataSource u FacebookReq where
  fetch = facebookFetch
```

There are two methods:

 * `dataSourceName` is used by the framework to identify this data
   source when producing statistics about fetches, for example.

 * `fetch` is the operation for fetching data, which we'll implement
   next.

The `fetch` implementation has this type:

```haskell
facebookFetch
  :: State FacebookReq
  -> Flags
  -> u
  -> [BlockedFetch FacebookReq]
  -> PerformFetch
```

That is, it takes the current state for this data source, some `Flags`
defined by Haxl, a "user state" (in our case we won't need any user
state, so this is `()`), and a list of `BlockedFetch`es which tell us
which requests need to be fetched.

We're going to fetch these requests concurrently, using `numThreads`
threads.  We'll use the
[async](http://hackage.haskell.org/package/async) package together
with a `QSem` to control the degree of concurrency.

The fetch function returns a value of type `PerformFetch`, defined like this:

```haskell
data PerformFetch
  = SyncFetch  (IO ())
  | AsyncFetch (IO () -> IO ())
```

A data source can fetch either synchronously, indicated by
`SyncFetch`, or asynchronously, indicated by `AsyncFetch`.  The
argument to `AsyncFetch` is a function that takes an `IO` operation to
perform *while the data is being fetched*.  The Haxl framework takes
care of nesting all the data fetches correctly, overlapping as many
`AsyncFetch`es as possible.  Obviously asynchronous data sources are
preferable, although performance is only negatively affected if there
are two or more synchronous data sources.

In the case of the Facebook data source, we will implement an
asynchronous data source, so we return `AsyncFetch`.

```haskell
facebookFetch FacebookState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync credentials manager userAccessToken sem) bfs
    inner
    mapM_ wait asyncs
```

There are three phases: first we issue all the requests, then perform
the `inner` IO operation, and finally we wait for the results.

Issuing a request is done by `fetchAsync`:

```haskell
fetchAsync
  :: Credentials -> Manager -> UserAccessToken -> QSem
  -> BlockedFetch FacebookReq
  -> IO (Async ())
fetchAsync creds manager tok sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFacebookT creds manager $ fetchReq tok req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a
```

This function does several things:

 * it does everything in `async`, which performs the operation
   asynchronously and returns a handle that can be waited on later.

 * it obtains a token from the `QSem`, which is used to control the
   degree of concurrency,

 * it performs the fetch inside a `try`, which catches exceptions.
   This is very important: a data source should never throw
   exceptions, instead it should store the exception inside the
   `ResultVar` (here bound to `rvar`) using `putFailure`.  This
   exception will then be propagated by the Haxl framework to the
   computation that initiated the fetch.

 * it calls `fetchReq` to perform the actual fetch.

`fetchReq` is the application-specific code to fetch data from
Facebook.  Here is where we would add support for more types of
request:

```haskell
fetchReq
  :: UserAccessToken
  -> FacebookReq a
  -> FacebookT Auth (ResourceT IO) a

fetchReq tok (GetObject (Id id)) =
  getObject ("/" <> id) [] (Just tok)

fetchReq _tok (GetUser id) =
  getUser id [] Nothing

fetchReq tok (GetUserFriends id) = do
  f <- getUserFriends id [] tok
  source <- fetchAllNextPages f
  source $$ consume
```

## The User API: `FB`

The job of the user API is to wrap the request type in some nice
functions that we can call from the `Haxl` monad.  Each function is a
call to Haxl's `dataFetch` operation, passing the appropriate request:

```haskell
module FB
  ( getObject
  , getUser
  , getUserFriends
  , Id(..), Friend(..), User(..)
  ) where

import FB.DataSource
import Data.Aeson
import Facebook (Id(..), Friend(..), User(..))

import Haxl.Core

getObject :: Id -> GenHaxl u Object
getObject id = dataFetch (GetObject id)

getUser :: Id -> GenHaxl u User
getUser id = dataFetch (GetUser id)

getUserFriends :: Id -> GenHaxl u [Friend]
getUserFriends id = dataFetch (GetUserFriends id)
```

And that's it.  The whole data source is less than 150 lines, with a lot
of it being standard boilerplate that most data sources need.

Most users of Haxl will want to define a `Haxl` type instantiating the
`GenHaxl` type, something like this:

```haskell
type Haxl a = GenHaxl () a
```

The point of the `GenHaxl` type is that you can pass some
application-specific data through the computation and to the data
sources by instantiating the first paramter (here just `()`) with your
own type.

In the file [TestFB.hs](./TestFB.hs) you can find a simple example
program that uses this data source.  The `main` function looks like this:

```haskell
main :: IO ()
main = do
  (creds, access_token) <- getCredentials
  facebookState <- initGlobalState 10 creds access_token
  env <- initEnv (stateSet facebookState stateEmpty) ()
  r <- runHaxl env $ do
    likes <- getObject "me/likes"
    mapM getObject (likeIds likes) -- these happen concurrently
  print r
```

Once we have some credentials (obtained from the environment), we
initialize the Facebook data source with a maximum of 10 threads, and
then call `initEnv` to initialize Haxl's `Env`: think of this as the
container for Haxl's cache, amongst other things.  Each time we create
an `Env` with `initEnv`, it has an empty cache.

Then we call `runHaxl`, passing the `Env`.  Inside `runHaxl` we do two
fetches: first `getObject "me/likes"` which fetches the set of pages
liked by the user identified by the access token.  Then, we fetch the
objects associated with each of those pages.  This will result in two
round of fetches: one fetch in the first round, and then *N* fetches
in the second round, where *N* is the number of pages we need to
fetch.  The fetches in the second round will be performed
concurrently, using at most 10 threads.
