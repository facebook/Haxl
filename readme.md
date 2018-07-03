![Haxl Logo](https://github.com/facebook/Haxl/raw/master/logo.png)

# Haxl

Haxl is a Haskell library that simplifies access to remote data, such
as databases or web-based services. Haxl can automatically

 * batch multiple requests to the same data source,
 * request data from multiple data sources concurrently,
 * cache previous requests,
 * memoize computations.

Having all this handled for you behind the scenes means that your
data-fetching code can be much cleaner and clearer than it would
otherwise be if it had to worry about optimizing data-fetching. We'll
give some examples of how this works in the pages linked below.

There are two Haskell packages here:

 * `haxl`: The core Haxl framework
 * `haxl-facebook` (in [https://github.com/facebook/Haxl/tree/master/example/facebook](example/facebook)): An (incomplete) example data source for accessing the Facebook Graph API

To use Haxl in your own application, you will likely need to build one or more
*data sources*: the thin layer between Haxl and the data that you want
to fetch, be it a database, a web API, a cloud service, or whatever.

There is a generic datasource in "Haxl.DataSource.ConcurrentIO" that
can be used for performing arbitrary IO operations concurrently, given
a bit of boilerplate to define the IO operations you want to perform.

The `haxl-facebook` package shows how we might build a Haxl data
source based on the existing `fb` package for talking to the Facebook
Graph API.

## Where to go next?

 * [The Story of Haxl](https://code.facebook.com/posts/302060973291128/open-sourcing-haxl-a-library-for-haskell/)
   explains how Haxl came about at Facebook, and discusses our
   particular use case.

 * [An example Facebook data source](https://github.com/facebook/Haxl/blob/master/example/facebook/readme.md) walks
   through building an example data source that queries the Facebook
   Graph API concurrently.

 * [Fun with Haxl (part 1)](https://simonmar.github.io/posts/2015-10-20-Fun-With-Haxl-1.html)
   Walks through using Haxl from scratch for a simple SQLite-backed
   blog engine.

 * [The N+1 Selects Problem](https://github.com/facebook/Haxl/blob/master/example/sql/readme.md) explains how Haxl
   can address a common performance problem with SQL queries by
   automatically batching multiple queries into a single query,
   without the programmer having to specify this behavior.

 * [Haxl Documentation](http://hackage.haskell.org/package/haxl) on
   Hackage.

 * [There is no Fork: An Abstraction for Efficient, Concurrent, and Concise Data Access](http://simonmar.github.io/bib/papers/haxl-icfp14.pdf), our paper on Haxl, accepted for publication at ICFP'14.

[![Build Status](https://travis-ci.org/facebook/Haxl.svg?branch=master)](https://travis-ci.org/facebook/Haxl)
