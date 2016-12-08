# Changes in version 0.5.0.0
  * Rename 'Show1' to 'ShowP' ([#62](https://github.com/facebook/Haxl/issues/62))

# Changes in version 0.3.0.0

  * Some performance improvements, including avoiding quadratic
    slowdown with left-associated binds.

  * Documentation cleanup; Haxl.Core is the single entry point for the
    core and engine docs.

  * (>>) is now defined to be (*>), and therefore no longer forces
    sequencing.  This can have surprising consequences if you are
    using Haxl with side-effecting data sources, so watch out!

  * New function withEnv, for running a sub-computation in a local Env

  * Add a higher-level memoization API, see 'memo'

  * Show is no longer required for keys in cachedComputation

  * Exceptions now have `Eq` instances
