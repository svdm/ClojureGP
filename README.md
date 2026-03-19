# ClojureGP

ClojureGP is a genetic programming framework written in Clojure. Genetic
programming is an evolutionary technique that automatically generates
programs (in this case Clojure functions) by iteratively evolving a
population of candidate solutions toward some goal. ClojureGP aims to be
highly configurable while remaining concise and easy to pick up.

A GP run is represented as a lazy sequence of evolutionary generations, which fits
naturally with Clojure's sequence model. This means you can work with a
run using ordinary sequence operations: `map` statistics over it as each
generation is produced, call `last` to get the final result, or otherwise
consume it however you like.

## Quick start

The simplest way to see it in action is to load one of the included examples
from the repository root:

    $ clojure -M -i examples/reg_exp.clj --repl

Then call `run` from the REPL:

    user=> (reg-exp/run)

This runs a symbolic regression experiment (evolving a Clojure expression
to fit a target equation) and prints per-generation fitness statistics
along with a summary at the end. A step-by-step walkthrough of this
example is in [`docs/reg_example.md`](docs/reg_example.md).

## Documentation

The `docs/` directory contains:

- [`overview.md`](docs/overview.md): how ClojureGP works: the lazy sequence model, individual
  representation, and the configuration approach
- [`reg_example.md`](docs/reg_example.md): a walkthrough of the regression example covering all
  the basics, a good place to start
- [`config_reference.md`](docs/config_reference.md): full reference for all configuration keys

The [`examples/`](examples/) directory has five experiments of varying complexity, from
a bare skeleton in [`minimal.clj`](examples/minimal.clj) up to the Santa Fe trail ant problem in
[`ant_exp.clj`](examples/ant_exp.clj).

## Using ClojureGP as a library

ClojureGP is available as a git dependency for the Clojure CLI. Add the
following to your `deps.edn` (see the
[Clojure guide on git libraries](https://clojure.org/guides/deps_and_cli#_using_git_libraries)):

```clojure
io.github.svdm/ClojureGP {:git/tag "v2.0.0" :git/sha "f867199"}
```

ClojureGP only requires Clojure itself. Two optional extras are available
via aliases in `deps.edn`:

- `:unc-math`: example of using a third party RNG using
  [Uncommons Maths](https://maths.uncommons.org/)
- `:graph`: example of live fitness plotting using
  [XChart](https://knowm.org/open-source/xchart/)

## License

Copyright (c) Stefan A. van der Meer. All rights reserved.
Distributed under the Eclipse Public License 1.0, which can be found in
`epl-v10.html` at the root of this distribution.
