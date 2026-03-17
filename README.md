ClojureGP
=========

ClojureGP is a genetic programming framework written in Clojure. It aims to be
highly configurable and versatile while still being concise and easy to learn.


Usage
-----

Please refer to the included documentation and examples in the `./doc/` and
`./examples/` directories. The docs are also available at <http://svdm.github.com/ClojureGP>.

To see a basic genetic programming run in action, load one of the example
experiments as follows, using a terminal/command prompt at the repository root:

    $ java -cp ./examples/;./src/;./lib/* clojure.main -i examples/reg_exp.clj --repl

Then call the `run` function from the Clojure REPL:

    Clojure 1.3.0
    user=> (reg-exp/run)

This same example experiment is discussed in `./doc/reg_example.html`.

*NOTE:* On Linux versions of the JVM, replace the ; in the classpath with : (so
 semicolons to colons).

Dependencies
------------

ClojureGP only *requires* Clojure. Optional extras are available via aliases
in `deps.edn`:

 - `:graph` — live fitness plotting using [XChart](https://knowm.org/open-source/xchart/)
 - `:unc-math` — improved random number generation using [Uncommons Maths](https://maths.uncommons.org/)

License
-------

Copyright (c) Stefan A. van der Meer. All rights reserved.
The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
can be found in the file epl-v10.html at the root of this distribution. By
using this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other, from
this software.
