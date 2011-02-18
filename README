-*- markdown -*-

ClojureGP
=========

ClojureGP is a genetic programming framework written in Clojure. It aims to be
highly configurable and versatile while still being concise and easy to learn.


Usage
-----

Please refer to the included documentation and examples in the `./doc/` and
`./examples/` directories.

To see a basic genetic programming run in action, load one of the example
experiments as follows, using a terminal/command prompt at the repository root:

`$ java -cp ./examples/;./src/;./lib/* clojure.lang.Repl examples/reg_exp.clj `

Then call the `run` function from the Clojure REPL:

`  Clojure 1.1.0 `  
`  user=> (reg-exp/run)  `  

This same example experiment is discussed in `./doc/reg_example.html`.

*NOTE:* On Linux versions of the JVM, replace the ; in the classpath with : (so
 semicolons to colons).

Dependencies
------------

ClojureGP only *requires* Clojure 1.1.0 and the Clojure Contrib 1.1.0
library. For additional features such as plotting and improved random number
generation, the QN Plot library and Uncommons Maths library are needed.

Both Clojure and Uncommons Maths can be pulled in using Leiningen, via
`lein deps`.

QN Plot will have to be manually downloaded if you want to use the graphing
features. It can be found at the following locations:
- <http://quies.net/java/math/plot/>
- <http://sourceforge.net/projects/qn-plot/>

License
-------

Copyright (c) Stefan A. van der Meer. All rights reserved.
The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
can be found in the file epl-v10.html at the root of this distribution. By
using this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other, from
this software.
