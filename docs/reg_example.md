# ClojureGP: Experiment Example

In this document we will walk through the construction of the regression
example, available in **examples/reg\_exp.clj**. Some familiarity with Clojure
(or any other Lisp) is assumed. You should also read the [Overview](./overview.md)
article if you have not done so yet.

## Building a simple regression experiment

### Preparations

Create a **.clj** file with an appropriate name, like **reg\_exp.clj**.
Alternatively, open **examples/reg\_exp.clj** as included in the ClojureGP
package and follow along there.

It's good practice to keep an experiment in its own namespace, but this is not a
technical requirement.

```clojure
(ns reg-exp
  "Example 02: evolving a solution to a simple regression problem."
  (:require [cljgp.core :as core]
            [cljgp.config :as config :refer [prim]]
            [cljgp.random :as random]
            [cljgp.util :as util]
            [cljgp.tools.logging :as log]))
```

We will be using a few functions provided by ClojureGP from the listed
namespaces.  Besides the core function that will generate our GP run
(`core/generate-run`), we use `config/make-func-template` and `prim` to help
construct our experiment configuration. As our evaluation will use random
values, we need `random/gp-rand`. Then there's `util/tree-depth`, which we will
use in a tree validator function that will limit the size of the evolved
expression trees. Lastly, two logging functions that will generate some useful
output: `log/print-stats` and `log/reduce-to-summary`.

### Define an evaluation function

We will need to define a evaluation function that compares the results of an
evolved function to the target results. In context of the regression problem,
this involves computing the expected result of the target equation given certain
values of the variables involved. This target result then needs to be compared
to the result of applying the evolved function with the same values.

For this example we will use a simple target equation:
`z = x^2 * y^2 + x^4 + x * y`

The following function evaluates a given evolved function on a single random `x,
y` instance.

```clojure
(defn test-reg-once
  "Evaluate the given function on a single result, returning the abs error."
  [func]
  (let [x (gp-rand)
        y (gp-rand)
        target (+
                (+ (* (* x x) (* y y)) (* (* x x) (* x x)))
                (* x y))                ; z = x^2 * y^2 + x^4 + xy
        result (func x y)]
    (Math/abs (float (- target result)))))
```

First we use `random/gp-rand` to generate our random `x` and `y`. It is important to
use `random/gp-rand`, as it will be bound at runtime to a function that uses the
correct, thread-local, seeded RNG. This allows us to reproduce results and avoid
bottlenecking multi-threaded performance on a single synchronised RNG.

Then we compute the target value given those `x` and `y`, and the result the
evolved function produces for the same values. Because the given evolved `func`
is a proper Clojure function, we can simply apply it with `x` and `y` as its
arguments. We will set up our experiment configuration for this later on in this
tutorial.

Lastly, the absolute error is returned, as a convenient measure of the quality
of the result.

Evaluating on a single random instance is a bit unreliable, so we will
instead sum the result of a number of such tests, for our final evaluation function:

```clojure
(defn evaluate-reg
  "Evaluate the given function on multiple results and return a fitness."
  [func]
  (reduce + (take 10 (repeatedly #(test-reg-once func)))))
```

We simply repeatedly apply `test-reg-once` to the evolved function and sum the
errors. If all results are identical to the target, the total error will be 0,
which equates to a perfect fitness. Larger errors mean higher and therefore
worse fitness.

### Define a type hierarchy

At this point we would normally consider what types of values are playing a role
in the experiment, and use Clojure's ad hoc hierarchies with `derive` to create
an appropriate type hierarchy.

For this experiment however, we are evolving an equation, which only involves a
bunch of numbers. All function and terminal nodes will be of the same type,
which makes the experiment as a whole essentially untyped.

With some creativity we can still take a look at deriving type hierarchies:

```clojure
(derive ::number ::any)
(derive ::string ::any)
(derive Number ::number)
```

We pick `::any` as our root type here. This is arbitrary, as Clojure hierarchies
are very free-form, and let you use any keyword you want. We `derive` the
`::number` keyword from `::any`. This means we can define a function in our
experiment configuration's function set that requires an argument of type
`::any`, meaning the GP process can use any other node that has either the type
`::any`, or the type `::number`, because `(isa? ::number ::any)` is true.

Then we also derive `::string` from `::any`, meaning `(isa? ::string ::any)`
will be true, and for the hypothetical function mentioned just now any node of
type `::any`, `::number` or `::string` could be used.

Lastly we derive `Number`, that is the class `java.lang.Number`, from
`::any`. Java classes can (only) form the leaves in a Clojure hierarchy, and can
be used as types in ClojureGP experiments. Any Java subclasses will behave
properly, i.e. `(isa? Float Number)` is true, which is something you can also
employ in defining your types. In practice however, it's often nice to just
define a custom keyword hierarchy, as it is much easier to reorganise.

In this experiment, we will just define every type as being `Number`.

### Define an experiment configuration

The experiment configuration is a hashmap containing all the information the GP
process needs to run. From the function and terminal sets it should use, to the
evaluation function to use, to how many threads it should run in.

#### Function set and terminal set

Starting off with the function and terminal sets:

```clojure
(def config-reg
     {
      ;; Some mathematical operators
      :function-set [(prim `- {:gp-type Number
                               :gp-arg-types [Number Number]})

                     (prim `+ {:gp-type Number
                               :gp-arg-types [Number Number]})

                     (prim `* {:gp-type Number
                               :gp-arg-types [Number Number]})]

      ;; The two variables in the equation
      :terminal-set [(prim 'x {:gp-type Number})

                     (prim 'y {:gp-type Number})]

      ;; More to come...
      ...
      })
```

We define two keys in the map: [:function-set](./config_reference.md#function-set)
and [:terminal-set](./config_reference.md#terminal-set). As the
[Configuration Reference](./config_reference.md) will tell you, both take a
sequence of symbols. These symbols will be available for use by the GP process
in generating and breeding expression trees. Besides the symbols themselves, we
need to specify their type information. For both functions and terminals, this
includes what type of value they return. For functions we also need to define a
sequence of types that defines what type each of their arguments should be.

This type information is stored in the symbol's metadata. ClojureGP provides the
helper `primitive`, which takes a symbol and a type info map, performs some
error checking, and stores the map as the symbol's metadata. In the above
example the abbreviation `prim` is used, because the type definitions can get
crowded.

Let's take a closer look at the `+` function symbol definition. The types are
straightforward: applying `+` will result in a number, so `:gp-type` should be
`Number`. It takes two arguments, the first of which should be a `Number`, and
the second of which should be a `Number`. Hence `:gp-arg-types` is the vector
`[Number Number]`. This data is stored in a map, which is attached to this `+`
symbol as metadata.

Besides the type information, the symbol itself has an important property. We
will be using Clojure's built-in `+`, that is, `clojure.core/+`. Because we
don't want to make any assumptions about the context in which the tree will be
evaluated (where `'+` might evaluate to `my.crazy/+`, or more realistically
where a custom function will simply not be defined), we prefix it with a
syntax-quote, or backquote. This tells Clojure's reader to resolve the symbol to
its namespace right here. For `` `+ ``, this results in `clojure.core/+`, which is
exactly what we want.

You should resolve all symbols that will **not** be passed in as arguments by
backquoting them. This simple guideline will prevent all issues with symbols
resolving to the wrong thing. Or more likely, not resolving to anything: if we
define a function `foo` in our experiment namespace, and add it to our function
set as `'foo`, `eval` will not be able to resolve the symbol. As far as it knows
`foo` does not exist. Using `` `foo ``, will resolve it to `` `my.ns/foo ``, which
tells `eval` where to look and will prevent the issue.

The other two elements in the function set are similar, so we will move on to
the terminal set.

Much is the same there. We use `prim` to get a symbol with the right type data
attached. Both terminals, `x` and `y`, are of type `Number`. They are terminal
nodes of the tree, also known as "leaves" or a number of other terms. They will
not have arguments (child nodes in the tree), and we do not need to define types
for those.

We will pass `x` and `y` to our evolved functions as arguments when we evaluate
them, as we can see when we look back to our evaluation function. As per the
guideline given earlier, we should not resolve the symbols here, but leave them
as "clean" symbols, with a normal quote: `'x` and `'y`. In a moment we will
define our evolved functions' argument list to include these two symbols.

#### The other bits

With the function and terminal sets defined, most of our work is done. We only
need to define a few more things, and can let the rest fall back to defaults.

To start off with something easy: the [:root-type](./config_reference.md#root-type)
key should be set to the type that the root of the tree should satisfy. In this
case, that is simply `Number`.

As mentioned earlier, we need to specify that we want the argument list of
evolved functions to be `[x y]`. We can do this using the function template
functionality, by setting the [:func-template-fn](./config_reference.md#func-template-fn)
key with some help from `cljgp.config/make-func-template`.

Next up, we specify the `evaluate-reg` function we defined earlier as the value
of [:evaluation-fn](./config_reference.md#evaluation-fn), so that it will be
used to generate fitness values for all individuals during the evaluation phase.

```clojure
(def config-reg
     {
      ... ; snip

      ;; Evolved functions must return a number
      :root-type Number

      ;; Basic template for a fn with our arguments
      :func-template-fn (make-func-template '[x y])

      :evaluation-fn evaluate-reg

      ... ; snip
      })
```

Fairly straightforward. Take a look at the documentation for these keys linked
above if you're interested in the details.

The last few bits we will discuss here concern tree validation, the size of the
population, the number of threads and the RNG seeds.

First up is [:validate-tree-fn](./config_reference.md#validate-tree-fn). The
function specified for that key will be used to check every tree that is
generated. If it returns false, the tree is scrapped and a new one will be
generated. This can be useful to prevent excessively large trees, and that is
what we will use it for here. We simply define an anonymous function on the
spot: `#(< (util/tree-depth %) 10)`. In other words: the depth of the given tree
must be smaller than 10 levels. The `util/tree-depth` function comes from `cljgp.util`,
which also has a `util/tree-size` function for example.

Next is the [:population-size](./config_reference.md#population-size). This
value determines the number of individuals in the population, as you would
expect.

The value for [:threads](./config_reference.md#threads) determines the number
of threads used by ClojureGP to split up the work over multiple cores. Check the
linked documentation for more detail.

Lastly, [:rand-seeds](./config_reference.md#rand-seeds) should be a seq
containing the seeds that will be used to initialise the thread-local RNGs.
Hence, there should be at least as many seeds as there are threads. In
`cljgp.config` a function is provided that creates a lazy seq of seeds generated
from the time, but here we will simply set two arbitrary integers.

```clojure
(def config-reg
     {
      ... ; snip

      ;; Keep tree size sane
      :validate-tree-fn #(< (tree-depth %) 10)

      :population-size 128

      :threads 2

      :rand-seeds [9234 5327]
      })
```

#### Final configuration

This hard work results in the following configuration:

```clojure
(def config-reg
     {
      ;; Some mathematical operators
      :function-set [(prim `- {:gp-type Number
                               :gp-arg-types [Number Number]})

                     (prim `+ {:gp-type Number
                               :gp-arg-types [Number Number]})

                     (prim `* {:gp-type Number
                               :gp-arg-types [Number Number]})]

      ;; The two variables in the equation
      :terminal-set [(prim 'x {:gp-type Number})

                     (prim 'y {:gp-type Number})]

      ;; Evolved functions must return a number
      :root-type Number

      ;; Basic template for a fn with our arguments
      :func-template-fn (make-func-template '[x y])

      :evaluation-fn evaluate-reg

      ;; Keep tree size sane
      :validate-tree-fn #(< (tree-depth %) 10)

      :population-size 128

      :threads 2

      :rand-seeds [9234 5327]
      })
```

Many settings were left on their defaults, such as the functions that will be
used to breed new individuals. When you generate a run from this configuration
using `core/generate-run`, it will tell you about the keys that fell back to
defaults, as we will see in a moment.

### Run the experiment

To finish off this tutorial we will define a `run` function so we can
conveniently run the experiment from the REPL when we want to, and we'll take a
look at the output it generates.

#### Run function

If you have read the [Overview](./overview.md), you may recall that performing a
GP run in ClojureGP entails constructing a lazy seq of successive generations
using `core/generate-run`, and then consuming it until the last generation is
reached, which is the one for which the end condition has been
reached.[^end]

For this tutorial we are not really interested in advanced usage, so we will use
some of the functions provided by ClojureGP to handle this for us and give some
interesting information on the way:

```clojure
(defn run
  "Run experiment and print summary when done."
  ([]
     (run :basic))
  ([print-type]
     (log/reduce-to-summary
      (map #(log/print-stats print-type %)
           (core/generate-run config-reg)))))
```

Here `log/reduce-to-summary` consumes the sequence, tracking some statistics
about the run as a whole, and printing the best individual at the end along
with the stats. We also map `log/print-stats` over the sequence of generations,
so that we get fitness statistics of each generation immediately after it has
been generated. We leave out stuff like logging to a file for this example.

At this point we are done defining things, and we can try running the
experiment.

#### Performing a run

Personally, I find it convenient to simply run experiments in a REPL. It is
usually possible to use `clojure.core/load` to quickly and easily reload the
experiment after fixing some mistakes (or you could do it directly in the REPL,
just don't forget to apply the changes in the file as well if they work out). As
an example of performing a run, we will look at running the **reg-exp** example
experiment, which is virtually identical to the experiment we defined in this
tutorial.

From the repository root, we run:

```
clojure -M -i examples/reg_exp.clj --repl
```

This should give us a REPL prompt with the **reg\_exp.clj** file already loaded. We can
then simply run the experiment as follows:

```
user=> (reg-exp/run)
Run configuration preprocessing report:
  NOTE: The following keys were missing, using default values:
     (:rand-fn-maker :pop-generation-fn :end-condition-fn :breeding-retries :selection-fn :breeders)

Gen 000: Best: 1.39 -- Worst: 58.17 -- Avg: 8.69
Gen 001: Best: 0.92 -- Worst: 15.10 -- Avg: 3.52
Gen 002: Best: 0.86 -- Worst: 14.50 -- Avg: 3.13
Gen 003: Best: 0.96 -- Worst: 6.99 -- Avg: 2.80
Gen 004: Best: 0.65 -- Worst: 5.92 -- Avg: 2.57
Gen 005: Best: 0.77 -- Worst: 13.61 -- Avg: 2.89
Gen 006: Best: 0.78 -- Worst: 10.03 -- Avg: 2.92
Gen 007: Best: 0.57 -- Worst: 9.57 -- Avg: 2.64
Gen 008: Best: 0.49 -- Worst: 8.34 -- Avg: 2.68
Gen 009: Best: 0.27 -- Worst: 16.35 -- Avg: 2.55
Gen 010: Best: 0.45 -- Worst: 8.90 -- Avg: 2.43
Gen 011: Best: 0.19 -- Worst: 16.71 -- Avg: 2.55
Gen 012: Best: 0.22 -- Worst: 15.57 -- Avg: 2.38
Gen 013: Best: 0.27 -- Worst: 8.46 -- Avg: 2.19
Gen 014: Best: 0.28 -- Worst: 7.33 -- Avg: 1.69
Gen 015: Best: 0.18 -- Worst: 13.83 -- Avg: 1.99
Gen 016: Best: 0.00 -- Worst: 9.96 -- Avg: 1.59

#################
  Run complete
#################
Best individual of entire run:
Data of individual:
  Generation: 16
  Fitness: 3.1918912E-16
  Function:
(fn [x y] (* x (+ (+ (* x (* x x)) (* y (* y x))) y)))

Run statistics:
  Total time:      1954.50 msecs
  Inds. evaluated: 2176
#################

nil
user=>
```

With our basic reporting setup, we only get some fitness data for each
generation. At the end of the run, the summary tells us a few things about the
best individual, and reports the time it took.

### In conclusion

In this tutorial we have taken all the basic steps that one needs to take in
order to run a ClojureGP experiment, discussing a lot of different points of
interest along the way.

A next step in learning about ClojureGP would be to look at the other included
examples, looking up any configuration keys that are unfamiliar in the
[Reference](./config_reference.md). Of course, the rest of the documentation
should also be of interest, if you have not seen it yet. Lastly, you may be
tempted to take a look at the ClojureGP source to gain a deeper understanding.
All functions are documented in a (hopefully) useful manner.

[^end]: We left the [:end-condition-fn](./config_reference.md#end-condition-fn) at the default, which at the time of writing is max. 100 generations or an individual with a fitness extremely close to 0.

