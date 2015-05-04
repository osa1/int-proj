# Specializing interpreters: A comparison of two approaches
### Ömer Sinan Ağacan

## Introduction

The idea of specializing interpreters on programs is known for a long time.
It's known to be described for the first time by Futamura in his seminal 1971
paper, [Partial Evaluation of Computation Process -- An approach to a
Compiler-Compiler](https://cs.au.dk/~hosc/local/HOSC-12-4-pp381-391.pdf), but
also realized by Turchin(inventor of supercompilation) independently[^1].

In this short experiment we explore two approaches of specializing interpreters
on programs. We perform interpreter specialization using two different
methods: Partial evaluation and multi-stage programming. Our object language
is [Unlambda](http://www.madore.org/~david/programs/unlambda/): A variant of SKI
combinator calculus that has some extensions like "read character", "write
character", "compare read character with X" and "call/cc" functions.

For the meta language with partial evaluator, we use
[Idris](http://www.idris-lang.org/), which is the only relatively mature
language with a partial evaluator that we could find[^2]. For the language with
multi-staging constructs, we use
[MetaOCaml](http://okmij.org/ftp/ML/MetaOCaml.html).

These two methods, partial evaluation and multi-stage programming, represent two
ends of the program specialization tools spectrum: Partial evaluation is
completely automated, it's not possible to give hints or guide a partial
evaluator[^3]. In the other end of the spectrum is multi-stage programming,
which is completely manual. A programmer must annotate program terms with
constructs like MetaOCaml's "bracket", "escape"(or "splice"), and "run" to be
able to generate code and run it[^4].

A partial evaluator for language L is a compiler that takes an L program, a
subset of it's inputs, and generates another L program that is optimized for the
given input(e.g. specialized). Generated L program now doesn't take that input
in runtime.

A multi-stage language L is a language that provides constructs for generating L
code. Note that code generation in multi-stage language happens in runtime. To
support this a multi-stage language should provide a way to generated code. One
advantage of this is that it supports writing code generators that when run,
generates another code(which may be a code-generating-code again), but for our
purposes this is not very important.

Implementing interpreter specializers involves lots of tricky to solve problems,
and some of those problems are undecidable. For example, we can't know in
general if the program we're specializing our interpreter on terminates on all
inputs. In this experiment we compare tools for these properties:

* Once we had an interpreter, how hard was it to convert it to a specializers?
  Note that by itself this question doesn't mean anything: It's very hard to
  write specializers that do exactly the same thing in both languages.

* Termination checking is undecidable, which means we can't know if our
  specializers will loop forever and we should implement conservative
  specializers. Were we able to implement specializers with different powers?
  For example, we should be able to write specializers that

  1. Specializes everything, but loops is input program loops.
  2. Specializes a lot, doesn't loop if program loops depending on a dynamic
     input. (e.g. we don't specialize cases of conditionals if the condition is
     dynamic, that is, not known in specialization time)
  3. Fairly conservative, but specializes some specified cases. (will be
     described later)
  4. Only parses the program in specialization time. This is most basic form of
     compilation.


---

[^1]: Mentioned in [Intorduction to Supercompilation, Sørensen and
Glück](http://link.springer.com/chapter/10.1007%2F3-540-47018-2_10), without
referencing Turchin's papers. It's not clear when Turchin realized the idea and
whether he published it or not.

[^2]: Partial evaluator of Idris is described
[here](https://github.com/idris-lang/Idris-dev/wiki/Static-Arguments-and-Partial-Evaluation).

[^3]: But it's possible to guide a partial evaluator for some program
transformations, using staged interpreters. We'll explore this in this
experiment and it's also described in [Realistic Compilation by Partial
Evaluation, Sperber and
Thiemann](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.48.5519&rep=rep1&type=pdf).

[^4]: There are some alternatives to this approach. One example is "lightweight
modular staging", described in [Lightweight Modular Staging, A Pragmatic
Approach to Runtime Code Generation and Compiled DSLs, Rompf and
Odersky](http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf), which
allows staging by changing only type annotations.
