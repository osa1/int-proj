# Specializing interpreters: A comparison of two approaches
### Ömer Sinan Ağacan

## Introduction

---

TODO: Add these to somewhere

In most basic sense, specialization is easily possible by just parsing in
specialization time. But that doesn't specialize too much, ideally we'd like to
eliminate all interpretation costs, which is only possible if we could compile
everything to our meta language. On the other hand, we want to base our
specializer on our interpreters. It's tricky to do all these together. What we
do instead is to specialize as much as possible and fall back to interpreter
when we stop.

---

The idea of specializing interpreters on programs is known for a long time.
It's known to be described for the first time by Futamura in his seminal 1971
paper, [Partial Evaluation of Computation Process -- An approach to a
Compiler-Compiler](https://cs.au.dk/~hosc/local/HOSC-12-4-pp381-391.pdf), but
also realized by Turchin(inventor of supercompilation) independently[^1].

In this short experiment we explore two approaches of specializing interpreters
on programs. We perform interpreter specialization using two different methods:
Partial evaluation and multi-stage programming. Our object language is
[Unlambda](http://www.madore.org/~david/programs/unlambda/): A variant of SKI
combinator calculus that has some extensions like "read a character", "write a
character", "compare read character with X" and call/cc functions.

For the meta language with partial evaluator, I'm going to use
[Idris](http://www.idris-lang.org/)[^2]. For the language with multi-staging
constructs, I'm going to use
[MetaOCaml](http://okmij.org/ftp/ML/MetaOCaml.html).

These two methods, partial evaluation and multi-stage programming, represent two
ends of the program specialization tools spectrum: Partial evaluation is
completely automated, it's not possible to give hints to or guide a partial
evaluator[^3]. In the other end of the spectrum is multi-stage programming,
which is completely manual. A programmer must annotate program terms with
constructs like MetaOCaml's "bracket", "escape"(or "splice"), and "run" to be
able to generate code and run it[^4].

Simplest form of interpreter specialization is parsing the object program and
generating meta language representations in specialization time. More advanced
specializations involve evaluating open terms, evaluating branches of
conditionals with dynamic conditions etc. similar to what's done in
supercompilation. Main issue is to not specialize too much or too aggressively.
We don't want our specializer to loop forever when we try to specialize a
looping program. In case of evaluating open terms we can have looping
specializer even if the program is guaranteed to terminate in runtime.

So we have to be conservative. Instead of finding and implementing an heuristic
that terminates for all programs, in current implementation we provide some
tuning parameters to the users to specify how much to specialize. One can start
with a very powerful specialization parameters, and disable some specializations
if his/her program causes specializer to loop.

We compare implementations for these see:

* Once we had an interpreter, how hard was it to convert it to a specializer?
  Note that by itself this question doesn't mean a lot: It may be equally simple
  to get some kind of specializer from an interpreter in both languages with
  very minimal changes, but those specializers will probably do very different
  specializations and will have different properties. Answer to this question
  may still give some ideas.

* Termination checking is undecidable, which means we can't know if our
  specializers will loop forever in general and we should implement conservative
  specializers. Were we able to implement specializers with different powers?
  For example, we should be able to write specializers that

  1. Specializes very aggressively, but loops if input program loops.
  2. Specializes a lot, doesn't loop if program loops depending on a dynamic
     input. (e.g. we don't specialize branches of conditionals if the condition
     is dynamic, that is, not known in specialization time)
  3. Fairly conservative, but specializes some specified cases. (will be
     described later)
  4. Only parses the program in specialization time. This is most basic form of
     compilation. Always terminates.

Couple of notes before describing the implementation and results: What's meant
by "specialization" is sometimes becoming confusing. Let's define it as
eliminating interpretation costs. A more powerful specializer means eliminating
more interpretation costs. Most of the time it's possible to specialize and
optimize at the same time. In this project we explore this: We try to have an
optimizing specializer.

Note that using a multi-stage programming it's sometimes possible to compile
object language programs to meta language programs, eliminating all the
interpretation costs. This is done in `UnlambdaCompiler.ml` (OCaml functions
corresponding to Unlambda builtins are defined in `UnlambdaFuns.ml`). This is
most powerful specialization possible.

Once you start modifying interpreters for specialization it's not clear when to
stop to still have "specialized interpreter" and not a "compiler evaluated by
partial evaluator". If you change it too much, you end up implementing a
compiler that you can use in runtime, as we did in `UnlambdaCompiler.ml`. (you
can try it using `-compile` parameter) In a multi-stage language, both versions
similarly easy to use. In a partial evaluator however, the story is little
different, because you can't run this "compiler" in runtime, so you need some
kind of interpreter at hand at all times.

## Caveats

At the time I did this writing, Idris was pretty unstable, and some of the bugs
actually effected the partial evaluator(see
[#2234](https://github.com/idris-lang/Idris-dev/issues/2234) as an example).
Also, printing partially evaluated programs in Idris is not easy. It doesn't
have any functions for that, so we need to load modules in REPL and inspect them
using `:printdef`. `outputs` directory in source repository keeps all the
outputs mentioned in the text.(I simplified them a lot and added some comments
to make them easier to read)

## Implementation

We start by implementing interpreters in both languages. OCaml implementations
is given in `UnlambdaInterp.ml`, Idris implementation is `eval` in
`Unlambda.idr`.

This is important for two reasons: 1) We make sure we get the semantics right
2) Because of the reasons described above, we need to be conservative in
specializers. When we stop specializing, we need to call interpreter to continue
evaluation. This is implementing using interpreters, e.g. we fall back to
interpretation.

To make implementation of call/cc easier, we implement interpreters in
continuation passing style, and save/restore interpreter continuations(which are
represented as functions) when we see call/cc or continuation application.

Our interpreters have one state that persists even across continuation calls:
current character. `@` function in Unlambda is used to read a character. That
character should then be saved. `|` function is used to compare "current
character" with a given character, and branch accordingly. Thus we have an extra
argument for "current character" in our interpreters and specializers.

In the specializers we won't be only manipulating terms, we'll also be
manipulating continuations. And when we want to fall back to the interpreter,
we'll need to pass a continuation to it. This means we need to convert
continuations of manipulated in specializers to continuations for interpreters.

This is tricky to do if we represent continuations as functions in our meta
level operations. As a solution, we implement same interpreters using ADT
implementation of continuations.

---

```idris
data Continuation
  = DelayGuard ExpS
  | ApplyTo ExpS
  | ApplyDelayed ExpS
```

TODO: Explain

---

Now we can pass continuations from specializers to interpreters without any
issues. They'll be interpreted differently by those functions. MetaOCaml
implementation is in `UnlambdaCont.ml` and Idris implementation is `eval_cont`
in `Unlambda.idr`.

## Compilation with MetaOCaml (multi-stage programming)

Implementation of staged interpreter is given in `UnlambdaStaged.ml`. It's based
on `UnlambdaCont.ml` interpreter. Only differences are:

* We add brackets(`.< ... >.`) and escapes(`.~`) for staging.
* We add checks for optimizations parameters.

As an example, here's the code that evaluates an `S` application in our
interpreter:

```ocaml
  match e1 with
  ...
  | S2_S (x, y) ->
      eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont
  ...
```

And here's staged version:

```ocaml
  match e1 with
  ...
  | S2_S (x, y) ->
      if opts.eval_S then
        eval (Backtick_S (Backtick_S (x, e2), Backtick_S (y, e2))) cc cont
      else
        .< UnlambdaCont.eval (Backtick_S
                               (Backtick_S ( .~ (lift_exp_s x), .~ (lift_exp_s e2)),
                                Backtick_S ( .~ (lift_exp_s y), .~ (lift_exp_s e2))))
                             cc .~ (lift_conts cont) >.
  ...
```

If `-eval-S` is given, then we continue specializing the term using staged
interpreter. Otherwise we generate a code that falls back to interpreter. This
case is exactly the same with our interpreter's handler for `S`, except we add
brackets and `lift` calls to be able to lift current terms to code values of
next stage.

We implement other specializations similarly. If we remove the tuning
parameters, our staged interpreter exactly the same with non-staged version in
most cases, we just wrap things with brackets, escapes and lifts[^5].

Some example executions:

* `Hello.unl` is a "hello world" program that prints "Hello world" a couple of
  times. If we run staged interpreter with `-staged -eval-S`, and print generated code, our MetaOCaml
  implementations generates this code:
  ```ocaml
  let _ = Pervasives.print_char 'H' in
  let _ = Pervasives.print_char 'e' in
  let _ = Pervasives.print_char 'l' in
  let _ = Pervasives.print_char 'l' in
  let _ = Pervasives.print_char 'o' in
  let _ = Pervasives.print_char ' ' in
  let _ = Pervasives.print_char 'w' in
  let _ = Pervasives.print_char 'o' in
  let _ = Pervasives.print_char 'r' in
  let _ = Pervasives.print_char 'l' in
  let _ = Pervasives.print_char 'd' in
  let _ = Pervasives.print_char '!' in
  let _ = Pervasives.print_char '\n' in
  ... repeats a couple of times ...
  Syntax.I_S>.
  ```
  With this specialization parameters, what we had in effect is that we compiled
  the Unlambda program to OCaml, we eliminated all the interpretation costs
  without actually writing a compiler. Another interesting thing is that we
  unrolled the loop, which we do all the time with `-eval-S`. (this is why it
  leads to loops in most programs)
* `Trivial.unl` is a program that prints "Unlambda, c'est trivial!" forever. If
  we run it using `-staged -eval-S`, it loops because the program is looping
  using `S` function. If we use `-staged -eval-cc`, it generates the code listed
  in `outputs/trivial_metaocaml`. This code is not very different than
  `-parse-only` generated code, because without `-eval-S`, we just fall back to
  interpreter without trying to specialize arguments of `S`. I think this
  improvement can be implemented without too much trouble, but current version
  doesn't do this.
* `eof-test.unl` is a program that tries to read a character. It prints 'T' when
  it succeeds and 'F' otherwise(e.g. EOF happens). With `-staged -eval-S` it
  specializes the code a lot, but leaves the code with a conditional with
  non-specialized branches. We can use `-eval-eof` parameter to specialize one
  branch of the conditional with the assumption that condition is true. Here's
  the generated code without `-eval-eof`:
  ```ocaml
  UnlambdaCont.apply Syntax.I_S Syntax.V_S None
    [Syntax.ApplyTo
       (Syntax.S2_S
          ((Syntax.K1_S Syntax.C_S),
            (Syntax.S2_S
               ((Syntax.K1_S
                   (Syntax.S1_S (Syntax.K1_S (Syntax.K1_S Syntax.K_S)))),
                 (Syntax.S2_S
                    (Syntax.S_S,
                      (Syntax.K1_S (Syntax.K1_S (Syntax.K1_S Syntax.I_S)))))))));
    Syntax.DelayGuard (Syntax.Print_S 'F');
    Syntax.DelayGuard (Syntax.Print_S 'T');
    Syntax.DelayGuard Syntax.I_S;
    Syntax.ApplyTo (Syntax.Print_S '\n')]
  ```
  It falls back to the interpreter. Here's the code generated with `-eval-eof`:
  ```
  let _ = Pervasives.print_char 'F' in
  let _ = Pervasives.print_char '\n' in Syntax.I_S
  ```
  It evaluated the term in specialization time and generated OCaml code that
  prints 'F' and returns the value of the term.

To print generated code and run it `-run` argument can be used. `-parse-only`
only parses the code in code-generation time. Generated program is an
interpreter and it's specialized for the given program, but no optimizations are
done.

## Compilation with Idris (partial evaluation)

Specializing Idris implementation of the interpreter is just a matter of adding
some `[static]` annotations in interpreter functions. But it has the obvious
problem: It loops in compile time for most programs.

One might think that implementing a simple compilation like MetaOCaml
implementation's `-parse-only` is easily possible by just adding `[static]` to
parsing functions and not adding it to evaluator. However, for some reason,
doing that doesn't prevent Idris from partially evaluating the interpreter. For
example, in `Unlambda.idr`, `peHello` is compiled to a sequence of `putchar`
calls by the partial evaluator, even though we don't have `[static]` in
`eval` function. Similarly, `peLoop` loops. I assume that this should have
worked and it's a bug in Idris.

Tuning the specializer is a lot tricky when compared with MetaOCaml
implementation, in which case we just had to add a couple of conditionals and
brackets. We need to add some optimization functions that does the optimizations
we want. These functions should have type `Exp -> (Exp, [Continuation])` and
they have to be terminating on all inputs. We then need to make sure those
functions are applied completely in compile time by the partial evaluator. This
is the approach used by Sperber and Thiemann in '96 paper(see [^3]).

For this purpose we implement `eval_static`, which unlike the name doesn't
actually evaluate, it's an optimizer. It does the transformations depending on
the optimization parameters.

This approach of applying optimizers statically using partial evaluation may
look OK at first sight, but it's actually a lot less flexible. As an example,
see implementation of `-eval-eof` optimization in `apply_static` in
`Unlambda.idr`. We can evaluate the branch statically, but incorporating the
result with existing code is tricky. We need to generate a term of object
language that reads a character, takes the optimized branch if it's failed, and
takes the other branch otherwise. Thankfully, in this case this is possible,
because we can generate an object language term that does exactly that. The
problem is, evaluating that term takes more than 20 steps. If we didn't do a lot
of optimizations in EOF branch, then we may end up de-optimizing the program by
replacing a small term with a big one that takes a lot more to reduce. (see also
comments in Idris file)

What we did in MetaOCaml is that we generated OCaml code that does this. It was
this code:

```ocaml
let _ = Pervasives.print_char 'F' in
let _ = Pervasives.print_char '\n' in Syntax.I_S
```

But in partial evaluation our optimizers have to return object language terms.
We're not generating Idris code that represents runs optimized procedures and
returns same value that our object language term returns, like we did in
MetaOCaml.

It should be possible to do exactly the same thing using partial evaluation, but
it's not clear to me how to do this. In the worst case we can cheat and add a
new term to the object language that does whatever we want, and then generate
that syntax in optimizers.

Idris implementation has optimized and non-optimized versions of programs we
described in MetaOCaml section. However, it optimizes less because of the
limitation mentioned above, and generated code is very hard to read and
understand, because unlike MetaOCaml, Idris is printing internal representation
of Idris AST instead of programs written in concrete Idris syntax.

# Discussions and results

MetaOCaml implementation was almost trivial. After initial interpreter I
followed these steps:

* I implemented a multi-stage version by only adding some bracket/escapes to
  original interpreter.
* It worked fine, but interpreter was looping most of the time. I decided to
  stop staged interpreter and generate code that calls normal interpreter in
  some places to avoid looping.
* This wasn't possible because two interpreters were using different
  representations of continuations. Defined an alternative representation of
  continuations to be able to evaluate them in both interpreters.
* Added couple of conditions to tune optimizations.

Staging constructs leave no room for confusion -- it's trivial to see when we're
falling back to interpreter, when we're specializing sub-terms and combining
results etc.

In the partial evaluation side, I had two major problems. First one is related
with the idea itself. Partial evaluation is completely automated. It's not easy
to see what will the generated code be, or when a partial evaluator loops.

For example, `peHello` loops, even though I'd expect it to just parse in
compilation time and doesn't do any further processing.

To make sure partial evaluation is doing the work I'm expecting it to do, I need
to print the code and make sure.

It seems like partial evaluation is too complicated to completely automate.

Second problem is that even though staged interpreters(as described in [^3])
help with getting optimizations done in partial evaluation, I think it's still
more limited than multi-stage programming approach, as already demonstrated in
the case of optimized branches. Even if it's possible to optimize this case like
in MetaOCaml implementation, it's not clear how to do so, whereas in MetaOCaml
it was trivial.

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
allows staging by changing type annotations. TODO: Improve this part. IIRC, LMS
also needs some kind of overloading to be able to represent some terms as code.

[^5]: One thing to note here is that explicit lifting should not be necessary,
and it's adding a lot of noise to the code. But in practice MetaOCaml is failing
to print lifted(CSP) values. The whole story is long and a bit complicated, so
let me just refer interested readers to the related Caml-list discussion:
[Starts here](https://sympa.inria.fr/sympa/arc/caml-list/2015-04/msg00151.html),
[and continues
here](https://sympa.inria.fr/sympa/arc/caml-list/2015-05/msg00031.html).
