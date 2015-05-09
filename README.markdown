# Building and running

Make sure Idris and MetaOCaml are in the $PATH, and run `make`.

```
➜  unlambda git:(master) ✗ make
idris Unlambda.idr -o unlambda_idris
metaocamlc -c Syntax.ml -g
metaocamlc -c Syntax.cmo UnlambdaInterp.ml -g
metaocamlc -c UnlambdaCont.ml -g
metaocamlc -c Syntax.cmo Lift.ml -g
metaocamlc -c CmdArgs.ml -g
metaocamlc -c Syntax.cmo Lift.cmo UnlambdaCont.cmo UnlambdaStaged.ml -g
metaocamlc -rectypes -c UnlambdaFuns.ml -g
metaocamlc -rectypes -c UnlambdaFuns.cmo UnlambdaCompiler.ml -g
metaocamlc -rectypes Syntax.cmo UnlambdaInterp.cmo UnlambdaCont.cmo Lift.cmo CmdArgs.cmo UnlambdaStaged.cmo UnlambdaCompiler.cmo UnlambdaMain.ml -o unlambda_metaocaml -g
```

Two executables will be generated:

* `unlambda_idris`: Idris implementation.
* `unlambda_metaocaml`: MetaOCaml implementation.

To see effect of partial evaluation we have to load Idris implementation to the
REPL and print definitions.

```
➜  unlambda git:(master) ✗ idris Unlambda.idr
*Unlambda> peHello
^Cuser interrupt <- interrupted, loops forever
*Unlambda> peEOFTest1
(prints huge Idris AST)
```

The `unlambda_idris` executable just runs the reference interpreter.

MetaOCaml implementation comes with command line arguments for tunning the specializer:

```
➜  unlambda git:(master) ✗ ./unlambda_metaocaml -help

  -staged Run staged interpreter.
  -compile Compile and run compiled program.
  -run Run specialized code. Makes sense only in staged mode.
  -parse-only Do only parsing in specialization time
  -eval-S Specialize applications of S. WARNING: Results in loops most of the time.
  -eval-eof Specialize EOF branches of "read"('@') calls.
  -eval-cc Specialize continuation calls. WARNING: May result in loops.
  -help  Display this list of options
  --help  Display this list of options
```

Some example executions:

* `$ ./unlambda_metaocaml programs/Hello.unl -staged -eval-S`
* `$ ./unlambda_metaocaml programs/trivial.unl -staged` loops when `-eval-S` is added.
* `$ ./unlambda_metaocaml eof-test.unl -staged -eval-S -eval-eof`
* Add `-run` to the mix to run generated code.
* Use `-parse-only` to run minimal amount of specialization. (only does parsing
  in specialization time)

### Bonus content

`programs/Jerabek-unlambda1.unl` is an Unlambda interpreter written in
Unlambda. It's not useful for anything(exercise: We can't generate a second
Futamura projection using it, why?), but you can still have some `mind = blown`
moments using MetaOCaml implementation with `-staged` argument.

Example:

```
$ cat programs/Hello.unl | ./unlambda_metaocaml programs/Jerabek-unlambda1.unl -staged -eval-S -eval-cc -eval-eof
```

~~Interestingly, this doesn't print same program that `./unlambda_metaocaml
programs/Hello.unl -staged -eval-S -eval-cc -eval-eof` prints, but the
specialization is correct, `-run` works correctly. Figuring out why generated
code is different may be an interesting exercise.~~

Apparently my sleep deprived brain has failed me. It's obvious that these two
commands can't generate same programs:

```
$ cat programs/Hello.unl | ./unlambda_metaocaml programs/Jerabek-unlambda1.unl -staged -eval-S -eval-cc -eval-eof
$ ./unlambda_metaocaml programs/Hello.unl -staged -eval-S -eval-cc -eval-eof
```

Because first command isn't expecting any inputs at all. It's just compiling
the interpreter. We need a partial evaluator if we want that command to compile
programs.

I implemented `-partial-eval` argument, and now we can use the interpreter for
compiling programs. These two commands generate same programs:

```
$ cat programs/Hello.unl | ./unlambda_metaocaml programs/Jerabek-unlambda1.unl -staged -eval-S -eval-cc -eval-eof -partial-eval
$ ./unlambda_metaocaml programs/Hello.unl -staged -eval-S -eval-cc -eval-eof
```

For a truly `mind = blown` moment, here's a fun experiment. This program prints
"Hello World!", 8 times:

```
`
``si`k``s.H``s.e``s.l``s.l``s.o``s.
``s.w``s.o``s.r``s.l``s.d``s.!``sri
``si``si``si``si``si``si``si``si`ki
```

Let's split this file into two files, first one:

```
`
``si`k``s.H``s.e``s.l``s.l``s.o``s.
```

Second one:

```
``s.w``s.o``s.r``s.l``s.d``s.!``sri
``si``si``si``si``si``si``si``si`ki
```

(Note: Pushed these programs to the repo: `hello1.unl` and `hello2.unl`)

Now partially evaluate Unlambda interpreter using first program:

```
$ cat hello1.unl | ./unlambda_metaocaml programs/Jerabek-unlambda1.unl -staged -eval-S -eval-cc -eval-eof -partial-eval > pe_hello.ml
```

What do you think generated program does?

Before compiling generated file, just edit it and remove first two and last two
characters. MetaOCaml is printing code wrapped with code brackets(`.<` and `>.`)
so we need to remove those.

After that compile and run it like this:

```
$ ocamlc UnlambdaCont.cmo pe_hello.ml -o pe_hello
$ cat hello2.unl | ./pe_hello
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
```

---

Programs in `programs` directory are taken from
http://www.madore.org/~david/programs/unlambda/
