# Building

Make sure Idris and MetaOCaml is in the path, and run `make`.

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
* `$ ./unlambda_metaocaml programs/trivial.unl -staged` loop when `-eval-S` is added.
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

Interestingly, this doesn't print same program that `./unlambda_metaocaml
programs/Hello.unl -staged -eval-S -eval-cc -eval-eof` prints, but the
specialization is correct, `-run` works correctly. Figuring out why generated
code is different may be an interesting exercise.

---

Programs in `programs` directory are taken from
http://www.madore.org/~david/programs/unlambda/
