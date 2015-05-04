# TODO: Maybe get into this make depend business.

ALL: unlambda_idris unlambda_metaocaml

unlambda_idris: Unlambda.idr
	idris $< -o $@

unlambda_metaocaml: Syntax.cmo UnlambdaInterp.cmo UnlambdaCont.cmo Lift.cmo CmdArgs.cmo UnlambdaStaged.cmo UnlambdaMain.ml
	metaocamlc $^ -o $@ -g

qbf_metaocaml: QBFSyntax.cmo QBFEval.cmo QBFMain.ml
	metaocamlc $^ -o $@ -g

UnlambdaStaged.cmo: Syntax.cmo Lift.cmo UnlambdaCont.cmo UnlambdaStaged.ml
	metaocamlc -c $^ -g

Lift.cmo: Syntax.cmo Lift.ml
	metaocamlc -c $^ -g

UnlambdaInterp.cmo: Syntax.cmo UnlambdaInterp.ml
	metaocamlc -c $^ -g

UnalamdaCont.cmo: Syntax.cmo UnlambdaCont.ml
	metaocamlc -c $^ -g

QBFEval.cmo: QBFSyntax.cmo QBFEval.ml
	metaocamlc -c $^ -g

%.cmo: %.ml
	metaocamlc -c $< -g

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.ibc
	rm -f unlambda_metaocaml
	rm -f unlambda_idris
	rm -f qbf_metaocaml
	rm -f qbf_idris
