ALL: unlambda_idris unlambda_metaocaml

unlambda_idris: Unlambda.idr
	idris $< -o $@

unlambda_metaocaml: Syntax.cmo Lift.cmo Unlambda.cmo Unlambda_compile.ml
	metaocamlc $^ -o $@ -g

Lift.cmo: Syntax.cmo Lift.ml
	metaocamlc -c $^ -g

%.cmo: %.ml
	metaocamlc -c $< -g

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.ibc
	rm -f unlambda_metaocaml
	rm -f unlambda_idris
