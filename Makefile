ALL: unlambda_idris unlambda_metaocaml

unlambda_idris: Unlambda.idr
	idris $< -o $@

unlambda_metaocaml: Syntax.cmo Unlambda.ml
	metaocamlc $^ -o $@

%.cmo: %.ml
	metaocamlc -c $<

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.ibc
	rm -f unlambda_metaocaml
	rm -f unlambda_idris
