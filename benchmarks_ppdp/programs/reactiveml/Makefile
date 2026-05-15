RMLC=rmlc
OCAMLOPT=ocamlopt
RML_WHERE=$(shell $(RMLC) -where)

FILE=rml_bench

all: $(FILE)

$(FILE).ml: $(FILE).rml
	$(RMLC) -n -1 -sampling -1.0 $<

$(FILE).cmx: $(FILE).ml
	$(OCAMLOPT) -c -I "$(RML_WHERE)" $(FILE).ml

$(FILE): $(FILE).cmx
	$(OCAMLOPT) -o $(FILE) unix.cmxa -I "$(RML_WHERE)" rmllib.cmxa $(FILE).cmx

clean:
	rm -f $(FILE) *.cm* *.o *.annot *.rzi $(FILE).ml

.PHONY: all clean
