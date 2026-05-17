RMLC=rmlc
OCAMLOPT=ocamlopt
RML_WHERE=$(shell $(RMLC) -where)
RML_EXTRA_INCLUDE=
RML_INCLUDE_FLAGS=-I "$(RML_WHERE)" $(RML_EXTRA_INCLUDE)

FILE=rml_bench

all: $(FILE)

$(FILE).ml: $(FILE).rml
	$(RMLC) -n -1 -sampling -1.0 $<

$(FILE).cmx: $(FILE).ml
	$(OCAMLOPT) -c $(RML_INCLUDE_FLAGS) $(FILE).ml

$(FILE): $(FILE).cmx
	$(OCAMLOPT) -o $(FILE) unix.cmxa $(RML_INCLUDE_FLAGS) rmllib.cmxa $(FILE).cmx

clean:
	rm -f $(FILE) *.cm* *.o *.annot *.rzi $(FILE).ml

.PHONY: all clean
