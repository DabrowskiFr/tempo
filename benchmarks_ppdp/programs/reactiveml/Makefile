RMLC=rmlc
OCAMLC=ocamlc

FILE=rml_bench

all: $(FILE)

$(FILE).ml: $(FILE).rml
	$(RMLC) -n -1 -sampling -1.0 $<

$(FILE).cmo: $(FILE).ml
	$(OCAMLC) -c -I `$(RMLC) -where` $(FILE).ml

$(FILE): $(FILE).cmo
	$(OCAMLC) -o $(FILE) unix.cma -I `$(RMLC) -where` rmllib.cma $(FILE).cmo

clean:
	rm -f $(FILE) *.cm* *.o *.annot *.rzi $(FILE).ml

.PHONY: all clean
