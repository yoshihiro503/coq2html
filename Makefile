OCAMLOPT=ocamlopt -I +str -annot
OCAMLLEX=ocamllex

GEN_IDX=generate_index

PROJ_OBJS=common.cmx graphviz.cmx range.cmx xrefTable.cmx generate_index.cmx

all: coq2html ocamldot/ocamldot

coq2html: $(PROJ_OBJS:.cmx=.cmi) $(PROJ_OBJS)  coq2html.cmx
	$(OCAMLOPT) -o coq2html str.cmxa resources.cmx $(PROJ_OBJS) coq2html.cmx

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

generate_index.cmx: resources.cmx

coq2html.cmx: resources.cmx
resources.cmx: resources.cmi

RESOURCES=header footer css js redirect

resources.ml: $(RESOURCES:%=coq2html.%)
	(for i in $(RESOURCES); do \
         echo "let $$i = {xxx|"; \
         cat coq2html.$$i; \
         echo '|xxx}'; \
         echo ''; \
         done) > resources.ml

.PHONY: test

test: coq2html
	./test.sh

clean:
	rm -f coq2html
	rm -f coq2html.ml resources.ml
	rm -f *.o *.cm?
	$(MAKE) -C ocamldot/ clean

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

install:
	install coq2html $(BINDIR)/coq2html

depend:
	ocamldep *.mli *.ml > .depend

-include .depend

# ocamldot
ocamldot/ocamldot: ocamldot/
	$(MAKE) -C ocamldot/ ocamldot
