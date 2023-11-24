OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex

GEN_IDX=generate_index

coq2html: $(GEN_IDX).cmi $(GEN_IDX).cmx coq2html.cmx
	$(OCAMLOPT) -o coq2html str.cmxa resources.cmx $(GEN_IDX).cmx coq2html.cmx

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml

%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli

%.ml: %.mll
	$(OCAMLLEX) $*.mll

generate_index.cmx: resources.cmx

resources.ml: coq2html.css coq2html.js coq2html.header coq2html.footer coq2html.redirect
	(for i in header footer css js redirect; do \
         echo "let $$i = {xxx|"; \
         cat coq2html.$$i; \
         echo '|xxx}'; \
         done) > resources.ml

clean:
	rm -f coq2html
	rm -f coq2html.ml resources.ml
	rm -f *.o *.cm?

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

install:
	install coq2html $(BINDIR)/coq2html

