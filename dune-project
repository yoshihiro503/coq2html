(lang dune 3.0)
(name rocq-navi)
(version dev)
(authors
 "Xavier Leroy <xavier.leroy@college-de-france.fr>"
 "Reynald Affeldt <reynald.affeldt@aist.go.jp>"
 "Yoshihiro Imai <y.imai@aist.go.jp>")
(maintainers "Yoshihiro Imai <y.imai@aist.go.jp>")
(license GPL-2.0-or-later)
(homepage https://github.com/affeldt-aist/rocqnavi)
(bug_reports https://github.com/affeldt-aist/rocqnavi/issues)
(source (github yoshihiro503/rocqnavi))

(generate_opam_files true)

(package
 (name rocq-navi)
 (synopsis "Extension of coq2html Document Generator")
 (description "Extension of coq2html Document Generator")
 (tags
  ("category:Tools/Document Generator"
   "keyword:document"
   "keyword:html"))
 (depends
  (ocaml (>= 4.14))
  ocamlfind
  dune-glob
  yojson
  (coq-lsp :with-test)
  (rocq-hierarchy-builder :with-test)))
