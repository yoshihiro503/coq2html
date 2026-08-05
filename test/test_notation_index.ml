(* Example taken from an actual .glob entry produced by:
     Notation "x +++ y" := (myadd x y) (at level 50).
   which rocq records as: not 55:55 <> :::x_'+++'_x *)
let test_html_of_notation_no_scope () =
  let scope, notation, module_, linkname =
    Notation_index.notation_of_item
      ("notest",
       "notest.html#884a3825e256bb17d8a2d0c65f90e67d",
       ":::x_'+++'_x")
  in
  let actual = Notation_index.html_of_notation scope notation module_ linkname in
  Alcotest.(check string) "html_of_notation"
    {|<a href="notest.html#884a3825e256bb17d8a2d0c65f90e67d">x <span class="notation-symbol">+++</span> x</a> [not, in notest] (<span class="warning">no scope</span>)|}
    actual

(* Example taken from an actual .glob entry in mathcomp-analysis:
     not 14761:14761 <> ::classical_set_scope:'<<M'_x_'>>' *)
let test_html_of_notation_with_scope () =
  let scope, notation, module_, linkname =
    Notation_index.notation_of_item
      ("measurable_structure",
       "measurable_structure.html#deadbeef",
       "::classical_set_scope:'<<M'_x_'>>'")
  in
  let actual = Notation_index.html_of_notation scope notation module_ linkname in
  Alcotest.(check string) "html_of_notation"
    {|<a href="measurable_structure.html#deadbeef"><span class="notation-symbol">&lt;&lt;M</span> x <span class="notation-symbol">&gt;&gt;</span></a> [not, in measurable_structure] (in classical_set_scope)|}
    actual

let () =
  Alcotest.run "notation_index" [
    "html_of_notation", [
      Alcotest.test_case "no scope" `Quick test_html_of_notation_no_scope;
      Alcotest.test_case "with scope" `Quick test_html_of_notation_with_scope;
    ];
  ]
