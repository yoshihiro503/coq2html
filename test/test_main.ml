let () =
  Alcotest.run "rocqnavi" [
    "notation_index", Test_notation_index.tests;
    "glob_parser", Test_glob_parser.tests;
  ]
