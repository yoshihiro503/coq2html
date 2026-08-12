module K = Glob_kind

let kind_testable =
  Alcotest.testable (fun fmt k -> Format.pp_print_string fmt (K.to_string k)) (=)

let with_temp_glob content f =
  let path = Filename.temp_file "test" ".glob" in
  let write () =
    let oc = open_out path in
    output_string oc content;
    close_out oc
  in
  let read () =
    let ic = open_in path in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
  in
  write ();
  Fun.protect ~finally:(fun () -> Sys.remove path) read

(* Taken verbatim from examples/test1/Main.glob *)
let sample = {|DIGEST ab442acba3a6d54df5f466dfd62d902b
FMain
R15:20 Stdlib.Strings.String <> <> lib
def 171:171 <> x
|}

let test_module_name_from_f_line () =
  with_temp_glob sample (fun ic ->
      let glob = Glob_parser.parse_channel ic in
      Alcotest.(check string) "module name" "Main" glob.file_module)

let test_entries_in_file_order () =
  with_temp_glob sample (fun ic ->
      let glob = Glob_parser.parse_channel ic in
      match glob.entries with
      | [ Glob.Reference
            { pos_from; pos_to; logical_path; section_path; id; kind };
          Glob.Definition
            { pos_from = dpos_from; pos_to = dpos_to; section_path = dsp;
              id = did; kind = dkind } ] ->
         Alcotest.(check (pair int int)) "reference position" (15, 20)
           (pos_from, pos_to);
         Alcotest.(check string) "reference logical_path"
           "Stdlib.Strings.String" logical_path;
         Alcotest.(check string) "reference section_path" "<>" section_path;
         Alcotest.(check string) "reference id" "<>" id;
         Alcotest.check kind_testable "reference kind" K.Require kind;
         Alcotest.(check (pair int int)) "definition position" (171, 171)
           (dpos_from, dpos_to);
         Alcotest.(check string) "definition section_path" "<>" dsp;
         Alcotest.(check string) "definition id" "x" did;
         Alcotest.check kind_testable "definition kind" K.Definition dkind
      | entries ->
         Alcotest.failf "expected exactly one reference and one definition, got %d entries"
           (List.length entries))

let tests = [
  Alcotest.test_case "module name from F line" `Quick test_module_name_from_f_line;
  Alcotest.test_case "DIGEST line ignored, entries in order" `Quick test_entries_in_file_order;
]
