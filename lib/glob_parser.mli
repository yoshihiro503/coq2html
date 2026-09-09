(** [parse_channel ic] parses the contents of [ic] as a Rocq/Coq [.glob] file.
    Returns the module name declared by the "F" line, together with the
    definitions/references found (in file order). *)
val parse_channel : in_channel -> Glob.t
