(**
   This is a data type that represents a connection to the Rocq interpreter.
*)
type conn

(**
   Launch the Rocq interpreter to start an interactive session.
*)
val using : ?coqtop_bin:string -> (conn -> 'a) -> 'a

val about : conn -> string -> (string, string) result

val send : ?wait:float -> conn -> string -> (string, string) result
val exit : conn -> unit

(**
   If Rocq is installed, it returns [Some "rocq top"]; if Coq is installed,
   it returns [Some "coqtop"].
*)
val find_available_command : unit -> string option
