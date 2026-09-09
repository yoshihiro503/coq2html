type t

val tag_with_tooltip : string -> string -> string -> t -> string -> string

val make : Env.t -> string -> Lexing.position -> string -> Glob_kind.kind
           -> t option
