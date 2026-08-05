(**
   This is similar to [Unix.open_process_full]. It closes the connected channel
   regardless of whether an error occurs during the process.
*)
val using : string -> (in_channel * out_channel * in_channel -> 'a) -> 'a

val send : out_channel -> string -> unit

(**
   This retrieves data from the input channel without blocking. If there is no
   data to read, it returns [None].
*)
val read_available : ?max:int -> in_channel -> string option

(**
   [input_line_with_timeout time_sec ch] is similar to [input_line ch]. However,
   it ensures that the wait does not exceed [time_sec] seconds.
*)
val input_line_with_timeout : float -> in_channel -> string option

(**
   This checks whether the given command is available.
*)
val is_command_available : string -> bool
