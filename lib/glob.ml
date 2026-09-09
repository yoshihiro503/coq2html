open Glob_kind

type entry =
  | Definition of
      { pos_from : int; pos_to : int;
        section_path : string; id : string; kind : kind }
  | Reference of
      { pos_from : int; pos_to : int;
        logical_path : string; section_path : string; id : string; kind : kind }

type t =
  { file_module : string; entries : entry list }
