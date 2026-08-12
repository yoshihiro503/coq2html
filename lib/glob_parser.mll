{
open Glob
module K = Glob_kind
}

let space = [' ' '\t']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart = ['A'-'Z' 'a'-'z' '_'] | utf8
let identnext  = ['A'-'Z' 'a'-'z' '_' '0'-'9' '\''] | utf8
let ident = identstart identnext*
let modpath = ident ("." ident)*
let globkind = ['a'-'z']+
let xref = (['A'-'Z' 'a'-'z' '0'-'9' '!' '#'-'~'] | utf8)+ | "<>"
let integer = ['0'-'9']+

rule globfile current_module entries = parse
  | eof
      { (current_module, List.rev entries) }
  | "F" (modpath as m) space* "\n"
      { globfile m entries lexbuf }
  | "R" (integer as pos1) ":" (integer as pos2)
    space+ (xref as dp)
    space+ (xref as sp)
    space+ (xref as id)
    space+ (globkind as kind)
    space* "\n"
      { let entry = Reference
          { pos_from = int_of_string pos1; pos_to = int_of_string pos2;
            logical_path = dp; section_path = sp; id; kind = K.of_string kind }
        in
        globfile current_module (entry :: entries) lexbuf }
  | (globkind as kind)
    space+ (integer as pos1) ":" (integer as pos2)
    space+ (xref as sp)
    space+ (xref as id)
    space* "\n"
      { let entry = Definition
          { pos_from = int_of_string pos1; pos_to = int_of_string pos2;
            section_path = sp; id; kind = K.of_string kind }
        in
        globfile current_module (entry :: entries) lexbuf }
  | [^ '\n']* "\n"
      { globfile current_module entries lexbuf }

{
let parse_channel ic =
  let (file_module, entries) = globfile "" [] (Lexing.from_channel ic) in
  { file_module; entries }
}
