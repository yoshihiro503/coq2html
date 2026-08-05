open Common

let get_viewbox svgstring =
  let re = Str.regexp {|viewBox="\([^"]+\)"|} in
  try
    ignore (Str.search_forward re svgstring 0);
    let s = Str.matched_group 1 svgstring in
    Scanf.sscanf s "%f %f %f %f"
      (fun x y width height ->  (x, y, width, height))
    |> Option.some
  with
  | Not_found ->
    let _ = prerr_endline (!%"regexp error '%s'" svgstring) in
     None
  | Scanf.Scan_failure _ ->
       prerr_endline (!%"scan error '%s'" svgstring); None

let is_small svgstring =
  match get_viewbox svgstring with
  | Some (_, _, width, _) when
    width < 800. -> true
  | _ -> false

let div id svgstring =
  let class_ = if is_small svgstring then "small-graph" else "graph" in
  !%{|<div id="%s" class="%s">%s</div>|} id class_ svgstring
