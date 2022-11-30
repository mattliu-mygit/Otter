(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to also to help generate any modified files.
*)

type infraction = {file_name: string; lines: int * int; columns: (int * int) list; issue_description: string; infraction_lines: string list}

let infraction_to_output (infraction: infraction) : string =
  "File: \"" ^ infraction.file_name ^ "\", lines " ^ (string_of_int (fst infraction.lines)) ^ "-" ^ (string_of_int (snd infraction.lines)) ^ ", characters " ^ (String.concat ", " (List.map (fun a -> (string_of_int (fst a)) ^ "-" ^ (string_of_int (snd a))) infraction.columns)) ^ "\n\t" ^ infraction.issue_description ^ "\n"

let create_infraction (file_name: string) (lines: int * int) (columns: (int * int) list) (issue_description: string) (infraction_lines: string list) : infraction =
  {file_name; lines; columns; issue_description; infraction_lines}