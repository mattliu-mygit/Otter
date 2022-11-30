(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to also to help generate any modified files.
*)

type infraction = {file_name: string; lines: int * int; columns: (int * int) list; issue_description: string; infraction_lines: string list}

val empty: infraction

val update_file_name: infraction -> string -> infraction

val update_lines: infraction -> int * int -> infraction

val update_columns: infraction -> (int * int) list -> infraction

val update_issue_description: infraction -> string -> infraction

val update_infraction_lines: infraction -> string list -> infraction