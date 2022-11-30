(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to also to help generate any modified files.
*)

type infraction = {file_name: string; lines: int * int; columns: (int * int) list; issue_description: string; infraction_lines: string list}

val infraction_to_output: infraction -> string

val create_infraction: string -> int  * int -> (int * int) list -> string -> string list -> infraction