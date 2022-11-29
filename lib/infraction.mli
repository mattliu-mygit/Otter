(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to
 also to help generate any modified files.
*)

(* Name of file with infraction. *)
type file_name = string

(* Lines and columns infraction occurred on. *)
type lines = int * int
type columns = (int * int) list

(* The description of the infraction. *)
type issue_description = string

(* Actual lines of code that infraction occurred on. *)
type infraction_lines = string list

type infraction = (file_name * lines * columns * issue_description * infraction_lines)

val empty: infraction

val update_file_name: infraction -> file_name -> infraction

val update_lines: infraction -> lines -> infraction

val update_columns: infraction -> columns -> infraction

val update_issue_description: infraction -> issue_description -> infraction

val update_infraction_lines: infraction -> infraction_lines -> infraction