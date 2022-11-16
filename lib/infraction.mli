(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to
 also to help generate any modified files.
*)

(* Name of file with infraction. *)
val file_name: string

(* Lines and columns infraction occurred on. *)
val lines: int * int
val columns: (int * int) list

(* The description of the infraction. *)
val issue_description: string

(* Actual lines of code that infraction occurred on. *)
val infraction_lines: string list