(* 
 This defines the structure of the formatting infractions in the code.
 It is used to generate the output of the formatting issues to fix in the code and to also to help generate any modified files.
*)

type infraction = {file_name: string; lines: int * int; columns: (int * int) list; issue_description: string; infraction_lines: string list}

let empty : infraction = {file_name = ""; lines = (0, 0); columns = []; issue_description = ""; infraction_lines = []}

let update_file_name (file_name : string) (infraction : infraction) : infraction = {infraction with file_name = file_name}

let update_lines (lines : int * int) (infraction : infraction) : infraction = {infraction with lines = lines}

let update_columns (columns : (int * int) list) (infraction : infraction) : infraction = {infraction with columns = columns}

let update_issue_description (issue_description : string) (infraction : infraction) : infraction = {infraction with issue_description = issue_description}

let update_infraction_lines (infraction_lines : string list) (infraction : infraction) : infraction = {infraction with infraction_lines = infraction_lines}