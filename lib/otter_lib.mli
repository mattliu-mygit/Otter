open Block

(* 
   To normalize files, we will sanitize them by converting 
   the list of strings representing a file into a list of Blocks.
*)
val sanitize: string list -> block list

(* Below are our formatter functions *)
val check_indent: block list -> block list

val check_parameter_count: block list -> block list

val check_duplicate: block list -> block list

val check_variable_names: block list -> block list

val check_function_length: block list -> block list

type modification_type = Indent | Parameter_count | Duplicate | Variable_names | Function_length

(* 
   Will produce a list of updated infractions and either produce a modified list of string lines 
   represented based on the given modification flag and areas to modify or return just an indent and line-formatted version.
*)
val update_file: block list -> bool -> modification_type list -> int -> int -> string list * infraction list