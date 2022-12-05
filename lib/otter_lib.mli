(* (*
 Gets params from the command line and returns them as a list of strings.
*)
type function_block = {
    name: string;
    params: (string*string) list;
    return_type: string;
    body: string list;
    block_list: string list;
}

type variable_block = {
    name: string;
    value_type: string;
    block_list: string list;
}


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

(* Type of modification to check for. *)
type modification_type = Indent | Parameter_count | Duplicate | Variable_names | Function_length

(* 
 Will produce a list of updated infractions and either produce a modified list of string lines 
 represented based on the given modification flag and areas to modify or return just an indent and line-formatted version.
*)
val update_file: block list -> bool -> modification_type list -> int -> int -> string list * infraction list

(* decision tree backtracking-esque way of parsing blocks within other blocks *)
val parse_block: string -> block list *)


type comment = {
    content: string list;
}

type unknown = {
    content: string list;
}

type block = Comment | Unknown

val str_to_block: string list -> block list -> block list