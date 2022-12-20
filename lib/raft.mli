(*
  This file contains all the common helper functions and types for the executable.
*)

(*
  This is a record lists of each block type.
*)
type block_count = {
 comments: Comment.comment list;
 functions: Function.function_ list;
 unknowns: Unknown.unknown list;
}

(*
  Recursively turns a file content string str into a block_count record.
  It does this by parsing string str block by block, adding the block into its respective block_count record field, and then calling itself again on the rest of the string.
*)
val str_to_block: string -> block_count -> int -> block_count

(* 
 check if there are any more blocks remaining in any value in the given block count
 if there are, then process the block with the next smallest sequence number and add it to the out string
*)
val block_to_str: block_count -> int -> int -> string

(*
  Processes the command line arguments and returns a function that will write the output file.
*)
val process_args: int option -> int option -> string -> unit -> unit

(*
  This function goes through string str and wraps lines longer than width characters. If there exists an "in" keyword in the line, then it will wrap the line at the in keyword. Otherwise, it will wrap the line at the last space character. If there exists no "in" or whitespace in the line, then it will not wrap the line.
*)
val wrap_columns: string -> int -> int -> int -> string
