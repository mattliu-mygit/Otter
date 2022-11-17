(*  
 This defines the structure of each block of code. It will be used to help check code for
 formatter rules and be used to put together the final output file.
*)

type block = Block

(* Parameters, if any, of the code block.  *)
type params = (string * string) list

(* Return value, if any, of the code block. *)
type return_val = string

(* The type of code block. *)
type block_type = Function | Module | Variable | Type | Comment | Match | Unknown

type infraction = Infraction

(* Variable name of code block. *)
val name: string

(* Code content in string format. *)
val content_strings: string list

(* The list of infractions relating to this block of code. *)
val infractions: infraction list

(* Function converting a string of code to a code block. *)
val str_to_block: string -> block

(* Function converting a code block to a string of code. *)
val block_to_str: block -> string

(* List of blocks of code within this block of code. *)
val content_blocks: block list