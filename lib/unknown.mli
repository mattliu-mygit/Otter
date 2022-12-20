(*
  This file contains the types and functions used in the parsing of the Unknown code block.
*)

(*
  Unknown block type. It contains the content of the unknown block and the sequence number of the unknown block in relation to other blocks around it.
*)
type unknown = {content: string; sequence: int}

(*
  Gets everything before the next non-unknown block (determined by a first_sight list of the next index each block type is sighted) in str and puts it in an unknown block.
*)
val get_unknown : string -> int -> int list -> unknown * string

(* 
  Finds the index of the first occurence of x in lst.
  Raises Failure "Not Found" if x is not in lst.
*)
val find: int -> int list -> int

(*
  Returns the index of the minimum value in lst.
  If the minimum value is -1, returns 5, which is out of bound of the number of block types.
*)
val min_index: int list -> int

(*
  Gets a sublist of list l between the index positions start and finish.
*)
val sublist: int -> int -> int list -> int list