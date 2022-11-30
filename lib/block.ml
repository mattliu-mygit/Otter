
(*  
 This defines the structure of each block of code. It will be used to help check code for
 formatter rules and be used to put together the final output file.
*)

(* Parameters, if any, of the code block.  *)
type params = (string * string) list

(* Return type, if any, of the code block. *)
type return_type = string

(* The type of code block. *)
type block_type = Function | Module | Variable | Type | Comment | Match | Unknown

(* Variable name of code block. *)
type name =  string

type content_strings = string list

type block = {
  name: name;
  return_type: return_type;
  block_type: block_type;
  content_strings: content_strings
}

let empty: block =
  failwith "unimplemented"
;;

let update_name (name: string) (block: block_type): block =
  failwith "unimplemented"
;;

let update_params params block: block =
  failwith "unimplemented"
;;

let update_return_type: return_type -> block -> block
  failwith "unimplemented"
;;

let update_block_type: block_type -> block -> block
  failwith "unimplemented"
;;

let update_infraction_list: infraction_list -> block -> block
  failwith "unimplemented"
;;

let update_content_strings: content_strings -> block -> block
  failwith "unimplemented"
;;

(* Function converting a string of code to a code block. *)
val str_to_block: string -> (block * string)
  failwith "unimplemented"
;;