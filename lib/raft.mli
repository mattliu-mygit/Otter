type comment = {
  content: string list;
}

type unknown = {
  content: string list;
}

module type Function_sig = sig
  type t
  val make : string -> t
end

type block = Comment | Unknown

val str_to_block: string list -> block list -> block list

(* val get_function: string -> (Function.t * string) *)

val remove_leading_whitespaces: string -> string

