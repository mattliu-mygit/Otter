module type Comment = sig
 type t 
 val make : string -> t
end

type t
type block_list = t list
val make : string -> t
val remove_leading_whitespaces: string -> string