module type Comment = sig
 type t
 val make : string -> t
end

val comment_regexp: string -> Str.regexp

val end_comment_regexp: string -> Str.regexp

val get_comment: string -> int -> string -> (Block.t * int)