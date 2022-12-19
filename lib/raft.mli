type block_count = {
 comments: Comment.comment list;
 functions: Function.function_ list;
 unknowns: Unknown.unknown list;
}

val str_to_block: string -> block_count -> int -> block_count

val block_to_str: block_count -> int -> int -> string

val process_args: int option -> int option -> string -> unit -> unit
