
type unknown = {content: string; sequence: int}

val get_unknown : string -> int -> int list -> unknown * string

val find: int -> int list -> int

val min_index: int list -> int