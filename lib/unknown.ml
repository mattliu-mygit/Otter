
type unknown = {content: string; sequence: int}
(* let make s = {content = s; sequence = 0} *)

let get_unknown (str:string) (sequence:int) : unknown * string =
  {content = str; sequence = sequence}, ""