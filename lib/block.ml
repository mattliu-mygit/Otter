
module Comment = struct
 type t = {content: string}
 let make s = {content = s}
end

type t = Comment of Comment.t | Unknown of Unknown.t
type block_list = t list

let make s = Unknown (Unknown.make s)

let remove_leading_whitespaces (str: string): string = Str.replace_first (Str.regexp "^[ \n\t\r]+") "" str;;