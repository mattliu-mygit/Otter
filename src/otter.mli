module Function = struct
 type params = (string * Type.t) list
 type ret = Type.t
 type name = string
 let compare = 
 let sanitize = 
end

module Module = struct
 type name = string
 type params = (string * Type.t) list
 let compare = 
 let sanitize = 
end

module Variable = struct
 type name = string
 type t = Type.t
 let compare = 
 let sanitize = 
end