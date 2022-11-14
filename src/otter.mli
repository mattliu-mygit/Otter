module Function = struct
 type params = (string * Type.t) list
 type ret = Type.t
 type name = string
 let compare (func1:Function) (func2:Function) = int
 let to_func (func: string) = Function
 let from_func (func: Function) = string
end

module Module = struct
 type name = string
 type params = (string * Type.t) list
 let compare = 
 let to_module (func: string) = Function
 let from_module (func: Function) = string
end

module Variable = struct
 type name = string
 type t = Type.t
 let compare = 
 let to_variable (func: string) = Function
 let from_variable (func: Function) = string
end