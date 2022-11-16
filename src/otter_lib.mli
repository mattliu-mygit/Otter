(*Maybe make one super parent*)

module Function = struct
 type params = (string * Type.t) list
 type ret = Type.t
 let name = string
 let compare (func1:Function) (func2:Function) = int
 let to_func (func: string) = Function
 let from_func (func: Function) = string
end

module Module = struct
 type params = (string * Type.t) list
 let name = string
 let compare (mod1: Variable) (mod2:Variable) = int
 let to_module (func: string) = Function
 let from_module (func: Function) = string
end

module Variable = struct
 type t = Type.t
 let name = string
 let compare (var1: Variable) (var2:Variable) = int
 let to_variable (func: string) = Function
 let from_variable (func: Function) = string
end

module Undetermined = struct
 type params = (string * Type.t) list
 type t = Type.t
 let name = string
 let compare = int
 let to_undetermined (func: string) = Function
 let from_undetermined (func: Function) = string
end

let sanitize (file: string list) = string list

let check_indent (file: string list) = bool

let check_parameter_count (file: string list) = bool