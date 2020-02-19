type op = Add | Mul | Sub | Div | Not | And | Or | Eq | Lt 
type cbool = True | False
type alt = If
type tprim = Int | Bool 
type arrow = Arrow
type argA = Arg
type points = Points

type expr =
ASTNum of int
| ASTId of string
| ASTPrim of op * expr list
| ASTBool of cbool
| ASTAlt of alt * expr list

type stat =
	ASTEcho of expr 

type typeAps = 
	ASTTprim of tprim
	| ASTArrow of typeAps list * arrow * typeAps

type arg =
	ASTArg of expr * points * typeAps 


let string_of_op op =
		match op with
			Add -> "add"
		| Mul -> "mul"
		| Sub -> "sub"
		| Div -> "div"
		| Not -> "not"
		| And -> "and"
		| Or  -> "or"
		| Eq  -> "eq"
		| Lt  -> "lt"
		
let op_of_string op =
	match op with
		"add" -> Add
	| "mul" -> Mul
	| "sub" -> Sub
	| "div" -> Div
	| "not" -> Not 
	| "and" -> And
	| "or"  -> Or
	| "eq"  -> Eq
	| "lt"  -> Lt

let string_of_bool cbool =
	match cbool with
	True -> "true"
	| False -> "false"

let bool_of_string cbool = 
	match cbool with
	"true" -> True
	| "false" -> False

let string_of_type t =
	match t with
	Int -> "int"
	| Bool -> "bool"

let type_of_string t=
	match t with
	"int" -> Int
	| "bool" -> Bool


let string_of_arr arr =
		match arr with
		Arrow -> "->"
let arr_of_string arr =
		match arr with
		"->" -> "Arrow"

let string_of_p points =
			match points with
			Points -> ":"
let p_of_string ar =
			match ar with
			":" -> Points
