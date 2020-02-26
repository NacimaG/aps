type op = Add | Mul | Sub | Div | Not | And | Or | Eq | Lt 
type cbool = True | False
type alt = If
type tprim = Int | Bool 
type arrow = Arrow
type argA = Arg
type points = Points
type comma = Comma
type const = CONST
type func = FUN
type rc = REC
type pc = PC


type expr =
ASTNum of int
| ASTId of string
| ASTPrim of op * expr list
| ASTBool of cbool
| ASTAlt of alt * expr list
| ASTArgsE of arg list * expr
| ASTExpressions of expr * expr list

and arg =
	ASTArg of expr * points * typeAps 

and typeAps = 
		ASTTprim of tprim
		| ASTArrow of typeAps list * arrow * typeAps
	
and dec =
	 ASTConst of const * string * typeAps * expr
	 | ASTFun of func * expr * typeAps * expr
	 | ASTRec of func * rc * expr * typeAps * expr
 
and stat =
	ASTEcho of expr 

and cmds = 
	ASTTerm of stat
	| ASTNTermD of dec * pc * cmds
	| ASTNTermS of stat * pc * cmds

and progs =
	ASTProg of cmds 

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

let string_of_c comma =
				match comma with
				Comma -> ","
let c_of_string comma =
				match comma with
				"," -> Comma

let string_of_const cons =
	match cons with
	CONST -> "const"
let const_of_string cons =
	match cons with
	"const" -> CONST

let string_of_fun f =
	match f with
	FUN -> "fun"
let fun_of_string f =
	match f with
	"fun" -> FUN

let string_of_rec r =
	match r with
	REC -> "rec"
let rec_of_string r =
	match r with
	"rec" -> REC

let string_of_pc p =
	match p with
	PC -> ";"
let pc_of_string p =
	match p with
	";" -> PC