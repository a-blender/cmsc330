(* EVALUATOR By Anna Blendermann *)
(* UMD ID: 114474025 *)

open Types
open Utils

exception TypeError of string
exception DeclarationError of string
exception DivByZeroError

(* extend function *)
let extend env x v = (x,v)::env
;;

(* search function *)
let rec search env x = match env with
	| [] -> false
	| (y,z)::t -> if x=y then true else (search t x) 

(* lookup function *)
let rec lookup env x = match env with
	| [] -> failwith "no environment vars"
	| (y,z)::t -> if x=y then z else (lookup t x)
;;

(* eval_expr function *)
(* evaluates an expression to an int or bool value *)
let rec eval_expr env e = match e with


	| Int x -> Val_Int x


	| Bool x -> Val_Bool x


	| Id x -> if (search env x)=false
		then raise (DeclarationError("Id not found"))
		else (lookup env x)

	| Plus (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with 
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Int (x+y))
			| Val_Bool y -> raise (TypeError("plus2 bool found")))
		| Val_Bool x -> raise (TypeError("plus1 bool found")))

	
	| Sub (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Int (x-y))
			| Val_Bool y -> raise (TypeError("sub1 bool found")))
		| Val_Bool x -> raise (TypeError("sub2 bool found")))


	| Mult (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Int (x*y))
			| Val_Bool y -> raise (TypeError("mult2 bool found")))
		| Val_Bool x -> raise (TypeError("mult1 bool found")))


	| Div (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with 
		| Val_Int x -> (match num2 with
			| Val_Int y -> if y=0 
				then raise (DivByZeroError) else (Val_Int (x/y))
			| Val_Bool y -> raise (TypeError("div2 bool found")))
		| Val_Bool x -> raise (TypeError("div1 bool found")))


	| Pow (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> 
				let z = ((float_of_int x)**(float_of_int y)) 
				in (Val_Int (int_of_float z))
			| Val_Bool y -> raise (TypeError("pow2 bool found")))
		| Val_Bool x -> raise (TypeError("pow1 bool found")))
	
	
	| Or (e1,e2) -> let bool1 = (eval_expr env e1) in
		let bool2 = (eval_expr env e2) in
		(match bool1 with
		| Val_Bool x -> (match bool2 with
			| Val_Bool y -> (Val_Bool (x || y))
			| Val_Int y -> raise (TypeError("or2 int found")))
		| Val_Int x -> raise (TypeError("or1 int found")))


	| And (e1,e2) -> let bool1 = (eval_expr env e1) in
		let bool2 = (eval_expr env e2) in
		(match bool1 with
		| Val_Bool x -> (match bool2 with
			| Val_Bool y -> (Val_Bool (x && y))
			| Val_Int y -> raise (TypeError("and2 int found")))
		| Val_Int x -> raise (TypeError("and1 int found")))

	
	| Not exp -> let bool = (eval_expr env exp) in
		(match bool with
		| Val_Bool x -> (Val_Bool (!x))
		| Val_Int x -> raise (TypeError("not (func) int found")))


	| Greater (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Bool (x>y))
			| Val_Bool y -> raise (TypeError("greater2 bool found")))
		| Val_Bool x -> raise (TypeError("greater1 bool found")))


	| Less (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Bool (x<y))
			| Val_Bool y -> raise (TypeError("less2 bool found")))
		| Val_Bool x -> raise (TypeError("less1 bool found")))


	| GreaterEqual (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Bool (x>=y))
			| Val_Bool y -> raise (TypeError("greaterEqual2 bool found")))
		| Val_Bool x -> raise (TypeError("greaterEqual1 bool found")))


	| LessEqual (e1,e2) -> let num1 = (eval_expr env e1) in
		let num2 = (eval_expr env e2) in
		(match num1 with
		| Val_Int x -> (match num2 with
			| Val_Int y -> (Val_Bool (x<=y))
			| Val_Bool y -> raise (TypeError("lessEqual2 bool found")))
		| Val_Bool x -> raise (TypeError("lessEqual1 bool found")))


	| Equal (e1,e2) -> let exp1 = (eval_expr env e1) in
		let exp2 = (eval_expr env e2) in
		(match exp1 with
		
		| Val_Int x -> (match exp2 with
			| Val_Int y -> (Val_Bool (x=y))
			| Val_Bool y -> raise (TypeError("equal1 invalid exp found")))
				
		| Val_Bool x ->	(match exp2 with
			| Val_Bool y -> (Val_Bool (x=y))
			| Val_Int y -> raise (TypeError("equal2 invalid exp found"))))


	| NotEqual (e1,e2) -> let exp1 = (eval_expr env e1) in
		let exp2 = (eval_expr env e2) in
		(match exp1 with
		
		| Val_Int x -> (match exp2 with
			| Val_Int y -> (Val_Bool (x!=y))
			| Val_Bool y -> raise (TypeError("notEqual1 invalid exp found")))
				
		| Val_Bool x ->	(match exp2 with
			| Val_Bool y -> (Val_Bool (x!=y))
			| Val_Int y -> raise (TypeError("notEqual2 invalid exp found"))))


	| _ -> raise (TypeError("no valid expression found")) 
;;

(* eval_stmt function *)
let rec eval_stmt env s = failwith "unimplemented"




