open SmallCTypes
open Utils

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list = 
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* lookahead function *)
(* returns the head token or parse error for empty list *)
let lookahead tok_list = match tok_list with
	| [] -> raise (InvalidInputException "no tokens")
	| (h::t) -> h
;;

(* parse_expr function *)
(* returns a tuple of (list of remaining tokens, parsed expr) *)
let rec parse_expr toks = 
	parse_or toks
	
(* parse_or function *)
and parse_or lst = 
		let (lst2, e1) = (parse_and lst) in
		match lst2 with
		| Tok_Or::t -> let (lst3, e2) = (parse_or lst2) in (lst3, Or(e1,e2))
		| _ -> (lst2, e1)

(* parse_or function *)
and parse_and lst = 
		let (lst2, e1) = (parse_equal lst) in
		match lst2 with
		| Tok_And::t -> let (lst3, e2) = (parse_and lst2) in (lst3, And(e1,e2))
		| _ -> (lst2, e1)

and parse_equal lst = 
		let (lst2, e1) = (parse_relational lst) in
		match lst2 with
		| Tok_Equal::t -> let (lst3, e2) = (parse_equal lst2) in (lst3, Equal(e1,e2))
		| Tok_NotEqual::t -> let (lst3, e2) = (parse_equal lst2) in (lst3, NotEqual(e1,e2))
		| _ -> (lst2, e1)

and parse_relational lst = 
		let (lst2, e1) = (parse_add lst) in
		match lst2 with
		| Tok_Less::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, Less(e1,e2))
		| Tok_Greater::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, Greater(e1,e2))
		| Tok_LessEqual::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, LessEqual(e1,e2))
		| Tok_GreaterEqual::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, GreaterEqual(e1,e2))
		| _ -> (lst2, e1) 

and parse_add lst = 
	let (lst2, e1) = (parse_mult lst) in
		match lst2 with
		| Tok_Plus::t -> let (lst3, e2) = (parse_add lst2) in (lst3, Plus(e1,e2))
		| Tok_Sub::t -> let (lst3, e2) = (parse_add lst2) in (lst3, Sub(e1,e2))
		| _ -> (lst2, e1) 

and parse_mult lst = 
	let (lst2, e1) = (parse_pow lst) in
		match lst2 with
		| Tok_Mult::t -> let (lst3, e2) = (parse_mult lst2) in (lst3, Mult(e1,e2))
		| Tok_Div::t -> let (lst3, e2) = (parse_mult lst2) in (lst3, Div(e1,e2))
		| _ -> (lst2, e1) 

and parse_pow lst = 
	let (lst2, e1) = (parse_urnary lst) in
		match lst2 with
		| Tok_Pow::t -> let (lst3, e2) = (parse_pow lst2) in (lst3, Pow(e1,e2))
		| _ -> (lst2, e1)

and parse_urnary lst = 
	let (lst2, e1) = (parse_primary lst) in
		match lst2 with
		| Tok_Not::t -> let (lst3, e2) = (parse_urnary lst2) in (lst3, Not(e2))
		| _ -> (lst2, e1)

and parse_primary lst = 
		let (lst2, e1) = (parse_or lst) in
		match lst2 with
		| Tok_Int x::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Int x)
		| Tok_Bool x::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Bool x)
		| Tok_ID x::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Id "x")
		| _ -> (lst2,e1)
;;

let rec parse_stmt toks = failwith "Unimplemented"

let parse_main toks = failwith "Unimplemented"

