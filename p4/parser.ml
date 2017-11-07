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
	| [EOF] -> raise (ParseError "no tokens")
	| (h::t) -> h
;;



(* parse_AndExpression function *)
let parse_AndExpression lst = failwith "Unimplemented"

(* parse_EqualityExpression function *)
let parse_EqualityExpression lst = failwith "Unimplemented"

(* parse_RelationalExpression function *)
let parse_RelationalExpression = failwith "Unimplemented"

(* parse_AdditiveExpression function *)
let parse_AdditiveExpression = failwith "Unimplemented"

(* parse_MultiplicativeExpression function *)
let parse_MultiplicativeExpression = failwith "Unimplemented"

(* parse_PowerExpression function *)
let parse_PowerExpression = failwith "Unimplemented"

(* parse_UnaryExpression function *)
let parse_UnaryExpression = failwith "Unimplemented"

(* parse_PrimaryExpression function *)
let parse_PrimaryExpression = failwith "Unimplemented"
	
	
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
and parse_or lst = 
		let (lst2, e1) = (parse_and lst) in
		match lst2 with
		| Tok_Or::t -> let (lst3, e2) = (parse_or lst2) in (lst3, Or(e1,e2))
		| _ -> (lst2, e1)
;;


let rec parse_stmt toks = failwith "Unimplemented"

let parse_main toks = failwith "Unimplemented"

