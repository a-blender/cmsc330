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
	| [] -> raise (InvalidInputException("no tokens"))
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

(* parse_and function *)
and parse_and lst = 
		let (lst2, e1) = (parse_equal lst) in
		match lst2 with
		| Tok_And::t -> let (lst3, e2) = (parse_and lst2) in (lst3, And(e1,e2))
		| _ -> (lst2, e1)

(* parse_equal function *)
and parse_equal lst = 
		let (lst2, e1) = (parse_relational lst) in
		match lst2 with
		| Tok_Equal::t -> let (lst3, e2) = (parse_equal lst2) in (lst3, Equal(e1,e2))
		| Tok_NotEqual::t -> let (lst3, e2) = (parse_equal lst2) in (lst3, NotEqual(e1,e2))
		| _ -> (lst2, e1)

(* parse_relational function *)
and parse_relational lst = 
		let (lst2, e1) = (parse_add lst) in
		match lst2 with
		| Tok_Less::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, Less(e1,e2))
		| Tok_Greater::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, Greater(e1,e2))
		| Tok_LessEqual::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, LessEqual(e1,e2))
		| Tok_GreaterEqual::t -> let (lst3, e2) = (parse_relational lst2) in (lst3, GreaterEqual(e1,e2))
		| _ -> (lst2, e1) 

(* parse_add function *)
and parse_add lst = 
	let (lst2, e1) = (parse_mult lst) in
		match lst2 with
		| Tok_Plus::t -> let (lst3, e2) = (parse_add lst2) in (lst3, Plus(e1,e2))
		| Tok_Sub::t -> let (lst3, e2) = (parse_add lst2) in (lst3, Sub(e1,e2))
		| _ -> (lst2, e1) 

(* parse_mult function *)
and parse_mult lst = 
	let (lst2, e1) = (parse_pow lst) in
		match lst2 with
		| Tok_Mult::t -> let (lst3, e2) = (parse_mult lst2) in (lst3, Mult(e1,e2))
		| Tok_Div::t -> let (lst3, e2) = (parse_mult lst2) in (lst3, Div(e1,e2))
		| _ -> (lst2, e1) 

(* parse_pow function *)
and parse_pow lst = 
	let (lst2, e1) = (parse_urnary lst) in
		match lst2 with
		| Tok_Pow::t -> let (lst3, e2) = (parse_pow lst2) in (lst3, Pow(e1,e2))
		| _ -> (lst2, e1)

(* parse_urnary function *)
and parse_urnary lst = 
	let (lst2, e1) = (parse_primary lst) in
		match lst2 with
		| Tok_Not::t -> let (lst3, e2) = (parse_urnary lst2) in (lst3, Not(e2))
		| _ -> (lst2, e1)

(* parse_primary function *)
and parse_primary lst = 
		let (lst2, e1) = (parse_or lst) in
		match lst2 with
		| (Tok_Int x)::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Int x)
		| (Tok_Bool x)::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Bool x)
		| (Tok_ID x)::t -> let (lst3, e2) = (parse_primary lst2) in (lst3, Id x)
		| Tok_LParen::t -> let (lst3, e2) = (parse_expr lst2) in
				match lst3 with
				| Tok_RParen::t -> (t, e2)
				| _ -> raise (InvalidInputException("right paren not found"))
;;

(* parse_stmt function *)
let rec parse_stmt toks = 
	let lst = (lookahead toks) in
	match lst with
	| Tok_Type_Int::t -> declareStatement lst
	| Tok_Bool::t -> declareStatement lst
	| Tok_Assign::t -> assignStatement lst
	| Tok_Print::t -> printStatement lst
	| Tok_If::t -> ifStatement lst
	| Tok_While::t -> whileStatement lst
	| NoOp -> (lst, NoOp)
	
(* DeclareStatement *)
and declareStatement lst = match lst with
	
	| Tok_Type_Int::t -> let lst2 = (match_tokens lst Tok_Int) in
		 	let x = (lookahead lst2) in
			let lst3 = (match_token lst2 Tok_ID) in
			(lst3, Declare(Type_Int, x))
	
	| Tok_Type_Bool::t -> let lst2 = (match_tokens lst Tok_Bool) in
			let x = (lookahead lst2) in
			let lst3 = (match_token lst2 Tok_Bool) in
			(lst3, Declare(Type_Bool, x))
	
	(*
	(* AssignStatement *)		
	| (Tok_ID x)::t -> let lst2 = (match_tokens lst Tok_ID) in
			let lst3 = (match_tokens lst Tok_Assign) in
			let (lst4, exp) = (parse_expr lst3) in (lst4, Assign(x, exp))
		
	(* Print Statement *)		
	| Tok_Print::t -> let lst2 = (match_tokens lst Tok_Print) in
			let lst3 = (match_tokens lst2 Tok_LParen) in
			let (lst4, exp) = (parse_expr lst3) in
			let lst5 = (match_token lst4 Tok_RParen) in (lst5, Print(exp))
	
	(* correct up until this point *)		
	(* IfStatement *)				
	| Tok_If::t -> let lst2 = (match_tokens lst Tok_If) in
			(* if (expression) *)
			let lst3 = (match_tokens lst2 Tok_LParen) in
			let (lst4, exp) = (parse_expr lst3) in
			let lst5 = (match_token lst4 Tok_RParen) in
			
			(* {statement} *)
			let lst6 = (match_token lst5 Tok_LBrace) in
			let (lst7, if_stmt) = (parse_stmt lst6) in
			let lst8 = (match_token lst7 Tok_RBrace) in
			
			(* else or NoOp move *)
			(match lst8 with
			| Tok_Else::t -> let lst9 = (match_tokens lst8 Tok_LBrace) in
					let (lst10, else_stmt) = (parse_stmt lst9) in
					let lst11 = (match_tokens lst10 Tok_RBrace) in
					(lst11, 
			| _ ->    

			
						
						
			  
	
	(* WhileStatement *)		
	| Tok_While::t -> 
		*)
;;

let parse_main toks = 
	let lst = (match_token toks Tok_Type_Int) in
	let lst1 = (match_token lst Tok_Main) in
	let lst2 = (match_token lst1 Tok_LParen) in
	let lst3 = (match_token lst2 Tok_RParen) in 
	let lst4 = (match_token lst3 Tok_LBrace) in 
	let (lst4, stmt) = parse_stmt lst4 in
	
	let lst5 = (match_token lst4 Tok_RBrace) in
	let lst6 = (match_token lst5 EOF) in stmt 
;;


