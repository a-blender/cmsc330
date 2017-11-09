open SmallCTypes
open Utils
(* fix this shit *)

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
		| Tok_Or::t -> let lst3 = (match_token lst2 Tok_Or) in 
			let (lst4, e2) = (parse_or lst3) in (lst4, Or(e1,e2))
		| _ -> (lst2, e1)

(* parse_and function *)
and parse_and lst = 
		let (lst2, e1) = (parse_equal lst) in
		match lst2 with
		| Tok_And::t -> let lst3 = (match_token lst2 Tok_And) in 
			let (lst4, e2) = (parse_and lst3) in (lst4, And(e1,e2))
		| _ -> (lst2, e1)

(* parse_equal function *)
and parse_equal lst = 
		let (lst2, e1) = (parse_relational lst) in
		match lst2 with
		| Tok_Equal::t -> let lst3 = (match_token lst2 Tok_Equal) in
			let (lst4, e2) = (parse_equal lst3) in (lst4, Equal(e1,e2))
		| Tok_NotEqual::t -> let lst3 = (match_token lst2 Tok_NotEqual) in
			let (lst4, e2) = (parse_equal lst3) in (lst4, NotEqual(e1,e2))
		| _ -> (lst2, e1)

(* parse_relational function *)
and parse_relational lst = 
		let (lst2, e1) = (parse_add lst) in
		match lst2 with

		| Tok_Less::t -> let lst3 = (match_token lst2 Tok_Less) in
			let (lst4, e2) = (parse_relational lst3) in (lst4, Less(e1,e2))

		| Tok_Greater::t -> let lst3 = (match_token lst2 Tok_Greater) in
			let (lst4, e2) = (parse_relational lst3) in (lst4, Greater(e1,e2))

		| Tok_LessEqual::t -> let lst3 = (match_token lst2 Tok_LessEqual) in
			let (lst4, e2) = (parse_relational lst3) in (lst4, LessEqual(e1,e2))

		| Tok_GreaterEqual::t -> let lst3 = (match_token lst2 Tok_GreaterEqual) in
			let (lst4, e2) = (parse_relational lst3) in (lst4, GreaterEqual(e1,e2))

		| _ -> (lst2, e1) 

(* parse_add function *)
and parse_add lst = 
	let (lst2, e1) = (parse_mult lst) in
		match lst2 with
		| Tok_Plus::t -> let lst3 = (match_token lst2 Tok_Plus) in
			let (lst4, e2) = (parse_add lst3) in (lst4, Plus(e1,e2))

		| Tok_Sub::t -> let lst3 = (match_token lst2 Tok_Sub) in
			let (lst4, e2) = (parse_add lst3) in (lst4, Sub(e1,e2))

		| _ -> (lst2, e1) 

(* parse_mult function *)
and parse_mult lst = 
	let (lst2, e1) = (parse_pow lst) in
		match lst2 with
		| Tok_Mult::t -> let lst3 = (match_token lst2 Tok_Mult) in 
			let (lst4, e2) = (parse_mult lst3) in (lst4, Mult(e1,e2))

		| Tok_Div::t -> let lst3 = (match_token lst2 Tok_Div) in 
			let (lst4, e2) = (parse_mult lst3) in (lst4, Div(e1,e2))
		| _ -> (lst2, e1) 

(* parse_pow function *)
and parse_pow lst = 
	let (lst2, e1) = (parse_unary lst) in
		match lst2 with
		| Tok_Pow::t -> let lst3 = (match_token lst2 Tok_Pow) in 
			let (lst4, e2) = (parse_pow lst3) in (lst4, Pow(e1,e2))
		| _ -> (lst2, e1)

(* parse_unary function *)
and parse_unary lst = 
	match lst with
	| Tok_Not::t -> let lst2 = (match_token lst Tok_Not) in
		let (lst3, e2) = (parse_unary lst2) in (lst3, Not(e2))
	| _ -> (parse_primary lst)

(* parse_primary function *)
and parse_primary lst = 
	match lst with

	| (Tok_Int(x))::t -> let lst2 = (match_token lst (Tok_Int(x))) in
		(lst2, Int(x))

	| (Tok_Bool(x))::t -> let lst2 = (match_token lst (Tok_Bool(x))) in
		(lst2, Bool x)

	| (Tok_ID(x))::t -> let lst2 = (match_token lst (Tok_ID(x))) in
		(lst2, Id x)

	| Tok_LParen::t -> let lst2 = (match_token lst Tok_LParen) in
		(let (lst3, e2) = (parse_expr lst2) in
		match lst3 with
			| Tok_RParen::t -> (t, e2)
			| _ -> raise (InvalidInputException("right paren not found")))
	| _ -> let x = (List.hd lst) in
		raise (InvalidInputException("x"))
;;

(* parse_stmt function *)
let rec parse_stmt toks = 
	match toks with

	| Tok_Type_Int::t -> 
		let (lst, dec) = (declareStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(dec, stmt))

	| Tok_Type_Bool::t -> 
		let (lst, bool) = (declareStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(bool, stmt))

	| Tok_ID(x)::t -> 
		let (lst, ass) = (assignStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(ass, stmt))

	| Tok_Print::t -> 
		let (lst, print) = (printStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(print, stmt))

	| Tok_If::t -> 
		let (lst, if_stmt) = (ifStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(if_stmt, stmt))

	| Tok_While::t -> 
		let (lst, while_stmt) = (whileStatement toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(while_stmt, stmt))

	| _ -> (toks, NoOp)

	
(* DeclareStatement *)
and declareStatement lst = match lst with
 
	| (Tok_Type_Int)::t -> let lst2 = (match_token lst Tok_Type_Int) in
		(match lst2 with
			| (Tok_ID(x))::t ->   
				let lst3 = (match_token lst2 (Tok_ID(x))) in
				let lst4 = (match_token lst3 Tok_Semi) in
				(lst4, Declare(Type_Int, x))
			| _ -> raise (InvalidInputException("declare error1")))
	
	| Tok_Type_Bool::t -> let lst2 = (match_token lst Tok_Type_Bool) in
		(match lst2 with
			| (Tok_ID(x))::t ->
				let lst3 = (match_token lst2 (Tok_ID(x))) in
				let lst4 = (match_token lst3 Tok_Semi) in
				(lst4, Declare(Type_Bool, x))
			| _ -> raise (InvalidInputException("declare error2")))
	| _ -> raise (InvalidInputException("declare error3"))
	

(* AssignStatement *)
and assignStatement lst = match lst with

	| (Tok_ID(x))::t -> let lst2 = (match_token lst (Tok_ID(x))) in
			let lst3 = (match_token lst2 Tok_Assign) in
			let (lst4, exp) = (parse_expr lst3) in 
			let lst5 = (match_token lst4 Tok_Semi) in
			(lst5, Assign(x, exp))
	| _ -> raise (InvalidInputException("assign error"))

		
(* Print Statement *)
and printStatement lst = match lst with
		
	| Tok_Print::t -> let lst2 = (match_token lst Tok_Print) in
			let lst3 = (match_token lst2 Tok_LParen) in
			let (lst4, exp) = (parse_expr lst3) in
			let lst5 = (match_token lst4 Tok_RParen) in 
			let lst6 = (match_token lst5 Tok_Semi) in
			(lst6, Print(exp))
	| _ -> raise (InvalidInputException("print error"))

	
(* IfStatement *)	
and ifStatement lst = match lst with
			
	| Tok_If::t -> let lst2 = (match_token lst Tok_If) in
		(* if (expression) *)
		let lst3 = (match_token lst2 Tok_LParen) in
		let (lst4, if_exp) = (parse_expr lst3) in
		let lst5 = (match_token lst4 Tok_RParen) in
			
		(* {statement} *)
		let lst6 = (match_token lst5 Tok_LBrace) in
		let (lst7, if_stmt) = (parse_stmt lst6) in
		let lst8 = (match_token lst7 Tok_RBrace) in
			
		(* else or NoOp move *)
		(match lst8 with
		| Tok_Else::t -> let lst9 = (match_token lst8 Tok_Else) in
			let lst10 = (match_token lst9 Tok_LBrace) in
			let (lst11, else_stmt) = (parse_stmt lst10) in
			let lst12 = (match_token lst11 Tok_RBrace) in
			(lst12, If(if_exp, if_stmt, else_stmt))  
		| _ -> (lst8, If(if_exp, if_stmt, NoOp)))
	
	| _ -> raise (InvalidInputException("if error"))    

	
(* WhileStatement *)
and whileStatement lst = match lst with		
	
	| Tok_While::t -> let lst2 = (match_token lst Tok_While) in
		let lst3 = (match_token lst2 Tok_LParen) in
		let (lst4, exp) = (parse_expr lst3) in
		let lst5 = (match_token lst4 Tok_RParen) in
		let lst6 = (match_token lst5 Tok_LBrace) in
		let (lst7, stmt) = (parse_stmt lst6) in
		let lst8 = (match_token lst7 Tok_RBrace) in (lst8, While(exp, stmt))	
	| _ -> raise (InvalidInputException("while error"))
;;

let parse_main toks = 
	let lst = (match_token toks Tok_Type_Int) in
	let lst1 = (match_token lst Tok_Main) in
	let lst2 = (match_token lst1 Tok_LParen) in
	let lst3 = (match_token lst2 Tok_RParen) in 
	let lst4 = (match_token lst3 Tok_LBrace) in 
	let (lst4, stmt) = (parse_stmt lst4) in
	
	let lst5 = (match_token lst4 Tok_RBrace) in
	let lst6 = (match_token lst5 EOF) in stmt 
;;


