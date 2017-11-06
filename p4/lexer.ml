open SmallCTypes

let tokenize input = 
	
	let re_lparen = Str.regexp "(" in
	let re_rparen = Str.regexp ")" in
	let re_lbrace = Str.regexp "{" in 
	let re_rbrace = Str.regexp "}" in
	let re_equal = Str.regexp "==" in 
	let re_notequal = Str.regexp "!=" in
	let re_assign = Str.regexp "=" in 
	let re_greater = Str.regexp ">" in
	let re_less = Str.regexp "<" in
	let re_greaterequal = Str.regexp ">=" in
	let re_lessequal = Str.regexp "<=" in
	
	let re_or = Str.regexp "||" in 
	let re_and = Str.regexp "&&" in
	let re_not = Str.regexp "!" in
	let re_semi = Str.regexp ";" in
	let re_type_int = Str.regexp "int" in (*continue here *)
	let re_type_bool = Str.regexp "bool" in
	let re_print = Str.regexp "printf" in
	let re_main = Str.regexp "main" in
	let re_if = Str.regexp "if" in
	let re_else = Str.regexp "else" in
	let re_while = Str.regexp "while" in
	
	let re_plus = Str.regexp ">" in
	let re_sub = Str.regexp ">" in
	let re_mult = Str.regexp ">" in
	let re_div = Str.regexp "/" in
	let re_pow = Str.regexp "[\^]" in

	(* tokens with complex rules *)
	let re_bool = Str.regexp "true\\|false" in
	let re_int = Str.regexp "[-]?[0-9]+" in
	let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
	let re_skip = Str.regexp "[ \t\n]*" in

	(* inner function to process the current token *)
	let rec next_token str pos = 
		if pos >= (String.length str) 
			then [EOF]
		
		else if (Str.string_match re_lparen str pos) 
			then let token = Str.matched_string str in
			(Tok_LParen)::(next_token str (pos+1))
			
		else if (Str.string_match re_rparen str pos) 
			then let token = Str.matched_string str in
			(Tok_RParen)::(next_token str (pos+1))
			
		else if (Str.string_match re_lbrace str pos) 
			then let token = Str.matched_string str in
			(Tok_LBrace)::(next_token str (pos+1))
			
		else if (Str.string_match re_rbrace str pos) 
			then let token = Str.matched_string str in
			(Tok_RBrace)::(next_token str (pos+1))
			
		else if (Str.string_match re_equal str pos) 
			then let token = Str.matched_string str in
			(Tok_Equal)::(next_token str (pos+2))
			
		else if (Str.string_match re_notequal str pos) 
			then let token = Str.matched_string str in
			(Tok_NotEqual)::(next_token str (pos+2))	
			
		else if (Str.string_match re_assign str pos) 
			then let token = Str.matched_string str in
			(Tok_Assign)::(next_token str (pos+1))
			
		else if (Str.string_match re_greater str pos) 
			then let token = Str.matched_string str in
			(Tok_Greater)::(next_token str (pos+1))
			
		else if (Str.string_match re_less str pos) 
			then let token = Str.matched_string str in
			(Tok_Less)::(next_token str (pos+1))
			
		else if (Str.string_match re_greaterequal str pos) 
			then let token = Str.matched_string str in
			(Tok_GreaterEqual)::(next_token str (pos+2))
			
		else if (Str.string_match re_lessequal str pos) 
			then let token = Str.matched_string str in
			(Tok_LessEqual)::(next_token str (pos+2))
			
		else if (Str.string_match re_or str pos) 
			then let token = Str.matched_string str in
			(Tok_Or)::(next_token str (pos+1))
			
		else if (Str.string_match re_and str pos) 
			then let token = Str.matched_string str in
			(Tok_And)::(next_token str (pos+1))
			
		else if (Str.string_match re_not str pos) 
			then let token = Str.matched_string str in
			(Tok_Not)::(next_token str (pos+1))
			
		else if (Str.string_match re_semi str pos) 
			then let token = Str.matched_string str in
			(Tok_Semi)::(next_token str (pos+1))
			
		
		(* ADD MORE TOKENS HERE *)
		
		else if (Str.string_match re_while str pos) 
			then let token = Str.matched_string str in
			(Tok_While)::(next_token str (pos+(String.length token)))
		
		(* FINISH ADDING TOKENS HERE *)
						
		(* not preferring longest match - FIX THIS *)	
		else if (Str.string_match re_int str pos) 
			then let token = Str.matched_string str in
			(Tok_Int (int_of_string token))::(next_token str (pos+(String.length token)))
		
		else raise (InvalidInputException "Lex Error")
	in

(* call next_token on the start position of input *)		
next_token input 0
;;
	
