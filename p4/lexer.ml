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
	let re_type_int = Str.regexp "int" in 
	let re_type_bool = Str.regexp "bool" in
	let re_print = Str.regexp "printf" in
	let re_main = Str.regexp "main" in (*continue here *)
	let re_if = Str.regexp "if" in
	let re_else = Str.regexp "else" in
	let re_while = Str.regexp "while" in
	
	let re_plus = Str.regexp "+" in
	let re_sub = Str.regexp "-" in
	let re_mult = Str.regexp "*" in
	let re_div = Str.regexp "/" in
	let re_pow = Str.regexp "\\^" in

	(* tokens with complex rules *)
	let re_bool = Str.regexp "true\\|false" in
	let re_int = Str.regexp "[-]?[0-9]+" in
	let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
	
	let re_skip = Str.regexp "[ \t\n]*" in
	let re_extra = Str.regexp "[a-zA-Z0-9]+" in

	(* inner function to process the current token *)
	let rec next_token str pos = 
		if pos >= (String.length str) 
			then [EOF]
		
		(* Tok_LParen *)
		else if (Str.string_match re_lparen str pos) 
			then (Tok_LParen)::(next_token str (pos+1))
			
		(* Tok_RParen *)
		else if (Str.string_match re_rparen str pos) 
			then (Tok_RParen)::(next_token str (pos+1))
		
		(* Tok_LBrace *)	
		else if (Str.string_match re_lbrace str pos) 
			then (Tok_LBrace)::(next_token str (pos+1))
		
		(* Tok_RBrace *)	
		else if (Str.string_match re_rbrace str pos) 
			then (Tok_RBrace)::(next_token str (pos+1))
			
		(* match the longer operators first *)
		(* Tok_Equal *)
		else if (Str.string_match re_equal str pos) 
			then (Tok_Equal)::(next_token str (pos+2))
			
		(* Tok_NotEqual *)
		else if (Str.string_match re_notequal str pos) 
			then (Tok_NotEqual)::(next_token str (pos+2))	
			
		(* Tok_GreaterEqual *)	
		else if (Str.string_match re_greaterequal str pos) 
			then (Tok_GreaterEqual)::(next_token str (pos+2))
		
		(* Tok_LessEqual *)	
		else if (Str.string_match re_lessequal str pos) 
			then (Tok_LessEqual)::(next_token str (pos+2))	
		
		(* now match the one char operators *)
		(* Tok_Assign *)
		else if (Str.string_match re_assign str pos) 
			then (Tok_Assign)::(next_token str (pos+1))	
			
		(* Tok_Greater *)
		else if (Str.string_match re_greater str pos) 
			then (Tok_Greater)::(next_token str (pos+1))
			
		(* Tok_Less *)
		else if (Str.string_match re_less str pos) 
			then (Tok_Less)::(next_token str (pos+1))
			
		(* Tok_Or *)
		else if (Str.string_match re_or str pos) 
			then (Tok_Or)::(next_token str (pos+2))
			
		(* Tok_And *)
		else if (Str.string_match re_and str pos) 
			then (Tok_And)::(next_token str (pos+2))
		
		(* Tok_Not *)	
		else if (Str.string_match re_not str pos) 
			then (Tok_Not)::(next_token str (pos+1))
			
		(* Tok_Semi *)	
		else if (Str.string_match re_semi str pos) 
			then (Tok_Semi)::(next_token str (pos+1))
				
		(* match for keywords *)
		(* if there are chars/ints after then match tok_id *)
		
		(* Tok_Type_Int *)			
		else if (Str.string_match re_type_int str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_Type_Int)::(next_token str new_pos)
			
		(* Tok_Type_Bool *)
		else if (Str.string_match re_type_bool str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_Type_Bool)::(next_token str new_pos)
			
		(* Tok_Print *)
		else if (Str.string_match re_print str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_Print)::(next_token str new_pos)
		
		(* Tok_Main *)
		else if (Str.string_match re_main str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_Main)::(next_token str new_pos)
		
		(* Tok_If *)
		else if (Str.string_match re_if str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_If)::(next_token str new_pos)
		
		(* Tok_Else *)
		else if (Str.string_match re_else str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_Else)::(next_token str new_pos)
		
		(* Tok_While *)
		else if (Str.string_match re_while str pos) 
			then let token_key = Str.matched_string str in
			let new_pos = Str.match_end() in
			
			if (Str.string_match re_extra str new_pos)
				then let token_id = Str.matched_string str in
				(Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
			else (Tok_While)::(next_token str new_pos)
		
		(* now process the urnary operators *)
		
		(* Tok_Plus *)
		else if (Str.string_match re_plus str pos) 
			then (Tok_Plus)::(next_token str (pos+1))
			
		(* Tok_Sub *)
		else if (Str.string_match re_sub str pos) 
			then (Tok_And)::(next_token str (pos+1))
			
		(* Tok_Mult *)
		else if (Str.string_match re_mult str pos) 
			then (Tok_And)::(next_token str (pos+1))
			
		(* Tok_Div *)
		else if (Str.string_match re_div str pos) 
			then (Tok_And)::(next_token str (pos+1))
		
		(* Tok_Pow *)
		else if (Str.string_match re_pow str pos) 
			then (Tok_And)::(next_token str (pos+1))
		
		(* finish by processing complex tokens *)
		
		(* Tok_Bool *)
		else if (Str.string_match re_bool str pos)
			then let token = Str.matched_string str in
			(Tok_Bool (bool_of_string token))::(next_token str (Str.match_end()))
		
		(* Tok_Int *)						
		else if (Str.string_match re_int str pos)
			then let token = Str.matched_string str in 
			(Tok_Int (int_of_string token))::(next_token str (Str.match_end()))
			
		(* Tok_ID *)					
		(* id also checked after keywords too *)
		else if (Str.string_match re_id str pos)
			then let token = Str.matched_string str in 
			(Tok_ID token)::(next_token str (Str.match_end()))
		

		(* Whitespace, Tab, Newline check *)
		else if (Str.string_match re_skip str pos)
			then (next_token str (Str.match_end()))

		else 
			raise (InvalidInputException "Lex Error")
	in

(* call next_token on the start position of input *)		
next_token input 0
;;
	
