open SmallCTypes

let tokenize input = 
	
	let re_while = Str.regexp "while" in
	let re_type_int = Str.regexp "[0-9]" in

	(* inner function to process the current token *)
	let rec next_token str pos = 
		if pos >= (String.length str) 
			then [Tok_END]
		
		else if (Str.string_match re_while str pos) 
			then let token = Str.matched_string str in
			(Tok_WhileNum)::(next_token str (pos+(String.length token)))
			
		else if (Str.string_match re_type_int str pos) 
			then let token = Str.matched_string str in
			(Tok_Type_Int token.[0])::(next_token str (pos+1))
		
		else raise (IllegalExpression "tokenize")
	in

(* call next_token on the start position of input *)		
next_token input 0
;;
	
