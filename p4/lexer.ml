open SmallCTypes

let tokenize input = 
	
	let re_while = Str.regexp "while" in
	let re_int = Str.regexp "-?[0-9]+" in

	(* inner function to process the current token *)
	let rec next_token str pos = 
		if pos >= (String.length str) 
			then [EOF]
		
		else if (Str.string_match re_while str pos) 
			then let token = Str.matched_string str in
			(Tok_While)::(next_token str (pos+(String.length token)))
			
		else if (Str.string_match re_int str pos) 
			then let token = Str.matched_string str in
			(Tok_Int (int_of_string token))::(next_token str (pos+(String.length token)))
		
		else raise (InvalidInputException "Lex Error")
	in

(* call next_token on the start position of input *)		
next_token input 0
;;
	
