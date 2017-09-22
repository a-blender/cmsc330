(****************************)
(* Part 1: Simple Functions *)
(****************************)

let mult_of_y x y =
	if (x mod y) == 0 then true else false
;; 

let head_divisor lst = 
	match lst with
	| [] -> false
	| h::[] -> false
	| h::s::t -> if (h mod s) == 0 then true else false
;;

let second_element lst =
	match lst with
	| [] -> -1
	| h::[] -> -1
	| h::s::t -> s
;;

let sum_first_three lst = 
	match lst with
	| [] -> 0
	| h::[] -> h
	| h::x::[] -> (h + x)
	| h::x::y::t -> (h + x + y) 
;;

(************************************)
(* Part 2: Recursive List Functions *)
(************************************)


let rec get_val i lst = 
	match lst with
	| [] -> -1
	| h::t -> if i=0 then h else get_val (i-1) t 
;;

let rec get_vals is lst = 
	match is with
	| [] -> []
	| h::t -> get_val h lst::(get_vals t lst)  
;;

let rec list_swap_val lst x y =
	match lst with
	| [] -> []
	| h::t -> (if h=x then y
		else if h=y then x
		else h)::(list_swap_list t x y)
;;

let rec unzip lst = failwith "unimplemented"

let rec index_help x lst curr = failwith "unimplemented"
	

let rec index x lst = failwith "unimplemented"

(****************)
(* Part 3: Sets *)
(****************)

let rec insert x a = failwith "unimplemented"

let rec eq a b = failwith "unimplemented"

let rec card a = failwith "unimplemented"

let rec elem x a = failwith "unimplemented"

let rec remove x a = failwith "unimplemented"

let rec union a b = failwith "unimplemented"

let rec intersection a b = failwith "unimplemented"

let rec subset a b = failwith "unimplemented"
