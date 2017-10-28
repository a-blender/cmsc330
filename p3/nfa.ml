(* CMSC 330 / Fall 2017 / Project 3 *)
(* Anna Blendermann, 114474025 *)

type transition = int * char option * int
type stats = {num_states : int; num_finals : int; outgoing_counts : (int * int) list}

let get_next_gen () =
  let x = ref 0 in
  (fun () -> let r = !x in x := !x + 1; r)
let next = get_next_gen ()

let int_list_to_int =
  let next' = get_next_gen () in
  let tbl = Hashtbl.create 10 in
  let compare a b = if a < b then -1 else if a = b then 0 else 1 in
  (fun (lst : int list) ->
      let slst = List.sort_uniq compare lst in
      if Hashtbl.mem tbl slst then Hashtbl.find tbl slst
    else let n = next' () in Hashtbl.add tbl slst n; n)

(* YOUR CODE BEGINS HERE *)

type nfa_t = (int * int list * transition list)  

let get_start m = match m with
	| (ss,fs,ts) -> ss
;;

let get_finals m = match m with
	| (ss,fs,ts) -> fs
;; 

let get_transitions m = match m with
	| (ss,fs,ts) -> ts
;;

let make_nfa ss fs ts = (ss,fs,ts)

(* E-CLOSURE FUNCTIONS *)

let concat_lists lst1 lst2 =
	List.fold_left (fun l x -> if (List.mem x l)=false then x::l
		else l) lst2 lst1
;;

let ep_edges n l =
	List.fold_left (fun lst (x,y,z) -> if x=n && y=None then z::lst 
		else lst) [] l
;;

let rec closure_help n m l =
	let list = n::l in
	let edges = ep_edges n (get_transitions m) in
	List.fold_left (fun lst e -> if (List.mem e list)=false 
		then (closure_help e m lst) else lst) list edges
;;

let e_closure m l = match l with
	| [] -> []
	| h::t -> List.fold_left (fun lst e -> 
		concat_lists (closure_help e m []) lst) [] l
;; 

(* END OF E-CLOSURE FUNCTIONS *)

(* MOVE FUNCTIONS *)

let get_edges n l c = 
	List.fold_left (fun lst (x,y,z) -> if x=n && y=Some c && 
		(List.mem z lst)=false then z::lst else lst) [] l
;;

let move m l c =
	List.fold_left (fun lst x -> 
		concat_lists lst (get_edges x (get_transitions m) c)) [] l
;;
 
(* END OF MOVE FUNCTIONS *)

(* NFA -> DFA FUNCTIONS *)

let rec remove_head lst = match lst with
	| [] -> []
	| h::t -> t
;;

let get_alphabet m = 
	let l = get_transitions m in
	List.fold_left (fun lst x -> match x with
		| (s,None,d) -> lst
		| (s,Some c,d) -> if (List.mem c lst)=false then c::lst
			else lst) [] l
;;

let update_lists r1 r2 l = 
	List.fold_left (fun lst e -> if (List.mem e r1)=false then lst@[e]
		else lst) r2 l
;;   

let check_fs m l = match m with 
	| (ss,fs,ts) ->
		let lst = List.map (fun x -> (List.mem x l)) fs in
		let stat = (List.mem true lst) in stat
;;

let rec nfa_help nfa r1 r2 d = match r2 with
	| [] -> d
	| h::t -> nfa_help2 nfa r1 r2 h d 

and nfa_help2 nfa r1 r2 x d =  
	
	(* move state x from unvisited list to visited list *)
	let uv = remove_head r2 in
	let v = r1@[x] in

	(* get tuple -> (reachable states from x, transition list) *)

	let (sl,fl,tl) = List.fold_left (fun (l1,l2,l3) a ->
		let s = move nfa x a in
		let e = (e_closure nfa s) in match e with
			| [] -> (l1,l2,l3)
			| h::t -> let curr = int_list_to_int x in
			let next = int_list_to_int e in
		
		if (check_fs nfa e)=true then
		(l1@[e], l2@[next], l3@[(curr, Some a, next)]) else
		(l1@[e], l2, l3@[(curr, Some a, next)])

	) ([],[],[]) (get_alphabet nfa) in
	
	(* update the unvisited list with the new states *)	
	let uv2 = update_lists v uv sl in

	(* update the dfa final state and transition list *)
	match d with
		| (f,t) -> let d_list = (f@fl, t@tl) in

	(* call nfa_help with the updated lists *)
	nfa_help nfa v uv2 d_list
;;

let nfa_to_dfa m = match m with
	| (x,y,z) -> let ss = int_list_to_int (e_closure m [x]) in
		let (fl,tl) = (nfa_help m [] [[ss]] ([],[])) in (ss, fl, tl)
;;

(* END OF NFA -> DFA FUNCTIONS *)

(* ACCEPT FUNCTIONS *)

let rec str_list s = match s with
    | "" -> []
    | s -> (String.get s 0)::(str_list (String.sub s 1 ((String.length s)-1)))
;;

let next_state ts s c = 
	List.fold_left (fun lst t -> match t with
		| (x,None,z) -> lst
		| (x,Some y,z) -> if x=s && y=c then z::lst else lst) [] ts
;;

let rec accept_help m fs s str = 
	let st = List.hd s in match str with
	| [] -> (List.mem st fs)
	| h::t -> if (move m [st] h)=[] then false else
		accept_help m fs (move m [st] h) t
;;
	
let accept m s = 
	let str = str_list s in
	match m with
	| (ss,fs,ts) -> if str=[] then false else (accept_help m fs [ss] str)

(* END OF ACCEPT FUNCTIONS *)

(* STATS FUNCTIONS *)

let num_states m = 
	let t = get_transitions m in
	let s = List.fold_left (fun lst (x,y,z) ->  x::z::lst) [] t in
		List.length (List.sort_uniq compare s)
;;

let num_finals m = match m with
	| (ss,fs,ts) -> List.length fs
;;

let outgoing_counts m = failwith "Unimplemented"

let stats m = failwith "Unimplemented"

(* ANNA TEST *)
let m = make_nfa 0 [2] [(0, Some 'a', 1); (1, None, 2)];;
let n = make_nfa 0 [1] [(0, Some 'a', 1); (1, Some 'b', 0)];;

let x = make_nfa 0 [2;4] [(0, Some 'a', 1); (1, None, 2); (1, None, 3); (3, Some 'a', 4); (4, Some 'b', 0)];;

let m1 = make_nfa 0 [1; 3] [(0,Some 'a',1); (0,Some 'a',2); (2,Some 'b',3)];;
let m2 = make_nfa 0 [2] [(0,None, 1); (1, Some 'b', 2)];;


