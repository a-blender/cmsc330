open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count e lst =
	fold (fun acc x -> if x=e then acc+1 else acc) 0 lst
;;   

let divisible_by n lst =
	map (fun x -> if n=0 then true 
		else if (x mod n)=0 then true else false) lst
;;

let divisible_by_first lst = match lst with
	| [] -> []
	| h::t -> divisible_by h lst
;;

let pairup lst1 lst2 = 
	fold (fun acc x -> append acc (map (fun y -> (x,y)) lst2)) [] lst1
;;

let concat_lists lst = 
	fold (fun acc x -> append acc x) [] lst
;;

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
    IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
    match t with
      IntLeaf -> IntNode(x,IntLeaf,IntLeaf)
    | IntNode (y,l,r) when x > y -> IntNode (y,l,int_insert x r)
    | IntNode (y,l,r) when x = y -> t
    | IntNode (y,l,r) -> IntNode(y,int_insert x l,r)

let rec int_mem x t =
    match t with
      IntLeaf -> false
    | IntNode (y,l,r) when x > y -> int_mem x r
    | IntNode (y,l,r) when x = y -> true
    | IntNode (y,l,r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
	  IntLeaf -> 0
	| IntNode (y,l,r) -> 1 + (int_size r) + (int_size l)
;;

let rec int_max t = match t with
	IntLeaf -> raise (Invalid_argument("int_max")) 
	| IntNode (y,l,r) -> try int_max r with Invalid_argument _ -> y  
;;
	
let rec int_insert_all lst t = 
	fold (fun tree x -> int_insert x tree) t lst
;;

let rec int_as_list t = match t with
	IntLeaf -> []
	| IntNode (y,l,r) -> 
		append (int_as_list l) (append [y] (int_as_list r))
;;		

let rec common_help t x y = match t with
	IntLeaf -> raise (Invalid_argument("int_common"))
	| IntNode (n,l,r) when (x<n && y<n) -> common_help l x y
	| IntNode (n,l,r) when (x>n && y>n) -> common_help r x y
	| IntNode (n,l,r) -> n
;;

let rec int_common t x y = match t with
	IntLeaf -> raise (Invalid_argument("int_common"))
	| IntNode (n,l,r) -> common_help t x y
;;

(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert_help x cp t = match t with  
	Leaf -> Node (x,Leaf,Leaf)
	| Node (y,l,r) when (cp x y)<0 -> Node (y,pinsert_help x cp l,r)
	| Node (y,l,r) when (cp x y)=0 -> t
	| Node (y,l,r) -> Node (y,l,pinsert_help x cp r) 
;;

let pinsert x t = match t with
	| compfn, atree -> (compfn, pinsert_help x compfn atree)
;;

let rec pmem_help x cp t = match t with
	Leaf -> false
	| Node (y,l,r) when (cp x y)<0 -> pmem_help x cp l
	| Node (y,l,r) when (cp x y)=0 -> true
	| Node (y,l,r) -> pmem_help x cp r 
;;

let pmem x t = match t with
	| compfn, atree -> pmem_help x compfn atree
;;

(*******************************)
(* Part 4: Graphs with Records *)
(*******************************)

type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : int_tree; edges : edge list; }

let empty_graph = {nodes = empty_int_tree; edges = [] }

let add_edge e { nodes = ns; edges = es } =
    let { src = s; dst = d } = e in
    let ns' = int_insert s ns in
    let ns'' = int_insert d ns' in
    let es' = e::es in
    { nodes = ns''; edges = es' }

let add_edges es g = List.fold_left (fun g e -> add_edge e g) g es

(* Implement the functions below. *)

let graph_empty g = 
	let { nodes = tree; edges = lst } = g in
		if tree=empty_int_tree && lst=[] then true else false 
;; 

let graph_size g = 
	let { nodes = tree; edges = lst } = g in
	int_size tree
;;

let is_dst n e =  
	let { src = s; dst = d } = e in
		if n=d then true else false
;;

let src_edges n g = 
	let { nodes = tree; edges = lst } = g in
		fold (fun acc e -> let { src = s; dst = d } = e in
		if n=s then (append [e] acc) else acc) [] lst
;; 

let rec r_help n g t = 
	let tree = int_insert n t in
	let edges = src_edges n g in
	fold (fun tr e -> let { src = s; dst = d } = e in
		if (int_mem d tr)=false then (r_help d g tr) 
		else tree) tree edges
;;

let reachable n g =
	let { nodes = tree; edges = lst } = g in 
	if (int_mem n tree)=true 
		then (r_help n g empty_int_tree) else empty_int_tree
;;

