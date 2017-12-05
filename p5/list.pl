:- module(list,[
       product/2,
       flat/2,
       index/3,
       nodups/2,
       powerset/2
   ]).

% Predicate: product(List,Prod)
% Description: Prod is the product of every number in List.
% Assumptions: The product of the empty list is 1.
% Usage: If List is a list of integers, then Product(List,Prod) succeeds with a unique solution for Prod.

product([],1).

product([H], H). 

product([H|T], Prod) :-
	product(T, Prod1),	
   	Prod is H*Prod1. 

% Predicate: index(List,Elem,Index)
% Description: Index is the index of Elem in List.
% Assumptions: Index is a nonnegative integer.
% Usage: If List is a list, then index(List,Elem,Index) succeeds with all solutions for Elem and Index.
% Notes: In this case, order and uniqueness of solutions matters.
% Hints: Define a tail-recursive helper predicate.

index([H|_],H,0).

index([_|T],Elem,Index) :-
	index(T,Elem,Index1),
	Index is Index1+1.

% Predicate: flat(NestedList,FlatList)
% Description: FlatList is the result of removing one level of nesting from NestedList.
% Usage: If NestedList is a list, then flat(NestedList,FlatList) succeeds with one solution for FlatList.

flat([],[]).

flat([H|T],[H|T2]) :-
	flat(T,T2).

flat([H|T],Flatlist) :-
	is_list(H),
	flat(T,Newlist),
	append(H,Newlist,Flatlist).

% Predicate: nodups(List,Unique)
% Description: Unique is List with all duplicates removed.
% Notes: The order of elements in Unique is the order of elements in List.
% Usage: If List is a list, then nodups(List,Unique) succeeds with one solution for Unique.

nodups(List,Unique) :-
	list_to_set(List,Unique),
	!.

% Predicate: powerset(Set,Sub)
% Description: Sub is an element of the powerset of Set
% Assumptions: Set and Pow are ordered lists without duplicates.
% Usage: If Set is an ordered list without duplicates, then powerset(Set,Sub) succeeds with all solutions for Sub.
% Notes: The powerset of a set S is the set of all subsets of S.
% Hints: Give a recursive definition of the powerset operation. In the recursive case, you must make one binary choice.

powerset([],[]).

powerset([_|T],Sub) :-
	powerset(T,Sub).

powerset([H|T],[H|Sub]) :-
	powerset(T,Sub).


