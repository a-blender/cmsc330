:- module(arith,[
       gcd/3,
       factor/2,
       prime/1,
       partition/2
   ]).

% Predicate: factor(N,F)
% Description: F is a factor of N.
% Assumptions: N and F are positive integers.
% Notes: F is a factor of N iff N = K * F for some integer K.
% Usage: If N and F are positive integers, then factor(N,F) succeeds with all solutions for F.

factor(N,F) :-
	between(1,N,F),
	0 is N mod F.

% Predicate: gcd(A,B,D)
% Description: D is the greatest common divisor of A and B.
% Assumptions: A and B are nonnegative integers.
% Notes: Use the Euclidean algorithm to compute gcd(A,B,D).
% Usage: If A and B are nonnegative integers, then gcd(A,B,D) succeeds with one solution for D.

gcd(A,0,A) :- !. 

gcd(A,B,D) :- 
	A >= B, 
	X is A mod B, 
	gcd(B,X,D).

gcd(A,B,D) :- 
	A < B, 
	gcd(B,A,D).

% Predicate: prime(N)
% Description: N is prime.
% Assumptions: N is a nonnegative integer.
% Notes: N is prime iff its only positive factors are 1 and N.
% Usage: If N is a nonnegative integer, then prime(N) succeeds iff N is prime.

divisible(X,Y) :- 
	N is Y*Y,
	N =< X,
	X mod Y =:= 0.

divisible(X,Y) :-
	Y < X,
	Y1 is Y+1,
	divisible(X,Y1).

prime(N) :-
	N > 1,
	X is 2,
	\+divisible(N,X).

% Predicate: partition(N,Part)
% Description: Part is a partition of N into primes.
% Assumptions: N is a positive integer and Part an ordered list of distinct positive integers.
% Notes: A partition of a positive integer N is a set S of positive integers such that S sums to N.
% Usage: If N is a positive integer, then partition(N,Part) succeeds with all solutions for Part.
% Hints: Compare your solution against OEIS A000586 for a quick sanity check.

partition(0,[]).

partition(N,Part) :-
	between(1,N,X), prime(X),
	List = [X|T], Next is N-X,
	partition(Next,T), msort(List,NewList), 
	Part = NewList, is_set(Part).
	 
	
	
		

	 
