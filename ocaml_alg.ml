

(* Question 1    [-b +/- sqrt(b^2 - 4ac)] / 2a   *)
exception No_Real_Roots;;
let quadratic a b c = 
	let s = (b *. b -. 4 *. a *. c) in
	if s < 0 
	then raise No_Real_Roots
	else
		let r1 = (-.b +. sqrt (s)) /. (2. *. a) in
		let r2 = (-.b -. sqrt (s)) /. (2. *. a) in
		(r1, r2);;
 
(* quadratic 4. 5. 1.;; *)

(* Question 2 *)

let third (_, _, n) = n;;

(* third (1 , 2 , 3);; *)

(* Question 3 *)
let rec reverse l =
	match l with
	| [] -> l
	| head :: rest -> reverse rest@[head];;

(* Question 4 *)
let rec member i l =
	match l with
	| [] -> false
	| head :: rest -> if i = head then true else member i rest;;

(* Question 5 *)
(* use member to filter out duplicates *)
let rec union l1 l2 =
	match l1 with
	| [] -> l2
	| head :: rest -> 	if member head l2
						then union rest l2
						else (union rest l2)@[head];;
						

(* Question 6 *)

let rec intersect l1 l2 =
	match l1 with
	| [] -> []
	| head :: rest -> 	if member head l2
						then (intersect rest l2)@[head]
						else intersect rest l2;;
(* Question 7 *) 

let rec partition p l =
	let rec helper p l l1 l2 =
		match l with
		| [] -> (l1, l2)
		| head :: rest -> 	if head < p 
							then helper p rest (l1@[head]) l2 
							else if head > p then helper p rest l1 (l2@[head])
							else helper p rest l1 l2 in
	helper p l [] [];;
(* Question 8 *) 

let rec quicksort l =
	match l with
	| [] -> []
	| head :: rest -> 	let lhs, rhs = partition head rest in
						quicksort lhs@([head]@quicksort rhs);;
