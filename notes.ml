[1; 2; 3];;
let five_primes = [| 1; 2; 3; 4; 5 |];;
five_primes.(2);;
five_primes.(2) <- 4;;
let greeting = "Hi, mom!";;
greeting.[7];;
let mercury = ("hg", 80, 200.592);;
fst mercury;;
snd mercury;;
type element = {name: string; atomic_number: int; atomic_weight: float};;
let mercury = {atomic_number = 80; name = "Hg"; atomic_weight = 200.592};;
mercury.atomic_number;;
type sale_item = {name: string; mutable price: float};;
let my_item = {name = "bike"; price = 699.95};;
my_item.price <- 800.;;
(*exceptions*)
exception Not_Found;; (*declared before the function*)
(* Pattern Matching *)
let atomic_number (_, n, _) = n;; (* The underscore is a wild card*)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;
let rec inorder t = 
	match t with 
	| Empty -> []
	| Node (v, left, right) -> inorder left @ [v] @ inorder right;;
let rec find key l =
	match l with
	| [] -> raise Not_found
	| (k, v) :: rest when k = key -> v
	| head :: rest -> find key rest;;
let squares = [(1 ,1); (2,4); (3,9); (4 ,16); (5 ,25)];;
find 3 squares ;; (* returns 9 *)
find 6 squares ;; (* raises a Not_found exception *)

# let s = ((1, 2), (3, 4));;
val s : (int * int) * (int * int) = ((1, 2), (3, 4))
# find 1
  ;;
- : (int * '_a) list -> '_a = <fun>
# find 1;;
- : (int * '_a) list -> '_a = <fun>
# let (((x1, y1) as p1), ((x2, y2) as p2)) = s;;
val x1 : int = 1
val y1 : int = 2
val p1 : int * int = (1, 2)
val x2 : int = 3
val y2 : int = 4
val p2 : int * int = (3, 4)
# let rec append l1 l2 =
        if l1 = [] then l2
        else let h::t = in h :: append t l2;;
Error: Syntax error
# let rec append l1 l2 =
        if l1 = [] then l2
        else let h::t = l1 in h :: append t l2;;
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val append : 'a list -> 'a list -> 'a list = <fun>
# let rec append l1 l2 =
        match l1 with
        | [] -> l2
        | h::t -> h :: append t l2;;
Error: Syntax error
# let rec append l1 l2 =
        match l1 with
        | [] -> l2
        | h::t -> h :: append t l2;;
val append : 'a list -> 'a list -> 'a list = <fun>
# append [] [1; 2; 3];;
- : int list = [1; 2; 3]
# append [1; 2; 3] [4; 5; 6];;
- : int list = [1; 2; 3; 4; 5; 6]

 let rec append l1 l2 =
        match l1 with
        | [] -> l2
        | h::t -> h :: append t l2;;
val append : 'a list -> 'a list -> 'a list = <fun>
# append [] [1; 2; 3];;
- : int list = [1; 2; 3]
# append [1; 2; 3] [4; 5; 6];;
- : int list = [1; 2; 3; 4; 5; 6]
# let stats l =
        let rec helper rest n sum sum_squares =
                match rest with
                | [] -> let nf = float_of_int n in
                        (sum /. nf, sqrt (sum_squares /. nf))
                | h :: t ->
                        helper t (n +1) (sum +. h) (sum_squares +. (h *. h)) in
        helper l 0 0. 0. ;;
val stats : float list -> float * float = <fun>
# stats [1.; 2.; 3.; 4.; 5.];;
- : float * float = (3., 3.3166247903554)
# let (mean, sd) = stats [1.;2.;3.;4.;5.];;
val mean : float = 3.
val sd : float = 3.3166247903554
# let insertion_sort a =
        for i = 1 to Array.length a - 1 do
                let t = a.(i) in
                let j = ref i in
                while !j > 0 && t < a.(!j -1) do
                        a.(!j) <- a.(!j -1);
                        j := !j - 1
                done;
                a.(!j) <- t
        done;;
val insertion_sort : 'a array -> unit = <fun>
# Interrupted.
#

if lst = [] then ... else ...



