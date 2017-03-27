
(* Problem 1 *)
exception No_Real_Roots

let quadratic a b c =
  let disc = b ** 2. -. 4. *. a *. c in
  if disc < 0.0 then raise No_Real_Roots
  else ((-. b +. sqrt disc) /. (2. *. a)),
    ((-. b -. sqrt disc) /. (2. *. a)) ;;

(* Problem 2 *)
let third (_, _, item) = item ;;

(* Problem 3 *)
let rec reverse lst =
  match lst with
  | [] -> []
  | head::tail -> reverse tail @ [head] ;;

(* Problem 4 *)
let rec member item lst =
  match lst with
  | [] -> false
  | head::tail -> item = head || member item tail ;;

(* Problem 5 *)
let rec union lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | head::tail ->
      if member head lst2 then union tail lst2
      else head :: union tail lst2 ;;
 
(* Problem 6 *)
let rec intersect lst1 lst2 =
  match lst1 with
  | [] -> []
  | head::tail ->
      if member head lst2 then head :: intersect tail lst2
      else intersect tail lst2 ;;

(* Problem 7 *)
let rec partition pivot lst =
  match lst with
  | [] -> [], []
  | head::tail ->
      let part1, part2 = partition pivot tail in
      if head < pivot then head :: part1, part2
      else part1, head :: part2;;

(* Problem 8 *)
let rec quicksort lst =
  match lst with
  | [] -> []
  | pivot::rest ->
      let part1, part2 = partition pivot rest in
      quicksort part1 @ [pivot] @ quicksort part2;;


