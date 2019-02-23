(*Marek Padlewski*)
(*Dopisano zad 1b 1c 4*)

(*Zadanie 1*)

(* a *)
let rec czyIstnieje p xs =
  match xs with
    [] -> false
  | hd :: tl -> if p(hd) = true then
                  true
                else
                  czyIstnieje p tl;;

czyIstnieje (function x -> x=2) [1;2;3;5];;
czyIstnieje (function x -> x<0) [-5;2;7;10];;
czyIstnieje (function x -> x mod 2 = 0) [1;3;5;7];;

(* b *)
let czyIstniejeFL f xs =
  List.fold_left (fun x y -> if f y then x || true else x || false) false xs;;

czyIstniejeFL (function x -> x=2) [1;2;3;5];;
czyIstniejeFL (function x -> x<0) [-5;2;7;10];;
czyIstniejeFL (function x -> x mod 2 = 0) [1;3;5;7];;
 
(* c *)
let czyIstniejeFR f xs =
  List.fold_right (fun x y -> if f x then true || y else false || y) xs false;;

czyIstniejeFR (function x -> x=2) [1;2;3;5];;
czyIstniejeFR (function x -> x<0) [-5;2;7;10];;
czyIstniejeFR (function x -> x mod 2 = 0) [1;3;5;7];;


(*Zadanie 2*)

let rec filter p xs =
  List.fold_right (fun x y -> if p x then x::y else y) xs [];;

filter (fun x -> x > 0) [1;-5;4;0;7];;
filter (fun x -> x mod 3 = 0) [1;3;6;7;10;12];;

(*Zadanie 3*)

let rec usun1_a p xs =
  match xs with
    [] -> []
  | hd :: tl -> if p hd then
                  tl
                else
                  hd :: usun1_a p tl;;

usun1_a (fun x -> x=2) [1;2;3;2;5];;
usun1_a (fun x -> x<0) [-1;-2;-3;0];;
usun1_a (fun x -> x>3) [4];;


let usun1_b p xs =
  let rec del l acc =
    match l with
      [] -> acc
    | hd :: tl ->  if p hd then
                     del [] (List.rev_append acc tl)
                   else
                     del tl (hd::acc)
  in del xs [];;

usun1_b (fun x -> x=2) [1;2;3;2;5];;
usun1_b (fun x -> x<0) [-1;-2;-3;0];;
usun1_b (fun x -> x>3) [];;


(*Zadanie 4*)

let rec merge cmp x y = match (x,y) with 
  | ([],_) -> y
  | (_,[]) -> x
  | (h1::t1, h2::t2) -> 
    if cmp h2 h1 
    then h1::(merge cmp t1 y)
    else h2::(merge cmp x t2)

and split x y z = match x with
  | [] -> (y,z)
  | x::resto -> split resto z (x::y)

and  mergesort cmp x = match x with
  | ([] | _::[]) -> x
  | _ -> let (pri,seg) = split x [] [] 
     in (merge cmp (mergesort cmp pri) (mergesort cmp seg));;
 
mergesort (>) [2;6;1;8];;
mergesort (fun (x1,y1) (x2,y2) -> x1 > x2)
          [(3,"a");(1,"b");(3,"i");(7,"c");(9,"e");(3,"f");(5,"g")];;
