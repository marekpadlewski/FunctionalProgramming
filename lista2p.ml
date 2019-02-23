(*Marek Padlewski*)

(*zadanie 1*)

let rec fib n = 
  if n = 0 || n = 1 
  then n
  else fib (n-1) + fib (n-2);;

fib(3);;
fib(5);;

let fib2 n =
  let rec help n2 n1 curr i =
    if i = (n-1) then curr
    else help n1 curr (n1 + curr) (i+1)
  in help 0 1 1 1;;

fib2(3);;
fib2(5);;
fib2(42);;

(*W metodzie pierwszej na obliczenie 42 elementu musimy czekac ok 15 sekund, przy rek ogonowej wynik mamy od razu*)


(*zadanie 2*)

let (<->) v1 v2 =
  match (v1,v2) with
    (x,y) when  x = y -> 0.
   |([xa;ya;za],[xb;yb;zb]) ->
     sqrt((xa -. xb) *. (xa -. xb) +. (ya -. yb) *. (ya -. yb) +. (za -. zb) *. (za -. zb));;

([1.;1.;1.] <-> [1.;1.;0.]);;
([2.;3.;4.] <-> [10.;17.;42.]);;

(*zadanie 3*)

let rec (<--) x y =
  match x with
    [] -> y :: []
  | hd :: tl -> if y <= hd then
                  y :: hd :: tl
                else
                  hd :: (tl <-- y);;

[1;3;5;5;7] <-- 2;;
[1;2] <-- 0;;
[17;17] <-- 18;;


(*zadanie 4*)

let rec take k l =
  match l with
    [] -> []
   | hd :: tl -> if k > 0 then
                  hd :: (take (k-1) tl)
                else
                  [];;

take 2 [1;2;3;5;6];;
take (-2) [1;2;3;5;6];;
take 8 [1;2;3;5;6];;


(*zadanie 5*)

let rec drop k l =
  match l with
    [] -> []
   | hd :: tl -> if k <= 0 then
                  hd :: (drop k tl)
                else
                  drop (k-1) tl;;

drop 2 [1;2;3;5;6];;
drop (-2) [1;2;3;5;6];;
drop 8 [1;2;3;5;6];;


(*egzamin*)

let a = 5;;
let bref = ref a;;
let cref = bref;;
let a = 3;;
let f x = !bref + a;;
let a = 1;;
bref:=2;;
cref:=4;;

type foo = A | B of int | C of int list;;

let f1 x = match x with y -> 1;;
let f2 x = match x with A -> 0 | B y -> 1 | C y -> 2;;
let f3 x = match x with A -> 0 | B y -> 1 | C (h::t) -> 2;;
let f4 x = match x with A -> 0 | _ -> 2 | B _ -> 1;;

let f5 = List.fold_left (fun x y -> if y mod 2 = 0 then y::x else x) [] [3;4;5;6]

let f z y x = z (y x)

let g x = truncate x

let res = g 5
