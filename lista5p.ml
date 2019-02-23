(*Marek Padlewski*)
(*Dopisano zad 2*)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a lBT = LEmpty | LNode of 'a * ('a lBT Lazy.t) * ('a lBT Lazy.t);;

let rec lfrom k = LCons (k, function () -> lfrom (k+1));;

let lhd = function
    LNil -> failwith "lhd"
  | LCons (x, _) -> x;;

let ltl = function
    LNil ->
     failwith "ltl"
  | LCons (_, xf) -> xf();;

let rec ltake = function
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf())

(*Zadanie 1*)                              
                              
let lFib =
  let rec fibo curr next =
    LCons (curr, function () ->
                LCons(next, function () ->
                           fibo (curr+next) (curr+next+next)))
  in fibo 0 1;;


ltake (10,lFib);;

(*Zadanie 3*)

let ints_from10 =
  let rec ints curr =
    LCons (curr, function () -> ints (curr+1))
  in ints 10;;
    

let sublist xs ll =
  let rec sub i l =
    if List.mem i xs then
      sub (i+1) (ltl l)
    else
      LCons (lhd l, function () -> sub (i+1) (ltl l))
  in sub 0 ll;;

ltake(8, (sublist [1;4;7;2] ints_from10));;



(*Zadanie 2*)

let rec lrepeat f xs =
  let rec aux = function
      (LCons(x, xf), i, 0) -> aux(xf(), i+1, (f (i + 1)))
    | (LCons(x, xf) as xl, i, r) -> LCons(x, fun() -> aux(xl, i, (r - 1)))
    | (LNil, _, _) -> LNil
  in aux(xs, 0, (f 0));;

let llExample = LCons(9, fun() -> LCons(8, fun() -> LCons(7, fun() -> LNil)));;


ltake(30, lrepeat (fun x -> x*2 + 1) (lfrom 1));;

ltake(30, lrepeat (fun x -> x+3) (lfrom 10));;

ltake(30, lrepeat (fun x -> x*4 + 1) LNil);;

ltake(30, lrepeat (fun x -> x+1) llExample);;

  
  
