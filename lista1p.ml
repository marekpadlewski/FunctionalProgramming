(*Marek Padlewski*)
(*Dodano zadanie 4 i 5*)

(*zadanie 1*)

let f1 (x,y) = (x+y,x*y);;

let f2 (x,y) = x +. y = y -. x;;

let f3 (l,x) =
  if x+1 = x
  then List.rev l
  else List.tl l;;


(*zadanie 2*)

let ends l =
  if l = []
  then failwith "Empty list!"
  else let first = List.hd l
       in let last = List.hd(List.rev l)
       in (first,last);;

ends([1;2;3]);;

ends([1]);;


(*zadanie 3*)

let rec isSorted l =
  if l = [] or List.tl l = []
  then true
  else if List.hd l > List.hd (List.tl l)
  then false
  else isSorted(List.tl l);;

isSorted([1;2;3]);;
isSorted([1;2;5;5]);;
isSorted([1;5;3;4]);;
isSorted([10]);;
isSorted([]);;

(*zadanie 4*)


let rec powers p =
  let rec help i prev =
    if i > snd(p) then prev
    else let curr = fst(p) * (List.hd prev) :: prev
         in help (i+1) curr 
          
  in List.rev (help 1 [1]);;

powers(2,3);;
powers(3,10);;
powers(1,4);;

(*zadanie 5*)

let split (l,e) =
  let rec help low gre curr =
    if  curr = [] then (low,gre)
    else if (List.hd curr) <= e
    then help ((List.hd curr) :: low) gre (List.tl curr)
    else help low ((List.hd curr) :: gre) (List.tl curr)

  in help [] [] l;;

split(['a';'s';'h';'g'],'g');;
split([10;12],11);;
split(['o'],'t');;
split(['r'], 'b');;