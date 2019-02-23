(*Marek Padlewski*)

type 'a t = { mutable size : int; mutable amount : int; mutable head : int;
              mutable tail : int; mutable a : 'a option array }
exception Empty of string
exception Full of string

let isEmpty q =
  q.amount = 0

let isFull q =
  q.amount = q.size
                
let create n =
  {size = n ; amount = 0 ; a = Array.make n None ; head = 0 ; tail = 0}

let enqueue(x,q) =
  if isFull q then
    raise (Full "module queue_mut: enqueue")
  else
    begin
      q.a.(q.tail) <- Some x;
      q.tail <- (q.tail + 1) mod q.size;
      q.amount <- succ q.amount;
    end

let dequeue q =
  if isEmpty q then
    raise (Empty "module queue_mut: dequeue")
  else
    begin
      q.a.(q.head) <- None;
      q.head <- (q.head + 1) mod q.size;
      q.amount <- pred q.amount;
    end

let first q =
  if isEmpty q then
    raise (Empty "module queue_mut: first")
  else
    match q.a.(q.head) with
      Some x -> x
    | None -> failwith
                "module queue_mut: first (implementation error!!!)" ;;
    
    
