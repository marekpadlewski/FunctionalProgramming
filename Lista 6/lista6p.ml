(*Marek Padlewski*)

(*Zadanie 1*)

let zgadnij() =
  begin
    print_string "Wpisz liczbe pomiedzy 0 a 100: ";
    let num = Random.int 101
    and input = ref (read_int()) in
    while !input <> num do 
      if !input > num then
        print_string "Moja jest mniejsza. Podaj liczbe ponownie: "
      else if !input < num then
        print_string "Moja jest wieksza. Podaj liczbe ponownie: ";
      input := read_int()
    done;
    print_endline "Brawo, zgadles!"
  end;;


(*Zadanie 3*)

let sortuj_plik() =
  begin
    let ic = open_in "liczby.in" in
    let ib = Scanf.Scanning.from_channel ic in
    let amount = (Scanf.bscanf ib " %d" (fun x -> x))  in
    let arr = (Array.make amount 0) in
    for i = 0 to (amount - 1) do
      arr.(i) <- (Scanf.bscanf ib " %d" (fun x -> x));
    done;
    close_in ic;
    Array.stable_sort compare arr;
    let oc = open_out "liczby.out" in
    for i = 0 to (Array.length arr) - 1 do
      Printf.fprintf oc "%d " arr.(i);
    done;

    close_out oc;
    
  end;;
 
    
