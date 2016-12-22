(* Functions for operations on binary numbers represented as arrays of booleans *)

let twos_compl bin =
  let complement = Array.copy bin in
  let foundOne = ref false in
  for i = (Array.length bin) - 1 downto 0 do
    if !foundOne then complement.(i) <- not complement.(i)
    else if complement.(i) then foundOne := true;
  done;
  complement;;

let int_of_binary_unsigned bin =
  let result = ref 0.0 in
  for i = 0 to Array.length bin - 1 do
    match bin.(i) with
      | true -> result := !result +. 2. ** float_of_int ((i - ((Array.length bin) - 1)) * -1);
      | _ -> ();
  done;
  int_of_float !result;;

let int_of_binary_signed bin =
  if not bin.(0) then int_of_binary_unsigned bin
  else -1 * int_of_binary_unsigned (twos_compl bin);;

let rec binary_of_int n =
  if n < 0 then twos_compl (binary_of_int (n * -1))
  else begin
    let num = ref n in
    let bin = Array.make 32 false in
    for i = (Array.length bin) - 1 downto 0 do
      if (!num < 0) then bin.(i) <- false
      else begin
        match !num mod 2 with
          | 0 -> bin.(i) <- false; num := !num / 2;
          | 1 -> bin.(i) <- true; num := !num / 2;
          | _ -> failwith "Something is terribly wrong with modular arithmetic!"
      end;
    done;
    bin;
  end;;

let plus op1 op2 =
  let carry = ref false in
  let sum = Array.make 32 false in
  for i = (Array.length op1) - 1 downto 0 do
    match (op1.(i), op2.(i), !carry) with
      | (false, false, false) -> ();
      | (false, false, true) -> sum.(i) <- true; carry := false;
      | (false, true, false) -> sum.(i) <- true;
      | (false, true, true) -> carry := true;
      | (true, false, false) -> sum.(i) <- true;
      | (true, false, true) -> carry := true;
      | (true, true, false) -> carry := true;
      | (true, true, true) -> sum.(i) <- true; carry := true;
  done;
  sum;;

let minus op1 op2 =
  plus op1 (twos_compl op2);;
