(* Functions for operations on binary numbers *)

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
