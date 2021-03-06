(* Functions for operations on binary numbers represented as arrays of booleans *)

let ba_of_bs bs =
  Array.init (String.length bs) (fun i -> if bs.[i] == '1' then true else false)

let bs_of_ba ba =
  String.init (Array.length ba) (fun i -> if ba.(i) then '1' else '0')

let int_of_binary_unsigned bin =
  let result = ref 0.0 in
  let iterFunc i x =
    match x with
      | true -> result := !result +. 2. ** float_of_int ((i - ((Array.length bin) - 1)) * -1)
      | false -> ()
  in
  Array.iteri iterFunc bin;
  int_of_float !result;;

let comp_eq_unsigned op1 op2 =
  let equal = ref true in
  Array.iter2 (fun x y -> if x != y then equal := false) op1 op2;
  !equal;;

let comp_lt_unsigned op1 op2 =
  let index1 = ref (-1) in
  let index2 = ref (-1) in
  let oneFound = ref false in
  Array.iteri (fun i x ->
    if not !oneFound then
      if x then begin
        index1 := i;
        oneFound := true
      end) op1;
  oneFound := false;
  Array.iteri (fun i x ->
    if not !oneFound then
      if x then begin
        index2 := i;
        oneFound := true
      end) op2;
  if !index1 = -1 && !index2 != -1 then true
  else if !index2 = -1 && !index1 != -1 then false
  else if !index1 = -1 && !index2 = -1 then false
  else !index1 > !index2;;

let comp_gt_unsigned op1 op2 =
  let index1 = ref (-1) in
  let index2 = ref (-1) in
  let oneFound = ref false in
  Array.iteri (fun i x ->
    if not !oneFound then
      if x then begin
        index1 := i;
        oneFound := true
      end) op1;
  oneFound := false;
  Array.iteri (fun i x ->
    if not !oneFound then
      if x then begin
        index2 := i;
        oneFound := true
      end) op2;
  if !index1 = -1 && !index2 != -1 then false
  else if !index2 = -1 && !index1 != -1 then true
  else if !index1 = -1 && !index2 = -1 then false
  else !index1 < !index2;;

let twos_compl bin =
  let complement = Array.copy bin in
  let foundOne = ref false in
  for i = (Array.length bin) - 1 downto 0 do
    if !foundOne then complement.(i) <- not complement.(i)
    else if complement.(i) then foundOne := true
  done;
  complement;;

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
          | 0 -> bin.(i) <- false; num := !num / 2
          | 1 -> bin.(i) <- true; num := !num / 2
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
      | (false, false, false) -> ()
      | (false, false, true) -> sum.(i) <- true; carry := false
      | (false, true, false) -> sum.(i) <- true
      | (false, true, true) -> carry := true
      | (true, false, false) -> sum.(i) <- true
      | (true, false, true) -> carry := true
      | (true, true, false) -> carry := true
      | (true, true, true) -> sum.(i) <- true; carry := true
  done;
  sum;;

let minus op1 op2 =
  plus op1 (twos_compl op2);;

let logical_shift_left op1 amount =
  let shifted = Array.make (Array.length op1) false in
  if amount < Array.length op1 then begin
    let startIndex = (amount mod (Array.length op1)) - 1 in
    Array.iteri (fun i x -> if i <= startIndex then () else shifted.((i - amount) mod (Array.length op1)) <- x) op1;
  end;
  shifted;;

let logical_shift_right op1 amount =
  let shifted = Array.make (Array.length op1) false in
  if amount < Array.length op1 then begin
    let endIndex = (Array.length op1) - amount - 1 in
    Array.iteri (fun i x -> if i > endIndex then () else shifted.((i + amount) mod (Array.length op1)) <- x) op1;
  end;
  shifted;;

let arith_shift_left op1 amount =
  logical_shift_left op1 amount

let arith_shift_right op1 amount =
  let shifted = Array.make (Array.length op1) op1.(0) in
  if amount < Array.length op1 then begin
    let endIndex = (Array.length op1) - amount - 1 in
    Array.iteri (fun i x -> if i > endIndex then () else shifted.((i + amount) mod (Array.length op1)) <- x) op1;
  end;
  shifted;;

let rec rotate_left op1 amount =
  if amount = 0 then op1
  else begin
    let rotated = Array.make (Array.length op1) false in
    Array.iteri (fun i x -> if i = 0 then () else rotated.(i-1) <- x) op1;
    rotated.((Array.length rotated) - 1) <- op1.(0);
    rotate_left rotated (amount - 1)
  end;;

let rec rotate_right op1 amount =
  if amount = 0 then op1
  else begin
    let rotated = Array.make (Array.length op1) false in
    Array.iteri (fun i x -> if i > (Array.length op1) - 2 then () else rotated.(i+1) <- x) op1;
    rotated.(0) <- op1.((Array.length op1) - 1);
    rotate_right rotated (amount - 1)
  end;;

let logical_and op1 op2 =
  let anded = Array.make (Array.length op1) false in
  Array.iteri (fun i x -> if x && op2.(i) then anded.(i) <- true) op1;
  anded;;

let logical_or op1 op2 =
  let ored = Array.make (Array.length op1) false in
  Array.iteri (fun i x -> if x || op2.(i) then ored.(i) <- true) op1;
  ored;;

let logical_xor op1 op2 =
  let xored = Array.make (Array.length op1) false in
  Array.iteri (fun i x -> if (x || op2.(i)) && not (x && op2.(i)) then xored.(i) <- true) op1;
  xored;;

let logical_not op1 =
  let noted = Array.make (Array.length op1) false in
  Array.iteri (fun i x -> if not x then noted.(i) <- true) op1;
  noted;;
