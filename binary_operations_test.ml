(* Unit tests for binary operations *)

open Binary_operations;;
open General_test_ops;;

let test msg f =
  Printf.printf "Testing %s\n" msg;
  f ();
  finishTest ();;

print_endline "\027[35mTesting binary operations... \027[0m";;

test "integer from binary array..." (fun () ->
  let binArray : bool array = [|true; true; true; false|] in
  assertEq (-2) (int_of_binary_signed binArray) ""
);;

test "unsigned equality comparison..." (fun () ->
  let bin1 = [|false; false; true|] in
  let bin2 = [|true; false; false|] in
  assertEq false (comp_eq_unsigned bin1 bin2) ""
);;

test "unsigned less than comparison..." (fun () ->
  let bin1 = [|true; false; true|] in
  let bin2 = [|true; false; true|] in
  assertEq false (comp_lt_unsigned bin1 bin2) ""
);;

test "unsigned greater than comparison..." (fun () ->
  let bin1 = [|true; false; true|] in
  let bin2 = [|false; false; true|] in
  assertEq true (comp_gt_unsigned bin1 bin2) ""
);;

test "two's complement..." (fun () ->
  assertEq [|true; false; true;|] (twos_compl [|false; true; true;|]) ""
);;

test "addition..." (fun () ->
  let bin1 = binary_of_int (-9) in
  let bin2 = binary_of_int (18) in
  assertEq 9 (int_of_binary_signed (plus bin1 bin2)) ""
);;

test "subtraction..." (fun () ->
  let bin1 = binary_of_int (3) in
  let bin2 = binary_of_int (-18) in
  assertEq 21 (int_of_binary_signed (minus bin1 bin2)) ""
);;

test "logical right shift..." (fun () ->
  let bin = [|true; false; true; true|] in
  let shifted = [|false; true; false; true|] in
  assertEq shifted (logical_shift_right bin 1) ""
);;

test "arithmetic left shift..." (fun () ->
  let bin = [|true; false; true; true|] in
  let shifted = [|false; true; true; false|] in
  assertEq shifted (arith_shift_left bin 1) ""
);;

test "arithmetic right shift..." (fun () ->
  let bin = [|true; false; true; true|]in
  let shifted = [|true; true; false; true|] in
  assertEq shifted (arith_shift_right bin 1) ""
);;

test "rotate left..." (fun () ->
  let bin = [|false; true; false; true|] in
  let rotated = [|true; false; true; false|] in
  assertEq rotated (rotate_left bin 1) ""
);;

test "rotate right..." (fun () ->
  let bin = [|false; true; true; true|] in
  let rotated = [|true; false; true; true|] in
  assertEq rotated (rotate_right bin 1) ""
);;

test "logical and..." (fun () ->
  let bin1 = [|false; true; true; false|] in
  let bin2 = [|true; true; false; true|] in
  let anded = [|false; true; false; false|] in
  assertEq anded (logical_and bin1 bin2) ""
);;

test "logical or..." (fun () ->
  let bin1 = [|false; true; true; false|] in
  let bin2 = [|true; true; false; true|] in
  let ored = [|true; true; true; true|] in
  assertEq ored (logical_or bin1 bin2) ""
);;

test "logical xor..." (fun () ->
  let bin1 = [|false; true; true; false|] in
  let bin2 = [|true; true; false; true|] in
  let xored = [|true; false; true; true|] in
  assertEq xored (logical_xor bin1 bin2) ""
);;

test "logical not..." (fun () ->
  let bin = [|false; true; true; false|] in
  let notbin = [|true; false; false; true|] in
  assertEq notbin (logical_not bin) ""
);;

print_endline "\027[35mBinary operations tests complete! \027[0m";;
