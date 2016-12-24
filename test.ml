open Cpu;;
open Binary_operations;;

let processor = new cpu;;

let boolArray = processor#ba_of_bs "000011110101";;
for i = 0 to (Array.length boolArray) - 1 do
  print_endline (string_of_bool boolArray.(i))
done;;

processor#printState;;

processor#setIr [|true; false; false; true; true|];;
processor#decode;;

let testShiftLeft = logical_shift_left [|true; false; true;|];;
for i = 0 to 2 do
  print_endline (string_of_bool testShiftLeft.(i))
done;;

let testShiftRight = logical_shift_right [|true; true; true;|];;
for i = 0 to 2 do
  print_endline (string_of_bool testShiftRight.(i))
done;;
