open Cpu;;
open Binary_operations;;

let processor = new cpu;;

processor#setIr [|false; false; false; true; true|];;
processor#decode;;

let testShiftLeft = logical_shift_left [|true; false; true;|];;
for i = 0 to 2 do
  print_endline (string_of_bool testShiftLeft.(i))
done;;

let testShiftRight = logical_shift_right [|true; true; true;|];;
for i = 0 to 2 do
  print_endline (string_of_bool testShiftRight.(i))
done;;
