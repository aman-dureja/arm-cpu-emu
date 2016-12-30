open Cpu;;
open Binary_operations;;

let processor = new cpu;;

(* Put 5 in R2, and 3 in R3, and the sum in R1 *)

let codeArray : bool array array = Array.make 6 (Array.make 8 true);;

codeArray.(0) <- ba_of_bs "00100010";;
codeArray.(1) <- ba_of_bs "00000101";;
codeArray.(2) <- ba_of_bs "00100011";;
codeArray.(3) <- ba_of_bs "00000011";;
codeArray.(4) <- ba_of_bs "00011000";;
codeArray.(5) <- ba_of_bs "11010001";;

processor#loadProgramInMem codeArray;;

for i = 1 to 3 do
  processor#fetch
done;;

processor#printState;;

let shifted = arith_shift_left (binary_of_int 28) 1;;

Array.iter (fun x -> print_endline (string_of_bool x)) shifted;;
