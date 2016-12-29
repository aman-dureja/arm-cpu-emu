open Cpu;;
open Binary_operations;;

let processor = new cpu;;

(* Put 5 in R2, and 3 in R3, and the sum in R1 *)

let codeArray : bool array array = Array.make 4 (Array.make 8 true);;

codeArray.(0) <- ba_of_bs "00100010";;
codeArray.(1) <- ba_of_bs "00000101";;
codeArray.(2) <- ba_of_bs "00100110";;
codeArray.(3) <- ba_of_bs "00000011";;
(* codeArray.(0) <- ba_of_bs "1";;
codeArray.(0) <- ba_of_bs "1";; *)

processor#loadProgramInMem codeArray;;

for i = 1 to 2 do
  processor#fetch
done;;

processor#printState;;
