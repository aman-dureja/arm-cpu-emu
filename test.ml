open Cpu;;
open Binary_operations;;

let processor = new cpu;;

(* Put 5 in R2, and 3 in R3, and the sum in R1 *)

let codeArray : bool array array = Array.make 6 (Array.make 8 true);;

codeArray.(0) <- ba_of_bs "00011100";;
codeArray.(1) <- ba_of_bs "00100000";;
codeArray.(2) <- ba_of_bs "00100011";;
codeArray.(3) <- ba_of_bs "00000011";;
codeArray.(4) <- ba_of_bs "00000000";;
codeArray.(5) <- ba_of_bs "00000000";;

processor#loadProgramInMem codeArray;;

processor#runProgram;;

processor#printState;;

let shifted = arith_shift_left (binary_of_int 28) 1;;

let added = plus (binary_of_int 5) (binary_of_int (-3));;

Array.iter (fun x -> print_endline (string_of_bool x)) added;;

print_endline (bs_of_ba (ba_of_bs "101010101010110100"));;

Array.iter (fun x -> print_endline (string_of_bool x)) (ba_of_bs "110100");;
