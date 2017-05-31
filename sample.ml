open Cpu;;
open Binary_operations;;

let processor = new cpu;;

(* Put 3 in R2, and 2 in R3, and the sum in R1 *)

Printf.printf "This program will load the value 3 into R2, 2 into R3, and store their sum in R1.\n"

let codeArray : bool array array = Array.make 8 (Array.make 8 true);;

codeArray.(0) <- ba_of_bs "00011100";;
codeArray.(1) <- ba_of_bs "11000010";;
codeArray.(2) <- ba_of_bs "00011100";;
codeArray.(3) <- ba_of_bs "10000011";;
codeArray.(4) <- ba_of_bs "00011000";;
codeArray.(5) <- ba_of_bs "10011001";;
codeArray.(6) <- ba_of_bs "00000000";;
codeArray.(7) <- ba_of_bs "00000000";;

processor#loadProgramInMem codeArray;;

processor#runProgram;;

processor#printState;;

