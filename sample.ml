open Cpu;;
open Binary_operations;;

let processor = new cpu;;

(* Put 3 in R2, and 2 in R3, and the sum in R1 *)

Printf.printf "This program will load the value 3 into R2, 2 into R3, and store their sum in R1.\n"

let binaryInstructionsArray : string array = [|
  "00011100"; "11000010";
  "00011100"; "10000011";
  "00011000"; "10011001";
  "00000000"; "00000000"
|];;

let codeArray : bool array array = Array.map (fun instr -> ba_of_bs instr) binaryInstructionsArray;;

processor#loadProgramInMem codeArray;;

processor#runProgram;;

processor#printState;;

