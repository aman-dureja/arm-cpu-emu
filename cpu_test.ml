(* Unit tests for the cpu object *)

open Cpu;;
open Binary_operations;;

let numFailures = ref 0;;

(* TODO: assertEq and finishTest aren't getting called. Figure out why. *)

let assertEq x y msg =
  if x <> y then begin
    numFailures := !numFailures + 1;
    print_string "\027[31mFailed! \027[0m";
    print_endline msg
  end;;

let finishTest () =
  if !numFailures = 0 then
    print_endline "\027[32mPASS \027[0m"
  else
    Printf.printf "\027[31mFAIL: %d tests failed!\n\027[0m" !numFailures;
  numFailures := 0;;

let proc = new cpu;;
let pc = 15;;

let resetProc () =
  proc#setGeneralRegisters (Array.make 16 (Array.make 32 false));
  proc#setMem (Array.make 2048 (Array.make 8 false));;

print_endline "\027[35mTesting cpu... \027[0m";;

print_string "Testing loading program in memory... ";;
resetProc ();;
let codeArray : bool array array = Array.make 12 (Array.make 8 true) in
proc#loadProgramInMem codeArray;
assertEq codeArray (Array.sub proc#getMem 0 12) "";;
finishTest ();;

print_string "Testing fetch stage... ";;
resetProc ();;
assertEq proc#getGeneralRegisters.(15) (Array.make 32 false) "";;
let newMem = [| (ba_of_bs "10101010"); (ba_of_bs "01010101"); (ba_of_bs "11111111") |] in
proc#setMem newMem;;
proc#fetch;;
assertEq proc#getGeneralRegisters.(pc) (ba_of_bs "00000000000000000000000000000010") "";;
assertEq proc#getIr (ba_of_bs "1010101001010101") "";;
finishTest ();;

print_endline "Testing instruction decoding...";;
resetProc ();;

print_endline "\027[35mCpu tests complete! \027[0m";;
