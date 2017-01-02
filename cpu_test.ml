(* Unit tests for the cpu object *)

open Cpu;;
open Binary_operations;;

let numFailures = ref 0;;
let baseBin = ref "";;
let instruction = ref "";;

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
  proc#setGeneralRegistersAll (Array.make 16 (Array.make 32 false));
  proc#setMem (Array.make 2048 (Array.make 8 false));;

let executeInstruction () =
  proc#decode;
  proc#execute;
  proc#memory;
  proc#writeback;;

let testOp msg instr dest expect =
  Printf.printf " Test %s\n" msg;
  proc#setIr instr;
  executeInstruction ();
  assertEq (int_of_binary_signed proc#getGeneralRegisters.(dest)) expect;;

print_endline "\027[35mTesting cpu... \027[0m";;


print_endline "Testing loading program in memory... ";;
resetProc ();;

let codeArray : bool array array = Array.make 12 (Array.make 8 true) in
proc#loadProgramInMem codeArray;
assertEq codeArray (Array.sub proc#getMem 0 12) "";;

finishTest ();;


print_endline "Testing fetch stage... ";;
resetProc ();;

assertEq proc#getGeneralRegisters.(15) (Array.make 32 false) "";;
let newMem = [| (ba_of_bs "10101010"); (ba_of_bs "01010101"); (ba_of_bs "11111111") |] in
proc#setMem newMem;;
proc#fetch;;
assertEq proc#getGeneralRegisters.(pc) (ba_of_bs "00000000000000000000000000000010") "";;
assertEq proc#getIr (ba_of_bs "1010101001010101") "";;

finishTest ();;


print_endline "Testing Format 1: Move Shifted Register instructions...";;
resetProc ();;

print_endline " Source reg is R3, holding value 2...";;
proc#setGeneralRegisters 3 (binary_of_int 2);;
print_endline " Test move R3 logically left shifted by 4, into R7...";;
proc#setIr (ba_of_bs "0000000100011111");;
executeInstruction ();;
assertEq (int_of_binary_signed proc#getGeneralRegisters.(7)) 32 "";;

print_endline " Make [R3] equal to 4...";;
proc#setGeneralRegisters 3 (binary_of_int 4);;
print_endline " Test move R3 logically right shifted by 3, into R7...";;
proc#setIr (ba_of_bs "0000100011011111");;
executeInstruction ();;
assertEq (int_of_binary_signed proc#getGeneralRegisters.(7)) 0 "";;

print_endline " Make [R3] equal to -2...";;
proc#setGeneralRegisters 3 (binary_of_int (-2));;
print_endline " Test move R3 arithmetically right shifted by 31, into R7...";;
proc#setIr (ba_of_bs "0001011111011111");;
executeInstruction ();;
assertEq (int_of_binary_signed proc#getGeneralRegisters.(7)) (-1) "";;

finishTest ();;


print_endline "Testing Format 2: Add/Subtract instructions...";;
resetProc ();;

baseBin := "00011";;
let rN = "000";;
let rS = "001";;
let rD = "011";;
let imm = "111";;

print_endline " Make [R0] equal to -5...";;
proc#setGeneralRegisters 0 (binary_of_int (-5));;
print_endline " Make [R1] equal to 7...";;
proc#setGeneralRegisters 1 (binary_of_int 7);;

testOp "ADD R3, R1, R0..." (ba_of_bs (!baseBin ^ "00" ^ rN ^ rS ^ rD)) 3 2;;

print_endline " Test adding negative...";;
proc#setGeneralRegisters 1 (binary_of_int 3);;
executeInstruction ();;
assertEq (int_of_binary_signed proc#getGeneralRegisters.(3)) (-2);;

testOp "adding immediate..." (ba_of_bs (!baseBin ^ "10" ^ rS ^ rN ^ rD)) 3 (-4);;

print_endline " Test SUB R3, R1, R0...";;
proc#setIr (ba_of_bs (!baseBin ^ "01" ^ rN ^ rS ^ rD));;
executeInstruction ();;

finishTest ();;


print_endline "Testing Format 3: Move/Compare/Add/Subtract Immediate instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 4: ALU Operations instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 5: Hi Register Operations/Branch Exchange instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 6: PC-Relative Load instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 7: Load/Store With Register Offset instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 8: Load/Store Sign-Extended Byte/Halfword instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 9: Load/Store With Immediate Offset instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 10: Load/Store Halfword instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 11: SP-Relative Load/Store instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 12: Load Address instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 13: Add Offset To Stack Pointer instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 14: Push/Pop Registers instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 15: Multiple Load/Store instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 16: Conditional Branch instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 18: Unconditional Branch instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "Testing Format 19: Long Branch With Link instructions...";;
resetProc ();;

(* Tests go here *)

finishTest ();;


print_endline "\027[35mCpu tests complete! \027[0m";;
