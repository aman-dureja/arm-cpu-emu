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

let testInstr msg instr dest expect =
  Printf.printf " Test %s\n" msg;
  proc#setIr instr;
  executeInstruction ();
  assertEq (int_of_binary_signed proc#getGeneralRegisters.(dest)) expect "";;

let test msg f =
  Printf.printf "Testing %s\n" msg;
  resetProc ();
  f ();
  finishTest ();;

print_endline "\027[35mTesting cpu... \027[0m";;

test "loading program in memory..." (fun () ->
  let codeArray : bool array array = Array.make 12 (Array.make 8 true) in
  proc#loadProgramInMem codeArray;
  assertEq codeArray (Array.sub proc#getMem 0 12) ""
);;

test "fetch stage..." (fun () ->
  assertEq proc#getGeneralRegisters.(15) (Array.make 32 false) "";
  let newMem = [| (ba_of_bs "10101010"); (ba_of_bs "01010101"); (ba_of_bs "11111111") |] in
  proc#setMem newMem;
  proc#fetch;
  assertEq proc#getGeneralRegisters.(pc) (ba_of_bs "00000000000000000000000000000010") "";
  assertEq proc#getIr (ba_of_bs "1010101001010101") ""
);;

test "Format 1: Move Shifted Register instructions..." (fun () ->
  print_endline " Source reg is R3, holding value 2";
  proc#setGeneralRegisters 3 (binary_of_int 2);
  testInstr "move R3 logically left shifted by 4, into R7" (ba_of_bs "0000000100011111") 7 32;

  print_endline " Make [R3] equal to 4";
  proc#setGeneralRegisters 3 (binary_of_int 4);
  testInstr "move R3 logically right shifted by 3, into R7" (ba_of_bs "0000100011011111") 7 0;

  print_endline " Make [R3] equal to -2";
  proc#setGeneralRegisters 3 (binary_of_int (-2));
  testInstr "move R3 arithmetically right shifted by 31, into R7" (ba_of_bs "0001011111011111") 7 (-1)
);;

test "Format 2: Add/Subtract instructions..." (fun () ->
  baseBin := "00011";
  let rN = "000" in
  let rS = "001" in
  let rD = "011" in

  print_endline " Make [R0] equal to -5";
  proc#setGeneralRegisters 0 (binary_of_int (-5));
  print_endline " Make [R1] equal to 7";
  proc#setGeneralRegisters 1 (binary_of_int 7);

  testInstr "ADD R3, R1, R0" (ba_of_bs (!baseBin ^ "00" ^ rN ^ rS ^ rD)) 3 2;

  print_endline " Test adding negative";
  proc#setGeneralRegisters 1 (binary_of_int 3);
  executeInstruction ();
  assertEq (int_of_binary_signed proc#getGeneralRegisters.(3)) (-2) "";

  testInstr "adding immediate" (ba_of_bs (!baseBin ^ "10" ^ rS ^ rN ^ rD)) 3 (-4);

  testInstr "SUB R3, R1, R0" (ba_of_bs (!baseBin ^ "01" ^ rN ^ rS ^ rD)) 3 8;

  testInstr "subtracting immediate" (ba_of_bs (!baseBin ^ "11" ^ "111" ^ rN ^ rD)) 3 (-4)
);;

test "Format 3: Move/Compare/Add/Subtract Immediate instructions..." (fun () ->
  (* Dest register is R3, immediate is -2 *)
  let tester msg instr expected = testInstr msg (ba_of_bs ("001" ^ instr ^ "01111111110")) 3 expected in

  tester "MOV R3, #-2" "00" (-2);

  proc#setGeneralRegisters 3 (binary_of_int 24);
  tester "ADD R3, #-2" "10" 22;

  proc#setGeneralRegisters 3 (binary_of_int 24);
  tester "SUB R3, #-2" "11" 26;

  print_endline " Test CMP";
  proc#setGeneralRegisters 3 (binary_of_int 4);
  proc#setIr (ba_of_bs "0010101111111110");
  executeInstruction ();
  assertEq proc#getRz (binary_of_int 6)
);;

print_endline "\027[35mCpu tests complete! \027[0m";;
