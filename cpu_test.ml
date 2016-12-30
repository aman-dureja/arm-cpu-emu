open Cpu;;

let numFailures = ref 0;;

let assertEq x y msg =
  if x != y then begin
    numFailures := !numFailures + 1;
    print_string "Failed! ";
    print_endline msg
  end;;

let finishTest =
  if !numFailures = 0 then
    print_endline "\027[32m PASS \027[0m"
  else
    Printf.printf "\027[31m FAIL: %d tests failed! \027[0m" !numFailures;;

print_endline "\027[35m Testing cpu... \027[0m";;

(* TODO: Tests go here *)

print_endline "\027[35m Cpu tests complete! \027[0m";;
