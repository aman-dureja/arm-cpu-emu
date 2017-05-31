let numFailures = ref 0;;

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
