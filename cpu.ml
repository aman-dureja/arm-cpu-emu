(* The main CPU object *)

open Binary_operations;;

(* IMPLEMENTS ARMV7 CORTEX WITH THUMB INSTRUCTION SET *)
(* TO DO: add/sub instructions, then mult/div, then branch with no condition
    codes, then at the very end, implement condition codes. *)

(* Central Processing Unit *)
class cpu =
    object (self)
      (* General Setup *)
      val minReg = 0;
      val maxReg = 15;
      val pc = 15;
      val stackPointer = 13;
      val linkRegister = 14;
      val mutable generalRegisters : bool array array = Array.make 32 [||];
      val mutable memory : bool array array = Array.make 2048 [||];
      (* Interstage Registers *)
      val mutable rA : bool array = Array.make 32 false;
      val mutable rB : bool array = Array.make 32 false;
      val mutable rM : bool array = Array.make 32 false;
      val mutable rZ : bool array = Array.make 32 false;
      val mutable rY : bool array = Array.make 32 false;
      (* Instruction Register *)
      val mutable ir : bool array = Array.make 16 false;
      (* Methods and Instruction Execution Stages *)
      method validateRegNumber n =
        if n < 0 || n > 31 then failwith "Invalid register number!";
      method printState = ()
      method loadProgramInMem = ()
      method aluOp command =
        match command with
          | "plus" -> ()
          | "minus" -> ()
          | _ -> failwith "Invalid ALU command!";
      method fetch =
        ir = Array.append memory.(int_of_binary_unsigned generalRegisters.(pc)) memory.(int_of_binary_unsigned (plus generalRegisters.(pc) (binary_of_int 1)));
        generalRegisters.(pc) <- plus generalRegisters.(pc) (binary_of_int 2);
        self#decode;
      method decode =

        self#execute;
      method execute =

        self#memory;
      method memory =

        self#writeback;
      method writeback = ()
    end;;
