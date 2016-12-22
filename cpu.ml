(* The main CPU object *)

open Binary_operations;;

(* IMPLEMENTS ARMV7 CORTEX WITH THUMB INSTRUCTION SET *)
(* TO DO: add/sub instructions, then mult/div, then branch with no condition
    codes, then at the very end, implement condition codes. *)

(* Central Processing Unit *)
class cpu =
    object (self)

      (* General Setup *)
      val minReg = 0
      val maxReg = 15
      val pc = 15
      val stackPointer = 13
      val linkRegister = 14
      val mutable dest = 0
      val mutable generalRegisters : bool array array = Array.make 32 (Array.make 32 false)
      val mutable memory : bool array array = Array.make 2048 (Array.make 8 false)
      val mutable operation : string = "none"
      val mutable memoryOp : bool = false
      (* Interstage Registers *)
      val mutable rA : bool array = Array.make 32 false
      val mutable rM : bool array = Array.make 32 false
      val mutable rZ : bool array = Array.make 32 false
      val mutable rY : bool array = Array.make 32 false
      (* Multiplexers *)
      val mutable muxB : bool array = Array.make 32 false
      (* Instruction Register *)
      val mutable ir : bool array = Array.make 16 false

      (* Methods and Instruction Execution Stages *)

      (* Below method exclusively for testing! *)
      method setIr bin = ir <- Array.append bin (Array.make (32 - Array.length bin) false)

      method validateRegNumber n =
        if n < 0 || n > 31 then failwith "Invalid register number!"

      method printState = ()

      method loadProgramInMem = ()

      method aluOp command =
        match command with
          | "plus" -> rZ <- plus rA muxB
          | "minus" -> rZ <- minus rA muxB
          | _ -> failwith "Invalid ALU command!"

      method fetch =
        ir <- Array.append memory.(int_of_binary_unsigned generalRegisters.(pc)) memory.(int_of_binary_unsigned (plus generalRegisters.(pc) (binary_of_int 1)));
        generalRegisters.(pc) <- plus generalRegisters.(pc) (binary_of_int 2);
        self#decode

      method decode =
        (match (ir.(0), ir.(1), ir.(2)) with

          (* Add/Sub or Move Shifted Register instruction *)
          | (false, false, false) ->
            (match (ir.(3), ir.(4)) with

              (* Add/Sub instruction *)
              | (true, true) -> begin
                (match ir.(5) with
                  | false -> muxB <- generalRegisters.(int_of_binary_unsigned [|ir.(7); ir.(8); ir.(9)|])
                  | true -> muxB <- Array.append (Array.make 29 ir.(7)) [|ir.(7); ir.(8); ir.(9)|]
                );
                (match ir.(6) with
                  | false -> operation <- "plus"
                  | true -> operation <- "minus"
                );
                rA <- Array.append (Array.make 29 ir.(10)) [|ir.(10); ir.(11); ir.(12)|];
                dest <- int_of_binary_unsigned [|ir.(13); ir.(14); ir.(15)|];
                end;

              (* Move Shifted Register instruction *)
              | _ -> ()
            );

          | _ -> failwith "Error! Invalid opcode!"
        );
        self#execute

      method doOperation =
        match operation with
          | "add" -> self#aluOp "plus";
          | "subtract" -> self#aluOp "minus";
          | _ -> failwith "Invalid operation to execute!"

      method execute =
        self#doOperation;
        self#memory

      method memory =

        self#writeback

      method writeback = ()
    end;;
