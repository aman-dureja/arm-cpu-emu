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
        let testBin = Array.sub ir 0 3 in
        (match testBin with

          (* Add/Sub or Move Shifted Register instruction *)
          | [|false; false; false|] ->
            (match (ir.(3), ir.(4)) with

              (* Add/Sub instruction *)
              | (true, true) ->
                (match ir.(5) with
                  | false -> muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 7 3))
                  | true -> muxB <- Array.append (Array.make 29 ir.(7)) (Array.sub ir 7 3)
                );
                (match ir.(6) with
                  | false -> operation <- "plus"
                  | true -> operation <- "minus"
                );
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                dest <- int_of_binary_unsigned (Array.sub ir 13 3)

              (* Move Shifted Register instruction *)
              | _ -> ()
            );

          (* Move/Compare/Add/Sub immediate *)
          | [|false; false; true|] ->
            dest <- int_of_binary_unsigned (Array.sub ir 5 3);
            rA <- Array.append (Array.make 26 ir.(8)) (Array.sub ir 8 8);
            (match (ir.(3), ir.(4)) with

              | (false, false) ->
                operation <- "plus";
                muxB <- Array.make 32 false

              | (false, true) -> operation <- "compare" (* TODO: implement this *)

              | (true, false) ->
                operation <- "plus";
                muxB <- generalRegisters.(dest)

              | (true, true) ->
                operation <- "minus";
                muxB <- generalRegisters.(dest)
            );

          | [|false; true; false|] ->
            match (ir.(3), ir.(4)) with

              | (false, false) ->
                (match ir.(5) with

                  | false ->
                    dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                    rA <- generalRegisters.(dest);
                    muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                    (let opArray = Array.sub ir 6 4 in
                      match opArray with
                        | [|false; false; false; false|] -> operation <- "and"
                        | [|false; false; false; true|] -> operation <- "eor"
                        | [|false; false; true; false|] -> operation <- "lsl"
                        | [|false; false; true; true|] -> operation <- "lsr"
                        | [|false; true; false; false|] -> operation <- "asr"
                        | [|false; true; false; true|] -> operation <- "adc"
                        | [|false; true; true; false|] -> operation <- "sbc"
                        | [|false; true; true; true|] -> operation <- "ror"
                        | [|true; false; false; false|] -> operation <- "tst"
                        | [|true; false; false; true|] -> operation <- "neg"
                        | [|true; false; true; false|] -> operation <- "cmp"
                        | [|true; false; true; true|] -> operation <- "cmn"
                        | [|true; true; false; false|] -> operation <- "orr"
                        | [|true; true; false; true|] -> operation <- "mul"
                        | [|true; true; true; false|] -> operation <- "bic"
                        | [|true; true; true; true|] -> operation <- "mvn"
                    )

                  | true -> ()

                );

              | _ -> ()

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
