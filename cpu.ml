(* The main CPU object *)

open Binary_operations;;

(* IMPLEMENTS ARMV7 CORTEX WITH THUMB INSTRUCTION SET *)
(* TODO: implement condition codes *)
(* TODO: fix bit shifted things for stuff like Offset5 and Word8, etc *)

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
      val mutable generalRegisters : bool array array = Array.make 16 (Array.make 32 false)
      val mutable memory : bool array array = Array.make 2048 (Array.make 8 false)
      val mutable operation : string = "none"
      val mutable memoryOp : bool = false
      val mutable memOperation : string = "none"
      val mutable regList : int array = [||]
      val mutable baseReg : int = 0
      val mutable byteWordLoadStoreFlag : string = "none"
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

      method ba_of_bs bs =
        let boolArray = Array.make (String.length bs) false in
        for i = 0 to (String.length bs) - 1 do
          if bs.[i] == '1' then boolArray.(i) <- true
        done;
        boolArray

      method validateRegNumber n =
        if n < 0 || n > 31 then failwith "Invalid register number!"

      method printState =
        let boolsToInts a = Array.map (fun x -> if x then 1 else 0) a in
        let printReg i reg =
          let ints = boolsToInts generalRegisters.(i) in
          Printf.printf "%d: " i;
          if i < 10 then Printf.printf " ";
          Array.iter (fun x -> Printf.printf "%d" x) ints;
          Printf.printf "\n"
        in
        Array.iteri printReg generalRegisters

      method loadProgramInMem = ()

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
                  | false -> operation <- "ADD"
                  | true -> operation <- "SUB"
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
                operation <- "ADD";
                muxB <- Array.make 32 false

              | (false, true) -> operation <- "CMP" (* TODO: implement this *)

              | (true, false) ->
                operation <- "ADD";
                muxB <- generalRegisters.(dest)

              | (true, true) ->
                operation <- "SUB";
                muxB <- generalRegisters.(dest)
            );

          | [|false; true; false|] ->
            (match ir.(3) with

              | false ->
              (match ir.(4) with

                | false ->
                (match ir.(5) with

                  (* ALU Operations *)
                  | false ->
                    dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                    rA <- generalRegisters.(dest);
                    muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                    (let opArray = Array.sub ir 6 4 in
                      match opArray with
                        | [|false; false; false; false|] -> operation <- "AND"
                        | [|false; false; false; true|] -> operation <- "EOR"
                        | [|false; false; true; false|] -> operation <- "LSL"
                        | [|false; false; true; true|] -> operation <- "LSR"
                        | [|false; true; false; false|] -> operation <- "ASR"
                        | [|false; true; false; true|] -> operation <- "ADC"
                        | [|false; true; true; false|] -> operation <- "SBC"
                        | [|false; true; true; true|] -> operation <- "ROR"
                        | [|true; false; false; false|] -> operation <- "TST"
                        | [|true; false; false; true|] -> operation <- "NEG"
                        | [|true; false; true; false|] -> operation <- "CMP"
                        | [|true; false; true; true|] -> operation <- "CMN"
                        | [|true; true; false; false|] -> operation <- "ORR"
                        | [|true; true; false; true|] -> operation <- "MUL"
                        | [|true; true; true; false|] -> operation <- "BIC"
                        | [|true; true; true; true|] -> operation <- "MVN"
                    )

                  (* Hi Register Operations *)
                  | true ->
                    (match ir.(8) with
                      | false -> rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 13 3))
                      | true -> rA <- generalRegisters.((int_of_binary_unsigned (Array.sub ir 13 3)) + 8)
                    );
                    (match ir.(9) with
                      | false -> muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3))
                      | true -> muxB <- generalRegisters.((int_of_binary_unsigned (Array.sub ir 10 3)) + 8)
                    );
                    (match (ir.(6), ir.(7)) with
                      | (false, false) -> operation <- "ADD"
                      | (false, true) -> operation <- "CMP"
                      | (true, false) -> operation <- "MOV"
                      | (true, true) -> operation <- "BX"
                    );
                  );

                (* PC Relative Load *)
                | true ->
                  operation <- "ADD";
                  memoryOp <- true;
                  memOperation <- "LDR";
                  dest <- int_of_binary_unsigned (Array.sub ir 5 3);
                  rA <- Array.append (Array.make 24 false) (Array.sub ir 8 8);
                  muxB <- generalRegisters.(15)
              )

              | true ->
                memoryOp <- true;
                operation <- "ADD";
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 7 3));
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                (match ir.(6) with

                  (* Load/store with register offset *)
                  | false ->
                    (match ir.(4) with
                      | false -> memOperation <- "STR"
                      | true -> memOperation <- "LDR"
                    );
                    (match ir.(5) with
                      | false -> byteWordLoadStoreFlag <- "word"
                      | true -> byteWordLoadStoreFlag <- "byte"
                    )

                  (* Load/store sign-extended byte/halfword *)
                  | true ->
                    (match (ir.(5), ir.(4)) with
                      | (false, false) -> memOperation <- "STRH"
                      | (false, true) -> memOperation <- "LDRH"
                      | (true, false) -> memOperation <- "LDSB"
                      | (true, true) -> memOperation <- "LDSH"
                    );
                )
            );

          (* Load/store with immediate offset *)
          | [|false; true; true|] ->
            memoryOp <- true;
            operation <- "ADD";
            rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
            muxB <- Array.append (Array.make 27 ir.(5)) (Array.sub ir 5 5);
            dest <- int_of_binary_unsigned (Array.sub ir 13 3);
            (match (ir.(4), ir.(3)) with
              | (false, false) -> memOperation <- "STR"
              | (false, true) -> memOperation <- "LDR"
              | (true, false) -> memOperation <- "STRB"
              | (true, true) -> memOperation <- "LDRB"
            )

          | [|true; false; false|] ->
            (match ir.(3) with

              (* Load/store halfword *)
              | false ->
                memoryOp <- true;
                operation <- "ADD";
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                muxB <- Array.append (Array.make 27 ir.(5)) (Array.sub ir 5 5);
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                (match ir.(4) with
                  | false -> memOperation <- "STRH"
                  | true -> memOperation <- "LDRH"
                )

              (* SP-relative load/store *)
              | true ->
                memoryOp <- true;
                dest <- int_of_binary_unsigned (Array.sub ir 5 3);
                rA <- generalRegisters.(13);
                muxB <- Array.append (Array.make 24 false) (Array.sub ir 8 8);
                operation <- "ADD";
                (match ir.(4) with
                  | false -> memOperation <- "STR"
                  | true -> memOperation <- "LDR"
                )
            )

          | [|true; false; true|] ->
            (match ir.(3) with

              (* Load address *)
              | false ->
                muxB <- Array.append (Array.make 24 false) (Array.sub ir 8 8);
                operation <- "ADD";
                dest <- int_of_binary_unsigned (Array.sub ir 5 3);
                memoryOp <- false;
                (match ir.(4) with
                  | false -> rA <- generalRegisters.(pc)
                  | true -> rA <- generalRegisters.(stackPointer)
                )

              | true ->
                (match ir.(5) with

                  (* Add offset to Stack Pointer *)
                  | false ->
                    memoryOp <- false;
                    rA <- generalRegisters.(stackPointer);
                    muxB <- Array.append (Array.make 25 false) (Array.sub ir 9 7);
                    (match ir.(8) with
                      | false -> operation <- "ADD"
                      | true -> operation <- "SUB"
                    )

                  (* Push/pop registers *)
                  | true ->
                    memoryOp <- true;
                    regList <- [||];
                    let registerList = Array.sub ir 8 8 in
                    Array.iteri (fun i x -> if x then regList <- Array.append regList [|i|]) registerList;
                    (match (ir.(4), ir.(7)) with
                      | (false, false) -> memOperation <- "PUSH"
                      | (false, true) -> memOperation <- "PUSHLR"
                      | (true, false) -> memOperation <- "POP"
                      | (true, true) -> memOperation <- "POPPC"
                    )
                )
            )

          | [|true; true; false|] ->
            (match ir.(3) with

              (* Multiple load/store *)
              | false ->
                memoryOp <- true;
                regList <- [||];
                baseReg <- int_of_binary_unsigned (Array.sub ir 5 3);
                let registerList = Array.sub ir 8 8 in
                Array.iteri (fun i x -> if x then regList <- Array.append regList [|i|]) registerList;
                (match ir.(4) with
                  | false -> memOperation <- "STMIA"
                  | true -> memOperation <- "LDMIA"
                )

              (* Conditional branch *)
              | true -> ()

            )

          | [|true; true; true|] ->
            (match ir.(3) with

              (* Unconditional branch *)
              | false -> ()

              (* Long branch with link *)
              | true -> ()
            )

          | _ -> failwith "Error! Invalid instruction!"
        );
        self#execute

      method execute =
        (match operation with
          | "ADD" -> rZ <- plus rA muxB
          | "SUB" -> rZ <- minus rA muxB
          | _ -> failwith "Invalid ALU command!"
        );
        self#memory

      method memory =

        self#writeback

      method writeback = ()
    end;;
