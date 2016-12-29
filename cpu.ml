(* The main CPU object *)

open Binary_operations;;

(* TODO: fix bit shifted things for stuff like Offset5 and Word8, etc *)

(* Central Processing Unit *)
class cpu =
    object (self)

      (* General Setup *)
      val minReg = 0
      val maxReg = 15
      val pc = 15 (* Program counter *)
      val sp = 13 (* Stack pointer *)
      val lr = 14 (* Link register *)
      val mutable dest = 0
      val mutable generalRegisters : bool array array = Array.make 16 (Array.make 32 false)
      val mutable memory : bool array array = Array.make 2048 (Array.make 8 false)
      val mutable operation : string = "none"
      val mutable memoryOp : bool = false
      val mutable memOperation : string = "none"
      val mutable regList : int array = [||]
      val mutable baseReg : int = 0
      val mutable byteWordLoadStoreFlag : string = "none"
      (* Condition code array: [|N; Z; C; V|] *)
      val mutable conditionCodes : bool array = Array.make 4 false;
      val mutable shouldSetCondCodes : bool = false;
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

      method printState =
        let boolsToInts a = Array.map (fun x -> if x then 1 else 0) a in
        let printReg i reg =
          let ints = boolsToInts generalRegisters.(i) in
          Printf.printf "%d: " i;
          if i < 10 then Printf.printf " ";
          Array.iter (fun x -> Printf.printf "%d" x) ints;
          Printf.printf "\n"
        in
        Array.iteri printReg generalRegisters;
        Printf.printf "RZ: ";
        Array.iter (fun x -> Printf.printf "%d" x) (boolsToInts rZ);
        Printf.printf "\n"

      method loadProgramInMem byteArray =
        Array.iteri (fun i byte -> memory.(i) <- byte) byteArray

      method fetch =
        ir <- Array.append memory.(int_of_binary_unsigned generalRegisters.(pc)) memory.(int_of_binary_unsigned (plus generalRegisters.(pc) (binary_of_int 1)));
        generalRegisters.(pc) <- plus generalRegisters.(pc) (binary_of_int 2);
        self#decode

      method setConditionCodes =
        conditionCodes.(0) <- rA.(0) = true; (* N *)
        conditionCodes.(1) <- int_of_binary_unsigned rA = 0; (* Z *)
        (* C *)
        if operation = "ADD" then
          if comp_lt_unsigned rZ rA || comp_lt_unsigned rZ muxB then conditionCodes.(2) <- true
        else if operation = "SUB" then
          if comp_lt_unsigned muxB rA then conditionCodes.(2) <- true;
        conditionCodes.(3) <- if rA.(0) = muxB.(0) && rZ.(0) != rA.(0) then true else false; (* V *)

      method decode =
        let testBin = Array.sub ir 0 3 in
        (match testBin with

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
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                shouldSetCondCodes <- true

              (* Move Shifted Register *)
              | _ ->
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                muxB <- Array.append (Array.make 27 false) (Array.sub ir 5 5);
                shouldSetCondCodes <- false;
                (match (ir.(3), ir.(4)) with
                  | (false, false) -> operation <- "LSL"
                  | (false, true) -> operation <- "LSR"
                  | (true, false) -> operation <- "ASR"
                  | _ -> failwith "Error! Invalid move shifted register instruction!"
                )
            );

          (* Move/Compare/Add/Sub immediate *)
          | [|false; false; true|] ->
            dest <- int_of_binary_unsigned (Array.sub ir 5 3);
            rA <- Array.append (Array.make 24 ir.(8)) (Array.sub ir 8 8);
            shouldSetCondCodes <- true;
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
            )

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
                    shouldSetCondCodes <- true;
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

                  (* Hi Register Operations / Branch Exchange *)
                  | true ->
                    shouldSetCondCodes <- false;
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
                  muxB <- generalRegisters.(15);
                  shouldSetCondCodes <- false
              )

              | true ->
                memoryOp <- true;
                operation <- "ADD";
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                muxB <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 7 3));
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                shouldSetCondCodes <- false;
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
            shouldSetCondCodes <- false;
            (match (ir.(4), ir.(3)) with
              | (false, false) -> memOperation <- "STR"
              | (false, true) -> memOperation <- "LDR"
              | (true, false) -> memOperation <- "STRB"
              | (true, true) -> memOperation <- "LDRB"
            )

          | [|true; false; false|] ->
            shouldSetCondCodes <- false;
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
            shouldSetCondCodes <- false;
            (match ir.(3) with

              (* Load address *)
              | false ->
                muxB <- Array.append (Array.make 24 false) (Array.sub ir 8 8);
                operation <- "ADD";
                dest <- int_of_binary_unsigned (Array.sub ir 5 3);
                memoryOp <- false;
                (match ir.(4) with
                  | false -> rA <- generalRegisters.(pc)
                  | true -> rA <- generalRegisters.(sp)
                )

              | true ->
                (match ir.(5) with

                  (* Add offset to Stack Pointer *)
                  | false ->
                    memoryOp <- false;
                    rA <- generalRegisters.(sp);
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
            shouldSetCondCodes <- false;
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
            shouldSetCondCodes <- false;
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
          | "AND" -> rZ <- logical_and rA muxB
          | "EOR" -> rZ <- logical_xor rA muxB
          | "LSL" -> rZ <- logical_shift_left rA (int_of_binary_unsigned muxB)
          | "LSR" -> rZ <- logical_shift_right rA (int_of_binary_unsigned muxB)
          | "ASR" -> rZ <- arith_shift_right rA (int_of_binary_unsigned muxB)
          | "ADC" -> rZ <- plus rA (plus muxB [|conditionCodes.(2)|])
          | "SBC" -> rZ <- minus rA (plus muxB [|not conditionCodes.(2)|])
          | "ROR" -> rZ <- rotate_right rA (int_of_binary_unsigned muxB)
          | "TST" -> rZ <- logical_and rA muxB
          | "NEG" -> rZ <- minus (Array.make 32 false) rA
          | "CMP" -> rZ <- minus rA muxB
          | "CMN" -> rZ <- plus rA muxB
          | "ORR" -> rZ <- logical_or rA muxB
          | "MUL" -> () (* TODO: Booth's Multiplication algorithm *)
          | "BIC" -> rZ <- logical_and rA (logical_not muxB)
          | "MVN" -> rZ <- logical_not muxB
          | _ -> failwith "Invalid ALU command!"
        );
        self#memory

      method memory =

        self#writeback

      method writeback = ()
    end;;
