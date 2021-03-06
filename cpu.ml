(* The main CPU class *)

open Binary_operations;;

(* Central Processing Unit *)
class cpu =
    object (self)

      (* General Setup *)
      val minReg = 0
      val maxReg = 15
      val pc = 15 (* Program counter *)
      val sp = 13 (* Stack pointer *)
      val lr = 14 (* Link register *)
      val mutable dest : int = 0
      val mutable generalRegisters : bool array array = Array.make 16 (Array.make 32 false)
      val mutable memory : bool array array = Array.make 2048 (Array.make 8 false)
      val mutable operation : string = "none"
      val mutable memoryOp : bool = false
      val mutable memOperation : string = "none"
      val mutable regList : int array = [||]
      val mutable multiBaseReg : int = 0
      val mutable strReg : int = 0
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
      val mutable shouldWriteback : bool = false

      (* Getters and setters exclusively for testing! *)
      method setIr bin = ir <- bin
      method getMem = memory
      method getIr = ir
      method getGeneralRegisters = generalRegisters
      method setMem updated = memory <- updated
      method setGeneralRegisters reg updated = generalRegisters.(reg) <- updated
      method setGeneralRegistersAll updated = generalRegisters <- updated
      method setRa updated = rA <- updated
      method setRm updated = rM <- updated
      method setRz updated = rZ <- updated
      method setRy updated = rY <- updated
      method setMuxB updated = muxB <- updated
      method getRa = rA
      method getRm = rM
      method getRz = rZ
      method getRy = rY
      method getMuxB = muxB

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
        Printf.printf "\n";

      method loadProgramInMem byteArray =
        if Array.length byteArray > 2048 then failwith "Error! Program too large to fit in memory!";
        Array.iteri (fun i byte -> memory.(i) <- byte) byteArray

      method setConditionCodes =
        conditionCodes.(0) <- rA.(0) = true; (* N *)
        conditionCodes.(1) <- int_of_binary_unsigned rA = 0; (* Z *)
        (* C *)
        if operation = "ADD" then
          if comp_lt_unsigned rZ rA || comp_lt_unsigned rZ muxB then conditionCodes.(2) <- true
        else if operation = "SUB" then
          if comp_lt_unsigned muxB rA then conditionCodes.(2) <- true;
        conditionCodes.(3) <- if rA.(0) = muxB.(0) && rZ.(0) != rA.(0) then true else false; (* V *)

      method fetch =
        ir <- Array.append memory.(int_of_binary_unsigned generalRegisters.(pc)) memory.(int_of_binary_unsigned (plus generalRegisters.(pc) (binary_of_int 1)));
        generalRegisters.(pc) <- plus generalRegisters.(pc) (binary_of_int 2)

      method decode =
        let testBin = Array.sub ir 0 3 in
        (match testBin with

          | [|false; false; false|] ->
            shouldWriteback <- true;
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
            shouldWriteback <- true;
            dest <- int_of_binary_unsigned (Array.sub ir 5 3);
            muxB <- Array.append (Array.make 24 ir.(8)) (Array.sub ir 8 8);
            shouldSetCondCodes <- true;
            (match (ir.(3), ir.(4)) with

              | (false, false) ->
                operation <- "ADD";
                rA <- Array.make 32 false

              | (false, true) -> operation <- "CMP"

              | (true, false) ->
                operation <- "ADD";
                rA <- generalRegisters.(dest)

              | (true, true) ->
                operation <- "SUB";
                rA <- generalRegisters.(dest)
            )

          | [|false; true; false|] ->
            shouldWriteback <- true;
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
                  rA <- arith_shift_left rA 2;
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
                      | false -> memOperation <- "STR"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
                      | true -> memOperation <- "LDR"
                    );
                    (match ir.(5) with
                      | false -> byteWordLoadStoreFlag <- "word"
                      | true -> byteWordLoadStoreFlag <- "byte"
                    )

                  (* Load/store sign-extended byte/halfword *)
                  | true ->
                    (match (ir.(5), ir.(4)) with
                      | (false, false) -> memOperation <- "STRH"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
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
              | (false, false) -> memOperation <- "STR"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
              | (false, true) -> memOperation <- "LDR"
              | (true, false) -> memOperation <- "STRB"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
              | (true, true) -> memOperation <- "LDRB"
            )

          | [|true; false; false|] ->
            shouldWriteback <- true;
            shouldSetCondCodes <- false;
            (match ir.(3) with

              (* Load/store halfword *)
              | false ->
                memoryOp <- true;
                operation <- "ADD";
                rA <- generalRegisters.(int_of_binary_unsigned (Array.sub ir 10 3));
                muxB <- Array.append (Array.make 27 ir.(5)) (Array.sub ir 5 5);
                muxB <- arith_shift_left muxB 1;
                dest <- int_of_binary_unsigned (Array.sub ir 13 3);
                (match ir.(4) with
                  | false -> memOperation <- "STRH"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
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
                  | false -> memOperation <- "STR"; strReg <- int_of_binary_unsigned (Array.sub ir 13 3); shouldWriteback <- false
                  | true -> memOperation <- "LDR"
                )
            )

          | [|true; false; true|] ->
            shouldWriteback <- true;
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
                    shouldWriteback <- false;
                    memoryOp <- true;
                    regList <- [||];
                    let registerList = Array.sub ir 8 8 in
                    Array.iteri (fun i x -> if x then regList <- Array.append regList [|7-i|]) registerList;
                    (match (ir.(4), ir.(7)) with
                      | (false, false) -> memOperation <- "PUSH"
                      | (false, true) -> memOperation <- "PUSHLR"
                      | (true, false) -> memOperation <- "POP"
                      | (true, true) -> memOperation <- "POPPC"
                    )
                )
            )

          | [|true; true; false|] ->
            shouldWriteback <- false;
            shouldSetCondCodes <- false;
            (match ir.(3) with

              (* Multiple load/store *)
              | false ->
                memoryOp <- true;
                regList <- [||];
                multiBaseReg <- int_of_binary_unsigned (Array.sub ir 5 3);
                let registerList = Array.sub ir 8 8 in
                Array.iteri (fun i x -> if x then regList <- Array.append regList [|7-i|]) registerList;
                (match ir.(4) with
                  | false -> memOperation <- "STMIA"
                  | true -> memOperation <- "LDMIA"
                )

              (* Conditional branch *)
              | true ->
                let offset = Array.append (Array.make 24 ir.(8)) (Array.sub ir 8 8) in
                let n = conditionCodes.(0) in
                let z = conditionCodes.(1) in
                let c = conditionCodes.(2) in
                let v = conditionCodes.(3) in
                let shouldBranch = (match Array.sub ir 4 4 with
                  | [|false; false; false; false|] -> z
                  | [|false; false; false; true|] -> not z
                  | [|false; false; true; false|] -> c
                  | [|false; false; true; true|] -> not c
                  | [|false; true; false; false|] -> n
                  | [|false; true; false; true|] -> not n
                  | [|false; true; true; false|] -> v
                  | [|false; true; true; true|] -> not v
                  | [|true; false; false; false|] -> c && not z
                  | [|true; false; false; true|] -> not c || z
                  | [|true; false; true; false|] -> (n && v) || (not n && not v)
                  | [|true; false; true; true|] -> (n && not v) || (not n && v)
                  | [|true; true; false; false|] -> not z && ((n && v) || (not n && not v))
                  | [|true; true; false; true|] -> z || (n && not v) || (not n && v)
                  | _ -> failwith "Invalid branch condition!"
                ) in
                if shouldBranch then begin
                  memoryOp <- false;
                  operation <- "ADD";
                  rA <- generalRegisters.(pc);
                  muxB <- offset;
                  shouldWriteback <- true;
                  dest <- pc
                end
            )

          | [|true; true; true|] ->
            shouldWriteback <- false;
            shouldSetCondCodes <- false;
            (match ir.(3) with

              (* Unconditional branch *)
              | false ->
                shouldWriteback <- true;
                memoryOp <- false;
                dest <- pc;
                rA <- generalRegisters.(pc);
                muxB <- Array.append (Array.make 21 ir.(5)) (Array.sub ir 5 11);
                operation <- "ADD"

              (* Long branch with link *)
              | true ->
                memoryOp <- false;
                shouldWriteback <- true;
                operation <- "ADD";
                let offset = Array.append (Array.make 21 false) (Array.sub ir 5 11) in
                (match ir.(4) with

                  (* Instruction 1; offset is  upper 11 bits of target address *)
                  | false ->
                    dest <- lr;
                    rA <- generalRegisters.(pc);
                    muxB <- logical_shift_left offset 12

                  (* Instruction 2; offset is lower 11 bits of target address *)
                  | true ->
                    dest <- pc;
                    rA <- binary_of_int 0;
                    muxB <- plus generalRegisters.(lr) (logical_shift_left offset 1);
                    generalRegisters.(lr) <- (logical_or generalRegisters.(pc) (binary_of_int 1))
                )
            )

          | _ -> failwith "Error! Invalid instruction!"
        );

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
          | "MUL" -> rZ <- binary_of_int (int_of_binary_signed rA * int_of_binary_signed muxB)
          | "BIC" -> rZ <- logical_and rA (logical_not muxB)
          | "MVN" -> rZ <- logical_not muxB
          | "BX" -> generalRegisters.(lr) <- generalRegisters.(pc); generalRegisters.(pc) <- muxB;
          | _ -> failwith "Invalid ALU command!"
        );
        if shouldSetCondCodes then self#setConditionCodes;

      method memory =
        if memoryOp then
          let memAddress = int_of_binary_unsigned rZ in
          match memOperation with
            | "LDR" ->
              rY <- Array.append (Array.append (Array.append generalRegisters.(memAddress) generalRegisters.(memAddress+1)) generalRegisters.(memAddress+2)) generalRegisters.(memAddress+3)

            | "STR" ->
              for i = 0 to 3 do
                memory.(memAddress+i) <- Array.sub generalRegisters.(strReg) (8 * i) 8
              done

            | "STRH" ->
              for i = 0 to 1 do
                memory.(memAddress+i) <- Array.sub generalRegisters.(strReg) (16 + 8 * i) 8
              done

            | "LDRH" ->
              rY <- Array.append (Array.make 16 false) (Array.append generalRegisters.(memAddress) generalRegisters.(memAddress+1))

            | "LDSB" ->
              rY <- Array.append (Array.make 24 generalRegisters.(memAddress).(0)) generalRegisters.(memAddress)

            | "LDSH" ->
              rY <- Array.append (Array.make 16 generalRegisters.(memAddress).(0)) (Array.append generalRegisters.(memAddress) generalRegisters.(memAddress+1))

            | "STRB" ->
              memory.(memAddress) <- Array.sub generalRegisters.(strReg) 24 8

            | "LDRB" ->
              rY <- Array.append (Array.make 24 false) generalRegisters.(memAddress)

            | "PUSH" ->
              let fourBin = binary_of_int 4 in
              Array.iter (fun regNum ->
                generalRegisters.(sp) <- minus generalRegisters.(sp) fourBin;
                for i = 0 to 3 do
                  memory.(sp+i) <- Array.sub generalRegisters.(regNum) (8 * i) 8
                done
              ) regList

            | "PUSHLR" ->
              regList <- Array.append regList [|14|];
              let fourBin = binary_of_int 4 in
              Array.iter (fun regNum ->
                generalRegisters.(sp) <- minus generalRegisters.(sp) fourBin;
                for i = 0 to 3 do
                  memory.(sp+i) <- Array.sub generalRegisters.(regNum) (8 * i) 8
                done
              ) regList;

            | "POP" ->
              let fourBin = binary_of_int 4 in
              for i = (Array.length regList) - 1 downto 0 do
                let regNum = regList.(i) in
                let tempBinArray = ref [||] in
                for i = 0 to 3 do
                  tempBinArray := Array.append !tempBinArray memory.(sp+i)
                done;
                generalRegisters.(regNum) <- !tempBinArray;
                generalRegisters.(sp) <- plus generalRegisters.(sp) fourBin
              done

            | "POPPC" ->
              regList <- Array.append regList [|15|];
              let fourBin = binary_of_int 4 in
              for i = (Array.length regList) - 1 downto 0 do
                let regNum = regList.(i) in
                let tempBinArray = ref [||] in
                for i = 0 to 3 do
                  tempBinArray := Array.append !tempBinArray memory.(sp+i)
                done;
                generalRegisters.(regNum) <- !tempBinArray;
                generalRegisters.(sp) <- plus generalRegisters.(sp) fourBin
              done

            | "STMIA" ->
              let fourBin = binary_of_int 4 in
              Array.iter (fun regNum ->
                for i = 0 to 3 do
                  memory.(i + int_of_binary_unsigned generalRegisters.(multiBaseReg)) <- Array.sub generalRegisters.(regNum) (8 * i) 8
                done;
                generalRegisters.(multiBaseReg) <- plus generalRegisters.(multiBaseReg) fourBin;
              ) regList

            | "LDMIA" ->
              let fourBin = binary_of_int 4 in
              for i = (Array.length regList) - 1 downto 0 do
                let regNum = regList.(i) in
                let tempBinArray = ref [||] in
                for i = 0 to 3 do
                  tempBinArray := Array.append !tempBinArray memory.(i + int_of_binary_unsigned generalRegisters.(multiBaseReg))
                done;
                generalRegisters.(regNum) <- !tempBinArray;
                generalRegisters.(multiBaseReg) <- plus generalRegisters.(multiBaseReg) fourBin
              done

            | _ -> failwith "Invalid memory operation!"

        else rY <- rZ

      method writeback =
        if shouldWriteback then generalRegisters.(dest) <- rY

      method runProgram =
        self#fetch;
        if ir = Array.make 16 false then () else begin
          self#decode;
          self#execute;
          self#memory;
          self#writeback;
          self#runProgram
        end

    end;;
