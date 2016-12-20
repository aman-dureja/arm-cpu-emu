(* The main CPU object *)

class cpu =
    object
      (* General Setup *)
      val minReg = 0
      val maxReg = 31
      val pc = 0
      val stackPointer = 0
      val mutable generalRegisters : int array array = Array.make 32 [||]
      (* Interstage Registers *)
      val mutable rA : int array = Array.make 32 0
      val mutable rB : int array = Array.make 32 0
      val mutable rM : int array = Array.make 32 0
      val mutable rZ : int array = Array.make 32 0
      val mutable rY : int array = Array.make 32 0
      (* Methods *)
      method validateRegNumber n =
        if n < 0 || n > 31 then failwith "Invalid register number!"
      method loadProgramInMem = ()
      method fetch = ()
      method decode = ()
      method execute = ()
      method memory = ()
      method writeback = ()
    end
