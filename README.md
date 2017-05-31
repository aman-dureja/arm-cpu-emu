# ARM CPU Emulator

Software implementation - written in OCAML - of an ARM7TDMI processor core that interprets the ARMv4T THUMB instruction set.

Instruction set documentation here: https://ece.uwaterloo.ca/~ece222/ARM/ARM7-TDMI-manual-pt3.pdf

### Background

The goal of this project is to emulate as closely as possible the hardware CPU core. As such, it makes use of the general five stage RISC datapath. It makes sense to use a functional programming language like OCAML, as the task of decoding and running binary instructions lends itself well to functional paradigms, which take advantage of the Lambda Calculus.

### Instruction Set Architecture

The emulator interprets the ARMv4T THUMB instruction set. THUMB instructions are 16 bits in length and provide an expansive subset of the functionality of the full 32-bit ARM instructions.

### Hardware

The provided "hardware" includes 16 general purpose registers, of which some are special registers. Registers 13, 14, and 15 are the Stack Pointer (SP), Link Registers (LR), and Program Counter (PC), respectively. There is also an Instruction Register (IR), used for storing the instruction to execute.

There is also a 2 GiB byte-addressable memory provided as an array for loading/storing data. The program is loaded beginning at memory address 0, and the stack grows upwards (towards lower memory addresses) beginning at memory address 2048.

### Datapath

The [five-stage RISC pipeline](https://en.wikipedia.org/wiki/Classic_RISC_pipeline) is used:

- **Fetch:** retrieve next instruction from the memory address pointed to by the PC, store it in the IR, and then increment the PC by 2 so that it points to the next instruction.
- **Decode:** decode the instruction in the IR to determine what operations to perform.
- **Execute:** execute the decoded instruction and perform the desired operations.
- **Memory:** if the current instruction requires a memory access, do that here.
- **Writeback:** if the current instruction writes data to a general register, do that here.

### Running a Program

To run a program, do the following:

1. Write the program in ARMv4T assembly language and assemble it to binary.
2. Separate the assembled code into 8-bit chunks encoded as a boolean array. For convenience, make use of the *ba_of_bs* method provided in *binary_operations.ml*.
3. Create a new instance of the CPU object and call the *loadProgramInMem* method, passing the array of 8-bit boolean arrays as the parameter.
4. Call the *runProgram* method of the CPU to run the program.
5. Call the *printState* method of the CPU to view the final state of the registers after the program has been run.

For an example, take a look at the *sample.ml* file.

### Next Steps

- Because the five stage RISC pipeline has been used, it is possible to modify the implementation to make use of superscalar operation and other pipelining optimizations.
- A disassembler would be cool.
- It would be nice to have an assembler rather than manually encoding instructions to binary. But that's another project :)
