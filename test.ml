open Cpu;;
open Binary_operations;;

let processor = new cpu;;

processor#setIr [|false; false; false; true; true|];;
processor#decode;;
