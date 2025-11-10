let print_only = ref false
let number_steps = ref (-1)

open Netlist_ast

exception Netlist_compilation_error of string

type dataValue = { mutable current : value; mutable previous : value }
type data = Var of ident | ROM of (ident * int) | RAM of ident * int

(* Approximatly 30 variables, 16 slots of rom and 2 * 16 slots of ram  *)
let (env : (data, dataValue) Hashtbl.t) = Hashtbl.create (30 + 16 + (2 * 16))
let boolToInt = function true -> 1 | false -> 0

let bitToInt = function
  | VBit b -> boolToInt b
  | VBitArray bA ->
      let v = ref 0 in
      List.iteri
        (fun i b -> v := !v + ((1 lsl i) * boolToInt b))
        (List.rev (Array.to_list bA));
      !v

let createMemory memoryDataF word_size =
  let defaultB =
    match word_size with
    | 1 -> VBit false
    | _ -> VBitArray (Array.make word_size false)
  in
  for i = 0 to 15 do
    Hashtbl.add env (memoryDataF i) { current = defaultB; previous = defaultB }
  done

(* TODO : error if Not_found from env*)
let evaluate expr x =
  let evaluateArg = function
    | Avar x -> (Hashtbl.find env (Var x)).current
    | Aconst c -> c
  in
  let processBits transform = function
    | VBit b -> VBit (transform b)
    | VBitArray bA -> VBitArray (Array.map (fun b -> transform b) bA)
  in
  let processBits2 transform2 a1 a2 =
    let lenghtException () =
      raise
        (Netlist_compilation_error
           "Cannot do binary operation between bits of different lenght.")
    in
    match (a1, a2) with
    | VBit b1, VBit b2 -> VBit (transform2 b1 b2)
    | VBitArray bA1, VBitArray bA2 ->
        if Array.length bA1 <> Array.length bA2 then lenghtException ()
        else VBitArray (Array.map2 (fun b1 b2 -> transform2 b1 b2) bA1 bA2)
    | _ -> lenghtException ()
  in
  match expr with
  | Earg a -> evaluateArg a
  | Ereg x -> (Hashtbl.find env (Var x)).previous
  | Enot a -> processBits (fun b -> not b) (evaluateArg a)
  | Ebinop (op, a1, a2) -> (
      let v1 = evaluateArg a1 in
      let v2 = evaluateArg a2 in
      match op with
      | Or -> processBits2 (fun b1 b2 -> b1 || b2) v1 v2
      | Xor -> processBits2 (fun b1 b2 -> b1 <> b2) v1 v2
      | And -> processBits2 (fun b1 b2 -> b1 && b2) v1 v2
      | Nand -> processBits2 (fun b1 b2 -> not (b1 && b2)) v1 v2)
  | Emux (choice, a1, a2) -> (
      match evaluateArg choice with
      | VBit c -> if c then evaluateArg a2 else evaluateArg a1
      | _ ->
          raise
            (Netlist_compilation_error
               "The choice entry for MUX operation must be only 1 bit"))
  | Erom (address_size, word_size, read_address) ->
      if Hashtbl.mem env (ROM (x, 0)) then ()
      else createMemory (fun i -> ROM (x, i)) word_size;
      let read_address = evaluateArg read_address in
      (Hashtbl.find env (ROM (x, bitToInt read_address))).current
  | Eram
      ( address_size,
        word_size,
        read_address,
        write_enable,
        write_address,
        write_data ) -> (
      if Hashtbl.mem env (RAM (x, 0)) then ()
      else createMemory (fun i -> RAM (x, i)) word_size;
      let read_address = evaluateArg read_address in
      match evaluateArg write_enable with
      | VBit b ->
          let write_address = evaluateArg write_address in
          let write_data = evaluateArg write_data in
          let v = (Hashtbl.find env (RAM (x, bitToInt read_address))).current in
          if b then
            let d = Hashtbl.find env (RAM (x, bitToInt write_address)) in
            d.current <- write_data
          else ();
          v
      | _ ->
          raise
            (Netlist_compilation_error
               "The write_enable entry for RAM operation must be only 1 bit"))
  | Econcat (a1, a2) ->
      let toArray = function VBit b -> [| b |] | VBitArray bA -> bA in
      let v1 = evaluateArg a1 in
      let v2 = evaluateArg a2 in
      VBitArray (Array.append (toArray v1) (toArray v2))
  | Eslice (i1, i2, a) -> (
      match evaluateArg a with
      | VBitArray bA ->
          if i2 < i1 || i1 < 0 || i2 >= Array.length bA then
            raise
              (Netlist_compilation_error
                 "Wrong int arguments for SLICE operator")
          else VBitArray (Array.sub bA i1 (i2 - i1 + 1))
      | _ ->
          raise
            (Netlist_compilation_error
               "SLICE operator must operate on bus of bits"))
  | Eselect (i, a) -> (
      match evaluateArg a with
      | VBitArray bA ->
          if i >= Array.length bA then
            raise
              (Netlist_compilation_error
                 "Out of range argument of SELECT operator")
          else VBit (Array.get bA i)
      | _ ->
          raise
            (Netlist_compilation_error
               "SELECT operator must operate on bus of bits"))

let getVariableNames program =
  Env.fold (fun ident ty acc -> (ident, ty) :: acc) program.p_vars []

let rec getCorrectInput x n =
  if n = 1 then Printf.printf "Value of %s : 1 bit ? " x
  else Printf.printf "Value of %s : %i bit ? " x n;

  let input = String.to_seq (read_line ()) in
  let output = ref [] in
  try
    if Seq.length input <> n then (
      Printf.printf "Invalid input, must contain exactly %i bit(s)\n" n;
      raise Not_found)
    else ();
    Seq.iter
      (fun c ->
        match c with
        | '0' -> output := false :: !output
        | '1' -> output := true :: !output
        | _ ->
            Printf.printf
              "Invalid input, must be a sequence 0 (false) or 1 (true) without \
               any space\n";
            raise Not_found)
      input;
    match List.rev !output with
    | b :: [] -> VBit b
    | bA -> VBitArray (Array.of_list bA)
  with Not_found -> getCorrectInput x n

let printVariables varList =
  List.iter
    (fun x ->
      Printf.printf "=> %s = " x;
      (match (Hashtbl.find env (Var x)).current with
      | VBit b -> Printf.printf "%i" (boolToInt b)
      | VBitArray bA ->
          Array.iter (fun b -> Printf.printf "%i" (boolToInt b)) bA);
      print_newline ())
    varList

(* The program is already scheduled *)
let simulator program number_steps =
  let variableNames = getVariableNames program in
  List.iter
    (fun (x, ty) ->
      let defaultB =
        match ty with
        | TBit -> VBit false
        | TBitArray n -> VBitArray (Array.make n false)
      in
      Hashtbl.add env (Var x) { current = defaultB; previous = defaultB })
    variableNames;
  for i = 1 to number_steps do
    Printf.printf "# Step n°%i\n" i;
    List.iter
      (fun x ->
        let d = Hashtbl.find env (Var x) in
        let v =
          getCorrectInput x
            (match d.current with
            | VBit _ -> 1
            | VBitArray bA -> Array.length bA)
        in
        d.current <- v)
      program.p_inputs;
    List.iter
      (fun (x, exp) ->
        let v = evaluate exp x in
        let d = Hashtbl.find env (Var x) in
        d.current <- v)
      program.p_eqs;

    printVariables program.p_outputs;

    Hashtbl.iter (fun x d -> d.previous <- d.current) env
  done

let compile filename =
  try
    let p = Netlist.read_file filename in
    try
      let p = Scheduler.schedule p in
      simulator p !number_steps
    with Scheduler.Combinational_cycle ->
      Format.eprintf "The netlist has a combinatory cycle.@."
  with Netlist.Parse_error s ->
    Format.eprintf "An error accurred: %s@." s;
    exit 2

let main () =
  Arg.parse
    [ ("-n", Arg.Set_int number_steps, "Number of steps to simulate") ]
    compile ""
;;

main ()
