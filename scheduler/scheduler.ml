open Netlist_ast
open Graph

exception Combinational_cycle
exception Same_variable_for_different_wires

let read_exp eq : ident list =
  let l = ref [] in
  let add_arg_to_list = function Avar x -> l := x :: !l | Aconst _ -> () in
  (let _, exp = eq in
   match exp with
   | Earg x -> add_arg_to_list x
   | Enot x -> add_arg_to_list x
   | Ebinop (_, x, y) ->
       add_arg_to_list x;
       add_arg_to_list y
   | Emux (x, y, z) ->
       add_arg_to_list x;
       add_arg_to_list y;
       add_arg_to_list z
   | Econcat (x, y) ->
       add_arg_to_list x;
       add_arg_to_list y
   | Eslice (_, _, x) -> add_arg_to_list x
   | Eselect (_, x) -> add_arg_to_list x
   | _ -> ());
  (* For now we don't add indent from registers, ram and rom*)
  !l

let schedule (p : program) : program =
  let g = mk_graph () in
  let vars = ref [] in
  List.iter
    (fun (x, exp) ->
      if List.exists (fun x2 -> x = x2) !vars then
        raise Same_variable_for_different_wires
      else vars := x :: !vars;
      if List.exists (fun n -> n.n_label = x) g.g_nodes then ()
      else add_node g x;
      List.iter
        (fun v ->
          if List.exists (fun n -> n.n_label = v) g.g_nodes then ()
          else add_node g v;
          add_edge g x v)
        (read_exp (x, exp)))
    p.p_eqs;
  if has_cycle g then raise Combinational_cycle;
  let gSorted = topological g in
  let eqsSorted = ref [] in
  List.iter
    (fun x ->
      List.iter
        (fun (y, e) -> if x = y then eqsSorted := (y, e) :: !eqsSorted else ())
        p.p_eqs)
    gSorted;
  {
    p_eqs = !eqsSorted;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }
