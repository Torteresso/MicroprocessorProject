exception Cycle

type mark = NotVisited | InProgress | Visited

type 'a graph = { mutable g_nodes : 'a node list }

and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n =
    { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] }
  in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x = List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found ->
    Format.eprintf "Tried to add an edge between non-existing nodes";
    raise Not_found

let clear_marks g = List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes
let find_roots g = List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let rec node_has_cycle (n : 'a node) : bool =
  if n.n_mark = InProgress then true
  else (
    n.n_mark <- InProgress;
    let node_has_cycle = List.exists node_has_cycle n.n_link_to in
    n.n_mark <- Visited;
    node_has_cycle)

let has_cycle (g : 'a graph) : bool =
  clear_marks g;
  let result =
    List.exists
      (fun n -> if n.n_mark = Visited then false else node_has_cycle n)
      g.g_nodes
  in
  clear_marks g;
  result

let rec add_node_to_topological_list n l : unit =
  if n.n_mark = Visited then ()
  else if n.n_link_to = [] then (
    l := n.n_label :: !l;
    n.n_mark <- Visited)
  else (
    List.iter
      (fun other_n -> add_node_to_topological_list other_n l)
      n.n_link_to;
    l := n.n_label :: !l;
    n.n_mark <- Visited)

let topological g : 'a list =
  clear_marks g;
  if has_cycle g then raise Cycle;
  let l = ref [] in
  List.iter
    (fun n -> if n.n_mark <> Visited then add_node_to_topological_list n l)
    g.g_nodes;
  clear_marks g;
  !l
