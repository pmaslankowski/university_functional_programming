(* Programowanie funkcyjne 2016 *
 *      Lista 6, grupa kzi      *
 *      Piotr Maślankowski      *)

(* exercise 1 *)
type 'a arr = 'a ref list

let example_array = [ref 0; ref 2; ref 3]

let rec modify a i new_val = 
  match a, i with
    | x::_, 0 -> x := new_val 
    | _::xs, _ -> modify xs (i-1) new_val
    | _, _ -> ()

(*żeby zmodyfikować robimy: modify example_array 0 1 *)


(* exercise 2 *)
type 'a graph = 'a list * ('a -> 'a list)
type raw_graph = int * (int -> int list)
type 'a opt_graph = raw_graph * (int -> 'a) * ('a -> int)

exception NotInList
let rec find el = function
  | x::xs -> if fst x = el then snd x + 1 else find el xs
  | [] -> raise NotInList

let rec enumerate v start = 
  match v with
    | x::xs -> (x, start)::enumerate xs (start+1)
    | [] -> [] 


let raw_graph ((vertices, edges) : 'a graph) : raw_graph  = 
  let n          = List.length vertices 
  and enumerated = enumerate vertices 0
  in 

  let adjArray    = Array.make n []
  and neighbors v = List.map (fun x -> find x enumerated) (edges v)
  in

  let rec makeAdj i = function
    | v::vs -> (adjArray.(i) <- neighbors v; makeAdj (i+1) vs)
    | [] -> ()
  in
  begin
    makeAdj 0 vertices;
    (n, fun i -> adjArray.(i-1))
  end


  let test : char graph = (['a'; 'b'; 'c'; 'd'; 'e'; 'f'], fun x -> match x with
    | 'a' -> ['b']
    | 'b' -> ['c']
    | 'c' -> ['d'; 'a']
    | 'd' -> ['b'; 'c'; 'e']
    | 'e' -> []
    | 'f' -> ['c'; 'd'])


(* exercise 3 *)
let opt_graph (g : 'a graph) : 'a opt_graph = 
  let raw = raw_graph g and (vertices, edges) = g in 
  let labArray = Array.make (List.length vertices) None
  and enumerated = enumerate vertices 0
  in
  let rec makeLabels i = function
    | v::vs -> (labArray.(i) <- Some v; makeLabels (i+1) vs)
    | [] -> ()
  and getLabel i = let Some lab = labArray.(i-1) in lab
  in
  let rec getIndex l lab =
    match l with 
      | (v, index)::vs -> if v = lab then index+1 else getIndex vs lab
  in
  begin
    makeLabels 0 vertices;
    (raw, getLabel, getIndex enumerated)
  end 


(* exercise 4 *)
let printArray a = 
  let printEl x = match x with
    | None -> print_string "None"
    | Some prev -> print_string ((string_of_int prev) ^ "\n")
  in Array.iter printEl a

exception NotReachable
let short_path (g : 'a opt_graph) (first : 'a) (last : 'a) = 
  let ((n, edges), lab, nod) = g in 
  let u = nod first and v = nod last in 
  let visited = Array.make (n+1) None
  and queue = Queue.create () in 

  let rec bfs () =
    if Queue.is_empty queue then ()
    else 
    let (prev, curr) = Queue.take queue in 
    if visited.(curr) = None then 
      begin 
        visited.(curr) <- Some prev; 
        List.iter (fun x -> Queue.add (curr, x) queue) (edges curr);
        bfs ()
      end
    else bfs () 
  in

  let rec build_path first last acc = 
    match visited.(last) with
      | None -> raise NotReachable
      | Some prev -> if last = first then (lab first)::acc 
                                     else build_path first prev ((lab last)::acc)
  in
  begin
    Queue.add (u,u) queue;
    bfs ();
    try
      Some (build_path u v [])
    with NotReachable -> None
  end