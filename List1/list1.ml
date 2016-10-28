(* Programowanie funkcyjne
 * Lista 1, grupa kzi
 * Piotr Maślankowski *)

(*zadanie 1 *)
type 'a flbt = nat -> 'a
(* jeśli pozycja nie występuje w drzewie, to rzucamy dla niej wyjątek *)


(*zadanie 2 *)
let rec (mytree : char flbt) = fun (n : nat) ->
    match n with
        | [true] -> 'a'
        | [false; true] -> 'a'
        | [true; true] -> 'b'
        | _ -> mytree (ndiv2 n)


(*zadanie 3 *)
let pos_root = [true]

let rec pos_left_child n =
    if n = [true] then [false; true] else if nmod2 n = 0 then nmul2(pos_left_child(ndiv2 n))
                                                        else nmul2p1(pos_left_child(ndiv2 n))

let rec pos_right_child n =
    if n = [true] then [true; true] else if nmod2 n = 0 then nmul2(pos_left_child(ndiv2 n))
                                                        else nmul2p1(pos_left_child(ndiv2 n))
 


(*zadanie 4*)
exception EmptyTree

let is_empty : 'a flbt -> bool = fun tree ->
    try tree pos_root <> tree pos_root with EmptyTree -> true

let get : 'a flbt -> nat -> 'a = fun tree n -> tree n


(*zadanie 5*)
let empty : 'a flbt = fun _ -> raise EmptyTree

let join : 'a flbt -> 'a flbt -> 'a -> 'a flbt = fun s t v ->
    fun n -> match n with
        | [true] -> v
        | n when nmod2 n = 0 -> s (ndiv2 n)
        | n when nmod2 n = 1 -> t (ndiv2 n)

let left_subtree : 'a flbt -> 'a flbt = fun tree ->
    fun n -> tree (nmul2 n)

let right_subtree : 'a flbt -> 'a flbt = fun tree ->
    fun n -> tree (nmul2p1 n)

(* drzewo testowe *)
(*
let test_tree : char flbt = fun n ->
    match n with
       | 1 -> '0'
       | 2 -> '1'
       | 3 -> '2'
       | 5 -> 'd'
       | 6 -> 'e'
       | _ -> raise EmptyTree
*)

(* zadanie 6 *)
let rec depth : 'a flbt -> int = fun tree ->
    if is_empty tree then -1 else max (depth (left_subtree tree)) (depth (right_subtree tree)) + 1

(* zadanie 7 *)
let rec memorize : 'a flbt -> 'a flbt = fun tree ->
    try
        let curr = tree [true] and left = memorize (left_subtree tree) and right = memorize (right_subtree tree) in
            join left right curr
    with EmptyTree -> fun _ -> raise EmptyTree

(* zadanie 8 *)
let rec loop : 'a flbt -> 'a flbt = fun tree ->
    (* drugi argument to aktualne poddrzewo. Będziemy je rekurencyjnie przechodzić
     * i jeśli wyjdziemy poza aktualne drzewo, to wtedy zaczynamy przechodzić je
     * od początku. Obserwacja polega na tym, że drzewo znajdujące się za każdym z liści drzewa
     * wyjściowego, wygląda tak samo jak drzewo zapętlone. *)
    let rec result curr_tree n =
        try
            let _  = curr_tree [true] in (* w ten sposób sprawdzamy, czy nie wyszliśmy poza aktualne drzewo *)
            match n with
                | [true] -> curr_tree [true] 
                | n when nmod2 n = 0 -> result (left_subtree curr_tree) (ndiv2 n)
                | n when nmod2 n = 1 -> result (right_subtree curr_tree)(ndiv2 n) 
        with EmptyTree -> result (loop tree) n
    in result tree

(*zadanie 4, lista 2*)
let rec sum_nt (tree : int flbt) : int = 
    if is_empty tree then 0 else 
        let left_sum = sum_nt @@ left_subtree tree
        and right_sum = sum_nt @@ right_subtree tree in
            tree [true] + left_sum + right_sum

let sum_t (tree : int flbt) : int =
    let rec aux acc res = (*Taki jakby BFS - acc to lista drzew, które musimy jeszcze wysumować. *) 
        match acc with
            | [] -> res
            | x::xs -> if is_empty x then aux xs res
                                     else let left = left_subtree x
                                          and right = right_subtree x
                                          and curr = x [true]
                                          in
                                          aux (left::right::xs) (res + curr)
    in 
    aux [tree] 0

(*drzewo testowe*) 
let test_tree : int flbt = fun n ->
    match n with
        | [true] -> 1
        | [false; true] -> 2
        | [true; true] -> 3
        | [false; false; true] -> 4
        | [true; false; true] -> 10
        | _ -> raise EmptyTree 

(* zadanie 5, lista 2 *)
let rec only_right p = 
    p = pos_root || nmod2 p = 1 && only_right (ndiv2 p)

let t_right1 (d : int) : int flbt =
    let rec check p acc = 
        if p = pos_root then acc > 0
                        else nmod2 p = 1 && check (ndiv2 p) (acc-1)
    in
    (fun p -> if check p d then 1 else raise EmptyTree)

