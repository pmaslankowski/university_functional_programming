(* Programowanie funkcyjne *
 *   Lista 5, grupa kzi    *
 *   Piotr Maślankowski    *)

(* exercise 1 *)
type 'a llist = 
  | LNil 
  | LCons of 'a * 'a llist Lazy.t

type 'a ltree =
  | LEmpty
  | LNode of 'a * 'a ltree Lazy.t llist


(* auxilary functions *)
let rec lappend x y = 
  match x with
    | LNil -> y
    | LCons (hd, lazy tl) -> LCons (hd, lazy (lappend tl y))

let rec lfromlist l = 
  match l with
    | [] -> LNil
    | x::xs -> LCons (x, lazy (lfromlist xs))

let rec fromllist l = 
  match l with
    | LNil -> []
    | LCons (x, lazy xs) -> x::fromllist xs

let rec ltake l n = 
  match l with
    | LNil -> []
    | LCons (x, lazy xs) -> if n > 0 then x::ltake xs (n-1) else []

(* exercise 2 *)
let ltree2llist (tree : 'a ltree) : 'a llist =
  (* linear bfs on given tree *)
  let rec aux curr_level next_level =
    match curr_level with
      | LNil -> if next_level = LNil then LNil else aux next_level LNil
      | LCons (lazy t, lazy curr_level') -> match t with
        | LEmpty -> aux curr_level' next_level
        | LNode (label, children) -> let next_level' = lappend children next_level
                                     in  LCons (label, lazy (aux next_level' curr_level'))
  in 
  match tree with
    | LEmpty -> LNil
    | LNode (root, children) -> LCons (root, lazy (aux children LNil))

let test1 = LNode (1, (lfromlist [lazy (LNode (2, LNil)); lazy (LNode (3, LNil)); lazy (LNode (4, LNil))]))
let rec test2 n = LNode (n, LCons (lazy (test2 (n+1)), lazy LNil))


(* exercise 3 *)