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

let concat_map f l = List.concat (List.map f l) 

let rec lmap f l = 
  match l with
    | LNil -> LNil
    | LCons (x, lazy xs) -> LCons (f x, lazy (lmap f xs))


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
let rec rewrite_tree (rules : (string * string) list) (word : string) =
  (* function returns list consisting all words created by rule *)
  let rec transform_with (pattern, dest) word =
    
    (*function returns list with indexes where pattern starts in a word *)
    let find word pattern = 
      let rec loop i j start = 
        match i, j with
          | _, _ when i < String.length word && j < String.length pattern ->
            if   (word.[i]) == (pattern.[j]) 
            then loop (i+1) (j+1) start
            else loop (start+1) 0 (start+1)
          | _, _ when j == String.length pattern -> start::(loop (start+1) 0 (start+1))
          | _, _ -> []
      in loop 0 0 0
    in

    let replace word dest start = String.init (String.length word) (fun i ->  
      if i < start || i >= start + (String.length dest) 
      then word.[i] else dest.[i - start])
    in

    let indexes = find word pattern
    in List.map (fun x -> replace word dest x) indexes
  in 
  let next_words = concat_map (fun (x, y) -> transform_with (x, y) word) rules in
  let lnext_words = lfromlist next_words in
  let children = lmap (fun x -> lazy (rewrite_tree rules x)) lnext_words
  in
  LNode (word, children)


(* exercise 4 *)
let isReachable rules u u' = 
  let rec bfs queue = 
    match queue with
      | [] -> false
      | (lazy x)::xs -> match x with
        | LNode (currWord, children) -> if currWord = u' then true 
                                        else bfs (xs @ (fromllist children))
        | LEmpty -> bfs xs
  in
  bfs [lazy (rewrite_tree rules u)]