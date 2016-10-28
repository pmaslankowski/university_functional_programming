(* Piotr Maślankowski
   Programowanie funkcyjne, Lista 3
   grupa kzi *)

(* auxilary function *)
let rec rev_app (l1 : 'a list) (l2 : 'a list) =
    match l1 with
        | [] -> l2
        | x::xs -> rev_app xs (x::l2)

(* exercise 1 *)
let rec unfoldr ( f : ('a -> ('a * 'b) option))  (seed : 'a) =
    match f seed with
        | Some(a', b') -> b' :: unfoldr f a' 
        | None -> []

(* exercise 2 *)
let rec scanl (f : 'a -> 'b -> 'a) (b : 'a) (l : 'b list) : 'a list =
    let rec aux f b l acc =
        match l with
            | x::xs -> let prev::_ = acc
                       in
                       aux f (f b x) xs ((f prev x)::acc)
            | [] -> acc
    in
    rev_app (aux f b l [b]) []

(* exercise 3 *)
(*TODO! *)

(* exercise 4 *)
let reverse (l : 'a list) : 'a list = 
    List.fold_left (fun x y -> y::x) [] l

(* exercise 5 *)
let map (f : 'a -> 'b) (l : 'a list) : 'b list =
    List.fold_right (fun x y -> (f x) :: y) l []

(* exercise 6 *)
let all (p : 'a -> bool) (l : 'a list) : bool =
    List.fold_left (fun x y -> x && p y) true l

(* exercise 7 *)
let any (p : 'a -> bool) (l : 'a list) : bool =
    try
        List.fold_left (fun x y -> if p y then raise Exit else x) false l
    with Exit -> true

(* exercise 8 *)
type arithm_literal = | Neg | Add | Sub | Mul | Div | Pow
                      | Num of float

(* exercise 9 *)
exception TooManyOperations
exception TooFewOperations
exception IllegalOperation of arithm_literal * float * float

let eval_rpn (l : arithm_literal list) : float = 
    let eval acc y =
        let eval_aux op l op_t = 
        (* funkcja wylicza wartość pary argumentów w akumulatorze
         * ostatni argument jest używany tylko przy rzucaniu wyjątku *)
            match l with
                | prev2::prev1::prevs -> let res = op prev1 prev2 in 
                                         if classify_float res <> FP_normal
                                         then raise (
                                            IllegalOperation (op_t, prev1, prev2))
                                         else res :: prevs 
                | _ -> raise TooManyOperations
        in
        match y with
            | Num(z) -> z::acc
            | Neg -> if acc = [] then raise TooManyOperations
                                 else let prev::prevs = acc in 
                                      -1. *. prev::prevs
            | Add -> eval_aux (+.) acc y
            | Sub -> eval_aux (-.) acc y
            | Mul -> eval_aux ( *. ) acc y
            | Div -> eval_aux (/.) acc y
            | Pow -> eval_aux ( ** ) acc y
    in 
    match List.fold_left eval [] l with
        | [x] -> x
        | _ -> raise TooFewOperations     