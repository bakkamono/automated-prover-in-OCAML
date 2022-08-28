(*
    DEFINITION DES TYPES
*)

type prop =
    |Var of int
    |Not of prop
    |Or of prop * prop
    |And of prop * prop

type litteral =
    |V of int
    |Nv of int

type clause = litteral list

type fnc = clause list

(*
    On définit la fonction `convertit: prop -> fnc` qui donne une version sous forme normale conjonctive de la proposition passée en argument.
    Pour cela, on utilise une table de conversion qui donne des équivalences entre deux propositions
*)

let rec flatten_or p = (*p = Or (...) avec que des Or, renvoie une expression en Or(Or(...))*)
    let rec aux p acc =
        match p with
        |Var n -> n::acc
        |Not (Var n) -> (-n)::acc
        |Or (p1, p2) -> aux p1 (aux p2 acc)
        |_ -> failwith "ne peut pas se produire"
    in let rec without_repeat u =
        match u with
        |[] -> u
        |[x] -> u
        |x::y::s -> if x=y then without_repeat (x::s) else x::(without_repeat (y::s))
    in let rec flatten u =
        match u with
        |[n] -> if n>0 then Var n else if n<0 then Not (Var n) else failwith "mauvaise indexation"
        |n::ns -> if n>0 then Or (flatten ns, Var n) else if n<0 then Or (flatten ns, Not (Var n)) else failwith "mauvaise indexation"
        |_ -> failwith "ne peut pas se produire"
    in let variables_rep = aux p []
    in let variables = without_repeat (List.sort (fun x y -> x-y) variables_rep)
    in flatten variables

let rec or_distribution p q =
    match p, q with
    |Var n , Var m -> Or (p, q)
    |Var n , Not (Var m) -> Or (p, q)
    |Var n , Or (q1, q2) -> flatten_or (Or (p, q))
    |Var n , And (q1, q2) -> And (or_distribution (Var n) q1, or_distribution (Var n) q2)

    |Not (Var n), Not (Var m) -> Or (p, q)
    |Not (Var n), Or (q1, q2) -> flatten_or (Or (p, q))
    |Not (Var n), And (q1, q2) -> And (or_distribution (Not (Var n)) q1, or_distribution (Not (Var n)) q2)

    |Or (p1, p2), Or (q1, q2) -> flatten_or (Or (p, q))
    |Or (p1, p2), And (q1, q2) -> or_distribution p1 (or_distribution p2 q)

    |And (p1, p2), And (q1, q2) -> And (And (And (Or (p1, q1), Or (p1, q2)), Or (p2, q1)), Or (p2, q2))

    |_ -> or_distribution q p

let rec extract_disj f = (*donne la liste de disjonctions d'une formule en FNC*)
    let rec aux f acc =
        match f with
        |And (p, q) -> aux p (aux q acc)
        |Or (p, q) -> f::acc
        |w -> w::acc
    in aux f []

let rec prop_to_propfnc f =
    match f with
    |Var n -> f
    |Not (Var n) -> f
    |And (p, q) -> And (prop_to_propfnc p, prop_to_propfnc q)
    |Or (p, q) -> or_distribution (prop_to_propfnc p, prop_to_propfnc q)
    |Not (Not p) -> p
    |Not (And (p, q)) -> prop_to_propfnc (Or (Not p, Not q))
    |Not (Or (p, q)) -> prop_to_propfnc (And (Not p, Not q))

let rec prop_to_clause d =
    let rec aux d acc =
        match d with
        |Var n -> (V n)::acc
        |Not (Var n) -> (Nv n)::acc
        |Or (p, q) -> aux p (aux q acc)
        |_ -> failwith "ne devrait pas se produire"
    in aux d []

let convert f = List.map prop_to_clause (extract_disj (prop_to_propfnc f))
