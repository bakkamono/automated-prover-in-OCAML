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
    |Or (p, q) ->
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
