(*
    DEFINITION DES TYPES
*)

type prop =
    |Var of int
    |Non of prop
    |Ou of prop * prop
    |Et of prop * prop

type litteral =
    |V of int
    |Nv of int

type clause = litteral list

type fnc = clause list

(*
    On définit la fonction `convertit: prop -> fnc` qui donne une version sous forme normale conjonctive de la proposition passée en argument.
    Pour cela, on utilise une table de conversion qui donne des équivalences entre deux propositions
*)

let rec extrait_disj f = (*donne la liste de disjonctions d'une formule en FNC*)
    let rec aux f acc =
        match f with
        |Et (p, q) -> aux p (aux q acc)
        |Ou (p, q) -> f::acc
        |w -> w::acc
    in aux f []

let rec prop_to_propfnc f =
    match f with
    |Var n -> f
    |Non (Var n) -> f
    |Et (p, q) -> Et (prop_to_propfnc p, prop_to_propfnc q)
    |Ou (p, q) ->
    |Non (Non p) -> p
    |Non (Et (p, q)) -> prop_to_propfnc (Ou (Non p, Non q))
    |Non (Ou (p, q)) -> prop_to_propfnc (Et (Non p, Non q))

let rec prop_to_clause d =
    let rec aux d acc =
        match d with
        |Var n -> (V n)::acc
        |Non (Var n) -> (Nv n)::acc
        |Ou (p, q) -> aux p (aux q acc)
        |_ -> failwith "ne devrait pas se produire"
    in aux d []

let convertit f = List.map prop_to_clause (extrait_disj (prop_to_propfnc f))
