(*
    On définit la fonction `convert: prop -> fnc` qui donne une version sous forme normale conjonctive de la proposition passée en argument.
    Pour cela, on utilise une table de conversion qui donne des équivalences entre deux propositions
*)

let rec flatten_or p = (*p = Types.Or (...) avec que des Types.Or, renvoie une expression en Types.Or(Types.Or(...))*)
    let rec aux p acc =
        match p with
        |Types.Var n -> n::acc
        |Types.Not (Types.Var n) -> (-n)::acc
        |Types.Or (p1, p2) -> aux p1 (aux p2 acc)
        |_ -> failwith "ne peut pas se produire"
    in let rec without_repeat u =
        match u with
        |[] -> u
        |[x] -> u
        |x::y::s -> if x=y then without_repeat (x::s) else x::(without_repeat (y::s))
    in let rec flatten u =
        match u with
        |[n] -> if n>0 then Types.Var n else if n<0 then Types.Not (Types.Var n) else failwith "mauvaise indexation"
        |n::ns -> if n>0 then Types.Or (flatten ns, Types.Var n) else if n<0 then Types.Or (flatten ns, Types.Not (Types.Var n)) else failwith "mauvaise indexation"
        |_ -> failwith "ne peut pas se produire"
    in let variables_rep = aux p []
    in let variables = without_repeat (List.sort (fun x y -> x-y) variables_rep)
    in flatten variables

let rec or_distribution p q =
    match p, q with
    |Types.Var n , Types.Var m -> Types.Or (p, q)
    |Types.Var n , Types.Not (Types.Var m) -> Types.Or (p, q)
    |Types.Var n , Types.Or (q1, q2) -> flatten_or (Types.Or (p, q))
    |Types.Var n , Types.And (q1, q2) -> Types.And (or_distribution (Types.Var n) q1, or_distribution (Types.Var n) q2)

    |Types.Not (Types.Var n), Types.Not (Types.Var m) -> Types.Or (p, q)
    |Types.Not (Types.Var n), Types.Or (q1, q2) -> flatten_or (Types.Or (p, q))
    |Types.Not (Types.Var n), Types.And (q1, q2) -> Types.And (or_distribution (Types.Not (Types.Var n)) q1, or_distribution (Types.Not (Types.Var n)) q2)

    |Types.Or (p1, p2), Types.Or (q1, q2) -> flatten_or (Types.Or (p, q))
    |Types.Or (p1, p2), Types.And (q1, q2) -> or_distribution p1 (or_distribution p2 q)

    |Types.And (p1, p2), Types.And (q1, q2) -> Types.And (Types.And (Types.And (Types.Or (p1, q1), Types.Or (p1, q2)), Types.Or (p2, q1)), Types.Or (p2, q2))

    |_ -> or_distribution q p

let rec extract_disj f = (*donne la liste de disjonctions d'une formule en FNC*)
    let rec aux f acc =
        match f with
        |Types.And (p, q) -> aux p (aux q acc)
        |Types.Or (p, q) -> f::acc
        |w -> w::acc
    in aux f []

let rec prop_to_propfnc f =
    match f with
    |Types.Var n -> f
    |Types.Not (Types.Var n) -> f
    |Types.And (p, q) -> Types.And (prop_to_propfnc p, prop_to_propfnc q)
    |Types.Or (p, q) -> or_distribution (prop_to_propfnc p) (prop_to_propfnc q)
    |Types.Not (Types.Not p) -> p
    |Types.Not (Types.And (p, q)) -> prop_to_propfnc (Types.Or (Types.Not p, Types.Not q))
    |Types.Not (Types.Or (p, q)) -> prop_to_propfnc (Types.And (Types.Not p, Types.Not q))

let rec prop_to_clause d =
    let rec aux d acc =
        match d with
        |Types.Var n -> (Types.V n)::acc
        |Types.Not (Types.Var n) -> (Types.Nv n)::acc
        |Types.Or (p, q) -> aux p (aux q acc)
        |_ -> failwith "ne devrait pas se produire"
    in aux d []

let convert f = List.map prop_to_clause (extract_disj (prop_to_propfnc f))
