open Printf

(*
Se réferer à types.ml pour comprendre la modélisation des propositions:
Attention: un élément `Var n` doit avoir n>=1
*)

(*Des fonction pour l'affichage*)

let rec string_of_formula f =
    match f with
    |Types.Var n -> string_of_int n
    |Types.And (p, q) ->
    sprintf "(%s and %s)" (string_of_formula p) (string_of_formula q)
    |Types.Or (p, q) ->
    sprintf "(%s or %s)" (string_of_formula p) (string_of_formula q)
    |Types.Not p ->
    sprintf "(not %s)" (string_of_formula p)

let rec string_of_clause f =
    match f with
    |[] -> ""
    |[Types.V n] -> sprintf " %d " n
    |[Types.Nv n] -> sprintf "(not %d)" (-n)
    |(Types.V n)::fprime -> sprintf "(%s or %s)" (string_of_int n) (string_of_clause fprime)
    |(Types.Nv n)::fprime -> sprintf "((not %d) or %s)" (-n) (string_of_clause fprime)

let rec string_of_fnc f =
    match f with
    |[] -> ""
    |[clause] -> sprintf " %s " (string_of_clause clause)
    |clause::fprime -> sprintf "(%s and %s)" (string_of_clause clause) (string_of_fnc fprime)

(*
    ENTREE
    On veut montrer que theorie => conclusion
*)

let theory = [Types.Var 1; Types.Not (Types.Var 2)]
let conclusion = Types.Or (Types.And (Types.Var 1, Types.Var 2), Types.And (Types.Var 1, Types.Not (Types.Var 3)))

(*
    APPELS AUX FONCTIONS
*)

let theorem_pre_conversion = (Types.Not conclusion)::theory
let theorem = List.map Conversion.convert theorem_pre_conversion

let () = List.iter print_endline (List.map string_of_fnc theorem)
