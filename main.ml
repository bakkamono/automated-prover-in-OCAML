open Printf

(*Une fonction pour l'affichage*)

let rec string_of_formula f =
    match f with
    |Types.Var n -> string_of_int n
    |Types.And (p, q) ->
    sprintf "(%s and %s)" (string_of_formula p) (string_of_formula q)
    |Types.Or (p, q) ->
    sprintf "(%s or %s)" (string_of_formula p) (string_of_formula q)
    |Types.Not p ->
    sprintf "(not %s)" (string_of_formula p)

(*
    ENTREE
    On veut montrer que theorie => conclusion
*)

let theory = [Types.Var 1]
let conclusion = Types.Or (Types.And (Types.Var 1, Types.Var 2), Types.And (Types.Var 1, Types.Not (Types.Var 3)))

(*
    APPELS AUX FONCTIONS
*)

let theorem_pre_conversion = (Types.Not conclusion)::theory
let theorem = List.map Conversion.convert theorem_pre_conversion

let () = print_string (string_of_formula conclusion); print_newline ()
