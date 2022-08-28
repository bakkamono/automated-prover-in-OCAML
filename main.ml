open Printf
open Conversion

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

(*Une fonction pour l'affichage*)

let rec string_of_formula f =
    match f with
    |Var n -> string_of_int n
    |And (p, q) ->
    sprintf "(%s and %s)" (string_of_formula p) (string_of_formula q)
    |Or (p, q) ->
    sprintf "(%s or %s)" (string_of_formula p) (string_of_formula q)
    |Not p ->
    sprintf "(not %s)" (string_of_formula p)


(*
    ENTREE
    On veut montrer que theorie => conclusion
*)

let theory = [Var 1]
let conclusion = Var 1

(*
    APPELS AUX FONCTIONS
*)

let theorem_pre_conversion = (Not conclusion)::theory
let theorem = List.map (Conversion.convert) theoreme_pre_conversion
