open Conversion

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
    ENTREE
    On veut montrer que theorie => conclusion
*)

let theorie = [Var 1]
let conclusion = Var 1

(*
    APPELS AUX FONCTIONS
*)

let theoreme_pre_conversion = (Non conclusion)::theorie
let theoreme = List.map (Conversion.convertit) theoreme_pre_conversion
