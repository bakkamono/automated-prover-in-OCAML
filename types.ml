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
