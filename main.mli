(*
main.mli
*)

val string_of_formula : Types.prop -> string
val string_of_clause : Types.clause -> string
val string_of_fnc : Types.fnc -> string
val theory : Types.prop list
val conclusion : Types.prop

val theorem_pre_conversion : Types.prop list
val theorem : Types.fnc list
