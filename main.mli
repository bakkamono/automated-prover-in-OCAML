(*
main.mli
*)

val string_of_formula : Types.prop -> string
val theory : Types.prop list
val conclusion : Types.prop

val theorem_pre_conversion : Types.prop list
val theorem : Types.fnc list


