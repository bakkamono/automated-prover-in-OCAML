(*
conversion.mli
*)

val simplification_or : Types.prop -> int list
val flatten_or : Types.prop -> Types.prop
val flatten_and : Types.prop -> Types.prop
val or_distribution : Types.prop -> Types.prop -> Types.prop
val extract_disj : Types.prop -> Types.prop list
val prop_to_propfnc : Types.prop -> Types.prop
val prop_to_clause : Types.prop -> Types.clause
val convert : Types.prop -> Types.fnc
