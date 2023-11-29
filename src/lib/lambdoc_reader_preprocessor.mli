(** Preprocessing on a document source.
*)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val verify_utf8: string -> [ `Okay | `Error of string * int list ]

