(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type input = [ `Lambtex | `Lambwiki | `Lambxml | `Markdown | `Sexp ]
type output = [ `Sexp | `Html ]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val input_parser: string -> [ `Ok of input | `Error of string ]
val output_parser: string -> [ `Ok of output | `Error of string ]

val input_converter: input Cmdliner.Arg.converter
val output_converter: output Cmdliner.Arg.converter

