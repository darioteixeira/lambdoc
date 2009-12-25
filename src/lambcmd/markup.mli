type input_t = [ `Lambtex | `Lamblite | `Lambhtml | `Sexp ]
type output_t = [ `Sexp | `Xhtml ]

val input_of_string: string -> input_t
val output_of_string: string -> output_t
val to_string: [< input_t | output_t ] -> string

