type class_t = [ `Manuscript | `Composition ]
type input_t = [ `Lambtex | `Lamblite | `Lambhtml | `Sexp ]
type output_t = [ `Sexp | `Xhtml ]


let input_of_string = function
	| "lambtex"  -> `Lambtex
	| "lamblite" -> `Lamblite
	| "lambhtml" -> `Lambhtml
	| "sexp"     -> `Sexp
	| x	     -> invalid_arg x


let output_of_string = function
	| "sexp"  -> `Sexp
	| "xhtml" -> `Xhtml
	| x	  -> invalid_arg x


let to_string = function
	| `Lambtex  -> "Lambtex"
	| `Lamblite -> "Lamblite"
	| `Lambhtml -> "Lambhtml"
	| `Sexp     -> "Sexp"
	| `Xhtml    -> "Xhtml"

