type t = [ `Manuscript | `Composition ]

let of_string = function
	| "manuscript" -> `Manuscript
	| "composition" -> `Composition
	| _		-> invalid_arg "Class.of_string"
