open ExtString

exception Syntax_error

type list_t =
	| Ulist of int
	| Olist of int

type line_t =
	| Begin_code of string
	| Begin_pre
	| Heading of string
	| Par of int * list_t option * string option


let count_char str what =
	String.fold_left (fun accum c -> if c = what then accum+1 else accum) 0 str


let classify_line = function
	| RE "{{{" (lower* as lang) ->
		Begin_code lang
	| "}}}" ->
		raise Syntax_error
	| "<<<" ->
		Begin_pre
	| ">>>" ->
		raise Syntax_error
	| RE "==" blank* (_+ as text) blank* "==" ->
		Heading text
	| RE (('>' ('>' | blank)* )? as quoted) blank* (('-'+ | '*'+ | '#'+)? as listed) blank* (_* as texted) ->
		let quote_level = count_char quoted '>'
		and list_level =
			let counter = count_char listed
			in match (counter '-', counter '*', counter '#') with
				| (x, 0, 0) when x > 0	-> Some (Ulist x)
				| (0, x, 0) when x > 0	-> Some (Ulist x)
				| (0, 0, x) when x > 0	-> Some (Olist x)
				| _			-> None
		and text =
			match String.length texted with
				| 0	-> None
				| _	-> Some texted
		in Par (quote_level, list_level, text)


let print_list_level = function
	| None			-> "(none)"
	| Some (Ulist x)	-> "UL " ^ (string_of_int x)
	| Some (Olist x)	-> "OL " ^ (string_of_int x)

let print_text = function
	| None		-> "(none)"
	| Some x	-> x

let describe_line = function
	| Begin_code lang	-> Printf.printf "Code: %s\n" lang
	| Begin_pre		-> Printf.printf "Pre\n"
	| Heading str		-> Printf.printf "Heading: #%s#\n" str
	| Par (x, y, z)		-> Printf.printf "Par (Q%d, %s, %s)\n" x (print_list_level y) (print_text z)


let () =
	let lines = Std.input_list stdin in
	List.iter (fun line -> describe_line (classify_line line)) lines

