(********************************************************************************)
(*	Extra.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open ExtArray
open ExtList
open ExtString
open Lambdoc_core
open Basic
open Error
open Ast
open Readconv


(********************************************************************************)
(**	{1 Module definitions}							*)
(********************************************************************************)

module Undecided =
struct
	include Map.Make (struct type t = int * int let compare = Pervasives.compare end)

	let clear_row row map =
		let func (r, c) v m =
			if row != r
			then add (r, c) v m
			else m
		in fold func map empty
end


(********************************************************************************)
(**	{1 Private type definitions}						*)
(********************************************************************************)

type property_kind_t =
	| Boolean_kind
	| Numeric_kind of int * int
	| Bullet_kind
	| Numbering_kind
	| Floatation_kind
	| Classname_kind
	| Lang_kind
	| Style_kind
	| Cover_kind


type property_id_t = string * property_kind_t


type property_data_t =
	| Boolean_data of bool
	| Boolean_auto_data of bool option
	| Numeric_data of int 
	| Numeric_auto_data of int option
	| Bullet_data of Bullet.t
	| Numbering_data of Numbering.t
	| Floatation_data of Floatation.t
	| Classname_data of Classname.t
	| Lang_data of Camlhighlight_core.lang_t
	| Style_data of Source.style_t
	| Cover_data of Book.cover_t


type field_t =
	| Boolean_field of string option * bool
	| Unnamed_field of string
	| Keyvalue_field of string * string
	| Invalid_field of string


type result_t =
	| Positive of property_data_t
	| Undecided of property_data_t
	| Negative


type solution_t =
	| No_solutions
	| One_solution of property_data_t option array * bool array
	| Multiple_solutions


(********************************************************************************)
(**	{1 Public type definitions}						*)
(********************************************************************************)

type handle_t =
	| Initial_hnd
	| Indent_hnd
	| Linenums_hnd
	| Mult_hnd
	| Frame_hnd
	| Width_hnd
	| Bullet_hnd
	| Numbering_hnd
	| Floatation_hnd
	| Classname_hnd
	| Lang_hnd
	| Style_hnd
	| Rating_hnd
	| Cover_hnd

type error_t = (int option * Error.error_msg_t) DynArray.t

type extra_t = (handle_t, property_data_t option) Hashtbl.t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Dummy values}							*)
(********************************************************************************)

let dummy_boolean_data = Boolean_data false
let dummy_numeric_data = Numeric_data 0
let dummy_bullet_data = Bullet_data Bullet.None
let dummy_numbering_data = Numbering_data Numbering.None
let dummy_floatation_data = Floatation_data Floatation.Center
let dummy_classname_data = Classname_data ""
let dummy_lang_data = Lang_data ""
let dummy_style_data = Style_data Source.Boxed
let dummy_cover_data = Cover_data Book.Medium


(********************************************************************************)
(**	{2 Low-level parsing functions}						*)
(********************************************************************************)

(**	For each handle we define the key name, the property kind, and whether
	or not it accepts auto values.
*)
let id_of_handle = function
	| Initial_hnd	 -> ("initial", Boolean_kind, false)
	| Indent_hnd	 -> ("indent", Boolean_kind, true)
	| Linenums_hnd	 -> ("nums", Boolean_kind, false)
	| Mult_hnd	 -> ("mult", Numeric_kind (0, 9), false)
	| Frame_hnd	 -> ("frame", Boolean_kind, false)
	| Width_hnd	 -> ("width", Numeric_kind (1, 100), true)
	| Bullet_hnd	 -> ("bul", Bullet_kind, false)
	| Numbering_hnd	 -> ("num", Numbering_kind, false)
	| Floatation_hnd -> ("float", Floatation_kind, false)
	| Classname_hnd	 -> ("class", Classname_kind, false)
	| Lang_hnd	 -> ("lang", Lang_kind, false)
	| Style_hnd	 -> ("style", Style_kind, false)
	| Rating_hnd	 -> ("rating", Numeric_kind (1, 5), false)
	| Cover_hnd	 -> ("cover", Cover_kind, false)


(**	This function does the low-level, regular-expression based parsing
	of each field.  At this point we don't care yet about the semantics
	or allowed meaning for each field.  All we care are the various
	ways that users can use to express a property.
*)
let fields_of_strings =
	let truth_rex = Pcre.regexp "^((?<key>[a-z]+)=)?((?<true>(true)|(yes)|(on))|(?<false>(false)|(no)|(off)))$"
	and negated_rex = Pcre.regexp "^!(?<negated>[a-z]+)$"
	and unnamed_rex = Pcre.regexp "^(?<unnamed>.+)$"
	and keyvalue_rex = Pcre.regexp "^(?<key>[a-z]+)=(?<value>.+)$"
	in fun strs ->
		let field_of_string str =
			try
				let subs = Pcre.exec ~rex:truth_rex str in
				let key = (try Some (Pcre.get_named_substring truth_rex "key" subs) with Not_found -> None)
				and value = (try ignore (Pcre.get_named_substring truth_rex "true" subs); true with Not_found -> false)
				in Boolean_field (key, value)
			with
				Not_found ->

			try
				let subs = Pcre.exec ~rex:negated_rex str
				in Boolean_field (Some (Pcre.get_named_substring negated_rex "negated" subs), false)
			with
				Not_found ->

			try
				let subs = Pcre.exec ~rex:keyvalue_rex str in
				let key = Pcre.get_named_substring keyvalue_rex "key" subs
				and value = Pcre.get_named_substring keyvalue_rex "value" subs
				in Keyvalue_field (key, value)
			with
				Not_found ->

			try
				let subs = Pcre.exec ~rex:unnamed_rex str
				in Unnamed_field (Pcre.get_named_substring unnamed_rex "unnamed" subs)
			with
				Not_found -> Invalid_field str

		in Array.map field_of_string strs


(**	This function determines whether the given string corresponds
	to a valid integer in the specified range of values.
*)
let match_numeric v low high =
	try
		let num = int_of_string v in
		if (num >= low) && (num <= high)
		then Some num
		else None
	with
		Failure _ -> None


(**	This function matches a property (defined by a key and a property kind)
	with a field parsed by {!fields_of_strings}.  It returns the associated
	data wrapped inside a {!result_t}.  That way, other functions can tell
	the degree of certainty associated with a matching.
*)
let matcher errors comm key kind auto field = match (key, kind, auto, field) with

	| (key, Boolean_kind, a, Boolean_field (Some k, v)) when k = key ->
		Positive (if a then Boolean_auto_data (Some v) else Boolean_data v)
	| (key, Boolean_kind, a, Boolean_field (None, v)) ->
		Undecided (if a then Boolean_auto_data (Some v) else Boolean_data v)
	| (key, Boolean_kind, true, Unnamed_field v) when v = "auto" ->
		Undecided (Boolean_auto_data None)
	| (key, Boolean_kind, a, Unnamed_field k) when k = key ->
		Positive (if a then Boolean_auto_data (Some true) else Boolean_data true)
	| (key, Boolean_kind, true, Keyvalue_field (k, v)) when k = key && v = "auto" ->
		Positive (Boolean_auto_data None)
	| (key, Boolean_kind, _, Keyvalue_field (k, v)) when k = key ->
		let msg = Error.Invalid_extra_boolean_parameter (comm.comm_tag, key, v) in
		DynArray.add errors (Some comm.comm_linenum, msg);
		Positive (dummy_boolean_data)

	| (key, Numeric_kind (low, high), true, Unnamed_field v) when v = "auto" ->
		Undecided (Numeric_auto_data None)
	| (key, Numeric_kind (low, high), a, Unnamed_field v) ->
		(match match_numeric v low high with
			| Some data -> Undecided (if a then Numeric_auto_data (Some data) else Numeric_data data)
			| None	    -> Negative)
	| (key, Numeric_kind (low, high), true, Keyvalue_field (k, v)) when k = key && v = "auto" ->
		Positive (Numeric_auto_data None)
	| (key, Numeric_kind (low, high), a, Keyvalue_field (k, v)) when k = key ->
		(match match_numeric v low high with
			| Some data ->
				Positive (if a then Numeric_auto_data (Some data) else Numeric_data data)
			| None ->
				let msg = Error.Invalid_extra_numeric_parameter (comm.comm_tag, key, v, low, high) in
				DynArray.add errors (Some comm.comm_linenum, msg);
				Positive (dummy_numeric_data))

	| (key, Bullet_kind, false, Unnamed_field v) ->
		(try Undecided (Bullet_data (Basic_input.bullet_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Bullet_kind, false, Keyvalue_field (k, v)) when k = key ->
		(try Positive (Bullet_data (Basic_input.bullet_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_bullet_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_bullet_data))

	| (key, Numbering_kind, false, Unnamed_field v) ->
		(try Undecided (Numbering_data (Basic_input.numbering_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Numbering_kind, false, Keyvalue_field (k, v)) when k = key ->
		(try Positive (Numbering_data (Basic_input.numbering_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_numbering_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_numbering_data))

	| (key, Floatation_kind, false, Unnamed_field v) ->
		(try Undecided (Floatation_data (Basic_input.floatation_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Floatation_kind, false, Keyvalue_field (k, v)) when k = key ->
		(try Positive (Floatation_data (Basic_input.floatation_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_floatation_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_floatation_data))

	| (key, Classname_kind, false, Unnamed_field v) ->
		if Basic_input.matches_ident v then Undecided (Classname_data v) else Negative
	| (key, Classname_kind, false, Keyvalue_field (k, v)) when k = key ->
		if Basic_input.matches_ident v
		then
			Positive (Classname_data v)
		else
			let msg = Error.Invalid_extra_classname_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_classname_data)

	| (key, Lang_kind, false, Unnamed_field v) ->
		if Camlhighlight_parser.is_available_lang v then Undecided (Lang_data v) else Negative
	| (key, Lang_kind, false, Keyvalue_field (k, v)) when k = key ->
		if Camlhighlight_parser.is_available_lang v
		then
			Positive (Lang_data v)
		else
			let msg = Error.Invalid_extra_lang_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_lang_data)

	| (key, Style_kind, false, Unnamed_field v) ->
		(try Undecided (Style_data (Source_input.style_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Style_kind, false, Keyvalue_field (k, v)) when k = key ->
		(try Positive (Style_data (Source_input.style_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_style_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_style_data))

	| (key, Cover_kind, false, Unnamed_field v) ->
		(try Undecided (Cover_data (Book_input.cover_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Cover_kind, false, Keyvalue_field (k, v)) when k = key ->
		(try Positive (Cover_data (Book_input.cover_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_cover_parameter (comm.comm_tag, key, v) in
			DynArray.add errors (Some comm.comm_linenum, msg);
			Positive (dummy_cover_data))

	| _ ->
		Negative


(**	Does the basic preprocessing on the raw data, preparing it for crunching
	by the subsequent functions.
*)
let prepare errors comm strs handles =
	let fields = fields_of_strings strs in
	let result_from_handle hnd =
		let (key, kind, auto) = id_of_handle hnd
		in Array.map (matcher errors comm key kind auto) fields
	in (fields, (List.map result_from_handle handles))


(**	Summarises the data processed so far, returning a triple containing
	a) an array of the assignments for each property handle, b) an array
	indicating whether each of the original fields has already been taken
	or not, and c) a map of possible but still undecided assignments.
*)
let summarise num_fields results =
	let num_rows = List.length results in
	let assigned = Array.make num_rows None
	and taken = Array.make num_fields false
	and undecided = ref Undecided.empty in
	let sum_result row result =
		let f col = function
			| Positive x ->
				(match (assigned.(row), taken.(col)) with
					| (None, false) ->
						assigned.(row) <- Some x;
						taken.(col) <- true;
						undecided := Undecided.clear_row row !undecided
					| (Some _, false) ->
						failwith "assigned"
					| (None, true) ->
						failwith "taken"
					| _ ->
						failwith "assigned and taken")
			| Undecided x ->
				(match assigned.(row) with
					| Some _ ->
						()
					| None ->
						undecided := Undecided.add (row, col) x !undecided)
			| Negative ->
				()
		in Array.iteri f result

	in	List.iteri sum_result results;
		(assigned, taken, !undecided)


(**	This function tries to assign all the fields that are still undecided.
	It is basically a poor-man's Prolog unification algorithm, complete
	with backtracking.  Note that all possible solutions are stored, so
	we can be sure the solution is unique.
*)
let solve assigned taken undecided =
	let solutions = DynArray.create () in
	let last_solution = ref None in
	let add_solution sol = match !last_solution with
		| Some x when x = sol ->
			()
		| _ ->
			last_solution := Some sol;
			DynArray.add solutions sol in
	let rec really_solve assigned taken undecided =
		let assign (row, col) value = match (assigned.(row), taken.(col)) with
			| (None, false) ->
				let new_assigned = Array.copy assigned
				and new_taken = Array.copy taken
				in	new_assigned.(row) <- Some value;
					new_taken.(col) <- true;
					really_solve new_assigned new_taken (Undecided.clear_row row undecided)
			| _ ->
				()
		in	if Undecided.is_empty undecided
			then add_solution (assigned, taken)
			else Undecided.iter assign undecided in
	let () =
		really_solve assigned taken undecided
	in match DynArray.to_list solutions with
		| [] ->
			No_solutions
		| [(x, y)] ->
			One_solution (x, y)
		| (x, y) :: tl ->
			if List.exists not (List.map ((=) (x, y)) tl)
			then Multiple_solutions
			else One_solution (x, y)


(**	High-level processing function.
*)
let process comm errors handles =
	let dummy = Array.make (List.length handles) None
	in match comm.comm_extra with
		| None ->
			dummy
		| Some extra ->
			let strs = Array.of_list (String.nsplit extra ",") in
			let (fields, results) = prepare errors comm strs handles in
			let (assigned, taken, undecided) = summarise (Array.length fields) results
			in match solve assigned taken undecided with
				| One_solution (assigned, taken) ->
					let any_untaken = ref false in
					let check col elem =
						if elem
						then	()
						else	(any_untaken := true;
							let msg = Error.Invalid_extra_unknown_parameter (comm.comm_tag, col, strs.(col))
							in DynArray.add errors (Some comm.comm_linenum, msg))
					in	Array.iteri check taken;
						assigned
				| No_solutions ->
					let msg = Error.Invalid_extra_no_solutions (comm.comm_tag, extra)
					in DynArray.add errors (Some comm.comm_linenum, msg);
					dummy
				| Multiple_solutions ->
					let msg = Error.Invalid_extra_multiple_solutions (comm.comm_tag, extra)
					in DynArray.add errors (Some comm.comm_linenum, msg);
					dummy


(********************************************************************************)
(**	{2 Parsing multiple handles}						*)
(********************************************************************************)

let parse comm errors handles =
	let extra = Hashtbl.create (List.length handles)
	and assigned = process comm errors handles in
	List.iteri (fun i el -> Hashtbl.add extra el assigned.(i)) handles;
	extra


let get_boolean ~default extra handle = match Hashtbl.find extra handle with
	| Some (Boolean_data x) -> x
	| _			-> default


let get_maybe_boolean ~default extra handle = match Hashtbl.find extra handle with
	| Some (Boolean_auto_data x) -> x
	| _			     -> default


let get_numeric ~default extra handle = match Hashtbl.find extra handle with
	| Some (Numeric_data x) -> x
	| _		        -> default


let get_maybe_numeric ~default extra handle = match Hashtbl.find extra handle with
	| Some (Numeric_auto_data x) -> x
	| _			     -> default


let get_bullet ~default extra handle = match Hashtbl.find extra handle with
	| Some (Bullet_data x) -> x
	| _		       -> default


let get_numbering ~default extra handle = match Hashtbl.find extra handle with
	| Some (Numbering_data x) -> x
	| _			  -> default


let get_floatation ~default extra handle = match Hashtbl.find extra handle with
	| Some (Floatation_data x) -> x
	| _			   -> default


let get_classname ~default extra handle = match Hashtbl.find extra handle with
	| Some (Classname_data x) -> Some x
	| _			  -> default


let get_lang ~default extra handle = match Hashtbl.find extra handle with
	| Some x when x = dummy_lang_data -> default
	| Some (Lang_data x)		  -> Some x
	| _				  -> default


let get_style ~default extra handle = match Hashtbl.find extra handle with
	| Some (Style_data x) -> x
	| _		      -> default


let get_cover ~default extra handle = match Hashtbl.find extra handle with
	| Some (Cover_data x) -> x
	| _		      -> default


(********************************************************************************)
(**	{2 Parsing a single handle}						*)
(********************************************************************************)

let fetcher getter ~default comm errors handle =
	let extra = parse comm errors [handle]
	in getter ~default extra handle

let fetch_boolean = fetcher get_boolean
let fetch_maybe_boolean = fetcher get_maybe_boolean
let fetch_numeric = fetcher get_numeric
let fetch_maybe_numeric = fetcher get_maybe_numeric
let fetch_bullet = fetcher get_bullet
let fetch_numbering = fetcher get_numbering
let fetch_floatation = fetcher get_floatation
let fetch_classname = fetcher get_classname
let fetch_lang = fetcher get_lang
let fetch_style = fetcher get_style
let fetch_cover = fetcher get_cover

