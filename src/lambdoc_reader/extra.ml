(********************************************************************************)
(*	Implementation file for Extra module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open ExtArray
open ExtList
open ExtString
open Lambdoc_core
open Error
open Ast


(********************************************************************************)
(**	{2 Module definitions}							*)
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
(**	{2 Type definitions}							*)
(********************************************************************************)

type handle_t =
	| Linenums_hnd
	| Zebra_hnd
	| Shadow_hnd
	| Width_hnd
	| Bullet_hnd
	| Numbering_hnd
	| Alignment_hnd
	| Lang_hnd
	| Classname_hnd


type property_kind_t =
	| Boolean_kind
	| Numeric_kind
	| Bullet_kind
	| Numbering_kind
	| Alignment_kind
	| Lang_kind
	| Classname_kind


type property_id_t = string * property_kind_t


type property_data_t =
	| Boolean_data of bool
	| Numeric_data of int 
	| Bullet_data of Bullet.t
	| Numbering_data of Numbering.t
	| Alignment_data of Alignment.t
	| Lang_data of Code.lang_t
	| Classname_data of string


type field_t =
	| Boolean_field of string option * bool
	| Unnamed_field of string
	| Keyvalue_field of string * string
	| Invalid_field of string


type result_t =
	| Positive of property_data_t
	| Undecided of property_data_t
	| Negative


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Solution_found of property_data_t option array * bool array


(********************************************************************************)
(**	{2 Private functions and values}					*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Low-level parsing functions}						*)
(********************************************************************************)

(**	Definition of the key and property kind associated with each handle.
*)
let id_of_handle = function
	| Linenums_hnd	-> ("linenums", Boolean_kind)
	| Zebra_hnd	-> ("zebra", Boolean_kind)
	| Shadow_hnd	-> ("shadow", Boolean_kind)
	| Width_hnd	-> ("width", Numeric_kind)
	| Bullet_hnd	-> ("bul", Bullet_kind)
	| Numbering_hnd	-> ("num", Numbering_kind)
	| Alignment_hnd	-> ("align", Alignment_kind)
	| Lang_hnd	-> ("lang", Lang_kind)
	| Classname_hnd	-> ("class", Classname_kind)


(**	This function does the low-level, regular-expression based parsing
	of each field.  At this point we don't care yet about the semantics
	or allowed meaning for each field.  All we care are the various
	ways that users can use to express a property.
*)
let fields_of_strings =
	let truth_rex = Pcre.regexp "^((?<key>[a-z]+)=)?((?<true>(true)|(yes))|(?<false>(false)|(no)))$"
	and negated_rex = Pcre.regexp "^!(?<negated>[a-z]+)$"
	and unnamed_rex = Pcre.regexp "^(?<unnamed>[a-z0-9]+)$"
	and keyvalue_rex = Pcre.regexp "^(?<key>[a-z]+)=(?<value>[a-z0-9]+)$"
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
				let subs = Pcre.exec ~rex:unnamed_rex str
				in Unnamed_field (Pcre.get_named_substring unnamed_rex "unnamed" subs)
			with
				Not_found ->

			try
				let subs = Pcre.exec ~rex:keyvalue_rex str in
				let key = Pcre.get_named_substring keyvalue_rex "key" subs
				and value = Pcre.get_named_substring keyvalue_rex "value" subs
				in Keyvalue_field (key, value)
			with
				Not_found -> Invalid_field str

		in Array.map field_of_string strs


(**	This function matches a property (defined by a key and a property kind)
	with a field parsed by {!fields_of_strings}.  It returns the associated
	data wrapped inside a {!result_t}.  That way, other functions can tell
	the degree of certainty associated with a matching.
*)
let matcher ~classnames errors comm key kind field = match (key, kind, field) with

	| (key, Boolean_kind, Boolean_field (Some k, v)) when key = k ->
		Positive (Boolean_data v)
	| (key, Boolean_kind, Boolean_field (None, v)) ->
		Undecided (Boolean_data v)
	| (key, Boolean_kind, Unnamed_field k) when key = k ->
		Positive (Boolean_data true)
	| (key, Boolean_kind, Keyvalue_field (k, v)) when key = k ->
		let msg = Error.Invalid_extra_boolean_parameter (comm.comm_tag, key, v)
		in	DynArray.add errors (comm.comm_linenum, msg);
			Negative

	| (key, Numeric_kind, Unnamed_field v) ->
		(try Undecided (Numeric_data (int_of_string v)) with Failure _ -> Negative)
	| (key, Numeric_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Numeric_data (int_of_string v))
		with Failure _ ->
			let msg = Error.Invalid_extra_numeric_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative)

	| (key, Bullet_kind, Unnamed_field v) ->
		(try Undecided (Bullet_data (Bullet.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Bullet_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Bullet_data (Bullet.of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_bullet_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative)

	| (key, Numbering_kind, Unnamed_field v) ->
		(try Undecided (Numbering_data (Numbering.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Numbering_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Numbering_data (Numbering.of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_numbering_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative)

	| (key, Alignment_kind, Unnamed_field v) ->
		(try Undecided (Alignment_data (Alignment.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Alignment_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Alignment_data (Alignment.of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_alignment_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative)

	| (key, Lang_kind, Unnamed_field v) ->
		(try Undecided (Lang_data (Code.lang_of_string v)) with Invalid_argument _ -> Negative)
	| (key, Lang_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Lang_data (Code.lang_of_string v))
		with Invalid_argument _ ->
			let msg = Error.Invalid_extra_lang_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative)

	| (key, Classname_kind, Unnamed_field v) ->
		if List.mem v classnames
		then Undecided (Classname_data v)
		else Negative
	| (key, Classname_kind, Keyvalue_field (k, v)) when key = k ->
		if List.mem v classnames
		then Positive (Classname_data v)
		else
			let msg = Error.Invalid_extra_classname_parameter (comm.comm_tag, key, v)
			in	DynArray.add errors (comm.comm_linenum, msg);
				Negative

	| _ ->
		Negative


(**	Does the basic preprocessing on the raw data, preparing it for crunching
	by the subsequent functions.
*)
let prepare ~classnames errors comm strs handles =
	let fields = fields_of_strings strs in
	let result_from_handle hnd =
		let (key, kind) = id_of_handle hnd
		in Array.map (matcher ~classnames errors comm key kind) fields
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
	with backtracking.
*)
let solve assigned taken undecided =
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
		in	if Undecided.is_empty undecided then raise (Solution_found (assigned, taken));
			Undecided.iter assign undecided
	in try
		really_solve assigned taken undecided; None
	with
		Solution_found (assigned, taken) -> Some (assigned, taken)


(**	High-level processing function.
*)
let process ?(classnames = []) errors comm handles =
	match comm.comm_extra with
		| Some extra ->
			let strs = Array.of_list (String.nsplit extra ",") in
			let (fields, results) = prepare ~classnames errors comm strs handles in
			let (assigned, taken, undecided) = summarise (Array.length fields) results
			in (match solve assigned taken undecided with
				| Some (assigned, taken) ->
					let any_untaken = ref false in
					let check col elem =
						if elem
						then	()
						else	(any_untaken := true;
							let msg = Error.Invalid_extra_unknown_parameter (comm.comm_tag, col, strs.(col))
							in DynArray.add errors (comm.comm_linenum, msg))
					in	Array.iteri check taken;
						assigned
				| None ->
					failwith "Solution not found")
		| None ->
			Array.make (List.length handles) None



(********************************************************************************)
(**	{3 Wrappers}								*)
(********************************************************************************)

let get_alignment = function
	| Some (Alignment_data x)	-> x
	| _				-> Alignment.Center


let parse_floater errors comm =
	let assigned = process errors comm [Alignment_hnd]
	in get_alignment assigned.(0)


let parse_for_image errors comm =
	let assigned = process errors comm [Alignment_hnd; Shadow_hnd; Width_hnd] in
	let alignment = match assigned.(0) with
		| Some (Alignment_data x)	-> x
		| _				-> Alignment.Center
	and shadow = match assigned.(1) with
		| Some (Boolean_data x)	-> x
		| _			-> false
	and width = match assigned.(2) with
		| Some (Numeric_data w)	-> Some w
		| _			-> None
	in (alignment, shadow, width)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let parse_for_itemize errors comm =
	let assigned = process errors comm [Bullet_hnd]
	in match assigned.(0) with
		| Some (Bullet_data x)		-> x
		| _				-> Bullet.Disc


let parse_for_enumerate errors comm =
	let assigned = process errors comm [Numbering_hnd]
	in match assigned.(0) with
		| Some (Numbering_data x)	-> x
		| _				-> Numbering.Decimal

let parse_for_quote = parse_floater

let parse_for_callout ?classnames errors comm =
	let assigned = process ?classnames errors comm [Alignment_hnd; Classname_hnd] in
	let alignment = get_alignment assigned.(0)
	and classname = match assigned.(1) with
		| Some (Classname_data x)	-> Some x
		| _				-> None
	in (alignment, classname)

let parse_for_mathtex = parse_floater

let parse_for_mathml = parse_floater

let parse_for_code errors comm =
	let assigned = process errors comm [Lang_hnd; Linenums_hnd; Zebra_hnd; Alignment_hnd] in
	let lang = match assigned.(0) with
		| Some (Lang_data x)		-> Some x
		| _				-> None in
	let linenums = match (lang, assigned.(1)) with
		| (_, Some (Boolean_data x))	-> x
		| (Some _, _)			-> true
		| (None, _)			-> false in
	let zebra = match (lang, assigned.(2)) with
		| (_, Some (Boolean_data x))	-> x
		| _				-> true in
	let alignment = get_alignment assigned.(3)
	in (alignment, linenums, zebra, lang)

let parse_for_tabular = parse_floater

let parse_for_verbatim = parse_floater

let parse_for_bitmap = parse_for_image

let parse_for_subpage = parse_floater

