(********************************************************************************)
(*	Implementation file for Extra module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the extra parameters.
*)

open ExtList
open ExtString
open Lambdoc_core


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
	| Bullet_hnd
	| Numbering_hnd
	| Alignment_hnd
	| Width_hnd
	| Height_hnd
	| Linenums_hnd
	| Zebra_hnd


type property_kind_t =
	| Boolean_kind
	| Numeric_kind
	| Bullet_kind
	| Numbering_kind
	| Alignment_kind


type property_id_t = string * property_kind_t


type property_data_t =
	| Boolean_data of bool
	| Numeric_data of int 
	| Bullet_data of Bullet.t
	| Numbering_data of Numbering.t
	| Alignment_data of Alignment.t


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
(**	{2 Definition of functions and values}					*)
(********************************************************************************)

let id_of_handle = function
	| Bullet_hnd	-> ("bul", Bullet_kind)
	| Numbering_hnd	-> ("num", Numbering_kind)
	| Alignment_hnd	-> ("align", Alignment_kind)
	| Width_hnd	-> ("w", Numeric_kind)
	| Height_hnd	-> ("h", Numeric_kind)
	| Linenums_hnd	-> ("linenums", Boolean_kind)
	| Zebra_hnd	-> ("zebra", Boolean_kind)


let fields_from_string =
	let truth_rex = Pcre.regexp "^((?<key>[a-z]+)=)?((?<true>(true)|(yes))|(?<false>(false)|(no)))$"
	and negated_rex = Pcre.regexp "^!(?<negated>[a-z]+)$"
	and unnamed_rex = Pcre.regexp "^(?<unnamed>[a-z0-9]+)$"
	and keyvalue_rex = Pcre.regexp "^(?<key>[a-z]+)=(?<value>[a-z0-9]+)$"
	in fun str ->
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

		in List.map field_of_string (String.nsplit str ",")


let matcher key kind field = match (key, kind, field) with

	| (key, Boolean_kind, Boolean_field (Some k, v)) when key = k ->
		Positive (Boolean_data v)
	| (key, Boolean_kind, Boolean_field (None, v)) ->
		Undecided (Boolean_data v)
	| (key, Boolean_kind, Unnamed_field k) when key = k ->
		Positive (Boolean_data true)
	| (key, Boolean_kind, Keyvalue_field (k, v)) when key = k ->
		failwith "oops"
	| (key, Numeric_kind, Unnamed_field v) ->
		(try Undecided (Numeric_data (int_of_string v)) with Failure _ -> Negative)
	| (key, Numeric_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Numeric_data (int_of_string v)) with Failure _ -> failwith "oops")
	| (key, Bullet_kind, Unnamed_field v) ->
		(try Undecided (Bullet_data (Bullet.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Bullet_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Bullet_data (Bullet.of_string v)) with Invalid_argument _ -> failwith "oops")
	| (key, Numbering_kind, Unnamed_field v) ->
		(try Undecided (Numbering_data (Numbering.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Numbering_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Numbering_data (Numbering.of_string v)) with Invalid_argument _ -> failwith "oops")
	| (key, Alignment_kind, Unnamed_field v) ->
		(try Undecided (Alignment_data (Alignment.of_string v)) with Invalid_argument _ -> Negative)
	| (key, Alignment_kind, Keyvalue_field (k, v)) when key = k ->
		(try Positive (Alignment_data (Alignment.of_string v)) with Invalid_argument _ -> failwith "oops")
	| _ ->
		Negative


let prepare str handles =
	let fields = fields_from_string str in
	let result_from_handle hnd =
		let (key, kind) = id_of_handle hnd
		in List.map (matcher key kind) fields
	in ((List.length fields), (List.map result_from_handle handles))


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
		in List.iteri f result

	in	List.iteri sum_result results;
		(assigned, taken, !undecided)


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


let process str handles =
	let (num_fields, results) = prepare str handles in
	let (assigned, taken, undecided) = summarise num_fields results
	in solve assigned taken undecided

