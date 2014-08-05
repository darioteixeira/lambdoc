(********************************************************************************)
(*	Style.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Basic
open Ast

module Array = BatArray
module List = BatList
module String = BatString


(********************************************************************************)
(**	{1 Private exceptions}							*)
(********************************************************************************)

exception Value_error of Error.error_msg_t


(********************************************************************************)
(**	{1 Private type definitions}						*)
(********************************************************************************)

type raw_t =
	| Unnamed of string
	| Named of string * string

type decl_t =
	| Classname_decl of Classname.t
	| Coversize_decl of Book.coversize_t
	| Lang_decl of Camlhighlight_core.lang_t option
	| Linenums_decl of bool
	| Width_decl of int option


(********************************************************************************)
(**	{1 Public type definitions}						*)
(********************************************************************************)

type _ handle_t =
	| Coversize_hnd: Book.coversize_t handle_t
	| Lang_hnd: Camlhighlight_core.lang_t option handle_t
	| Linenums_hnd: bool handle_t
	| Width_hnd: int option handle_t

type errors_t = (int option * Error.error_msg_t) BatDynArray.t

type parsing_t = (raw_t * decl_t) list


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

let coversize_of_string comm key = function
	| "small"  -> `Small
	| "medium" -> `Medium
	| "large"  -> `Large
	| x	   -> raise (Value_error (Error.Invalid_style_bad_coversize (comm.comm_tag, key, x)))


let lang_of_string comm key = function
	| "none" ->
		None
	| x ->
		if Camlhighlight_parser.is_available_lang x
		then Some x
		else raise (Value_error (Error.Invalid_style_bad_lang (comm.comm_tag, key, x)))


let boolean_of_string comm key = function
	| "true" | "on" | "yes"	 -> true
	| "false" | "off" | "no" -> false
	| x			 -> raise (Value_error (Error.Invalid_style_bad_boolean (comm.comm_tag, key, x)))


let numeric_of_string comm key ~low ~high = function
	| "none" ->
		None
	| x ->
		let exc = Value_error (Error.Invalid_style_bad_numeric (comm.comm_tag, key, x, low, high)) in
		let num =
			try int_of_string x
			with Failure "int_of_string" -> raise exc in
		if (num >= low) && (num <= high)
		then Some num
		else raise exc


let decl_dict =
	[
	("coversize", fun comm v -> Coversize_decl (coversize_of_string comm "coversize" v));
	("lang", fun comm v -> Lang_decl (lang_of_string comm "lang" v));
	("nums", fun comm v -> Linenums_decl (boolean_of_string comm "nums" v));
	("width", fun comm v -> Width_decl (numeric_of_string comm "width" ~low:1 ~high:100 v));
	]


let raws_of_string =
	let kv_rex = Pcre.regexp "^(?<key>[a-z]+)=(?<value>.+)$" in
	fun comm errors ->
		let conv str =
			try
				let _ = String.index str '=' in
				begin try
					let subs = Pcre.exec ~rex:kv_rex str in
					let key = Pcre.get_named_substring kv_rex "key" subs in
					let value = Pcre.get_named_substring kv_rex "value" subs in
					Some (Named (key, value))
				with Not_found ->
					let msg = Error.Invalid_style_bad_keyvalue (comm.comm_tag, str) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					None
				end
			with Not_found ->
				if Readconv.Identifier_input.matches_classname str
				then
					Some (Unnamed str)
				else
					let msg = Error.Invalid_style_bad_classname (comm.comm_tag, str) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					None in
		match comm.comm_style with
			| Some str -> String.nsplit str "," |> List.map String.strip |> List.filter_map conv
			| None	   -> []


let decl_of_raw comm errors = function
	| Unnamed str ->
		Some (Classname_decl str)
	| Named (key, value) ->
		try
			let maker = List.assoc key decl_dict in
			Some (maker comm value)
		with
			| Not_found ->
				let msg = Error.Invalid_style_unknown_keyvalue (comm.comm_tag, key, value) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				None
			| Value_error msg ->
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				None


let find_decl hnd parsing =
	let matches: type a. a handle_t -> raw_t * decl_t -> a option = fun hnd (_, decl) -> match (hnd, decl) with
		| (Coversize_hnd, Coversize_decl x) -> Some x
		| (Lang_hnd, Lang_decl x)	    -> Some x
		| (Linenums_hnd, Linenums_decl x)   -> Some x
		| (Width_hnd, Width_decl x)	    -> Some x
		| _				    -> None in
	let rec finder accum = function
		| [] ->
			(None, accum)
		| hd :: tl ->
			match matches hnd hd with
				| Some x -> (Some x, accum @ tl)
				| None	 -> finder (hd :: accum) tl in
	finder [] parsing


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let parse comm errors =
	let make_parsing raw = match decl_of_raw comm errors raw with
		| Some decl -> Some (raw, decl)
		| None	    -> None in
	let rec split attr_accum parsing_accum = function
		| [] ->
			(attr_accum, ref parsing_accum)
		| ((_, decl) as hd) :: tl ->
			match decl with
				| Classname_decl x -> split (x :: attr_accum) parsing_accum tl
				| _		   -> split attr_accum (hd :: parsing_accum) tl in
	raws_of_string comm errors |>
	List.filter_map make_parsing |>
	split [] []


let consume1 parsing_ref (hnd, default) =
	let (res, vs) = match find_decl hnd !parsing_ref with
		| (Some x, vs) -> (x, vs)
		| (None, vs)   -> (default, vs) in
	parsing_ref := vs;
	res


let consume2 parsing_ref (hnd1, default1) (hnd2, default2) =
	let (res1, vs) = match find_decl hnd1 !parsing_ref with
		| (Some x, vs) -> (x, vs)
		| (None, vs)   -> (default1, vs) in
	let (res2, vs) = match find_decl hnd2 vs with
		| (Some x, vs) -> (x, vs)
		| (None, vs)   -> (default2, vs) in
	parsing_ref := vs;
	(res1, res2)


let dispose comm errors parsing_ref = match !parsing_ref with
	| [] ->
		true
	| xs ->
		let aux (raw, _) = match raw with
			| Named (key, value) ->
				let msg = Error.Invalid_style_misplaced_keyvalue (comm.comm_tag, key, value) in
				BatDynArray.add errors (Some comm.comm_linenum, msg)
			| Unnamed _ ->
				assert false in
		List.iter aux xs;
		false

