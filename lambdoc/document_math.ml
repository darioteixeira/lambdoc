(********************************************************************************)
(*	Definition of the module that handles document math.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(*	{2 Math module}								*)
(********************************************************************************)

module Math:
sig
	exception Invalid_mathtex
	exception Invalid_mathml

	type native_t = string with sexp

	type t =
		| Mathtex of mathtex_t * native_t
		| Mathml of mathml_t * native_t
		with sexp

	val from_mathtex: mathtex_t -> t
	val from_mathml: mathml_t -> t
	val get_native: t -> native_t
end =
struct
	exception Invalid_mathtex
	exception Invalid_mathml

	type native_t = string with sexp

	type t =
		| Mathtex of mathtex_t * native_t
		| Mathml of mathml_t * native_t
		with sexp

	let from_mathtex txt =
		let conversion = "<math><mi>x</mi><mo>=</mo><mi>y</mi><mo>+</mo><mn>2</mn></math>"
		in Mathtex (txt, conversion)

	let from_mathml txt =
		let conversion = "<math><mi>x</mi><mo>=</mo><mi>y</mi><mo>+</mo><mn>3</mn></math>"
		in Mathml (txt, conversion)

	let get_native = function
		| Mathtex (_, native)	-> native
		| Mathml (_, native)	-> native
end

