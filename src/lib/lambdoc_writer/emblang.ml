(********************************************************************************)
(*	Emblang.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Emblang is a very small DSL whose purpose is to simplify the generation
	of Lambdoc_core values for internal use of the library itself.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type token_t =
	| Code
	| Plain of string


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

let tokenize lexbuf =
	let rec aggregate x1 = function
		| (Plain x2) :: tl -> (Plain (x2 ^ x1)) :: tl
		| xs		   -> (Plain x1) :: xs
	and tokenize_aux accum = lexer
		| '\\' _	-> tokenize_aux (aggregate (Ulexing.utf8_sub_lexeme lexbuf 1 1) accum) lexbuf
		| '#'		-> tokenize_aux (Code :: accum) lexbuf
		| [^ '#' '\\']+	-> tokenize_aux (aggregate (Ulexing.utf8_lexeme lexbuf) accum) lexbuf
		| eof		-> accum
	in List.rev (tokenize_aux [] lexbuf)


let rec process = parser
	| [< 'Plain s; rest >] -> (Inline.plain s) :: (process rest)
	| [< 'Code; 'Plain s; 'Code; rest >] -> (Inline.code ((Inline.plain s), [])) :: (process rest)
	| [< >] -> []


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let convert expl =
	let tokens = tokenize (Ulexing.from_utf8_string expl) in
	let stream = Stream.of_list tokens in
	match process stream with
		| []	 -> failwith "Emblang.convert"
		| hd::tl -> (hd, tl)

