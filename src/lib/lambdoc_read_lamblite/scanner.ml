(********************************************************************************)
(*	Scanner.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Scanner for Lamblite reader.  We use Ulex for handling the UTF-8 parsing.
*)

open ExtString


(********************************************************************************)
(*	{2 Exceptions}								*)
(********************************************************************************)

exception Lone_terminator


(********************************************************************************)
(*	{2 Type definitions}							*)
(********************************************************************************)

type list_t =
	| Ulist
	| Olist

type text_t =
	| Plain of string
	| Bold_mark
	| Emph_mark
	| Code_mark
	| Begin_link
	| End_link
	| Link_sep

type line_t =
	| Begin_source of string
	| End_source
	| Begin_verbatim
	| End_verbatim
	| Raw of string
	| Section of int * text_t list
	| Par of int * (list_t * int) option * text_t list


(********************************************************************************)
(*	{2 Auxiliary private functions}						*)
(********************************************************************************)

let count_char str what =
	String.fold_left (fun accum c -> if c = what then accum+1 else accum) 0 str


(********************************************************************************)
(*	{2 Regular expressions}							*)
(********************************************************************************)

let regexp alpha = ['a' - 'z']
let regexp blank = [' ' '\t']
let regexp space = ' '
let regexp section_pat = '='+
let regexp quote_pat = '>' ('>' | blank)*
let regexp list_pat = '-'+ | '*'+ | '#'+


(********************************************************************************)
(*	{2 Scanners}								*)
(********************************************************************************)

let text_scanner lexbuf =
	let coalesce accum el = match accum with
		| (Plain x) :: tl	-> (Plain (x ^ el)) :: tl
		| _			-> (Plain el) :: accum in
	let rec main_scanner accum = lexer
		| eof	 -> accum
		| '\\' _ -> main_scanner (coalesce accum (Ulexing.utf8_sub_lexeme lexbuf 1 1)) lexbuf
		| "**"	 -> main_scanner (Bold_mark :: accum) lexbuf
		| "//"	 -> main_scanner (Emph_mark :: accum) lexbuf
		| "__"	 -> main_scanner (Code_mark :: accum) lexbuf
		| "[["	 -> link_scanner (Begin_link :: accum) lexbuf
		| "]]"	 -> main_scanner (End_link :: accum) lexbuf
		| "||"	 -> main_scanner (Link_sep :: accum) lexbuf
		| _	 -> main_scanner (coalesce accum (Ulexing.utf8_lexeme lexbuf)) lexbuf
	and link_scanner accum = lexer
		| eof	 -> accum
		| '\\' _ -> link_scanner (coalesce accum (Ulexing.utf8_sub_lexeme lexbuf 1 1)) lexbuf
		| "]]"	 -> main_scanner (End_link :: accum) lexbuf
		| "||"	 -> main_scanner (Link_sep :: accum) lexbuf
		| _	 -> link_scanner (coalesce accum (Ulexing.utf8_lexeme lexbuf)) lexbuf
	in List.rev (main_scanner [] lexbuf)


let literal_scanner term_token = lexer
	| "}}" blank* eof -> term_token
	| _+		  -> Raw (Ulexing.utf8_lexeme lexbuf)


let general_scanner = lexer
	| "{{#" alpha* blank* eof ->
		Begin_source (String.slice ~first:3 (String.strip (Ulexing.latin1_lexeme lexbuf)))
	| "{{" blank* eof ->
		Begin_verbatim
	| "}}" blank* eof ->
		raise Lone_terminator
	| section_pat space? ->
		let lexeme = Ulexing.utf8_lexeme lexbuf in
		let section_level = count_char lexeme '='
		in Section (section_level, text_scanner lexbuf)
	| quote_pat? blank* list_pat? blank* ->
		let lexeme = Ulexing.utf8_lexeme lexbuf in
		let quote_level = count_char lexeme '>'
		and list_level =
			let counter = count_char lexeme
			in match (counter '-', counter '*', counter '#') with
				| (x, 0, 0) when x > 0	-> Some (Ulist, x)
				| (0, x, 0) when x > 0	-> Some (Ulist, x)
				| (0, 0, x) when x > 0	-> Some (Olist, x)
				| _			-> None
		and text = text_scanner lexbuf
		in Par (quote_level, list_level, text)

