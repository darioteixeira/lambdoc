(********************************************************************************)
(*	Tokenizer.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Tokenizer for the Lamblite reader.
*)

open ExtString
open Lexing
open Parser

class tokenizer str =

let lines = Array.of_list (String.nsplit str "\n")
in object (self)

	val lines = lines
	val mutable cur_line = 0
	val mutable quote_state = None
	val mutable list_state = None
	val mutable par_state = false
	val mutable productions = []

	method private store token =
		Printf.eprintf "store\n";
		productions <- match (productions, token) with
			| ([PLAIN (op1, txt1)], PLAIN (op2, txt2))	-> [PLAIN (op1, txt1 ^ txt2)]
			| ([RAW txt1], RAW txt2)			-> [RAW (txt1 ^ txt2)]
			| _						-> productions @ [token]

	method produce =
		Printf.eprintf "produce\n";
		let () = match cur_line with
			| x when x = Array.length lines	-> self#store EOF
			| _				-> self#store (RAW ("line " ^ (string_of_int cur_line)))
		in cur_line <- cur_line + 1

	method consume =
		Printf.eprintf "consume\n";
		match productions with
		| []
		| [PLAIN (_, _)]
		| [RAW _]	-> self#produce; self#consume
		| hd :: tl	-> productions <- tl; hd

	method position =
		Printf.eprintf "position\n";
		{
		pos_fname = "";
		pos_lnum = cur_line + 1;
		pos_bol = 0;
		pos_cnum = 0;
		}
end

