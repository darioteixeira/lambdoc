(********************************************************************************)
(*	Tokenizer.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Tokenizer for the Lamblite reader.
*)

open Lexing
open ExtString
open Lambdoc_reader.Ast
open Scanner
open Parser


(********************************************************************************)
(*	{2 Type definitions}							*)
(********************************************************************************)

type context_t =
	| General
	| Source
	| Verbatim


(********************************************************************************)
(*	{2 Tokenizer class}							*)
(********************************************************************************)

class tokenizer str =
let lines = Array.of_list (String.nsplit str "\n")
in object (self)

	val lines = lines
	val mutable line_counter = 0
	val mutable quote_state = 0
	val mutable list_state = []
	val mutable par_state = false
	val mutable productions = []
	val mutable context = General


	method private op =
		{
		comm_tag = None;
		comm_label = None;
		comm_order = None;
		comm_extra = None;
		comm_linenum = line_counter + 1;
		}


	method private comm ?(tag = None) ?(label = None) ?(order = None) ?(extra = None) () =
		{
		comm_tag = tag;
		comm_label = label;
		comm_order = order;
		comm_extra = extra;
		comm_linenum = line_counter + 1;
		}


	method private store token =
		productions <- match (productions, token) with
			| ([PLAIN (op, txt1)], PLAIN (_, txt2))	-> [PLAIN (op, txt1 ^ " " ^ txt2)]
			| ([RAW txt1], RAW txt2)		-> [RAW (txt1 ^ "\n" ^ txt2)]
			| _					-> productions @ [token]


	method private tokens_of_text text_list =
		let conv = function
			| Plain x	-> PLAIN (self#op, x)
			| Bold_mark	-> BOLD_MARK self#op
			| Emph_mark	-> EMPH_MARK self#op
			| Code_mark	-> CODE_MARK self#op
			| Begin_link	-> BEGIN_LINK self#op
			| End_link	-> END_LINK self#op
			| Link_sep	-> LINK_SEP self#op
		in List.map conv text_list


	method private token_of_list = function
		| Ulist, true	-> BEGIN_ITEMIZE self#op
		| Ulist, false	-> END_ITEMIZE self#op
		| Olist, true	-> BEGIN_ENUMERATE self#op
		| Olist, false	-> END_ENUMERATE self#op


	method private unwind_list new_level =
		let rec trim_list c = match (c, list_state) with
			| x, kind :: tl when x > 0 ->
				begin
					self#store (self#token_of_list (kind, false));
					list_state <- tl;
					trim_list (c - 1)
				end
			| _ ->
				()
		in trim_list ((List.length list_state) - new_level)


	method private unset_par =
		if par_state
		then self#store (END_PAR self#op); par_state <- false


	method ajust_quote target =
		let rec aux = function
			| x when x > 0	-> self#store (BEGIN_QUOTE self#op); aux (x - 1)
			| x when x < 0	-> self#store (END_QUOTE self#op); aux (x + 1)
			| _		-> ()
		in aux (target - quote_state)


	method private handle_par quote_new list_new text =

		if quote_new <> quote_state
		then begin
			self#unset_par;
			self#unwind_list 0;
			self#ajust_quote quote_new;
			quote_state <- quote_new
		end;

		let () = match (list_state, list_new) with

			| x, Some (kind, level) when level - (List.length x) = 1 ->
				begin
					self#unset_par;
					self#store (self#token_of_list (kind, true));
					self#store (ITEM self#op);
					list_state <- kind :: list_state
				end

			| x, Some (kind, level) when level <= List.length x ->
				begin
					self#unset_par;
					self#unwind_list level;
					match list_state with
						| top::tl when top = kind ->
							self#store (ITEM self#op);
						| _ ->
							begin
								self#unwind_list (level - 1);
								self#store (self#token_of_list (kind, true));
								self#store (ITEM self#op);
								list_state <- kind :: list_state
							end
				end

			| hd::tl, None when not par_state && (List.length text) > 0 ->
				self#unwind_list 0
				
			| _ ->
				() in
				

		let tokens = self#tokens_of_text text in
		let () = match (par_state, tokens) with
			| false, hd::tl	-> List.iter self#store ((BEGIN_PAR self#op) :: tokens)
			| true, hd::tl	-> List.iter self#store tokens
			| true, []	-> self#store (END_PAR self#op)
			| _		-> ()
		in par_state <- text <> [] 


	method private produce =
		let () =
			if line_counter >= Array.length lines
			then
				productions <- productions @ [EOF]
			else
				let scanner = match context with
					| General	-> Scanner.general_scanner
					| Source	-> Scanner.literal_scanner End_source
					| Verbatim	-> Scanner.literal_scanner End_verbatim in
				let lexbuf = Ulexing.from_utf8_string lines.(line_counter) in
				let tok = scanner lexbuf
				in match tok with
					| Begin_source lang ->
						context <- Source;
						let extra = if String.length lang = 0 then None else Some ("lang=" ^ lang)
						in self#store (BEGIN_SOURCE (self#comm ~tag:(Some "{{#") ~extra ()))
					| End_source ->
						context <- General;
						self#store (END_SOURCE self#op)
					| Begin_verbatim ->
						context <- Verbatim;
						self#store (BEGIN_VERBATIM self#op)
					| End_verbatim ->
						context <- General;
						self#store (END_VERBATIM self#op)
					| Heading text ->
						self#store (BEGIN_PARHEAD self#op);
						List.iter self#store (self#tokens_of_text text);
						self#store (END_PARHEAD self#op)
					| Par (quote_new, list_new, text) ->
						self#handle_par quote_new list_new text
					| Raw txt ->
						self#store (RAW txt)

		in line_counter <- line_counter + 1

	method consume = match productions with
		| []
		| [PLAIN (_, _)]
		| [RAW _]	-> self#produce; self#consume
		| hd :: tl	-> productions <- tl; hd

	method position =
		{
		pos_fname = "";
		pos_lnum = line_counter + 1;
		pos_bol = 0;
		pos_cnum = 0;
		}
end

