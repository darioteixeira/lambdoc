(********************************************************************************)
(*	Debugger for the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf
open Document_ast
open Lambtex_parser

let maybe_sprintf opening closing = function
	| Some thing	-> sprintf "%c%s%c" opening thing closing
	| None		-> ""

let sprintf_params params =
	let label = maybe_sprintf '[' ']' params.comm_label
	and order = maybe_sprintf '(' ')' params.comm_order
	and extra = maybe_sprintf '<' '>' params.comm_extra
	and secondary = maybe_sprintf '{' '}' params.comm_secondary
	in
	sprintf "%s %s %s %s #%d" label order extra secondary params.comm_linenum
	
let to_string = function
	| NEW_PARAGRAPH _		-> sprintf "NEW_PARAGRAPH"
	| COLUMN_SEP _			-> sprintf "COLUMN_SEP"
	| ROW_END _			-> sprintf "ROW_END"
	| BEGIN _			-> sprintf "BEGIN"
	| END _				-> sprintf "END"
	| EOF _				-> sprintf "EOF"

	| PLAIN txt			-> sprintf "PLAIN \"%s\"" txt
	| ENTITY txt			-> sprintf "ENTITY \"%s\"" txt

	| BEGIN_ITEMIZE params		-> sprintf "BEGIN_ITEMIZE: %s" (sprintf_params params)
	| END_ITEMIZE params		-> sprintf "END_ITEMIZE: %s" (sprintf_params params)
	| BEGIN_ENUMERATE params	-> sprintf "BEGIN_ENUMERATE: %s" (sprintf_params params)
	| END_ENUMERATE	params		-> sprintf "END_ENUMERATE: %s" (sprintf_params params)
	| BEGIN_QUOTE params		-> sprintf "BEGIN_QUOTE: %s" (sprintf_params params)
	| END_QUOTE params		-> sprintf "END_QUOTE: %s" (sprintf_params params)
	| BEGIN_ALGORITHM params	-> sprintf "BEGIN_ALGORITHM: %s" (sprintf_params params)
	| END_ALGORITHM params		-> sprintf "END_ALGORITHM: %s" (sprintf_params params)
	| BEGIN_EQUATION params		-> sprintf "BEGIN_EQUATION: %s" (sprintf_params params)
	| END_EQUATION params		-> sprintf "END_EQUATION: %s" (sprintf_params params)
	| BEGIN_FIGURE params		-> sprintf "BEGIN_FIGURE: %s" (sprintf_params params)
	| END_FIGURE params		-> sprintf "END_FIGURE: %s" (sprintf_params params)
	| BEGIN_TABLE params		-> sprintf "BEGIN_TABLE: %s" (sprintf_params params)
	| END_TABLE params		-> sprintf "END_TABLE: %s" (sprintf_params params)
	| BEGIN_BIB params		-> sprintf "BEGIN_BIB: %s" (sprintf_params params)
	| END_BIB params		-> sprintf "END_BIB: %s" (sprintf_params params)
	| BEGIN_VERBATIM params		-> sprintf "BEGIN_VERBATIM: %s" (sprintf_params params)
	| END_VERBATIM params		-> sprintf "END_VERBATIM: %s" (sprintf_params params)
	| BEGIN_MATH params		-> sprintf "BEGIN_MATH: %s" (sprintf_params params)
	| END_MATH params		-> sprintf "END_MATH: %s" (sprintf_params params)
	| BEGIN_SUBPAGE params		-> sprintf "BEGIN_SUBPAGE: %s" (sprintf_params params)
	| END_SUBPAGE params		-> sprintf "END_SUBPAGE: %s" (sprintf_params params)
	| BEGIN_TABULAR params		-> sprintf "BEGIN_TABULAR: %s" (sprintf_params params)
	| END_TABULAR params		-> sprintf "END_TABULAR: %s" (sprintf_params params)

	| BEGIN_MATHTEX _		-> sprintf "BEGIN_MATHTEX"
	| END_MATHTEX _			-> sprintf "END_MATHTEX"
	| BEGIN_MATHML _		-> sprintf "BEGIN_MATHML"
	| END_MATHML _			-> sprintf "END_MATHML"

	| SECTION params		-> sprintf "SECTION: %s" (sprintf_params params)
	| SUBSECTION params		-> sprintf "SUBSECTION: %s" (sprintf_params params)
	| SUBSUBSECTION params		-> sprintf "SUBSUBSECTION: %s" (sprintf_params params)
	| TOC params			-> sprintf "TOC: %s" (sprintf_params params)
	| BIBLIOGRAPHY params		-> sprintf "BIBLIOGRAPHY: %s" (sprintf_params params)
	| NOTES params			-> sprintf "NOTES: %s" (sprintf_params params)

	| BOLD params			-> sprintf "BOLD: %s" (sprintf_params params)
	| EMPH params			-> sprintf "EMPH: %s" (sprintf_params params)
	| MONO params			-> sprintf "MONO: %s" (sprintf_params params)
	| CAPS params			-> sprintf "CAPS: %s" (sprintf_params params)
	| THRU params			-> sprintf "THRU: %s" (sprintf_params params)
	| SUP params			-> sprintf "SUP: %s" (sprintf_params params)
	| SUB params			-> sprintf "SUB: %s" (sprintf_params params)
	| BOX params			-> sprintf "BOX: %s" (sprintf_params params)

	| LINK params			-> sprintf "LINK: %s" (sprintf_params params)
	| SEE params			-> sprintf "SEE: %s" (sprintf_params params)
	| CITE params			-> sprintf "CITE: %s" (sprintf_params params)
	| REF params			-> sprintf "REF: %s" (sprintf_params params)
	| SREF params			-> sprintf "SREF: %s" (sprintf_params params)
	| MREF params			-> sprintf "MREF: %s" (sprintf_params params)

	| APPENDIX params		-> sprintf "APPENDIX: %s" (sprintf_params params)
	| RULE params			-> sprintf "RULE: %s" (sprintf_params params)
	| SETTING params		-> sprintf "SETTING: %s" (sprintf_params params)
	| CAPTION params		-> sprintf "CAPTION: %s" (sprintf_params params)
	| LOAD params			-> sprintf "LOAD: %s" (sprintf_params params)
	| HEAD params			-> sprintf "HEAD: %s" (sprintf_params params)
	| BODY params			-> sprintf "BODY: %s" (sprintf_params params)
	| FOOT params			-> sprintf "FOOT: %s" (sprintf_params params)
	| AUTHOR params			-> sprintf "AUTHOR: %s" (sprintf_params params)
	| TITLE params			-> sprintf "TITLE: %s" (sprintf_params params)
	| RESOURCE params		-> sprintf "RESOURCE: %s" (sprintf_params params)
	| NOTE params			-> sprintf "NOTE: %s" (sprintf_params params)
	| NEW_ITEM params		-> sprintf "NEW_ITEM: %s" (sprintf_params params)

