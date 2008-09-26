(********************************************************************************)
(*	Debugger for the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf
open Document_ast.Ast
open Lambtex_parser


let maybe_sprintf opening closing = function
	| Some thing	-> sprintf "%c%s%c" opening thing closing
	| None		-> ""


let sprintf_command comm =
	let label = maybe_sprintf '[' ']' comm.comm_label
	and order = maybe_sprintf '(' ')' comm.comm_order
	and extra = maybe_sprintf '<' '>' comm.comm_extra
	and secondary = maybe_sprintf '{' '}' comm.comm_secondary
	in
	sprintf "%s %s %s %s #%d" label order extra secondary comm.comm_linenum


let sprintf_operator comm =
	sprintf "#%d" comm.op_linenum

	
let to_string = function
	| EOF op			-> sprintf "EOF: %s" (sprintf_operator op)
	| BEGIN op			-> sprintf "BEGIN: %s" (sprintf_operator op)
	| END op			-> sprintf "END: %s" (sprintf_operator op)
	| NEW_PARAGRAPH op		-> sprintf "NEW_PARAGRAPH: %s" (sprintf_operator op)
	| COLUMN_SEP op			-> sprintf "COLUMN_SEP: %s" (sprintf_operator op)
	| ROW_END op			-> sprintf "ROW_END: %s" (sprintf_operator op)

	| PLAIN txt			-> sprintf "PLAIN \"%s\"" txt
	| ENTITY txt			-> sprintf "ENTITY \"%s\"" txt

	| BEGIN_MATHTEX_INL op		-> sprintf "BEGIN_MATHTEX_INL: %s" (sprintf_operator op)
	| END_MATHTEX_INL op		-> sprintf "END_MATHTEX_INL: %s" (sprintf_operator op)
	| BEGIN_MATHML_INL op		-> sprintf "BEGIN_MATHML_INL: %s" (sprintf_operator op)
	| END_MATHML_INL op		-> sprintf "END_MATHML_INL: %s" (sprintf_operator op)

	| BEGIN_ABSTRACT comm		-> sprintf "BEGIN_ABSTRACT: %s" (sprintf_command comm)
	| END_ABSTRACT comm		-> sprintf "END_ABSTRACT: %s" (sprintf_command comm)
	| BEGIN_ITEMIZE comm		-> sprintf "BEGIN_ITEMIZE: %s" (sprintf_command comm)
	| END_ITEMIZE comm		-> sprintf "END_ITEMIZE: %s" (sprintf_command comm)
	| BEGIN_ENUMERATE comm		-> sprintf "BEGIN_ENUMERATE: %s" (sprintf_command comm)
	| END_ENUMERATE	comm		-> sprintf "END_ENUMERATE: %s" (sprintf_command comm)
	| BEGIN_QUOTE comm		-> sprintf "BEGIN_QUOTE: %s" (sprintf_command comm)
	| END_QUOTE comm		-> sprintf "END_QUOTE: %s" (sprintf_command comm)
	| BEGIN_MATHTEX_BLK comm	-> sprintf "BEGIN_MATHTEX_BLK: %s" (sprintf_command comm)
	| END_MATHTEX_BLK comm		-> sprintf "END_MATHTEX_BLK: %s" (sprintf_command comm)
	| BEGIN_MATHML_BLK comm		-> sprintf "BEGIN_MATHML_BLK: %s" (sprintf_command comm)
	| END_MATHML_BLK comm		-> sprintf "END_MATHML_BLK: %s" (sprintf_command comm)
	| BEGIN_CODE comm		-> sprintf "BEGIN_CODE: %s" (sprintf_command comm)
	| END_CODE comm			-> sprintf "END_CODE: %s" (sprintf_command comm)
	| BEGIN_VERBATIM comm		-> sprintf "BEGIN_VERBATIM: %s" (sprintf_command comm)
	| END_VERBATIM comm		-> sprintf "END_VERBATIM: %s" (sprintf_command comm)
	| BEGIN_TABULAR comm		-> sprintf "BEGIN_TABULAR: %s" (sprintf_command comm)
	| END_TABULAR comm		-> sprintf "END_TABULAR: %s" (sprintf_command comm)
	| BEGIN_SUBPAGE comm		-> sprintf "BEGIN_SUBPAGE: %s" (sprintf_command comm)
	| END_SUBPAGE comm		-> sprintf "END_SUBPAGE: %s" (sprintf_command comm)

	| BEGIN_EQUATION comm		-> sprintf "BEGIN_EQUATION: %s" (sprintf_command comm)
	| END_EQUATION comm		-> sprintf "END_EQUATION: %s" (sprintf_command comm)
	| BEGIN_ALGORITHM comm		-> sprintf "BEGIN_ALGORITHM: %s" (sprintf_command comm)
	| END_ALGORITHM comm		-> sprintf "END_ALGORITHM: %s" (sprintf_command comm)
	| BEGIN_TABLE comm		-> sprintf "BEGIN_TABLE: %s" (sprintf_command comm)
	| END_TABLE comm		-> sprintf "END_TABLE: %s" (sprintf_command comm)
	| BEGIN_FIGURE comm		-> sprintf "BEGIN_FIGURE: %s" (sprintf_command comm)
	| END_FIGURE comm		-> sprintf "END_FIGURE: %s" (sprintf_command comm)
	| BEGIN_BIB comm		-> sprintf "BEGIN_BIB: %s" (sprintf_command comm)
	| END_BIB comm			-> sprintf "END_BIB: %s" (sprintf_command comm)

	| BOLD comm			-> sprintf "BOLD: %s" (sprintf_command comm)
	| EMPH comm			-> sprintf "EMPH: %s" (sprintf_command comm)
	| MONO comm			-> sprintf "MONO: %s" (sprintf_command comm)
	| CAPS comm			-> sprintf "CAPS: %s" (sprintf_command comm)
	| THRU comm			-> sprintf "THRU: %s" (sprintf_command comm)
	| SUP comm			-> sprintf "SUP: %s" (sprintf_command comm)
	| SUB comm			-> sprintf "SUB: %s" (sprintf_command comm)
	| BOX comm			-> sprintf "BOX: %s" (sprintf_command comm)

	| LINK comm			-> sprintf "LINK: %s" (sprintf_command comm)
	| SEE comm			-> sprintf "SEE: %s" (sprintf_command comm)
	| CITE comm			-> sprintf "CITE: %s" (sprintf_command comm)
	| REF comm			-> sprintf "REF: %s" (sprintf_command comm)
	| SREF comm			-> sprintf "SREF: %s" (sprintf_command comm)
	| MREF comm			-> sprintf "MREF: %s" (sprintf_command comm)

	| SECTION comm			-> sprintf "SECTION: %s" (sprintf_command comm)
	| SUBSECTION comm		-> sprintf "SUBSECTION: %s" (sprintf_command comm)
	| SUBSUBSECTION comm		-> sprintf "SUBSUBSECTION: %s" (sprintf_command comm)
	| TOC comm			-> sprintf "TOC: %s" (sprintf_command comm)
	| BIBLIOGRAPHY comm		-> sprintf "BIBLIOGRAPHY: %s" (sprintf_command comm)
	| NOTES comm			-> sprintf "NOTES: %s" (sprintf_command comm)

	| TITLE comm			-> sprintf "TITLE: %s" (sprintf_command comm)
	| RULE comm			-> sprintf "RULE: %s" (sprintf_command comm)
	| APPENDIX comm			-> sprintf "APPENDIX: %s" (sprintf_command comm)
	| SETTING comm			-> sprintf "SETTING: %s" (sprintf_command comm)
	| NEW_ITEM comm			-> sprintf "NEW_ITEM: %s" (sprintf_command comm)
	| IMAGE comm			-> sprintf "IMAGE: %s" (sprintf_command comm)
	| CAPTION comm			-> sprintf "CAPTION: %s" (sprintf_command comm)
	| HEAD comm			-> sprintf "HEAD: %s" (sprintf_command comm)
	| FOOT comm			-> sprintf "FOOT: %s" (sprintf_command comm)
	| BODY comm			-> sprintf "BODY: %s" (sprintf_command comm)
	| BIB_TITLE comm		-> sprintf "BIB_TITLE: %s" (sprintf_command comm)
	| BIB_AUTHOR comm		-> sprintf "BIB_AUTHOR: %s" (sprintf_command comm)
	| BIB_RESOURCE comm		-> sprintf "BIB_RESOURCE: %s" (sprintf_command comm)
	| NOTE comm			-> sprintf "NOTE: %s" (sprintf_command comm)

