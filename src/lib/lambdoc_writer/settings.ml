(********************************************************************************)
(*	Settings.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document settings.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type names_t =
	{
	part_name: Inline.seq_t;
	appendix_name: Inline.seq_t;
	section_name: Inline.seq_t;
	bibliography_name: Inline.seq_t;
	notes_name: Inline.seq_t;
	toc_name: Inline.seq_t;
	equation_name: Inline.seq_t;
	printout_name: Inline.seq_t;
	table_name: Inline.seq_t;
	figure_name: Inline.seq_t;
	}

type t =
	{
	default_bullet: Bullet.t;
	default_numbering: Numbering.t;
	names: names_t;
	image_lookup: Alias.t -> XHTML.M.uri;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make_names part_name appendix_name section_name bibliography_name notes_name toc_name equation_name printout_name table_name figure_name =
	{
	part_name = Inline.get_seq part_name;
	appendix_name = Inline.get_seq appendix_name; 
	section_name = Inline.get_seq section_name; 
	bibliography_name = Inline.get_seq bibliography_name; 
	notes_name = Inline.get_seq notes_name; 
	toc_name = Inline.get_seq toc_name; 
	equation_name = Inline.get_seq equation_name; 
	printout_name = Inline.get_seq printout_name; 
	table_name = Inline.get_seq table_name; 
	figure_name = Inline.get_seq figure_name; 
	}


let english_names = make_names
	(Inline.plain "Part", [])
	(Inline.plain "Appendix", [])
	(Inline.plain "Section", [])
	(Inline.plain "Bibliography", [])
	(Inline.plain "Notes", [])
	(Inline.plain "Table of Contents", [])
	(Inline.plain "Eq.", [])
	(Inline.plain "Print.", [])
	(Inline.plain "Tab.", [])
	(Inline.plain "Fig.", [])


let portuguese_names = make_names
	(Inline.plain "Parte", [])
	(Inline.plain "Apêndice", [])
	(Inline.plain "Secção", [])
	(Inline.plain "Bibliografia", [])
	(Inline.plain "Notas", [])
	(Inline.plain "Índice", [])
	(Inline.plain "Eq.", [])
	(Inline.plain "List.", [])
	(Inline.plain "Tab.", [])
	(Inline.plain "Fig.", [])


let make bullet numbering names image_lookup =
	{
	default_bullet = bullet;
	default_numbering = numbering;
	names = names;
	image_lookup = image_lookup;
	}


let default = make Bullet.Disc Numbering.Decimal english_names XHTML.M.uri_of_string

