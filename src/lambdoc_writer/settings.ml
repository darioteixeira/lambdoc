(********************************************************************************)
(*	Implementation file for Settings module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document settings.
*)

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type names_t =
	{
	part_name: plain_t;
	appendix_name: plain_t;
	section_name: plain_t;
	bibliography_name: plain_t;
	notes_name: plain_t;
	toc_name: plain_t;
	equation_name: plain_t;
	algorithm_name: plain_t;
	table_name: plain_t;
	figure_name: plain_t;
	}

type t =
	{
	default_bullet: Bullet.t;
	default_numbering: Numbering.t;
	names: names_t;
	}


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let english_names =
	{
	part_name = "Part";
	appendix_name = "Appendix";
	section_name = "Section";
	bibliography_name = "Bibliography";
	notes_name = "Notes";
	toc_name = "Table of Contents";
	equation_name = "Eq.";
	algorithm_name = "Alg.";
	table_name = "Tab.";
	figure_name = "Fig.";
	}

let portuguese_names =
	{
	part_name = "Parte";
	appendix_name = "Apêndice";
	section_name = "Secção";
	bibliography_name = "Bibliografia";
	notes_name = "Notas";
	toc_name = "Índice";
	equation_name = "Eq.";
	algorithm_name = "Alg.";
	table_name = "Tab.";
	figure_name = "Fig.";
	}

let make bullet numbering names =
	{
	default_bullet = bullet;
	default_numbering = numbering;
	names = names;
	}

let default = make Bullet.Default Numbering.Default english_names

