(********************************************************************************)
(*	Implementation file for Document_settings.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document settings.
*)

open Document_basic
open Document_block


(********************************************************************************)
(*	{2 Settings module}							*)
(********************************************************************************)

module Settings =
struct
	type names_t =
		{
		section_name: plain_t;
		appendix_name: plain_t;
		equation_name: plain_t;
		algorithm_name: plain_t;
		table_name: plain_t;
		figure_name: plain_t;
		bibliography_name: plain_t;
		notes_name: plain_t;
		toc_name: plain_t;
		}

	type t =
		{
		default_bullet: Bullet.t;
		default_numbering: Numbering.t;
		names: names_t;
		}

	let english_names =
		{
		section_name = "Section";
		appendix_name = "Appendix";
		equation_name = "Eq.";
		algorithm_name = "Alg.";
		table_name = "Tab.";
		figure_name = "Fig.";
		bibliography_name = "Bibliography";
		notes_name = "Notes";
		toc_name = "Table of Contents";
		}

	let portuguese_names =
		{
		section_name = "Secção";
		appendix_name = "Apêndice";
		equation_name = "Eq.";
		algorithm_name = "Alg.";
		table_name = "Tab.";
		figure_name = "Fig.";
		bibliography_name = "Bibliografia";
		notes_name = "Notas";
		toc_name = "Índice";
		}

	let make bullet numbering names =
		{
		default_bullet = bullet;
		default_numbering = numbering;
                names = english_names;
		}

	let default = make Bullet.Default Numbering.Default english_names
end

