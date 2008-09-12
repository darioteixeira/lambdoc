(********************************************************************************)
(**	Definition document settings.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic
open Document_block


(********************************************************************************)
(*	{2 Settings module}							*)
(********************************************************************************)

module Settings:
sig
	type names_t =
		{
		lc_section_name: plain_t;
		lc_appendix_name: plain_t;
		lc_algorithm_name: plain_t;
		lc_equation_name: plain_t;
		lc_figure_name: plain_t;
		lc_table_name: plain_t;
		lc_bibliography_name: plain_t;
		lc_notes_name: plain_t;
		lc_toc_name: plain_t;
		} with sexp

	type locale_t = Locale_en | Locale_pt with sexp

	type t =
		{
		section_name: plain_t option;
		appendix_name: plain_t option;
		algorithm_name: plain_t option;
		equation_name: plain_t option;
		figure_name: plain_t option;
		table_name: plain_t option;
		bibliography_name: plain_t option;
		notes_name: plain_t option;
		toc_name: plain_t option;
		default_bullet: Bullet.t;
		default_numbering: Numbering.t;
		locale: locale_t;
		} with sexp

	val locale_names: (locale_t, names_t) Hashtbl.t

	val make_default: unit -> t

	val get_section_name: t -> plain_t
	val get_appendix_name: t -> plain_t
	val get_algorithm_name: t -> plain_t
	val get_equation_name: t -> plain_t
	val get_figure_name: t -> plain_t
	val get_table_name: t -> plain_t
	val get_bibliography_name: t -> plain_t
	val get_notes_name: t -> plain_t
	val get_toc_name: t -> plain_t
	val get_default_bullet: t -> Bullet.t
	val get_default_numbering: t -> Numbering.t
	val get_locale: t-> locale_t

	val set_section_name: t -> plain_t -> t
	val set_appendix_name: t -> plain_t -> t
	val set_algorithm_name: t -> plain_t -> t
	val set_equation_name: t -> plain_t -> t
	val set_figure_name: t -> plain_t -> t
	val set_table_name: t -> plain_t -> t
	val set_bibliography_name: t -> plain_t -> t
	val set_notes_name: t -> plain_t -> t
	val set_toc_name: t -> plain_t -> t
	val set_default_bullet: t -> Bullet.t -> t
	val set_default_numbering: t -> Numbering.t -> t
	val set_locale: t-> locale_t -> t
end =
struct
	type names_t =
		{
		lc_section_name: plain_t;
		lc_appendix_name: plain_t;
		lc_algorithm_name: plain_t;
		lc_equation_name: plain_t;
		lc_figure_name: plain_t;
		lc_table_name: plain_t;
		lc_bibliography_name: plain_t;
		lc_notes_name: plain_t;
		lc_toc_name: plain_t;
		} with sexp

	type locale_t = Locale_en | Locale_pt with sexp

	type t =
		{
		section_name: plain_t option;
		appendix_name: plain_t option;
		algorithm_name: plain_t option;
		equation_name: plain_t option;
		figure_name: plain_t option;
		table_name: plain_t option;
		bibliography_name: plain_t option;
		notes_name: plain_t option;
		toc_name: plain_t option;
		default_bullet: Bullet.t;
		default_numbering: Numbering.t;
		locale: locale_t;
		} with sexp

	let names_en =
		{
		lc_section_name = "Section";
		lc_appendix_name = "Appendix";
		lc_algorithm_name = "Alg.";
		lc_equation_name = "Eq.";
		lc_figure_name = "Fig.";
		lc_table_name = "Tab.";
		lc_bibliography_name = "Bibliography";
		lc_notes_name = "Notes";
		lc_toc_name = "Table of Contents";
		}

	and names_pt =
		{
		lc_section_name = "Secção";
		lc_appendix_name = "Apêndice";
		lc_algorithm_name = "Alg.";
		lc_equation_name = "Eq.";
		lc_figure_name = "Fig.";
		lc_table_name = "Tab.";
		lc_bibliography_name = "Bibliografia";
		lc_notes_name = "Notas";
		lc_toc_name = "Índice";
		}

	let locale_names = Hashtbl.create 3

	let () =
		Hashtbl.add locale_names Locale_en names_en;
		Hashtbl.add locale_names Locale_pt names_pt

	let make_default () =
		{
		section_name = None;
		appendix_name = None;
		algorithm_name = None;
		equation_name = None;
		figure_name = None;
		table_name = None;
		bibliography_name = None;
		notes_name = None;
		toc_name = None;
		default_bullet = Bullet.Default;
		default_numbering = Numbering.Default;
		locale = Locale_en;
		}

	let get_section_name settings =
		match settings.section_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale	
				in names.lc_section_name
			| Some name ->
				name

	let get_appendix_name settings =
		match settings.appendix_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale	
				in names.lc_appendix_name
			| Some name ->
				name

	let get_algorithm_name settings = 
		match settings.algorithm_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_algorithm_name
			| Some name ->
				name

	let get_equation_name settings = 
		match settings.equation_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_equation_name
			| Some name ->
				name

	let get_figure_name settings = 
		match settings.figure_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_figure_name
			| Some name ->
				name

	let get_table_name settings = 
		match settings.table_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_table_name
			| Some name ->
				name

	let get_bibliography_name settings = 
		match settings.bibliography_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_bibliography_name
			| Some name ->
				name

	let get_notes_name settings = 
		match settings.notes_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_notes_name
			| Some name ->
				name

	let get_toc_name settings = 
		match settings.toc_name with
			| None ->
				let names = Hashtbl.find locale_names settings.locale
				in names.lc_toc_name
			| Some name ->
				name

	let get_default_bullet settings =
		settings.default_bullet

	let get_default_numbering settings =
		settings.default_numbering

	let get_locale settings =
		settings.locale

	let set_section_name settings name =
		{settings with section_name = Some name}

	let set_appendix_name settings name =
		{settings with appendix_name = Some name}

	let set_algorithm_name settings name =
		{settings with algorithm_name = Some name}

	let set_equation_name settings name =
		{settings with equation_name = Some name}

	let set_figure_name settings name =
		{settings with figure_name = Some name}

	let set_table_name settings name =
		{settings with table_name = Some name}

	let set_bibliography_name settings name =
		{settings with bibliography_name = Some name}

	let set_notes_name settings name =
		{settings with notes_name = Some name}

	let set_toc_name settings name =
		{settings with toc_name = Some name}

	let set_default_bullet settings bullet =
		{settings with default_bullet = bullet}

	let set_default_numbering settings numbering =
		{settings with default_numbering = numbering}

	let set_locale settings locale =
		{settings with locale = locale}
end
