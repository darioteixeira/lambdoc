(********************************************************************************)
(*	Definition of the document AST that all parsers are supposed to generate.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Document_basic


(********************************************************************************)
(**	{2 Parsing data associated with operators and command tags}		*)
(********************************************************************************)

type command_t =
	{
	comm_tag: string;
	comm_label: string option;
	comm_order: string option;
	comm_extra: string option;
	comm_secondary: string option;
	comm_linenum: int;
	}

type operator_t =
	{
	op_linenum: int;
	}
	

(********************************************************************************)
(**	{2 Data types for inline context}					*)
(********************************************************************************)

type super_seq_t = super_node_t list

and nonlink_seq_t = nonlink_node_t list

and textual_seq_t = textual_node_t list

and super_node_t =
	[ `AST_nonlink_node of nonlink_node_t
	| `AST_link_node of link_node_t
	]

and nonlink_node_t =
	[ `AST_textual of textual_node_t
	| `AST_mathtex_inl of operator_t * plain_t
	| `AST_mathml_inl of operator_t * plain_t
	| `AST_bold of command_t * super_seq_t
	| `AST_emph of command_t * super_seq_t
	| `AST_mono of command_t * super_seq_t
	| `AST_caps of command_t * super_seq_t
	| `AST_thru of command_t * super_seq_t
	| `AST_sup of command_t * super_seq_t
	| `AST_sub of command_t * super_seq_t
	| `AST_box of command_t * super_seq_t
	]

and link_node_t =
	[ `AST_link of command_t * link_t * nonlink_seq_t
	| `AST_see of command_t * ref_t	
	| `AST_cite of command_t * ref_t
	| `AST_ref of command_t * ref_t
	| `AST_sref of command_t * ref_t
	| `AST_mref of command_t * ref_t * nonlink_seq_t
	]

and textual_node_t =
	[ `AST_plain of plain_t
	| `AST_entity of entity_t
	]


(********************************************************************************)
(**	{2 Data types for tabular environments}					*)
(********************************************************************************)

type tabular_row_t = operator_t * super_seq_t list

type tabular_group_t = command_t option * tabular_row_t list

type tabular_t =
	{
	thead: tabular_group_t option;
	tfoot: tabular_group_t option;
	tbodies: tabular_group_t list;
	}


(********************************************************************************)
(**	{2 Data types for document blocks}					*)
(********************************************************************************)

type super_frag_t = super_block_t list

and nestable_frag_t = nestable_block_t list

and super_block_t =
	[ `AST_top_block of top_block_t
	| `AST_nestable_block of nestable_block_t
	]

and top_block_t =
	[ `AST_heading of heading_t
	| `AST_appendix of command_t
	| `AST_rule of command_t
	| `AST_setting of command_t * string * string
	]

and heading_t =
	[ `AST_section of command_t * super_seq_t
	| `AST_subsection of command_t * super_seq_t
	| `AST_subsubsection of command_t * super_seq_t
	| `AST_toc of command_t
	| `AST_bibliography of command_t
	| `AST_notes of command_t
	]

and nestable_block_t =
	[ `AST_paragraph of operator_t * super_seq_t
	| `AST_itemize of command_t * item_t list
	| `AST_enumerate of command_t * item_t list
	| `AST_quote of command_t * nestable_frag_t
	| `AST_mathtex_blk of command_t * plain_t
	| `AST_mathml_blk of command_t * plain_t
	| `AST_code of command_t * textual_seq_t
	| `AST_verbatim of command_t * textual_seq_t
	| `AST_tabular of command_t * tabular_t
	| `AST_image of command_t * plain_t
	| `AST_subpage of command_t * super_frag_t
	| `AST_equation of command_t * caption_t * equation_t
	| `AST_algorithm of command_t * caption_t * algorithm_t
	| `AST_table of command_t * caption_t * table_t
	| `AST_figure of command_t * caption_t * figure_t
	| `AST_bib of command_t * bib_title_t * bib_author_t * bib_resource_t
	| `AST_note of command_t * super_seq_t
	]

and item_t =
	[ `AST_item of command_t * nestable_frag_t
	]

and caption_t =
	[ `AST_caption of command_t * super_seq_t
	]

and equation_t =
	[ `AST_mathtex_blk of command_t * plain_t
	| `AST_mathml_blk of command_t * plain_t
	]

and algorithm_t =
	[ `AST_code of command_t * textual_seq_t
	]

and table_t =
	[ `AST_tabular of command_t * tabular_t
	]

and figure_t =
	[ `AST_image of command_t * plain_t
	| `AST_verbatim of command_t * textual_seq_t
	| `AST_subpage of command_t * super_frag_t
	]

and bib_title_t =
	[ `AST_bib_title of command_t * super_seq_t
	]

and bib_author_t =
	[ `AST_bib_author of command_t * super_seq_t
	]

and bib_resource_t =
	[ `AST_bib_resource of command_t * super_seq_t
	]


(********************************************************************************)
(* 	{2 The type [Document_ast.t] itself}					*)
(********************************************************************************)

type t = super_frag_t

