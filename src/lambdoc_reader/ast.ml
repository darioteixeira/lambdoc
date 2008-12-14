(********************************************************************************)
(*	Implementation file for Ast module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the document AST that all parsers are supposed to generate.
*)

open Basic


module rec M:
sig

	(************************************************************************)
	(**	{2 Parsing data associated with operators and command tags}	*)
	(************************************************************************)

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
		

	(************************************************************************)
	(**	{2 Data types for inline context}				*)
	(************************************************************************)

	type super_seq_t = M.super_node_t list
	type nonlink_seq_t = M.nonlink_node_t list

	type nonlink_node_t =
		[ `AST_plain of operator_t * plain_t
		| `AST_entity of operator_t * entity_t
		| `AST_mathtex_inl of operator_t * raw_t
		| `AST_mathml_inl of operator_t * raw_t
		| `AST_bold of command_t * super_seq_t
		| `AST_emph of command_t * super_seq_t
		| `AST_mono of command_t * super_seq_t
		| `AST_caps of command_t * super_seq_t
		| `AST_thru of command_t * super_seq_t
		| `AST_sup of command_t * super_seq_t
		| `AST_sub of command_t * super_seq_t
		| `AST_mbox of command_t * super_seq_t
		]

	type link_node_t =
		[ `AST_link of command_t * raw_t * nonlink_seq_t
		| `AST_see of command_t * raw_t	
		| `AST_cite of command_t * raw_t
		| `AST_ref of command_t * raw_t
		| `AST_sref of command_t * raw_t
		| `AST_mref of command_t * raw_t * nonlink_seq_t
		]

	type super_node_t =
		[ nonlink_node_t
		| link_node_t
		]


	(************************************************************************)
	(**	{2 Data types for tabular environment}				*)
	(************************************************************************)

	type tabular_row_t = operator_t * super_seq_t list

	type tabular_group_t = command_t option * tabular_row_t list

	type tabular_t =
		{
		thead: tabular_group_t option;
		tfoot: tabular_group_t option;
		tbodies: tabular_group_t list;
		}


	(************************************************************************)
	(**	{2 Data types for document blocks}				*)
	(************************************************************************)

	type super_frag_t = M.super_block_t list
	type nestable_frag_t = M.nestable_block_t list
	type item_frag_t = M.item_block_t list

	type caption_block_t = [ `AST_caption of command_t * super_seq_t ]
	type item_block_t = [ `AST_item of command_t * nestable_frag_t ] 
	type paragraph_block_t = [ `AST_paragraph of operator_t * super_seq_t ]
	type itemize_block_t = [ `AST_itemize of command_t * item_frag_t ]
	type enumerate_block_t = [ `AST_enumerate of command_t * item_frag_t ]
	type quote_block_t = [ `AST_quote of command_t * nestable_frag_t ]
	type mathtex_block_t = [ `AST_mathtex_blk of command_t * raw_t ]
	type mathml_block_t = [ `AST_mathml_blk of command_t * raw_t ]
	type code_block_t = [ `AST_code of command_t * raw_t ]
	type verbatim_block_t = [ `AST_verbatim of command_t * raw_t ]
	type tabular_block_t = [ `AST_tabular of command_t * tabular_t ]
	type image_block_t = [ `AST_image of command_t * raw_t ]
	type subpage_block_t = [ `AST_subpage of command_t * super_frag_t ]
	type bib_title_block_t = [ `AST_bib_title of command_t * super_seq_t ] 
	type bib_author_block_t = [ `AST_bib_author of command_t * super_seq_t ] 
	type bib_resource_block_t = [ `AST_bib_resource of command_t * super_seq_t ] 

	type equation_block_t =
		[ mathtex_block_t
		| mathml_block_t
		]

	type algorithm_block_t =
		code_block_t

	type table_block_t =
		tabular_block_t

	type figure_block_t =
		[ image_block_t
		| verbatim_block_t
		| subpage_block_t
		]

	type nestable_block_t =
		[ paragraph_block_t
		| itemize_block_t
		| enumerate_block_t
		| quote_block_t
		| mathtex_block_t
		| mathml_block_t
		| code_block_t
		| verbatim_block_t
		| tabular_block_t
		| image_block_t
		| subpage_block_t
		| `AST_equation of command_t * caption_block_t * equation_block_t
		| `AST_algorithm of command_t * caption_block_t * algorithm_block_t
		| `AST_table of command_t * caption_block_t * table_block_t
		| `AST_figure of command_t * caption_block_t * figure_block_t
		| `AST_bib of command_t * bib_title_block_t * bib_author_block_t * bib_resource_block_t
		| `AST_note of command_t * nestable_frag_t
		]

	type heading_block_t =
		[ `AST_part of command_t * super_seq_t
		| `AST_appendix of command_t
		| `AST_section of hierarchical_level_t * command_t * super_seq_t
		| `AST_bibliography of command_t
		| `AST_notes of command_t
		| `AST_toc of command_t
		]

	type top_block_t =
		[ heading_block_t
		| `AST_title of title_level_t * command_t * super_seq_t
		| `AST_abstract of command_t * paragraph_block_t list
		| `AST_rule of command_t
		]

	type super_block_t =
		[ top_block_t
		| nestable_block_t
		]


	(************************************************************************)
	(* 	{2 The type {!t} itself}					*)
	(************************************************************************)

	type t = super_frag_t

end = M
