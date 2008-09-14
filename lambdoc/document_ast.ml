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

type textual_node_t =
	| Plain of plain_t
	| Entity of entity_t

type textual_seq_t = textual_node_t list

type super_seq_t = super_node_t list

 and nonlink_seq_t = nonlink_node_t list

 and super_node_t =
	| Nonlink_node of nonlink_node_t
	| Link_node of link_node_t

 and nonlink_node_t =
	| Textual of textual_node_t
	| Mathtex of operator_t * plain_t
	| Mathml of operator_t * plain_t
	| Bold of command_t * super_seq_t
	| Emph of command_t * super_seq_t
	| Mono of command_t * super_seq_t
	| Caps of command_t * super_seq_t
	| Thru of command_t * super_seq_t
	| Sup of command_t * super_seq_t
	| Sub of command_t * super_seq_t
	| Box of command_t * super_seq_t

 and link_node_t =
	| Link of command_t * link_t * nonlink_seq_t
	| See of command_t * ref_t	
	| Cite of command_t * ref_t
	| Ref of command_t * ref_t
	| Sref of command_t * ref_t
	| Mref of command_t * ref_t * nonlink_seq_t


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
	| Top_block of top_block_t
	| Nestable_block of nestable_block_t

 and top_block_t =
	| Heading of heading_t
	| Appendix of command_t
	| Rule of command_t
	| Setting of command_t * string * string

 and heading_t =
	| Section of command_t * super_seq_t
	| Subsection of command_t * super_seq_t
	| Subsubsection of command_t * super_seq_t
	| Toc of command_t
	| Bibliography of command_t
	| Notes of command_t

 and nestable_block_t =
	| Paragraph of operator_t * super_seq_t
	| Math of command_t * plain_t
	| Tabular of command_t * tabular_t
	| Preformat of command_t * textual_seq_t
	| Itemize of command_t * item_t list
	| Enumerate of command_t * item_t list
	| Quote of command_t * nestable_frag_t
	| Algorithm of command_t * algorithm_block_t list
	| Equation of command_t * equation_block_t list
	| Figure of command_t * figure_block_t list
	| Table of command_t * table_block_t list
	| Bib of command_t * bib_block_t list
	| Note of command_t * super_seq_t

 and item_t =
	| Item of command_t * nestable_frag_t

 and algorithm_block_t =
	[ `Caption of command_t * super_seq_t
	| `Verbatim of command_t * textual_seq_t
	]

 and equation_block_t =
	[ `Caption of command_t * super_seq_t
	| `Math of command_t * plain_t
	]

 and figure_block_t =
	[ `Caption of command_t * super_seq_t
	| `Load of command_t * filename_t
	| `Verbatim of command_t * textual_seq_t
	| `Subpage of command_t * super_frag_t
	]

 and table_block_t =
	[ `Caption of command_t * super_seq_t
	| `Tabular of command_t * tabular_t
	]

 and bib_block_t =
	[ `Author of command_t * super_seq_t
	| `Title of command_t * super_seq_t
	| `Resource of command_t * super_seq_t
	]


(********************************************************************************)
(* 	{2 The type [Document_ast.t] itself}					*)
(********************************************************************************)

type t = super_frag_t

