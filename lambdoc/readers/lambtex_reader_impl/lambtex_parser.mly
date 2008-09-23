/********************************************************************************/
/*	Parser for the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*/
/********************************************************************************/

%{
open Document_basic
open Document_ast
%}


/********************************************************************************/
/* Operators.									*/
/********************************************************************************/

%token <Document_ast.operator_t> EOF
%token <Document_ast.operator_t> BEGIN
%token <Document_ast.operator_t> END

%token <Document_ast.operator_t> NEW_PARAGRAPH
%token <Document_ast.operator_t> COLUMN_SEP
%token <Document_ast.operator_t> ROW_END


/********************************************************************************/
/* Basic elements.								*/
/********************************************************************************/

%token <Document_basic.plain_t> PLAIN
%token <Document_basic.entity_t> ENTITY


/********************************************************************************/
/* Environment operators.  These are used in an inline context.			*/
/* Presently, the only existing environment operators are [$ $] and <$ $>.	*/
/********************************************************************************/

%token <Document_ast.operator_t> BEGIN_MATHTEX_INL
%token <Document_ast.operator_t> END_MATHTEX_INL

%token <Document_ast.operator_t> BEGIN_MATHML_INL
%token <Document_ast.operator_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.		*/
/* All environment commands are composed of a begin/end pair.			*/
/********************************************************************************/

%token <Document_ast.command_t> BEGIN_ITEMIZE
%token <Document_ast.command_t> END_ITEMIZE

%token <Document_ast.command_t> BEGIN_ENUMERATE
%token <Document_ast.command_t> END_ENUMERATE

%token <Document_ast.command_t> BEGIN_QUOTE
%token <Document_ast.command_t> END_QUOTE

%token <Document_ast.command_t> BEGIN_MATHTEX_BLK
%token <Document_ast.command_t> END_MATHTEX_BLK

%token <Document_ast.command_t> BEGIN_MATHML_BLK
%token <Document_ast.command_t> END_MATHML_BLK

%token <Document_ast.command_t> BEGIN_CODE
%token <Document_ast.command_t> END_CODE

%token <Document_ast.command_t> BEGIN_VERBATIM
%token <Document_ast.command_t> END_VERBATIM

%token <Document_ast.command_t> BEGIN_TABULAR
%token <Document_ast.command_t> END_TABULAR

%token <Document_ast.command_t> BEGIN_SUBPAGE
%token <Document_ast.command_t> END_SUBPAGE

%token <Document_ast.command_t> BEGIN_EQUATION
%token <Document_ast.command_t> END_EQUATION

%token <Document_ast.command_t> BEGIN_ALGORITHM 
%token <Document_ast.command_t> END_ALGORITHM

%token <Document_ast.command_t> BEGIN_TABLE
%token <Document_ast.command_t> END_TABLE

%token <Document_ast.command_t> BEGIN_FIGURE
%token <Document_ast.command_t> END_FIGURE

%token <Document_ast.command_t> BEGIN_BIB
%token <Document_ast.command_t> END_BIB


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Document_ast.command_t> BOLD
%token <Document_ast.command_t> EMPH
%token <Document_ast.command_t> MONO
%token <Document_ast.command_t> CAPS
%token <Document_ast.command_t> THRU
%token <Document_ast.command_t> SUP
%token <Document_ast.command_t> SUB
%token <Document_ast.command_t> BOX

%token <Document_ast.command_t> LINK
%token <Document_ast.command_t> SEE
%token <Document_ast.command_t> CITE
%token <Document_ast.command_t> REF
%token <Document_ast.command_t> SREF
%token <Document_ast.command_t> MREF

%token <Document_ast.command_t> SECTION
%token <Document_ast.command_t> SUBSECTION
%token <Document_ast.command_t> SUBSUBSECTION
%token <Document_ast.command_t> TOC
%token <Document_ast.command_t> BIBLIOGRAPHY
%token <Document_ast.command_t> NOTES

%token <Document_ast.command_t> APPENDIX
%token <Document_ast.command_t> RULE
%token <Document_ast.command_t> SETTING
%token <Document_ast.command_t> NEW_ITEM
%token <Document_ast.command_t> IMAGE
%token <Document_ast.command_t> CAPTION
%token <Document_ast.command_t> HEAD
%token <Document_ast.command_t> FOOT
%token <Document_ast.command_t> BODY
%token <Document_ast.command_t> BIB_TITLE
%token <Document_ast.command_t> BIB_AUTHOR
%token <Document_ast.command_t> BIB_RESOURCE
%token <Document_ast.command_t> NOTE


/********************************************************************************/
/* Type declarations.  Because we are using polymorphic variants, explicitly	*/
/* declaring the types for all the productions adds some safety: this way the	*/
/* compiler can warn us if we forgot about some case.				*/
/********************************************************************************/

%type <Document_ast.t>				document
%type <Document_ast.super_block_t>		super_block
%type <Document_ast.top_block_t>		top_block
%type <Document_ast.heading_block_t>		heading
%type <Document_ast.nestable_block_t>		nestable_block
%type <Document_ast.item_block_t list>		items

%type <Document_ast.equation_block_t>		equation
%type <Document_ast.algorithm_block_t>		algorithm
%type <Document_ast.table_block_t>		table
%type <Document_ast.figure_block_t>		figure
%type <Document_ast.bib_title_block_t>		bib_title
%type <Document_ast.bib_author_block_t>		bib_author
%type <Document_ast.bib_resource_block_t>	bib_resource

%type <Document_ast.tabular_t>			tabular
%type <Document_ast.tabular_group_t>		head
%type <Document_ast.tabular_group_t>		foot
%type <Document_ast.tabular_group_t>		body
%type <Document_ast.tabular_row_t>		row
%type <Document_ast.super_seq_t list>		columns

%type <Document_ast.super_node_t>		super_node
%type <Document_ast.textual_node_t>		textual_node
%type <Document_ast.nonlink_node_t>		nonlink_node
%type <Document_ast.link_node_t>		link_node


/********************************************************************************/
/* Begin grammar specification and declare top-level rules.			*/
/********************************************************************************/

%start document

%%

document:
	| super_block* EOF					{$1}


/********************************************************************************/
/* Blocks.									*/
/********************************************************************************/

super_block:
	| top_block						{`AST_top_block $1}
	| nestable_block					{`AST_nestable_block $1}


top_block:
	| heading						{`AST_heading $1}
	| APPENDIX						{`AST_appendix $1}
	| RULE							{`AST_rule $1}
	| SETTING BEGIN PLAIN END BEGIN PLAIN END		{`AST_setting ($1, $3, $6)}


heading:
	| SECTION BEGIN super_node+ END				{`AST_section ($1, $3)}
	| SUBSECTION BEGIN super_node+ END			{`AST_subsection ($1, $3)}
	| SUBSUBSECTION BEGIN super_node+ END			{`AST_subsubsection ($1, $3)}
	| TOC							{`AST_toc $1} 
	| BIBLIOGRAPHY						{`AST_bibliography $1}
	| NOTES							{`AST_notes $1}


nestable_block:
	| NEW_PARAGRAPH super_node+				{`AST_paragraph ($1, $2)}
	| BEGIN_ITEMIZE items END_ITEMIZE			{`AST_itemize ($1, $2)}
	| BEGIN_ENUMERATE items END_ENUMERATE			{`AST_enumerate ($1, $2)}
	| BEGIN_QUOTE nestable_block+ END_QUOTE			{`AST_quote ($1, $2)}
	| BEGIN_MATHTEX_BLK PLAIN END_MATHTEX_BLK		{`AST_mathtex_blk ($1, $2)}
	| BEGIN_MATHML_BLK PLAIN END_MATHML_BLK			{`AST_mathml_blk ($1, $2)}
	| BEGIN_CODE textual_node+ END_CODE			{`AST_code ($1, $2)}
	| BEGIN_VERBATIM textual_node+ END_VERBATIM		{`AST_verbatim ($1, $2)}
	| BEGIN_TABULAR tabular END_TABULAR			{`AST_tabular ($1, $2)}
	| IMAGE BEGIN PLAIN END					{`AST_image ($1, $3)}
	| BEGIN_SUBPAGE super_block+ END_SUBPAGE		{`AST_subpage ($1, $2)}
	| BEGIN_EQUATION caption equation END_EQUATION		{`AST_equation ($1, $2, $3)}
	| BEGIN_ALGORITHM caption algorithm END_ALGORITHM	{`AST_algorithm ($1, $2, $3)}
	| BEGIN_TABLE caption table END_TABLE			{`AST_table ($1, $2, $3)}
	| BEGIN_FIGURE caption figure END_FIGURE		{`AST_figure ($1, $2, $3)}
	| BEGIN_BIB bib_title bib_author bib_resource END_BIB	{`AST_bib ($1, $2, $3, $4)}
	| NOTE BEGIN super_node+ END				{`AST_note ($1, $3)}


items:
	| NEW_ITEM nestable_block+				{[`AST_item ($1, $2)]}
	| items NEW_ITEM nestable_block+			{List.append $1 [`AST_item ($2, $3)]}


/********************************************************************************/
/* Definition of floaters and bibliography entries.				*/
/********************************************************************************/

caption:
	| CAPTION BEGIN super_node+ END				{`AST_caption ($1, $3)}


equation:
	| BEGIN_MATHTEX_BLK PLAIN END_MATHTEX_BLK		{`AST_mathtex_blk ($1, $2)}
	| BEGIN_MATHML_BLK PLAIN END_MATHML_BLK			{`AST_mathml_blk ($1, $2)}


algorithm:
	| BEGIN_CODE textual_node+ END_CODE			{`AST_code ($1, $2)}


table:
	| BEGIN_TABULAR tabular END_TABULAR			{`AST_tabular ($1, $2)}


figure:
	| IMAGE BEGIN PLAIN END					{`AST_image ($1, $3)}
	| BEGIN_VERBATIM textual_node+ END_VERBATIM		{`AST_verbatim ($1, $2)}
	| BEGIN_SUBPAGE super_block+ END_SUBPAGE		{`AST_subpage ($1, $2)}


bib_title:
	| BIB_TITLE BEGIN super_node+ END			{`AST_bib_title ($1, $3)}


bib_author:
	| BIB_AUTHOR BEGIN super_node+ END			{`AST_bib_author ($1, $3)}


bib_resource:
	| BIB_RESOURCE BEGIN super_node+ END			{`AST_bib_resource ($1, $3)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?					{{thead = $1; tfoot = $3; tbodies = $2;}}
	| row+ body* foot?					{{thead = None; tfoot = $3; tbodies = (None, $1) :: $2;}}


head:
	| HEAD row+						{(Some $1, $2)}


foot:
	| FOOT row+						{(Some $1, $2)}


body:
	| BODY row+						{(Some $1, $2)}


row:
	| NEW_PARAGRAPH columns ROW_END				{($1, $2)}


columns:
	| super_node+						{[$1]}
	| columns COLUMN_SEP super_node+			{List.append $1 [$3]}


/********************************************************************************/
/* Nodes.									*/
/********************************************************************************/

super_node:
	| nonlink_node						{`AST_nonlink_node $1}
	| link_node						{`AST_link_node $1}


textual_node:
	| PLAIN							{`AST_plain $1}
	| ENTITY						{`AST_entity $1}


nonlink_node:
	| textual_node						{`AST_textual $1}
	| BEGIN_MATHTEX_INL PLAIN END_MATHTEX_INL		{`AST_mathtex_inl ($1, $2)}
	| BEGIN_MATHML_INL PLAIN END_MATHML_INL			{`AST_mathml_inl ($1, $2)}
	| BOLD BEGIN super_node+ END				{`AST_bold ($1, $3)}
	| EMPH BEGIN super_node+ END				{`AST_emph ($1, $3)}
	| MONO BEGIN super_node+ END				{`AST_mono ($1, $3)}
	| CAPS BEGIN super_node+ END				{`AST_caps ($1, $3)}
	| THRU BEGIN super_node+ END				{`AST_thru ($1, $3)}
	| SUP BEGIN super_node+ END				{`AST_sup ($1, $3)}
	| SUB BEGIN super_node+ END				{`AST_sub ($1, $3)}
	| BOX BEGIN super_node+ END				{`AST_box ($1, $3)}


link_node:
	| LINK BEGIN PLAIN END BEGIN nonlink_node+ END		{`AST_link ($1, $3, $6)}
	| SEE BEGIN PLAIN END					{`AST_see ($1, $3)}
	| CITE BEGIN PLAIN END					{`AST_cite ($1, $3)}
	| REF BEGIN PLAIN END					{`AST_ref ($1, $3)}
	| SREF BEGIN PLAIN END					{`AST_sref ($1, $3)}
	| MREF BEGIN PLAIN END BEGIN nonlink_node+ END		{`AST_mref ($1, $3, $6)}

