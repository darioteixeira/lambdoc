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

%token <Document_ast.Ast.operator_t> EOF
%token <Document_ast.Ast.operator_t> BEGIN
%token <Document_ast.Ast.operator_t> END

%token <Document_ast.Ast.operator_t> NEW_PARAGRAPH
%token <Document_ast.Ast.operator_t> COLUMN_SEP
%token <Document_ast.Ast.operator_t> ROW_END


/********************************************************************************/
/* Basic elements.								*/
/********************************************************************************/

%token <Document_basic.plain_t> PLAIN
%token <Document_basic.entity_t> ENTITY


/********************************************************************************/
/* Environment operators.  These are used in an inline context.			*/
/* Presently, the only existing environment operators are [$ $] and <$ $>.	*/
/********************************************************************************/

%token <Document_ast.Ast.operator_t> BEGIN_MATHTEX_INL
%token <Document_ast.Ast.operator_t> END_MATHTEX_INL

%token <Document_ast.Ast.operator_t> BEGIN_MATHML_INL
%token <Document_ast.Ast.operator_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.		*/
/* All environment commands are composed of a begin/end pair.			*/
/********************************************************************************/

%token <Document_ast.Ast.command_t> BEGIN_ABSTRACT
%token <Document_ast.Ast.command_t> END_ABSTRACT

%token <Document_ast.Ast.command_t> BEGIN_ITEMIZE
%token <Document_ast.Ast.command_t> END_ITEMIZE

%token <Document_ast.Ast.command_t> BEGIN_ENUMERATE
%token <Document_ast.Ast.command_t> END_ENUMERATE

%token <Document_ast.Ast.command_t> BEGIN_QUOTE
%token <Document_ast.Ast.command_t> END_QUOTE

%token <Document_ast.Ast.command_t> BEGIN_MATHTEX_BLK
%token <Document_ast.Ast.command_t> END_MATHTEX_BLK

%token <Document_ast.Ast.command_t> BEGIN_MATHML_BLK
%token <Document_ast.Ast.command_t> END_MATHML_BLK

%token <Document_ast.Ast.command_t> BEGIN_CODE
%token <Document_ast.Ast.command_t> END_CODE

%token <Document_ast.Ast.command_t> BEGIN_VERBATIM
%token <Document_ast.Ast.command_t> END_VERBATIM

%token <Document_ast.Ast.command_t> BEGIN_TABULAR
%token <Document_ast.Ast.command_t> END_TABULAR

%token <Document_ast.Ast.command_t> BEGIN_SUBPAGE
%token <Document_ast.Ast.command_t> END_SUBPAGE

%token <Document_ast.Ast.command_t> BEGIN_EQUATION
%token <Document_ast.Ast.command_t> END_EQUATION

%token <Document_ast.Ast.command_t> BEGIN_ALGORITHM 
%token <Document_ast.Ast.command_t> END_ALGORITHM

%token <Document_ast.Ast.command_t> BEGIN_TABLE
%token <Document_ast.Ast.command_t> END_TABLE

%token <Document_ast.Ast.command_t> BEGIN_FIGURE
%token <Document_ast.Ast.command_t> END_FIGURE

%token <Document_ast.Ast.command_t> BEGIN_BIB
%token <Document_ast.Ast.command_t> END_BIB


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Document_ast.Ast.command_t> BOLD
%token <Document_ast.Ast.command_t> EMPH
%token <Document_ast.Ast.command_t> MONO
%token <Document_ast.Ast.command_t> CAPS
%token <Document_ast.Ast.command_t> THRU
%token <Document_ast.Ast.command_t> SUP
%token <Document_ast.Ast.command_t> SUB
%token <Document_ast.Ast.command_t> BOX

%token <Document_ast.Ast.command_t> LINK
%token <Document_ast.Ast.command_t> SEE
%token <Document_ast.Ast.command_t> CITE
%token <Document_ast.Ast.command_t> REF
%token <Document_ast.Ast.command_t> SREF
%token <Document_ast.Ast.command_t> MREF

%token <Document_ast.Ast.command_t> SECTION
%token <Document_ast.Ast.command_t> SUBSECTION
%token <Document_ast.Ast.command_t> SUBSUBSECTION
%token <Document_ast.Ast.command_t> TOC
%token <Document_ast.Ast.command_t> BIBLIOGRAPHY
%token <Document_ast.Ast.command_t> NOTES

%token <Document_ast.Ast.command_t> TITLE
%token <Document_ast.Ast.command_t> RULE
%token <Document_ast.Ast.command_t> APPENDIX
%token <Document_ast.Ast.command_t> SETTING
%token <Document_ast.Ast.command_t> NEW_ITEM
%token <Document_ast.Ast.command_t> IMAGE
%token <Document_ast.Ast.command_t> CAPTION
%token <Document_ast.Ast.command_t> HEAD
%token <Document_ast.Ast.command_t> FOOT
%token <Document_ast.Ast.command_t> BODY
%token <Document_ast.Ast.command_t> BIB_TITLE
%token <Document_ast.Ast.command_t> BIB_AUTHOR
%token <Document_ast.Ast.command_t> BIB_RESOURCE
%token <Document_ast.Ast.command_t> NOTE


/********************************************************************************/
/* Type declarations.  Because we are using polymorphic variants, explicitly	*/
/* declaring the types for all the productions adds some safety: this way the	*/
/* compiler can warn us if we forgot about some case.				*/
/********************************************************************************/

%type <Document_ast.Ast.t>			document
%type <Document_ast.Ast.super_block_t>		super_block
%type <Document_ast.Ast.top_block_t>		top_block
%type <Document_ast.Ast.heading_block_t>	heading_block
%type <Document_ast.Ast.nestable_block_t>	nestable_block
%type <Document_ast.Ast.item_frag_t>		items

%type <Document_ast.Ast.caption_block_t>	caption_block
%type <Document_ast.Ast.paragraph_block_t>	paragraph_block
%type <Document_ast.Ast.itemize_block_t>	itemize_block
%type <Document_ast.Ast.enumerate_block_t>	enumerate_block
%type <Document_ast.Ast.quote_block_t>		quote_block
%type <Document_ast.Ast.mathtex_block_t>	mathtex_block
%type <Document_ast.Ast.mathml_block_t>		mathml_block
%type <Document_ast.Ast.code_block_t>		code_block
%type <Document_ast.Ast.verbatim_block_t>	verbatim_block
%type <Document_ast.Ast.tabular_block_t>	tabular_block
%type <Document_ast.Ast.image_block_t>		image_block
%type <Document_ast.Ast.subpage_block_t>	subpage_block
%type <Document_ast.Ast.bib_title_block_t>	bib_title_block
%type <Document_ast.Ast.bib_author_block_t>	bib_author_block
%type <Document_ast.Ast.bib_resource_block_t>	bib_resource_block

%type <Document_ast.Ast.equation_block_t>	equation_block
%type <Document_ast.Ast.algorithm_block_t>	algorithm_block
%type <Document_ast.Ast.table_block_t>		table_block
%type <Document_ast.Ast.figure_block_t>		figure_block

%type <Document_ast.Ast.tabular_t>		tabular
%type <Document_ast.Ast.tabular_group_t>	head
%type <Document_ast.Ast.tabular_group_t>	foot
%type <Document_ast.Ast.tabular_group_t>	body
%type <Document_ast.Ast.tabular_row_t>		row
%type <Document_ast.Ast.super_seq_t list>	columns

%type <Document_ast.Ast.super_node_t>		super_node
%type <Document_ast.Ast.textual_node_t>		textual_node
%type <Document_ast.Ast.nonlink_node_t>		nonlink_node
%type <Document_ast.Ast.link_node_t>		link_node


/********************************************************************************/
/* Begin grammar specification and declare top-level rules.			*/
/********************************************************************************/

%start document

%%

document:
	| super_block* EOF	{$1}


/********************************************************************************/
/* Blocks.									*/
/********************************************************************************/

super_block:
	| top_block								{($1 :> Ast.super_block_t)}
	| nestable_block							{($1 :> Ast.super_block_t)}

top_block:
	| heading_block								{`AST_heading $1}
	| TITLE BEGIN super_node+ END						{`AST_title ($1, $3)}
	| BEGIN_ABSTRACT paragraph_block+ END_ABSTRACT				{`AST_abstract ($1, $2)}
	| RULE									{`AST_rule $1}
	| APPENDIX								{`AST_appendix $1}
	| SETTING BEGIN PLAIN END BEGIN PLAIN END				{`AST_setting ($1, $3, $6)}

heading_block:
	| SECTION BEGIN super_node+ END						{`AST_section ($1, $3)}
	| SUBSECTION BEGIN super_node+ END					{`AST_subsection ($1, $3)}
	| SUBSUBSECTION BEGIN super_node+ END					{`AST_subsubsection ($1, $3)}
	| TOC									{`AST_toc $1} 
	| BIBLIOGRAPHY								{`AST_bibliography $1}
	| NOTES									{`AST_notes $1}

nestable_block:
	| paragraph_block							{($1 :> Ast.nestable_block_t)}
	| itemize_block								{($1 :> Ast.nestable_block_t)}
	| enumerate_block							{($1 :> Ast.nestable_block_t)}
	| quote_block								{($1 :> Ast.nestable_block_t)}
	| mathtex_block								{($1 :> Ast.nestable_block_t)}
	| mathml_block								{($1 :> Ast.nestable_block_t)}
	| code_block								{($1 :> Ast.nestable_block_t)}
	| verbatim_block							{($1 :> Ast.nestable_block_t)}
	| tabular_block								{($1 :> Ast.nestable_block_t)}
	| image_block								{($1 :> Ast.nestable_block_t)}
	| subpage_block								{($1 :> Ast.nestable_block_t)}
	| BEGIN_EQUATION equation_block caption_block END_EQUATION		{`AST_equation ($1, $3, $2)}
	| BEGIN_ALGORITHM algorithm_block caption_block END_ALGORITHM		{`AST_algorithm ($1, $3, $2)}
	| BEGIN_TABLE table_block caption_block END_TABLE			{`AST_table ($1, $3, $2)}
	| BEGIN_FIGURE figure_block caption_block END_FIGURE			{`AST_figure ($1, $3, $2)}
	| BEGIN_BIB bib_title_block bib_author_block bib_resource_block END_BIB	{`AST_bib ($1, $2, $3, $4)}
	| NOTE BEGIN super_node+ END						{`AST_note ($1, $3)}

items:
	| NEW_ITEM nestable_block+						{[`AST_item ($1, $2)]}
	| items NEW_ITEM nestable_block+					{List.append $1 [`AST_item ($2, $3)]}


/********************************************************************************/
/* Definition of individual blocks.						*/
/********************************************************************************/

caption_block:		| CAPTION BEGIN super_node+ END			{`AST_caption ($1, $3)}
paragraph_block:	| NEW_PARAGRAPH super_node+			{`AST_paragraph ($1, $2)}
itemize_block:		| BEGIN_ITEMIZE items END_ITEMIZE		{`AST_itemize ($1, $2)}
enumerate_block:	| BEGIN_ENUMERATE items END_ENUMERATE		{`AST_enumerate ($1, $2)}
quote_block:		| BEGIN_QUOTE nestable_block+ END_QUOTE		{`AST_quote ($1, $2)}
mathtex_block:		| BEGIN_MATHTEX_BLK PLAIN END_MATHTEX_BLK	{`AST_mathtex_blk ($1, $2)}
mathml_block:		| BEGIN_MATHML_BLK PLAIN END_MATHML_BLK		{`AST_mathml_blk ($1, $2)}
code_block:		| BEGIN_CODE textual_node+ END_CODE		{`AST_code ($1, $2)}
verbatim_block:		| BEGIN_VERBATIM textual_node+ END_VERBATIM	{`AST_verbatim ($1, $2)}
tabular_block:		| BEGIN_TABULAR tabular END_TABULAR		{`AST_tabular ($1, $2)}
image_block:		| IMAGE BEGIN PLAIN END				{`AST_image ($1, $3)}
subpage_block:		| BEGIN_SUBPAGE super_block+ END_SUBPAGE	{`AST_subpage ($1, $2)}
bib_title_block:	| BIB_TITLE BEGIN super_node+ END		{`AST_bib_title ($1, $3)}
bib_author_block:	| BIB_AUTHOR BEGIN super_node+ END		{`AST_bib_author ($1, $3)}
bib_resource_block:	| BIB_RESOURCE BEGIN super_node+ END		{`AST_bib_resource ($1, $3)}


/********************************************************************************/
/* Definition of floaters.							*/
/********************************************************************************/

equation_block:
	| mathtex_block		{($1 :> Ast.equation_block_t)}
	| mathml_block		{($1 :> Ast.equation_block_t)}

algorithm_block:
	| code_block		{($1 :> Ast.algorithm_block_t)}

table_block:
	| tabular_block		{($1 :> Ast.table_block_t)}

figure_block:
	| image_block		{($1 :> Ast.figure_block_t)}
	| verbatim_block	{($1 :> Ast.figure_block_t)}
	| subpage_block		{($1 :> Ast.figure_block_t)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?			{{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
	| row+ body* foot?			{{Ast.thead = None; Ast.tfoot = $3; Ast.tbodies = (None, $1) :: $2;}}


head:
	| HEAD row+				{(Some $1, $2)}


foot:
	| FOOT row+				{(Some $1, $2)}


body:
	| BODY row+				{(Some $1, $2)}


row:
	| NEW_PARAGRAPH columns ROW_END		{($1, $2)}


columns:
	| super_node+				{[$1]}
	| columns COLUMN_SEP super_node+	{List.append $1 [$3]}


/********************************************************************************/
/* Nodes.									*/
/********************************************************************************/

textual_node:
	| PLAIN							{`AST_plain $1}
	| ENTITY						{`AST_entity $1}

nonlink_node:
	| textual_node						{($1 :> Ast.nonlink_node_t)}
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

super_node:
	| nonlink_node						{($1 :> Ast.super_node_t)}
	| link_node						{($1 :> Ast.super_node_t)}

