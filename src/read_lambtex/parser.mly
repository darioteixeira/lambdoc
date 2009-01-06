/********************************************************************************/
/*	Implementation file for Lambtex_parser.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*/
/********************************************************************************/

%{
open Lambdoc_reader
%}


/********************************************************************************/
/* Operators.									*/
/********************************************************************************/

%token EOF
%token BEGIN
%token END

%token <Lambdoc_reader.Ast.M.command_t> NEW_PAR
%token <Lambdoc_reader.Ast.M.command_t> COLUMN_SEP
%token <Lambdoc_reader.Ast.M.command_t> ROW_END


/********************************************************************************/
/* Basic elements.								*/
/********************************************************************************/

%token <Lambdoc_core.Basic.raw_t> RAW
%token <Lambdoc_reader.Ast.M.command_t * Lambdoc_core.Basic.plain_t> PLAIN
%token <Lambdoc_reader.Ast.M.command_t * Lambdoc_core.Basic.entity_t> ENTITY


/********************************************************************************/
/* Environment operators.  These are used in an inline context.			*/
/* Presently, the only existing environment operators are [$ $] and <$ $>.	*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_MATHTEX_INL
%token <Lambdoc_reader.Ast.M.command_t> END_MATHTEX_INL

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_MATHML_INL
%token <Lambdoc_reader.Ast.M.command_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.		*/
/* All environment commands are composed of a begin/end pair.			*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_ABSTRACT
%token <Lambdoc_reader.Ast.M.command_t> END_ABSTRACT

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_ITEMIZE
%token <Lambdoc_reader.Ast.M.command_t> END_ITEMIZE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_ENUMERATE
%token <Lambdoc_reader.Ast.M.command_t> END_ENUMERATE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_QUOTE
%token <Lambdoc_reader.Ast.M.command_t> END_QUOTE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_MATHTEX_BLK
%token <Lambdoc_reader.Ast.M.command_t> END_MATHTEX_BLK

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_MATHML_BLK
%token <Lambdoc_reader.Ast.M.command_t> END_MATHML_BLK

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_CODE
%token <Lambdoc_reader.Ast.M.command_t> END_CODE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.M.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_TABULAR
%token <Lambdoc_reader.Ast.M.command_t> END_TABULAR

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_SUBPAGE
%token <Lambdoc_reader.Ast.M.command_t> END_SUBPAGE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_EQUATION
%token <Lambdoc_reader.Ast.M.command_t> END_EQUATION

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_PRINTOUT 
%token <Lambdoc_reader.Ast.M.command_t> END_PRINTOUT

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_TABLE
%token <Lambdoc_reader.Ast.M.command_t> END_TABLE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_FIGURE
%token <Lambdoc_reader.Ast.M.command_t> END_FIGURE

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_BIB
%token <Lambdoc_reader.Ast.M.command_t> END_BIB

%token <Lambdoc_reader.Ast.M.command_t> BEGIN_NOTE
%token <Lambdoc_reader.Ast.M.command_t> END_NOTE


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.M.command_t> BOLD
%token <Lambdoc_reader.Ast.M.command_t> EMPH
%token <Lambdoc_reader.Ast.M.command_t> MONO
%token <Lambdoc_reader.Ast.M.command_t> CAPS
%token <Lambdoc_reader.Ast.M.command_t> THRU
%token <Lambdoc_reader.Ast.M.command_t> SUP
%token <Lambdoc_reader.Ast.M.command_t> SUB
%token <Lambdoc_reader.Ast.M.command_t> MBOX

%token <Lambdoc_reader.Ast.M.command_t> LINK
%token <Lambdoc_reader.Ast.M.command_t> SEE
%token <Lambdoc_reader.Ast.M.command_t> CITE
%token <Lambdoc_reader.Ast.M.command_t> REF
%token <Lambdoc_reader.Ast.M.command_t> SREF
%token <Lambdoc_reader.Ast.M.command_t> MREF

%token <Lambdoc_reader.Ast.M.command_t> PART
%token <Lambdoc_reader.Ast.M.command_t> APPENDIX
%token <Lambdoc_reader.Ast.M.command_t> SECTION
%token <Lambdoc_reader.Ast.M.command_t> SUBSECTION
%token <Lambdoc_reader.Ast.M.command_t> SUBSUBSECTION
%token <Lambdoc_reader.Ast.M.command_t> BIBLIOGRAPHY
%token <Lambdoc_reader.Ast.M.command_t> NOTES
%token <Lambdoc_reader.Ast.M.command_t> TOC
%token <Lambdoc_reader.Ast.M.command_t> TITLE
%token <Lambdoc_reader.Ast.M.command_t> SUBTITLE
%token <Lambdoc_reader.Ast.M.command_t> RULE

%token <Lambdoc_reader.Ast.M.command_t> NEW_ITEM
%token <Lambdoc_reader.Ast.M.command_t> BITMAP
%token <Lambdoc_reader.Ast.M.command_t> CAPTION
%token <Lambdoc_reader.Ast.M.command_t> HEAD
%token <Lambdoc_reader.Ast.M.command_t> FOOT
%token <Lambdoc_reader.Ast.M.command_t> BODY
%token <Lambdoc_reader.Ast.M.command_t> BIB_TITLE
%token <Lambdoc_reader.Ast.M.command_t> BIB_AUTHOR
%token <Lambdoc_reader.Ast.M.command_t> BIB_RESOURCE


/********************************************************************************/
/* Type declarations.  Because we are using polymorphic variants, explicitly	*/
/* declaring the types for all the productions adds some safety: this way the	*/
/* compiler can warn us if we forgot about some case.				*/
/********************************************************************************/

%type <Lambdoc_reader.Ast.M.t>				document
%type <Lambdoc_reader.Ast.M.super_block_t>		super_block
%type <Lambdoc_reader.Ast.M.top_block_t>		top_block
%type <Lambdoc_reader.Ast.M.heading_block_t>		heading_block
%type <Lambdoc_reader.Ast.M.nestable_block_t>		nestable_block
%type <Lambdoc_reader.Ast.M.item_frag_t>		items

%type <Lambdoc_reader.Ast.M.caption_block_t>		caption_block
%type <Lambdoc_reader.Ast.M.paragraph_block_t>		paragraph_block
%type <Lambdoc_reader.Ast.M.itemize_block_t>		itemize_block
%type <Lambdoc_reader.Ast.M.enumerate_block_t>		enumerate_block
%type <Lambdoc_reader.Ast.M.quote_block_t>		quote_block
%type <Lambdoc_reader.Ast.M.mathtex_block_t>		mathtex_block
%type <Lambdoc_reader.Ast.M.mathml_block_t>		mathml_block
%type <Lambdoc_reader.Ast.M.code_block_t>		code_block
%type <Lambdoc_reader.Ast.M.verbatim_block_t>		verbatim_block
%type <Lambdoc_reader.Ast.M.tabular_block_t>		tabular_block
%type <Lambdoc_reader.Ast.M.bitmap_block_t>		bitmap_block
%type <Lambdoc_reader.Ast.M.subpage_block_t>		subpage_block
%type <Lambdoc_reader.Ast.M.bib_title_block_t>		bib_title_block
%type <Lambdoc_reader.Ast.M.bib_author_block_t>		bib_author_block
%type <Lambdoc_reader.Ast.M.bib_resource_block_t>	bib_resource_block

%type <Lambdoc_reader.Ast.M.equation_block_t>		equation_block
%type <Lambdoc_reader.Ast.M.printout_block_t>		printout_block
%type <Lambdoc_reader.Ast.M.table_block_t>		table_block
%type <Lambdoc_reader.Ast.M.figure_block_t>		figure_block

%type <Lambdoc_reader.Ast.M.tabular_t>			tabular
%type <Lambdoc_reader.Ast.M.tabular_group_t>		head
%type <Lambdoc_reader.Ast.M.tabular_group_t>		foot
%type <Lambdoc_reader.Ast.M.tabular_group_t>		body
%type <Lambdoc_reader.Ast.M.tabular_row_t>		row
%type <Lambdoc_reader.Ast.M.super_seq_t list>		columns

%type <Lambdoc_reader.Ast.M.super_node_t>		super_node
%type <Lambdoc_reader.Ast.M.nonlink_node_t>		nonlink_node
%type <Lambdoc_reader.Ast.M.link_node_t>		link_node


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
	| top_block								{($1 :> Ast.M.super_block_t)}
	| nestable_block							{($1 :> Ast.M.super_block_t)}

top_block:
	| heading_block								{($1 :> Ast.M.top_block_t)}
	| TITLE BEGIN super_node+ END						{`AST_title (`Level1, $1, $3)}
	| SUBTITLE BEGIN super_node+ END					{`AST_title (`Level2, $1, $3)}
	| BEGIN_ABSTRACT paragraph_block+ END_ABSTRACT				{`AST_abstract ($1, $2)}
	| RULE									{`AST_rule $1}

heading_block:
	| PART BEGIN super_node+ END						{`AST_part ($1, $3)}
	| APPENDIX								{`AST_appendix $1}
	| SECTION BEGIN super_node+ END						{`AST_section (`Level1, $1, $3)}
	| SUBSECTION BEGIN super_node+ END					{`AST_section (`Level2, $1, $3)}
	| SUBSUBSECTION BEGIN super_node+ END					{`AST_section (`Level3, $1, $3)}
	| BIBLIOGRAPHY								{`AST_bibliography $1}
	| NOTES									{`AST_notes $1}
	| TOC									{`AST_toc $1} 

nestable_block:
	| paragraph_block							{($1 :> Ast.M.nestable_block_t)}
	| itemize_block								{($1 :> Ast.M.nestable_block_t)}
	| enumerate_block							{($1 :> Ast.M.nestable_block_t)}
	| quote_block								{($1 :> Ast.M.nestable_block_t)}
	| mathtex_block								{($1 :> Ast.M.nestable_block_t)}
	| mathml_block								{($1 :> Ast.M.nestable_block_t)}
	| code_block								{($1 :> Ast.M.nestable_block_t)}
	| verbatim_block							{($1 :> Ast.M.nestable_block_t)}
	| tabular_block								{($1 :> Ast.M.nestable_block_t)}
	| bitmap_block								{($1 :> Ast.M.nestable_block_t)}
	| subpage_block								{($1 :> Ast.M.nestable_block_t)}
	| BEGIN_EQUATION equation_block caption_block END_EQUATION		{`AST_equation ($1, $3, $2)}
	| BEGIN_PRINTOUT printout_block caption_block END_PRINTOUT		{`AST_printout ($1, $3, $2)}
	| BEGIN_TABLE table_block caption_block END_TABLE			{`AST_table ($1, $3, $2)}
	| BEGIN_FIGURE figure_block caption_block END_FIGURE			{`AST_figure ($1, $3, $2)}
	| BEGIN_BIB bib_title_block bib_author_block bib_resource_block END_BIB	{`AST_bib ($1, $2, $3, $4)}
	| BEGIN_NOTE nestable_block+ END_NOTE					{`AST_note ($1, $2)}

items:
	| NEW_ITEM nestable_block+						{[`AST_item ($1, $2)]}
	| items NEW_ITEM nestable_block+					{List.append $1 [`AST_item ($2, $3)]}


/********************************************************************************/
/* Definition of individual blocks.						*/
/********************************************************************************/

caption_block:		| CAPTION BEGIN super_node+ END				{`AST_caption ($1, $3)}
paragraph_block:	| NEW_PAR super_node+					{`AST_paragraph ($1, $2)}
itemize_block:		| BEGIN_ITEMIZE items END_ITEMIZE			{`AST_itemize ($1, $2)}
enumerate_block:	| BEGIN_ENUMERATE items END_ENUMERATE			{`AST_enumerate ($1, $2)}
quote_block:		| BEGIN_QUOTE nestable_block+ END_QUOTE			{`AST_quote ($1, $2)}
mathtex_block:		| BEGIN_MATHTEX_BLK RAW END_MATHTEX_BLK			{`AST_mathtex_blk ($1, $2)}
mathml_block:		| BEGIN_MATHML_BLK RAW END_MATHML_BLK			{`AST_mathml_blk ($1, $2)}
code_block:		| BEGIN_CODE RAW END_CODE				{`AST_code ($1, $2)}
verbatim_block:		| BEGIN_VERBATIM RAW END_VERBATIM			{`AST_verbatim ($1, $2)}
tabular_block:		| BEGIN_TABULAR tabular END_TABULAR			{`AST_tabular ($1, $2)}
bitmap_block:		| BITMAP BEGIN RAW END					{`AST_bitmap ($1, $3)}
subpage_block:		| BEGIN_SUBPAGE super_block+ END_SUBPAGE		{`AST_subpage ($1, $2)}
bib_title_block:	| BIB_TITLE BEGIN super_node+ END			{`AST_bib_title ($1, $3)}
bib_author_block:	| BIB_AUTHOR BEGIN super_node+ END			{`AST_bib_author ($1, $3)}
bib_resource_block:	| BIB_RESOURCE BEGIN super_node+ END			{`AST_bib_resource ($1, $3)}


/********************************************************************************/
/* Definition of floaters.							*/
/********************************************************************************/

equation_block:
	| mathtex_block		{($1 :> Ast.M.equation_block_t)}
	| mathml_block		{($1 :> Ast.M.equation_block_t)}

printout_block:
	| code_block		{($1 :> Ast.M.printout_block_t)}

table_block:
	| tabular_block		{($1 :> Ast.M.table_block_t)}

figure_block:
	| bitmap_block		{($1 :> Ast.M.figure_block_t)}
	| verbatim_block	{($1 :> Ast.M.figure_block_t)}
	| subpage_block		{($1 :> Ast.M.figure_block_t)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?			{{Ast.M.thead = $1; Ast.M.tfoot = $3; Ast.M.tbodies = $2;}}
	| row+ body* foot?			{{Ast.M.thead = None; Ast.M.tfoot = $3; Ast.M.tbodies = (None, $1) :: $2;}}


head:
	| HEAD row+				{(Some $1, $2)}


foot:
	| FOOT row+				{(Some $1, $2)}


body:
	| BODY row+				{(Some $1, $2)}


row:
	| columns ROW_END			{($2, $1)}


columns:
	| super_node+				{[$1]}
	| columns COLUMN_SEP super_node+	{List.append $1 [$3]}


/********************************************************************************/
/* Nodes.									*/
/********************************************************************************/

nonlink_node:
	| PLAIN							{`AST_plain $1}
	| ENTITY						{`AST_entity $1}
	| BEGIN_MATHTEX_INL RAW END_MATHTEX_INL			{`AST_mathtex_inl ($1, $2)}
	| BEGIN_MATHML_INL RAW END_MATHML_INL			{`AST_mathml_inl ($1, $2)}
	| BOLD BEGIN super_node+ END				{`AST_bold ($1, $3)}
	| EMPH BEGIN super_node+ END				{`AST_emph ($1, $3)}
	| MONO BEGIN super_node+ END				{`AST_mono ($1, $3)}
	| CAPS BEGIN super_node+ END				{`AST_caps ($1, $3)}
	| THRU BEGIN super_node+ END				{`AST_thru ($1, $3)}
	| SUP BEGIN super_node+ END				{`AST_sup ($1, $3)}
	| SUB BEGIN super_node+ END				{`AST_sub ($1, $3)}
	| MBOX BEGIN super_node+ END				{`AST_mbox ($1, $3)}

link_node:
	| LINK BEGIN RAW END BEGIN nonlink_node+ END		{`AST_link ($1, $3, $6)}
	| SEE BEGIN RAW END					{`AST_see ($1, $3)}
	| CITE BEGIN RAW END					{`AST_cite ($1, $3)}
	| REF BEGIN RAW END					{`AST_ref ($1, $3)}
	| SREF BEGIN RAW END					{`AST_sref ($1, $3)}
	| MREF BEGIN RAW END BEGIN nonlink_node+ END		{`AST_mref ($1, $3, $6)}

super_node:
	| nonlink_node						{($1 :> Ast.M.super_node_t)}
	| link_node						{($1 :> Ast.M.super_node_t)}

