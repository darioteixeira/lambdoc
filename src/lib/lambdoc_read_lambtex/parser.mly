/********************************************************************************/
/*	Parser.mly
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*/
/********************************************************************************/

%{
open Lambdoc_reader
open Globalenv

let the comm = match comm.Ast.comm_tag with
	| Some x -> x
	| None	 -> invalid_arg "the"
%}


/********************************************************************************/
/* Operators.									*/
/********************************************************************************/

%token EOF
%token BEGIN
%token END

%token <Lambdoc_reader.Ast.command_t> NEW_PAR
%token <Lambdoc_reader.Ast.command_t> ROW_END
%token <Lambdoc_reader.Ast.command_t> CELL_MARK

%token <Lambdoc_reader.Ast.command_t * string> PLAIN
%token <Lambdoc_reader.Ast.command_t * string> ENTITY
%token <string> RAW


/********************************************************************************/
/* Environment operators.  These are used in an inline context.			*/
/* Presently the only existing environment operators are [$ $] and <$ $>.	*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHTEX_INL
%token <Lambdoc_reader.Ast.command_t> END_MATHTEX_INL

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHML_INL
%token <Lambdoc_reader.Ast.command_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.		*/
/* All environment commands are composed of a begin/end pair.			*/
/* While the opening tag is different for each environment,			*/
/* END_BLOCK is used as the common termination tag.				*/
/********************************************************************************/

%token <string> BEGIN_ITEMIZE
%token <string> BEGIN_ENUMERATE
%token <string> BEGIN_DESCRIPTION
%token <string> BEGIN_QANDA
%token <string> BEGIN_VERSE
%token <string> BEGIN_QUOTE
%token <string> BEGIN_MATHTEX_BLK
%token <string> BEGIN_MATHML_BLK
%token <string> BEGIN_SOURCE
%token <string> BEGIN_TABULAR
%token <string> BEGIN_SUBPAGE
%token <string> BEGIN_VERBATIM
%token <string> BEGIN_DECOR
%token <string> BEGIN_PULLQUOTE
%token <string> BEGIN_CUSTOM
%token <string> BEGIN_EQUATION
%token <string> BEGIN_PRINTOUT 
%token <string> BEGIN_TABLE
%token <string> BEGIN_FIGURE
%token <string> BEGIN_ABSTRACT
%token <string> BEGIN_BIB
%token <string> BEGIN_NOTE

%token END_BLOCK


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t> LINEBREAK
%token <Lambdoc_reader.Ast.command_t> GLYPH
%token <Lambdoc_reader.Ast.command_t> BOLD
%token <Lambdoc_reader.Ast.command_t> EMPH
%token <Lambdoc_reader.Ast.command_t> CODE
%token <Lambdoc_reader.Ast.command_t> CAPS
%token <Lambdoc_reader.Ast.command_t> INS
%token <Lambdoc_reader.Ast.command_t> DEL
%token <Lambdoc_reader.Ast.command_t> SUP
%token <Lambdoc_reader.Ast.command_t> SUB
%token <Lambdoc_reader.Ast.command_t> MBOX
%token <Lambdoc_reader.Ast.command_t> SPAN
%token <Lambdoc_reader.Ast.command_t> UREF
%token <Lambdoc_reader.Ast.command_t> BREF
%token <Lambdoc_reader.Ast.command_t> NREF
%token <Lambdoc_reader.Ast.command_t> CREF
%token <Lambdoc_reader.Ast.command_t> DREF
%token <Lambdoc_reader.Ast.command_t> SREF
%token <Lambdoc_reader.Ast.command_t> MREF

%token <Lambdoc_reader.Ast.command_t> PARAGRAPH
%token <Lambdoc_reader.Ast.command_t> PICTURE
%token <Lambdoc_reader.Ast.command_t> BOOK
%token <Lambdoc_reader.Ast.command_t> PART
%token <Lambdoc_reader.Ast.command_t> APPENDIX
%token <Lambdoc_reader.Ast.command_t> SECTION
%token <Lambdoc_reader.Ast.command_t> SUBSECTION
%token <Lambdoc_reader.Ast.command_t> SUBSUBSECTION
%token <Lambdoc_reader.Ast.command_t> BIBLIOGRAPHY
%token <Lambdoc_reader.Ast.command_t> NOTES
%token <Lambdoc_reader.Ast.command_t> TOC
%token <Lambdoc_reader.Ast.command_t> TITLE
%token <Lambdoc_reader.Ast.command_t> SUBTITLE
%token <Lambdoc_reader.Ast.command_t> RULE
%token <Lambdoc_reader.Ast.command_t> MACRODEF
%token <Lambdoc_reader.Ast.command_t> BOXOUTDEF
%token <Lambdoc_reader.Ast.command_t> THEOREMDEF

%token <Lambdoc_reader.Ast.command_t> ITEM
%token <Lambdoc_reader.Ast.command_t> QUESTION
%token <Lambdoc_reader.Ast.command_t> RQUESTION
%token <Lambdoc_reader.Ast.command_t> ANSWER
%token <Lambdoc_reader.Ast.command_t> RANSWER

%token <Lambdoc_reader.Ast.command_t> THEAD
%token <Lambdoc_reader.Ast.command_t> TFOOT
%token <Lambdoc_reader.Ast.command_t> TBODY
%token <Lambdoc_reader.Ast.command_t> BIB_AUTHOR
%token <Lambdoc_reader.Ast.command_t> BIB_TITLE
%token <Lambdoc_reader.Ast.command_t> BIB_RESOURCE

%token <Lambdoc_reader.Ast.command_t> MACROARG
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t > MACROCALL


/********************************************************************************/
/* Dummy tokens.								*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t> BEGIN_DUMMY
%token <string> END_DUMMY
%token OPEN_DUMMY
%token CLOSE_DUMMY


/********************************************************************************/
/* Type declarations.								*/
/********************************************************************************/

%type <Lambdoc_reader.Ast.t> document


/********************************************************************************/
/* Begin grammar specification and declare top-level rules.			*/
/********************************************************************************/

%start document

%%

document:
	| block* EOF	{$1}


/********************************************************************************/
/* Blocks.									*/
/********************************************************************************/

block:
	| NEW_PAR inline+							{($1, Ast.Paragraph $2)}
	| simple_block								{$1}
	| env_block								{$1}

simple_block:
	| PARAGRAPH inline_bundle						{($1, Ast.Paragraph $2)}
	| PICTURE raw_bundle raw_bundle						{($1, Ast.Picture ($2, $3))}
	| BOOK raw_bundle							{($1, Ast.Book $2)}
	| PART inline_bundle							{($1, Ast.Part $2)}
	| APPENDIX								{($1, Ast.Appendix)}
	| SECTION inline_bundle							{($1, Ast.Section (`Level1, $2))}
	| SUBSECTION inline_bundle						{($1, Ast.Section (`Level2, $2))}
	| SUBSUBSECTION inline_bundle						{($1, Ast.Section (`Level3, $2))}
	| BIBLIOGRAPHY								{($1, Ast.Bibliography)}
	| NOTES									{($1, Ast.Notes)}
	| TOC									{($1, Ast.Toc)} 
	| TITLE inline_bundle							{($1, Ast.Title (`Level1, $2))}
	| SUBTITLE inline_bundle						{($1, Ast.Title (`Level2, $2))}
	| RULE									{($1, Ast.Rule)}
	| MACRODEF raw_bundle raw_bundle inline_bundle				{($1, Ast.Macrodef ($2, $3, $4))}
	| BOXOUTDEF raw_bundle boxoutdef					{let (caption, counter) = $3 in ($1, Ast.Boxoutdef ($2, caption, counter))}
	| THEOREMDEF raw_bundle theoremdef					{let (caption, counter) = $3 in ($1, Ast.Theoremdef ($2, caption, counter))}

env_block:
	| begin_block(blk_itemize) anon_item_frag* end_block			{($1, Ast.Itemize $2)}
	| begin_block(blk_enumerate) anon_item_frag* end_block			{($1, Ast.Enumerate $2)}
	| begin_block(blk_description) desc_item_frag* end_block		{($1, Ast.Description $2)}
	| begin_block(blk_qanda) qanda_frag* end_block				{($1, Ast.Qanda $2)}
	| begin_block(blk_verse) block* end_block				{($1, Ast.Verse $2)}
	| begin_block(blk_quote) block* end_block				{($1, Ast.Quote $2)}
	| begin_block(blk_mathtex_blk) RAW end_block				{($1, Ast.Mathtex_blk $2)}
	| begin_block(blk_mathml_blk) RAW end_block				{($1, Ast.Mathml_blk $2)}
	| begin_block(blk_source) RAW end_block					{($1, Ast.Source $2)}
	| begin_block(blk_tabular) raw_bundle tabular end_block			{($1, Ast.Tabular ($2, $3))}
	| begin_block(blk_subpage) block* end_block				{($1, Ast.Subpage $2)}
	| begin_block(blk_verbatim) RAW end_block				{($1, Ast.Verbatim $2)}
	| begin_block(blk_decor) block end_block				{($1, Ast.Decor $2)}
	| begin_block(blk_pullquote) inline_bundle? block* end_block		{($1, Ast.Pullquote ($2, $3))}
	| begin_block(blk_custom) inline_bundle? block* end_block		{($1, Ast.Custom (None, the $1, $2, $3))}
	| begin_block(blk_equation) inline_bundle? block end_block		{($1, Ast.Equation ($2, $3))}
	| begin_block(blk_printout) inline_bundle? block end_block		{($1, Ast.Printout ($2, $3))}
	| begin_block(blk_table) inline_bundle? block end_block			{($1, Ast.Table ($2, $3))}
	| begin_block(blk_figure) inline_bundle? block end_block		{($1, Ast.Figure ($2, $3))}
	| begin_block(blk_abstract) block* end_block				{($1, Ast.Abstract $2)}
	| begin_block(blk_bib) bib_author bib_title bib_resource end_block	{($1, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
	| begin_block(blk_note) block* end_block				{($1, Ast.Note $2)}

anon_item_frag:
	| ITEM block*								{($1, $2)}

desc_item_frag:
	| ITEM inline_bundle block*						{($1, $2, $3)}

qanda_frag:
	| question answer							{($1, $2)}

question:
	| QUESTION inline_bundle? block*					{($1, Ast.Different $2, $3)}
	| RQUESTION block*							{($1, Ast.Repeated, $2)}

answer:
	| ANSWER inline_bundle? block*						{($1, Ast.Different $2, $3)}
	| RANSWER block*							{($1, Ast.Repeated, $2)}

bib_author:
	| BIB_AUTHOR inline_bundle						{($1, $2)}

bib_title:
	| BIB_TITLE inline_bundle						{($1, $2)}

bib_resource:
	| BIB_RESOURCE inline_bundle						{($1, $2)}

boxoutdef:
	| /* empty */								{(None, None)}
	| inline_bundle								{(Some $1, None)}
	| inline_bundle raw_bundle						{(Some $1, Some $2)}

theoremdef:
	| inline_bundle								{($1, None)}
	| inline_bundle raw_bundle						{($1, Some $2)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?							{{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
	| row+ body* foot?							{{Ast.thead = None; Ast.tfoot = $3; Ast.tbodies = (None, $1) :: $2;}}

head:	THEAD row+								{(Some $1, $2)}
foot:	TFOOT row+								{(Some $1, $2)}
body:	TBODY row+								{(Some $1, $2)}
row:	cell+ ROW_END								{($2, $1)}
cell:	CELL_MARK raw_bundle? option(inline+)					{($1, $2, $3)}


/********************************************************************************/
/* Inline context.								*/
/********************************************************************************/

inline:
	| PLAIN										{let (comm, txt) = $1 in (comm, Ast.Plain txt)}
	| ENTITY									{let (comm, ent) = $1 in (comm, Ast.Entity ent)}
	| LINEBREAK									{($1, Ast.Linebreak)}
	| BEGIN_MATHTEX_INL push(mathtex_inl) OPEN_DUMMY RAW pop_brk END_MATHTEX_INL	{($1, Ast.Mathtex_inl $4)}
	| BEGIN_MATHML_INL push(mathml_inl) OPEN_DUMMY RAW pop_brk END_MATHML_INL	{($1, Ast.Mathml_inl $4)}
	| GLYPH raw_bundle raw_bundle							{($1, Ast.Glyph ($2, $3))}
	| BOLD inline_bundle								{($1, Ast.Bold $2)}
	| EMPH inline_bundle								{($1, Ast.Emph $2)}
	| CODE inline_bundle								{($1, Ast.Code $2)}
	| CAPS inline_bundle								{($1, Ast.Caps $2)}
	| INS inline_bundle								{($1, Ast.Ins $2)}
	| DEL inline_bundle								{($1, Ast.Del $2)}
	| SUP inline_bundle								{($1, Ast.Sup $2)}
	| SUB inline_bundle								{($1, Ast.Sub $2)}
	| MBOX inline_bundle								{($1, Ast.Mbox $2)}
	| SPAN inline_bundle								{($1, Ast.Span $2)}
	| UREF raw_bundle inline_bundle?						{($1, Ast.Uref ($2, $3))}
	| BREF raw_bundle inline_bundle?						{($1, Ast.Bref ($2, $3))}
	| NREF raw_bundle*								{($1, Ast.Nref $2)}
	| CREF raw_bundle*								{($1, Ast.Cref $2)}
	| DREF raw_bundle								{($1, Ast.Dref $2)}
	| SREF raw_bundle								{($1, Ast.Sref $2)}
	| MREF raw_bundle inline_bundle							{($1, Ast.Mref ($2, $3))}
	| MACROARG raw_bundle								{($1, Ast.Macroarg $2)}
	| MACROCALL inline_bundle*							{let (comm, label) = $1 in (comm, Ast.Macrocall (label, $2))}


/********************************************************************************/
/* Bundles.									*/
/********************************************************************************/

inline_bundle: 		BEGIN push(general) OPEN_DUMMY inline* pop_brk END	{$4}
raw_bundle: 		BEGIN push(raw) OPEN_DUMMY RAW pop_brk END		{$4}


/********************************************************************************/
/* Dummy actions.								*/
/********************************************************************************/

begin_block(x):		push(x) BEGIN_DUMMY					{$2}
end_block:		pop_blk END_BLOCK					{$2}

push(x):		x							{Globalenv.push $1}
pop_blk:		END_DUMMY						{Globalenv.pop (Some $1)}
pop_brk:		CLOSE_DUMMY						{Globalenv.pop None}


/********************************************************************************/

general:		/* empty */						{(None, General)}
raw:			/* empty */						{(None, Raw)}
mathtex_inl:		/* empty */						{(None, Mathtex_inl)}
mathml_inl:		/* empty */						{(None, Mathml_inl)}


/********************************************************************************/

blk_itemize:		BEGIN_ITEMIZE						{(Some $1, General)}
blk_enumerate:		BEGIN_ENUMERATE						{(Some $1, General)}
blk_description:	BEGIN_DESCRIPTION					{(Some $1, General)}
blk_qanda:		BEGIN_QANDA						{(Some $1, General)}
blk_verse:		BEGIN_VERSE						{(Some $1, General)}
blk_quote:		BEGIN_QUOTE						{(Some $1, General)}
blk_mathtex_blk:	BEGIN_MATHTEX_BLK					{(Some $1, Literal $1)}
blk_mathml_blk:		BEGIN_MATHML_BLK					{(Some $1, Literal $1)}
blk_source:		BEGIN_SOURCE						{(Some $1, Literal $1)}
blk_tabular:		BEGIN_TABULAR						{(Some $1, Tabular)}
blk_subpage:		BEGIN_SUBPAGE						{(Some $1, General)}
blk_verbatim:		BEGIN_VERBATIM						{(Some $1, Literal $1)}
blk_decor:		BEGIN_DECOR						{(Some $1, General)}
blk_pullquote:		BEGIN_PULLQUOTE						{(Some $1, General)}
blk_custom:		BEGIN_CUSTOM						{(Some $1, General)}
blk_equation:		BEGIN_EQUATION						{(Some $1, General)}
blk_printout:		BEGIN_PRINTOUT 						{(Some $1, General)}
blk_table:		BEGIN_TABLE						{(Some $1, General)}
blk_figure:		BEGIN_FIGURE						{(Some $1, General)}
blk_abstract:		BEGIN_ABSTRACT						{(Some $1, General)}
blk_bib:		BEGIN_BIB						{(Some $1, General)}
blk_note:		BEGIN_NOTE						{(Some $1, General)}

