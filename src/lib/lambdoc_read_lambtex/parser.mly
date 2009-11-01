/********************************************************************************/
/*	Parser.mly
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
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

%token <Lambdoc_reader.Ast.command_t> NEW_PAR
%token <Lambdoc_reader.Ast.command_t> COLUMN_SEP
%token <Lambdoc_reader.Ast.command_t> ROW_END


/********************************************************************************/
/* Raw text.									*/
/********************************************************************************/

%token <Lambdoc_core.Basic.raw_t> RAW


/********************************************************************************/
/* Environment operators.  These are used in an inline context.			*/
/* Presently, the only existing environment operators are [$ $] and <$ $>.	*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHTEX_INL
%token <Lambdoc_reader.Ast.command_t> END_MATHTEX_INL

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHML_INL
%token <Lambdoc_reader.Ast.command_t> END_MATHML_INL


/********************************************************************************/
/* Environment commands.  These are used only in a block context.		*/
/* All environment commands are composed of a begin/end pair.			*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t> BEGIN_ITEMIZE
%token <Lambdoc_reader.Ast.command_t> END_ITEMIZE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ITEMIZE_1
%token <Lambdoc_reader.Ast.command_t> END_ITEMIZE_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_ITEMIZE_2
%token <Lambdoc_reader.Ast.command_t> END_ITEMIZE_2

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE_1
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_DESCRIPTION
%token <Lambdoc_reader.Ast.command_t> END_DESCRIPTION

%token <Lambdoc_reader.Ast.command_t> BEGIN_DESCRIPTION_1
%token <Lambdoc_reader.Ast.command_t> END_DESCRIPTION_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_QANDA
%token <Lambdoc_reader.Ast.command_t> END_QANDA

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERSE
%token <Lambdoc_reader.Ast.command_t> END_VERSE

%token <Lambdoc_reader.Ast.command_t> BEGIN_QUOTE
%token <Lambdoc_reader.Ast.command_t> END_QUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHTEX_BLK
%token <Lambdoc_reader.Ast.command_t> END_MATHTEX_BLK

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHML_BLK
%token <Lambdoc_reader.Ast.command_t> END_MATHML_BLK

%token <Lambdoc_reader.Ast.command_t> BEGIN_PROGRAM
%token <Lambdoc_reader.Ast.command_t> END_PROGRAM

%token <Lambdoc_reader.Ast.command_t> BEGIN_TABULAR
%token <Lambdoc_reader.Ast.command_t> END_TABULAR

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM_1
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_SUBPAGE
%token <Lambdoc_reader.Ast.command_t> END_SUBPAGE

%token <Lambdoc_reader.Ast.command_t> BEGIN_PULLQUOTE
%token <Lambdoc_reader.Ast.command_t> END_PULLQUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_BOXOUT
%token <Lambdoc_reader.Ast.command_t> END_BOXOUT

%token <Lambdoc_reader.Ast.command_t> BEGIN_EQUATION
%token <Lambdoc_reader.Ast.command_t> END_EQUATION

%token <Lambdoc_reader.Ast.command_t> BEGIN_PRINTOUT 
%token <Lambdoc_reader.Ast.command_t> END_PRINTOUT

%token <Lambdoc_reader.Ast.command_t> BEGIN_TABLE
%token <Lambdoc_reader.Ast.command_t> END_TABLE

%token <Lambdoc_reader.Ast.command_t> BEGIN_FIGURE
%token <Lambdoc_reader.Ast.command_t> END_FIGURE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ABSTRACT
%token <Lambdoc_reader.Ast.command_t> END_ABSTRACT

%token <Lambdoc_reader.Ast.command_t> BEGIN_BIB
%token <Lambdoc_reader.Ast.command_t> END_BIB

%token <Lambdoc_reader.Ast.command_t> BEGIN_NOTE
%token <Lambdoc_reader.Ast.command_t> END_NOTE


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.plain_t> PLAIN
%token <Lambdoc_reader.Ast.command_t * Lambdoc_reader.Entity.t> ENTITY

%token <Lambdoc_reader.Ast.command_t> LINEBREAK
%token <Lambdoc_reader.Ast.command_t> BOLD
%token <Lambdoc_reader.Ast.command_t> EMPH
%token <Lambdoc_reader.Ast.command_t> CODE
%token <Lambdoc_reader.Ast.command_t> CAPS
%token <Lambdoc_reader.Ast.command_t> INS
%token <Lambdoc_reader.Ast.command_t> DEL
%token <Lambdoc_reader.Ast.command_t> SUP
%token <Lambdoc_reader.Ast.command_t> SUB
%token <Lambdoc_reader.Ast.command_t> MBOX
%token <Lambdoc_reader.Ast.command_t> LINK

%token <Lambdoc_reader.Ast.command_t> SEE
%token <Lambdoc_reader.Ast.command_t> CITE
%token <Lambdoc_reader.Ast.command_t> REF
%token <Lambdoc_reader.Ast.command_t> SREF
%token <Lambdoc_reader.Ast.command_t> MREF

%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.raw_t> MACROARG
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.raw_t> MACROCALL

%token <Lambdoc_reader.Ast.command_t> PARAGRAPH
%token <Lambdoc_reader.Ast.command_t> BITMAP
%token <Lambdoc_reader.Ast.command_t> PART
%token <Lambdoc_reader.Ast.command_t> APPENDIX
%token <Lambdoc_reader.Ast.command_t> SECTION
%token <Lambdoc_reader.Ast.command_t> SUBSECTION
%token <Lambdoc_reader.Ast.command_t> SUBSUBSECTION
%token <Lambdoc_reader.Ast.command_t> BIBLIOGRAPHY
%token <Lambdoc_reader.Ast.command_t> NOTES
%token <Lambdoc_reader.Ast.command_t> TOC
%token <Lambdoc_reader.Ast.command_t> PARHEAD
%token <Lambdoc_reader.Ast.command_t> TITLE
%token <Lambdoc_reader.Ast.command_t> SUBTITLE
%token <Lambdoc_reader.Ast.command_t> RULE
%token <Lambdoc_reader.Ast.command_t> MACRODEF

%token <Lambdoc_reader.Ast.command_t> ITEM
%token <Lambdoc_reader.Ast.command_t> QUESTION
%token <Lambdoc_reader.Ast.command_t> ANSWER

%token <Lambdoc_reader.Ast.command_t> THEAD
%token <Lambdoc_reader.Ast.command_t> TFOOT
%token <Lambdoc_reader.Ast.command_t> TBODY
%token <Lambdoc_reader.Ast.command_t> BIB_AUTHOR
%token <Lambdoc_reader.Ast.command_t> BIB_TITLE
%token <Lambdoc_reader.Ast.command_t> BIB_RESOURCE
%token <Lambdoc_reader.Ast.command_t> CAPTION


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
	| NEW_PAR inline+					{($1, Ast.Paragraph $2)}
	| PARAGRAPH inline_bundle				{($1, Ast.Paragraph $2)}
	| BEGIN_ITEMIZE anon_item_frag+ END_ITEMIZE		{($1, Ast.Itemize $2)}
	| BEGIN_ITEMIZE_1 anon_item_frag+ END_ITEMIZE_1		{($1, Ast.Itemize $2)}
	| BEGIN_ITEMIZE_2 anon_item_frag+ END_ITEMIZE_2		{($1, Ast.Itemize $2)}
	| BEGIN_ENUMERATE anon_item_frag+ END_ENUMERATE		{($1, Ast.Enumerate $2)}
	| BEGIN_ENUMERATE_1 anon_item_frag+ END_ENUMERATE_1	{($1, Ast.Enumerate $2)}
	| BEGIN_DESCRIPTION desc_item_frag+ END_DESCRIPTION	{($1, Ast.Description $2)}
	| BEGIN_DESCRIPTION_1 desc_item_frag+ END_DESCRIPTION_1	{($1, Ast.Description $2)}
	| BEGIN_QANDA qanda_frag+ END_QANDA			{($1, Ast.Qanda $2)}
	| BEGIN_VERSE block+ END_VERSE				{($1, Ast.Verse $2)}
	| BEGIN_QUOTE block+ END_QUOTE				{($1, Ast.Quote $2)}
	| BEGIN_MATHTEX_BLK RAW END_MATHTEX_BLK			{($1, Ast.Mathtex_blk $2)}
	| BEGIN_MATHML_BLK RAW END_MATHML_BLK			{($1, Ast.Mathml_blk $2)}
	| BEGIN_PROGRAM RAW END_PROGRAM				{($1, Ast.Program $2)}
	| BEGIN_TABULAR raw_bundle tabular END_TABULAR		{($1, Ast.Tabular ($2, $3))}
	| BEGIN_VERBATIM RAW END_VERBATIM			{($1, Ast.Verbatim $2)}
	| BEGIN_VERBATIM_1 RAW END_VERBATIM_1			{($1, Ast.Verbatim $2)}
	| BITMAP raw_bundle raw_bundle				{($1, Ast.Bitmap ($2, $3))}
	| BEGIN_SUBPAGE block+ END_SUBPAGE			{($1, Ast.Subpage $2)}
	| BEGIN_PULLQUOTE block+ END_PULLQUOTE			{($1, Ast.Pullquote $2)}
	| BEGIN_BOXOUT inline_bundle? block+ END_BOXOUT		{($1, Ast.Boxout ($2, $3))}
	| BEGIN_EQUATION block caption END_EQUATION		{($1, Ast.Equation ($3, $2))}
	| BEGIN_PRINTOUT block caption END_PRINTOUT		{($1, Ast.Printout ($3, $2))}
	| BEGIN_TABLE block caption END_TABLE			{($1, Ast.Table ($3, $2))}
	| BEGIN_FIGURE block caption END_FIGURE			{($1, Ast.Figure ($3, $2))}
	| PART inline_bundle					{($1, Ast.Part $2)}
	| APPENDIX						{($1, Ast.Appendix)}
	| SECTION inline_bundle					{($1, Ast.Section (`Level1, $2))}
	| SUBSECTION inline_bundle				{($1, Ast.Section (`Level2, $2))}
	| SUBSUBSECTION inline_bundle				{($1, Ast.Section (`Level3, $2))}
	| BIBLIOGRAPHY						{($1, Ast.Bibliography)}
	| NOTES							{($1, Ast.Notes)}
	| TOC							{($1, Ast.Toc)} 
	| PARHEAD inline_bundle					{($1, Ast.Parhead $2)}
	| TITLE inline_bundle					{($1, Ast.Title (`Level1, $2))}
	| SUBTITLE inline_bundle				{($1, Ast.Title (`Level2, $2))}
	| BEGIN_ABSTRACT block+ END_ABSTRACT			{($1, Ast.Abstract $2)}
	| RULE							{($1, Ast.Rule)}
	| BEGIN_BIB bib_author bib_title bib_resource END_BIB	{($1, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
	| BEGIN_NOTE block+ END_NOTE				{($1, Ast.Note $2)}
	| MACRODEF inline_bundle				{($1, Ast.Macrodef $2)}


anon_item_frag:	ITEM block+					{($1, $2)}
desc_item_frag:	ITEM inline_bundle block+			{($1, $2, $3)}
qanda_frag:	question answer					{($1, $2)}
question:	QUESTION inline_bundle0? block+			{($1, $2, $3)}
answer:		ANSWER inline_bundle0? block+			{($1, $2, $3)}
caption:	CAPTION inline_bundle				{($1, $2)}
bib_author:	BIB_AUTHOR inline_bundle			{($1, $2)}
bib_title:	BIB_TITLE inline_bundle				{($1, $2)}
bib_resource:	BIB_RESOURCE inline_bundle			{($1, $2)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?				{{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
	| row+ body* foot?				{{Ast.thead = None; Ast.tfoot = $3; Ast.tbodies = (None, $1) :: $2;}}


head:
	| THEAD row+					{(Some $1, $2)}


foot:
	| TFOOT row+					{(Some $1, $2)}


body:
	| TBODY row+					{(Some $1, $2)}


row:
	| columns ROW_END				{($2, $1)}


columns:
	| inline+					{[$1]}
	| columns COLUMN_SEP inline+			{List.append $1 [$3]}


/********************************************************************************/
/* Inline context.								*/
/********************************************************************************/

inline:
	| PLAIN						{let (comm, txt) = $1 in (comm, Ast.Plain txt)}
	| ENTITY					{let (comm, ent) = $1 in (comm, Ast.Entity ent)}
	| LINEBREAK					{($1, Ast.Linebreak)}
	| BEGIN_MATHTEX_INL RAW END_MATHTEX_INL		{($1, Ast.Mathtex_inl $2)}
	| BEGIN_MATHML_INL RAW END_MATHML_INL		{($1, Ast.Mathml_inl $2)}
	| BOLD inline_bundle				{($1, Ast.Bold $2)}
	| EMPH inline_bundle				{($1, Ast.Emph $2)}
	| CODE inline_bundle				{($1, Ast.Code $2)}
	| CAPS inline_bundle				{($1, Ast.Caps $2)}
	| INS inline_bundle				{($1, Ast.Ins $2)}
	| DEL inline_bundle				{($1, Ast.Del $2)}
	| SUP inline_bundle				{($1, Ast.Sup $2)}
	| SUB inline_bundle				{($1, Ast.Sub $2)}
	| MBOX inline_bundle				{($1, Ast.Mbox $2)}
	| LINK raw_bundle inline_bundle?		{($1, Ast.Link ($2, $3))}
	| SEE raw_bundle				{($1, Ast.See $2)}
	| CITE raw_bundle				{($1, Ast.Cite $2)}
	| REF raw_bundle				{($1, Ast.Ref $2)}
	| SREF raw_bundle				{($1, Ast.Sref $2)}
	| MREF raw_bundle inline_bundle			{($1, Ast.Mref ($2, $3))}
	| MACROARG					{let (comm, txt) = $1 in (comm, Ast.Macroarg txt)}
	| MACROCALL inline_bundle*			{let (comm, txt) = $1 in (comm, Ast.Macrocall (txt, $2))}


/********************************************************************************/
/* Bundles.									*/
/********************************************************************************/

inline_bundle:
	| BEGIN inline+ END				{$2}

inline_bundle0:
	| BEGIN inline* END				{$2}

raw_bundle:
	| BEGIN RAW END					{$2}

