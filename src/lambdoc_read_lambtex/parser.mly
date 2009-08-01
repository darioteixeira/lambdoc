/********************************************************************************/
/*	Lambtex_parser.mly
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
/* Basic elements.								*/
/********************************************************************************/

%token <Lambdoc_core.Basic.raw_t> RAW
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.plain_t> PLAIN
%token <Lambdoc_reader.Ast.command_t * Lambdoc_reader.Entity.t> ENTITY


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

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE_1
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_DESCRIPTION
%token <Lambdoc_reader.Ast.command_t> END_DESCRIPTION

%token <Lambdoc_reader.Ast.command_t> BEGIN_DESCRIPTION_1
%token <Lambdoc_reader.Ast.command_t> END_DESCRIPTION_1

%token <Lambdoc_reader.Ast.command_t> BEGIN_QUOTE
%token <Lambdoc_reader.Ast.command_t> END_QUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_PULLQUOTE
%token <Lambdoc_reader.Ast.command_t> END_PULLQUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_BOXOUT
%token <Lambdoc_reader.Ast.command_t> END_BOXOUT

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHTEX_BLK
%token <Lambdoc_reader.Ast.command_t> END_MATHTEX_BLK

%token <Lambdoc_reader.Ast.command_t> BEGIN_MATHML_BLK
%token <Lambdoc_reader.Ast.command_t> END_MATHML_BLK

%token <Lambdoc_reader.Ast.command_t> BEGIN_CODE
%token <Lambdoc_reader.Ast.command_t> END_CODE

%token <Lambdoc_reader.Ast.command_t> BEGIN_TABULAR
%token <Lambdoc_reader.Ast.command_t> END_TABULAR

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.command_t> BEGIN_SUBPAGE
%token <Lambdoc_reader.Ast.command_t> END_SUBPAGE

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

%token <Lambdoc_reader.Ast.command_t> LINEBREAK
%token <Lambdoc_reader.Ast.command_t> BOLD
%token <Lambdoc_reader.Ast.command_t> EMPH
%token <Lambdoc_reader.Ast.command_t> MONO
%token <Lambdoc_reader.Ast.command_t> CAPS
%token <Lambdoc_reader.Ast.command_t> THRU
%token <Lambdoc_reader.Ast.command_t> SUP
%token <Lambdoc_reader.Ast.command_t> SUB
%token <Lambdoc_reader.Ast.command_t> MBOX
%token <Lambdoc_reader.Ast.command_t> LINK
%token <Lambdoc_reader.Ast.command_t> SEE
%token <Lambdoc_reader.Ast.command_t> CITE
%token <Lambdoc_reader.Ast.command_t> REF
%token <Lambdoc_reader.Ast.command_t> SREF
%token <Lambdoc_reader.Ast.command_t> MREF

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

%token <Lambdoc_reader.Ast.command_t> ITEM
%token <Lambdoc_reader.Ast.command_t> DESCRIBE
%token <Lambdoc_reader.Ast.command_t> BITMAP
%token <Lambdoc_reader.Ast.command_t> CAPTION
%token <Lambdoc_reader.Ast.command_t> HEAD
%token <Lambdoc_reader.Ast.command_t> FOOT
%token <Lambdoc_reader.Ast.command_t> BODY
%token <Lambdoc_reader.Ast.command_t> BIB_AUTHOR
%token <Lambdoc_reader.Ast.command_t> BIB_TITLE
%token <Lambdoc_reader.Ast.command_t> BIB_RESOURCE


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
	| BEGIN_ITEMIZE item_frag END_ITEMIZE			{($1, Ast.Itemize $2)}
	| BEGIN_ITEMIZE_1 item_frag END_ITEMIZE_1		{($1, Ast.Itemize $2)}
	| BEGIN_ENUMERATE item_frag END_ENUMERATE		{($1, Ast.Enumerate $2)}
	| BEGIN_ENUMERATE_1 item_frag END_ENUMERATE_1		{($1, Ast.Enumerate $2)}
	| BEGIN_DESCRIPTION describe_frag END_DESCRIPTION	{($1, Ast.Description $2)}
	| BEGIN_DESCRIPTION_1 describe_frag END_DESCRIPTION_1	{($1, Ast.Description $2)}
	| BEGIN_QUOTE block+ END_QUOTE				{($1, Ast.Quote $2)}
	| BEGIN_PULLQUOTE block+ END_PULLQUOTE			{($1, Ast.Pullquote $2)}
	| BEGIN_BOXOUT BEGIN inline+ END block+ END_BOXOUT	{($1, Ast.Boxout ((match $3 with [] -> None | x -> Some x), $5))}
	| BEGIN_MATHTEX_BLK RAW END_MATHTEX_BLK			{($1, Ast.Mathtex_blk $2)}
	| BEGIN_MATHML_BLK RAW END_MATHML_BLK			{($1, Ast.Mathml_blk $2)}
	| BEGIN_CODE RAW END_CODE				{($1, Ast.Code $2)}
	| BEGIN_TABULAR BEGIN RAW END tabular END_TABULAR	{($1, Ast.Tabular ($3, $5))}
	| BEGIN_VERBATIM RAW END_VERBATIM			{($1, Ast.Verbatim $2)}
	| BITMAP BEGIN RAW END BEGIN RAW END			{($1, Ast.Bitmap ($3, $6))}
	| BEGIN_SUBPAGE block+ END_SUBPAGE			{($1, Ast.Subpage $2)}
	| BEGIN_EQUATION block caption END_EQUATION		{($1, Ast.Equation ($3, $2))}
	| BEGIN_PRINTOUT block caption END_PRINTOUT		{($1, Ast.Printout ($3, $2))}
	| BEGIN_TABLE block caption END_TABLE			{($1, Ast.Table ($3, $2))}
	| BEGIN_FIGURE block caption END_FIGURE			{($1, Ast.Figure ($3, $2))}
	| PART BEGIN inline+ END				{($1, Ast.Part $3)}
	| APPENDIX						{($1, Ast.Appendix)}
	| SECTION BEGIN inline+ END				{($1, Ast.Section (`Level1, $3))}
	| SUBSECTION BEGIN inline+ END				{($1, Ast.Section (`Level2, $3))}
	| SUBSUBSECTION BEGIN inline+ END			{($1, Ast.Section (`Level3, $3))}
	| BIBLIOGRAPHY						{($1, Ast.Bibliography)}
	| NOTES							{($1, Ast.Notes)}
	| TOC							{($1, Ast.Toc)} 
	| TITLE BEGIN inline+ END				{($1, Ast.Title (`Level1, $3))}
	| SUBTITLE BEGIN inline+ END				{($1, Ast.Title (`Level2, $3))}
	| BEGIN_ABSTRACT block+ END_ABSTRACT			{($1, Ast.Abstract $2)}
	| RULE							{($1, Ast.Rule)}
	| BEGIN_BIB bib_author bib_title bib_resource END_BIB	{($1, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
	| BEGIN_NOTE block+ END_NOTE				{($1, Ast.Note $2)}


item_frag:
	| ITEM block+						{[($1, $2)]}
	| item_frag ITEM block+					{List.append $1 [($2, $3)]}


describe_frag:
	| DESCRIBE BEGIN inline+ END block+			{[($1, $3, $5)]}
	| describe_frag DESCRIBE BEGIN inline+ END block+	{List.append $1 [($2, $4, $6)]}


caption:	CAPTION BEGIN inline+ END			{($1, $3)}
bib_author:	BIB_AUTHOR BEGIN inline+ END			{($1, $3)}
bib_title:	BIB_TITLE BEGIN inline+ END			{($1, $3)}
bib_resource:	BIB_RESOURCE BEGIN inline+ END			{($1, $3)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular:
	| head? body+ foot?				{{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
	| row+ body* foot?				{{Ast.thead = None; Ast.tfoot = $3; Ast.tbodies = (None, $1) :: $2;}}


head:
	| HEAD row+					{(Some $1, $2)}


foot:
	| FOOT row+					{(Some $1, $2)}


body:
	| BODY row+					{(Some $1, $2)}


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
	| BOLD BEGIN inline+ END			{($1, Ast.Bold $3)}
	| EMPH BEGIN inline+ END			{($1, Ast.Emph $3)}
	| MONO BEGIN inline+ END			{($1, Ast.Mono $3)}
	| CAPS BEGIN inline+ END			{($1, Ast.Caps $3)}
	| THRU BEGIN inline+ END			{($1, Ast.Thru $3)}
	| SUP BEGIN inline+ END				{($1, Ast.Sup $3)}
	| SUB BEGIN inline+ END				{($1, Ast.Sub $3)}
	| MBOX BEGIN inline+ END			{($1, Ast.Mbox $3)}
	| LINK BEGIN RAW END BEGIN inline* END		{($1, Ast.Link ($3, $6))}
	| SEE BEGIN RAW END				{($1, Ast.See $3)}
	| CITE BEGIN RAW END				{($1, Ast.Cite $3)}
	| REF BEGIN RAW END				{($1, Ast.Ref $3)}
	| SREF BEGIN RAW END				{($1, Ast.Sref $3)}
	| MREF BEGIN RAW END BEGIN inline+ END		{($1, Ast.Mref ($3, $6))}

