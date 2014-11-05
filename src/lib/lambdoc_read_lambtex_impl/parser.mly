/********************************************************************************/
/*	Parser.mly
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*/
/********************************************************************************/

%{
open Lambdoc_reader
open Globalenv
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

%token <Lambdoc_reader.Ast.command_t * BatText.t> PLAIN
%token <Lambdoc_reader.Ast.command_t * string> ENTITY
%token <BatText.t> RAW


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
%token <string> BEGIN_PULLQUOTE
%token <string> BEGIN_CUSTOM
%token <string> BEGIN_EQUATION
%token <string> BEGIN_PRINTOUT 
%token <string> BEGIN_TABLE
%token <string> BEGIN_FIGURE
%token <string> BEGIN_ABSTRACT
%token <string> BEGIN_BIB
%token <string> BEGIN_NOTE

%token <string> BEGIN_EXTBLK_ENVRAW
%token <string> BEGIN_EXTBLK_ENVRAWRAW
%token <string> BEGIN_EXTBLK_ENVSEQRAW
%token <string> BEGIN_EXTBLK_ENVRAWOPTRAW
%token <string> BEGIN_EXTBLK_ENVSEQOPTRAW

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
%token <Lambdoc_reader.Ast.command_t> LINK
%token <Lambdoc_reader.Ast.command_t> SEE
%token <Lambdoc_reader.Ast.command_t> CITE
%token <Lambdoc_reader.Ast.command_t> DREF
%token <Lambdoc_reader.Ast.command_t> SREF
%token <Lambdoc_reader.Ast.command_t> MREF

%token <Lambdoc_reader.Ast.command_t> PARAGRAPH
%token <Lambdoc_reader.Ast.command_t> PICTURE
%token <Lambdoc_reader.Ast.command_t> PART
%token <Lambdoc_reader.Ast.command_t> APPENDIX
%token <Lambdoc_reader.Ast.command_t * int> SECTION
%token <Lambdoc_reader.Ast.command_t> BIBLIOGRAPHY
%token <Lambdoc_reader.Ast.command_t> NOTES
%token <Lambdoc_reader.Ast.command_t> TOC
%token <Lambdoc_reader.Ast.command_t * int> TITLE
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

%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTINL_SIMSEQ
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTINL_SIMRAW
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTINL_SIMRAWSEQ
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTINL_SIMRAWSEQOPT

%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTBLK_SIMSEQ
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.Ident.t> EXTBLK_SIMRAW


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
	| PART inline_bundle							{($1, Ast.Part $2)}
	| APPENDIX								{($1, Ast.Appendix)}
	| SECTION inline_bundle							{let (comm, level) = $1 in (comm, Ast.Section (level, $2))}
	| BIBLIOGRAPHY								{($1, Ast.Bibliography)}
	| NOTES									{($1, Ast.Notes)}
	| TOC									{($1, Ast.Toc)} 
	| TITLE inline_bundle							{let (comm, level) = $1 in (comm, Ast.Title (level, $2))}
	| RULE									{($1, Ast.Rule)}
	| MACRODEF raw_bundle raw_bundle inline_bundle				{($1, Ast.Macrodef ($2, $3, $4))}
	| BOXOUTDEF raw_bundle boxoutdef					{let (caption, counter) = $3 in ($1, Ast.Boxoutdef ($2, caption, counter))}
	| THEOREMDEF raw_bundle theoremdef					{let (caption, counter) = $3 in ($1, Ast.Theoremdef ($2, caption, counter))}
	| sim_extblk								{let (comm, tag, extblk) = $1 in (comm, Ast.Extblk (tag, extblk))}

sim_extblk:
	| EXTBLK_SIMSEQ inline_bundle						{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_simseq $2)}
	| EXTBLK_SIMRAW raw_bundle						{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_simraw $2)}

env_block:
	| begin_block(blk_itemize) anon_item_frag* end_block			{let (comm, _) = $1 in (comm, Ast.Itemize $2)}
	| begin_block(blk_enumerate) anon_item_frag* end_block			{let (comm, _) = $1 in (comm, Ast.Enumerate $2)}
	| begin_block(blk_description) desc_item_frag* end_block		{let (comm, _) = $1 in (comm, Ast.Description $2)}
	| begin_block(blk_qanda) qanda_frag* end_block				{let (comm, _) = $1 in (comm, Ast.Qanda $2)}
	| begin_block(blk_verse) block* end_block				{let (comm, _) = $1 in (comm, Ast.Verse $2)}
	| begin_block(blk_quote) block* end_block				{let (comm, _) = $1 in (comm, Ast.Quote $2)}
	| begin_block(blk_mathtex_blk) RAW end_block				{let (comm, _) = $1 in (comm, Ast.Mathtex_blk (BatText.to_string $2))}
	| begin_block(blk_mathml_blk) RAW end_block				{let (comm, _) = $1 in (comm, Ast.Mathml_blk (BatText.to_string $2))}
	| begin_block(blk_source) RAW end_block					{let (comm, _) = $1 in (comm, Ast.Source (BatText.to_string $2))}
	| begin_block(blk_tabular) raw_bundle tabular end_block			{let (comm, _) = $1 in (comm, Ast.Tabular ($2, $3))}
	| begin_block(blk_subpage) block* end_block				{let (comm, _) = $1 in (comm, Ast.Subpage $2)}
	| begin_block(blk_verbatim) RAW end_block				{let (comm, _) = $1 in (comm, Ast.Verbatim (BatText.to_string $2))}
	| begin_block(blk_pullquote) inline_bundle? block* end_block		{let (comm, _) = $1 in (comm, Ast.Pullquote ($2, $3))}
	| begin_block(blk_custom) inline_bundle? block* end_block		{let (comm, tag) = $1 in (comm, Ast.Custom (None, tag, $2, $3))}
	| begin_block(blk_equation) inline_bundle? block end_block		{let (comm, _) = $1 in (comm, Ast.Equation ($2, $3))}
	| begin_block(blk_printout) inline_bundle? block end_block		{let (comm, _) = $1 in (comm, Ast.Printout ($2, $3))}
	| begin_block(blk_table) inline_bundle? block end_block			{let (comm, _) = $1 in (comm, Ast.Table ($2, $3))}
	| begin_block(blk_figure) inline_bundle? block end_block		{let (comm, _) = $1 in (comm, Ast.Figure ($2, $3))}
	| begin_block(blk_abstract) block* end_block				{let (comm, _) = $1 in (comm, Ast.Abstract $2)}
	| begin_block(blk_bib) bib_author bib_title bib_resource end_block	{let (comm, _) = $1 in (comm, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
	| begin_block(blk_note) block* end_block				{let (comm, _) = $1 in (comm, Ast.Note $2)}
	| env_extblk								{let (comm, tag, extblk) = $1 in (comm, Ast.Extblk (tag, extblk))}

anon_item_frag:
	| ITEM block*								{($1, $2)}

desc_item_frag:
	| ITEM inline_bundle block*						{($1, $2, $3)}

qanda_frag:
	| QUESTION inline_bundle? block*					{($1, Ast.New_questioner $2, $3)}
	| RQUESTION block*							{($1, Ast.Same_questioner, $2)}
	| ANSWER inline_bundle? block*						{($1, Ast.New_answerer $2, $3)}
	| RANSWER block*							{($1, Ast.Same_answerer, $2)}

env_extblk:
	| begin_block(extblk_envraw) RAW end_block				{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_envraw (BatText.to_string $2))}
	| begin_block(extblk_envseqraw) inline_bundle RAW end_block		{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_envseqraw ($2, BatText.to_string $3))}
	| begin_block(extblk_envrawraw) raw_bundle RAW end_block		{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_envrawraw ($2, BatText.to_string $3))}
	| begin_block(extblk_envseqoptraw) inline_bundle? RAW end_block		{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_envseqoptraw ($2, BatText.to_string $3))}
	| begin_block(extblk_envrawoptraw) raw_bundle? RAW end_block		{let (comm, tag) = $1 in (comm, tag, Ast.Extblk_envrawoptraw ($2, BatText.to_string $3))}

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
	| PLAIN										{let (comm, txt) = $1 in (comm, Ast.Plain (BatText.to_string txt))}
	| ENTITY									{let (comm, ent) = $1 in (comm, Ast.Entity ent)}
	| LINEBREAK									{($1, Ast.Linebreak)}
	| BEGIN_MATHTEX_INL push(mathtex_inl) OPEN_DUMMY RAW pop_brk END_MATHTEX_INL	{($1, Ast.Mathtex_inl (BatText.to_string $4))}
	| BEGIN_MATHML_INL push(mathml_inl) OPEN_DUMMY RAW pop_brk END_MATHML_INL	{($1, Ast.Mathml_inl (BatText.to_string $4))}
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
	| LINK raw_bundle inline_bundle?						{($1, Ast.Link ($2, $3))}
	| SEE raw_bundle*								{($1, Ast.See $2)}
	| CITE raw_bundle*								{($1, Ast.Cite $2)}
	| DREF raw_bundle inline_bundle?						{($1, Ast.Dref ($2, $3))}
	| SREF raw_bundle inline_bundle?						{($1, Ast.Sref ($2, $3))}
	| MREF raw_bundle inline_bundle							{($1, Ast.Mref ($2, $3))}
	| MACROARG raw_bundle								{($1, Ast.Macroarg $2)}
	| MACROCALL inline_bundle*							{let (comm, label) = $1 in (comm, Ast.Macrocall (label, $2))}
	| sim_extinl									{let (comm, tag, extinl) = $1 in (comm, Ast.Extinl (tag, extinl))}

sim_extinl:
	| EXTINL_SIMSEQ inline_bundle							{let (comm, tag) = $1 in (comm, tag, Ast.Extinl_simseq $2)}
	| EXTINL_SIMRAW raw_bundle							{let (comm, tag) = $1 in (comm, tag, Ast.Extinl_simraw $2)}
	| EXTINL_SIMRAWSEQ raw_bundle inline_bundle					{let (comm, tag) = $1 in (comm, tag, Ast.Extinl_simrawseq ($2, $3))}
	| EXTINL_SIMRAWSEQOPT raw_bundle inline_bundle?					{let (comm, tag) = $1 in (comm, tag, Ast.Extinl_simrawseqopt ($2, $3))}


/********************************************************************************/
/* Bundles.									*/
/********************************************************************************/

inline_bundle: 		BEGIN push(general) OPEN_DUMMY inline* pop_brk END	{$4}
raw_bundle: 		BEGIN push(raw) OPEN_DUMMY RAW pop_brk END		{BatText.to_string $4}


/********************************************************************************/
/* Dummy actions.								*/
/********************************************************************************/

begin_block(x):		push_blk(x) BEGIN_DUMMY					{($2, $1)}
end_block:		pop_blk END_BLOCK					{$2}

push_blk(x):		x							{let (tag, scanner) = $1 in Globalenv.push (Some tag, scanner); tag}
push(x):		x							{Globalenv.push $1}
pop_blk:		END_DUMMY						{Globalenv.pop (Some $1)}
pop_brk:		CLOSE_DUMMY						{Globalenv.pop None}


/********************************************************************************/

general:		/* empty */						{(None, Scanner_general)}
raw:			/* empty */						{(None, Scanner_raw)}
mathtex_inl:		/* empty */						{(None, Scanner_mathtex_inl)}
mathml_inl:		/* empty */						{(None, Scanner_mathml_inl)}


/********************************************************************************/

blk_itemize:			BEGIN_ITEMIZE					{($1, Scanner_general)}
blk_enumerate:			BEGIN_ENUMERATE					{($1, Scanner_general)}
blk_description:		BEGIN_DESCRIPTION				{($1, Scanner_general)}
blk_qanda:			BEGIN_QANDA					{($1, Scanner_general)}
blk_verse:			BEGIN_VERSE					{($1, Scanner_general)}
blk_quote:			BEGIN_QUOTE					{($1, Scanner_general)}
blk_mathtex_blk:		BEGIN_MATHTEX_BLK				{($1, Scanner_literal $1)}
blk_mathml_blk:			BEGIN_MATHML_BLK				{($1, Scanner_literal $1)}
blk_source:			BEGIN_SOURCE					{($1, Scanner_literal $1)}
blk_tabular:			BEGIN_TABULAR					{($1, Scanner_tabular)}
blk_subpage:			BEGIN_SUBPAGE					{($1, Scanner_general)}
blk_verbatim:			BEGIN_VERBATIM					{($1, Scanner_literal $1)}
blk_pullquote:			BEGIN_PULLQUOTE					{($1, Scanner_general)}
blk_custom:			BEGIN_CUSTOM					{($1, Scanner_general)}
blk_equation:			BEGIN_EQUATION					{($1, Scanner_general)}
blk_printout:			BEGIN_PRINTOUT 					{($1, Scanner_general)}
blk_table:			BEGIN_TABLE					{($1, Scanner_general)}
blk_figure:			BEGIN_FIGURE					{($1, Scanner_general)}
blk_abstract:			BEGIN_ABSTRACT					{($1, Scanner_general)}
blk_bib:			BEGIN_BIB					{($1, Scanner_general)}
blk_note:			BEGIN_NOTE					{($1, Scanner_general)}

extblk_envraw:			BEGIN_EXTBLK_ENVRAW				{($1, Scanner_literal $1)}
extblk_envseqraw:		BEGIN_EXTBLK_ENVSEQRAW				{($1, Scanner_literal $1)}
extblk_envrawraw:		BEGIN_EXTBLK_ENVRAWRAW				{($1, Scanner_literal $1)}
extblk_envseqoptraw:		BEGIN_EXTBLK_ENVSEQOPTRAW			{($1, Scanner_literal $1)}
extblk_envrawoptraw:		BEGIN_EXTBLK_ENVRAWOPTRAW			{($1, Scanner_literal $1)}

