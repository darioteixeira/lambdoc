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
/* Tokens.									*/
/********************************************************************************/

%token EOF

%token <string> RAW
%token <Lambdoc_reader.Ast.command_t * string> PLAIN
%token <Lambdoc_reader.Ast.command_t * Lambdoc_reader.Ast.entity_t> ENTITY

%token <Lambdoc_reader.Ast.command_t> BOLD_MARK
%token <Lambdoc_reader.Ast.command_t> EMPH_MARK
%token <Lambdoc_reader.Ast.command_t> SUP_MARK
%token <Lambdoc_reader.Ast.command_t> SUB_MARK
%token <Lambdoc_reader.Ast.command_t> BEGIN_CAPS
%token <Lambdoc_reader.Ast.command_t> END_CAPS
%token <Lambdoc_reader.Ast.command_t> BEGIN_CODE
%token <Lambdoc_reader.Ast.command_t> END_CODE
%token <Lambdoc_reader.Ast.command_t> BEGIN_LINK
%token <Lambdoc_reader.Ast.command_t> END_LINK
%token <Lambdoc_reader.Ast.command_t> LINK_SEP

%token <Lambdoc_reader.Ast.command_t> BEGIN_PAR
%token <Lambdoc_reader.Ast.command_t> END_PAR

%token <Lambdoc_reader.Ast.command_t> BEGIN_ITEMIZE
%token <Lambdoc_reader.Ast.command_t> END_ITEMIZE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE

%token <Lambdoc_reader.Ast.command_t> BEGIN_QUOTE
%token <Lambdoc_reader.Ast.command_t> END_QUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_SOURCE
%token <Lambdoc_reader.Ast.command_t> END_SOURCE

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.command_t> BEGIN_SECTION
%token <Lambdoc_reader.Ast.command_t> END_SECTION

%token <Lambdoc_reader.Ast.command_t> BEGIN_SUBSECTION
%token <Lambdoc_reader.Ast.command_t> END_SUBSECTION

%token <Lambdoc_reader.Ast.command_t> BEGIN_SUBSUBSECTION
%token <Lambdoc_reader.Ast.command_t> END_SUBSUBSECTION

%token <Lambdoc_reader.Ast.command_t> ITEM


/********************************************************************************/
/* Type declarations.								*/
/********************************************************************************/

%type <Lambdoc_reader.Ast.t> document


/********************************************************************************/
/* Begin grammar specification and declare rules.				*/
/********************************************************************************/

%start document

%%

document:
	| block* EOF					{$1}

block:
	| BEGIN_PAR inline+ END_PAR			{($1, Ast.Paragraph $2)}
	| BEGIN_ITEMIZE item+ END_ITEMIZE		{($1, Ast.Itemize $2)}
	| BEGIN_ENUMERATE item+ END_ENUMERATE		{($1, Ast.Enumerate $2)}
	| BEGIN_QUOTE block+ END_QUOTE			{($1, Ast.Quote $2)}
	| BEGIN_SOURCE RAW END_SOURCE			{($1, Ast.Source $2)}
	| BEGIN_VERBATIM RAW END_VERBATIM		{($1, Ast.Verbatim $2)}
	| BEGIN_SECTION inline+ END_SECTION		{($1, Ast.Section (`Level1, $2))}
	| BEGIN_SUBSECTION inline+ END_SUBSECTION	{($1, Ast.Section (`Level2, $2))}
	| BEGIN_SUBSUBSECTION inline+ END_SUBSUBSECTION	{($1, Ast.Section (`Level3, $2))}

item:
	| ITEM block+					{($1, $2)}

inline:
	| plain						{$1}
	| BOLD_MARK plain BOLD_MARK			{($1, Ast.Bold [$2])}
	| EMPH_MARK plain EMPH_MARK			{($1, Ast.Emph [$2])}
	| SUP_MARK plain SUP_MARK			{($1, Ast.Sup [$2])}
	| SUB_MARK plain SUB_MARK			{($1, Ast.Sub [$2])}
	| BEGIN_CAPS plain END_CAPS			{($1, Ast.Caps [$2])}
	| BEGIN_CODE plain END_CODE			{($1, Ast.Code [$2])}
	| BEGIN_LINK raw END_LINK			{($1, Ast.Link ($2, None))}
	| BEGIN_LINK raw LINK_SEP plain END_LINK	{($1, Ast.Link ($2, Some [$4]))}

plain:
	| PLAIN						{let (comm, txt) = $1 in (comm, Ast.Plain txt)}
	| ENTITY					{let (comm, ent) = $1 in (comm, Ast.Entity ent)}

raw:
	| PLAIN						{let (comm, txt) = $1 in txt}

