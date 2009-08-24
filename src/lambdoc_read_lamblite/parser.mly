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

%token <Lambdoc_core.Basic.raw_t> RAW
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.plain_t> PLAIN

%token <Lambdoc_reader.Ast.command_t> BOLD_MARK
%token <Lambdoc_reader.Ast.command_t> EMPH_MARK
%token <Lambdoc_reader.Ast.command_t> CODE_MARK

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

%token <Lambdoc_reader.Ast.command_t> BEGIN_PROGRAM
%token <Lambdoc_reader.Ast.command_t> END_PROGRAM

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.command_t> BEGIN_PARHEAD
%token <Lambdoc_reader.Ast.command_t> END_PARHEAD

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
	| BEGIN_PROGRAM RAW END_PROGRAM			{($1, Ast.Program $2)}
	| BEGIN_VERBATIM RAW END_VERBATIM		{($1, Ast.Verbatim $2)}
	| BEGIN_PARHEAD inline+ END_PARHEAD		{($1, Ast.Parhead $2)}

item:
	| ITEM block+					{($1, $2)}

inline:
	| plain						{$1}
	| BEGIN_LINK raw END_LINK			{($1, Ast.Link ($2, None))}
	| BEGIN_LINK raw LINK_SEP plain END_LINK	{($1, Ast.Link ($2, Some [$4]))}
	| BOLD_MARK plain BOLD_MARK			{($1, Ast.Bold [$2])}
	| EMPH_MARK plain EMPH_MARK			{($1, Ast.Emph [$2])}
	| CODE_MARK plain CODE_MARK			{($1, Ast.Code [$2])}

plain:
	| PLAIN						{let (comm, txt) = $1 in (comm, Ast.Plain txt)}

raw:
	| PLAIN						{let (comm, txt) = $1 in txt}

