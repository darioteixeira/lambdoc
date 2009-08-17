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
/********************************************************************************/

%token EOF

%token <Lambdoc_core.Basic.raw_t> RAW
%token <Lambdoc_reader.Ast.command_t * Lambdoc_core.Basic.plain_t> PLAIN

%token <Lambdoc_reader.Ast.command_t> BOLD_MARK
%token <Lambdoc_reader.Ast.command_t> EMPH_MARK
%token <Lambdoc_reader.Ast.command_t> MONO_MARK

%token <Lambdoc_reader.Ast.command_t> BEGIN_PAR
%token <Lambdoc_reader.Ast.command_t> END_PAR

%token <Lambdoc_reader.Ast.command_t> BEGIN_ITEMIZE
%token <Lambdoc_reader.Ast.command_t> END_ITEMIZE

%token <Lambdoc_reader.Ast.command_t> BEGIN_ENUMERATE
%token <Lambdoc_reader.Ast.command_t> END_ENUMERATE

%token <Lambdoc_reader.Ast.command_t> BEGIN_QUOTE
%token <Lambdoc_reader.Ast.command_t> END_QUOTE

%token <Lambdoc_reader.Ast.command_t> BEGIN_CODE
%token <Lambdoc_reader.Ast.command_t> END_CODE

%token <Lambdoc_reader.Ast.command_t> BEGIN_VERBATIM
%token <Lambdoc_reader.Ast.command_t> END_VERBATIM

%token <Lambdoc_reader.Ast.command_t> PARHEAD


/********************************************************************************/
/********************************************************************************/

%type <Lambdoc_reader.Ast.t> document


/********************************************************************************/
/********************************************************************************/

%start document

%%

document:
	| block* EOF	{$1}

block:
	| BEGIN_PAR inline+ END_PAR			{($1, Ast.Paragraph $2)}
	| BEGIN_ITEMIZE item+ END_ITEMIZE		{($1, Ast.Itemize $2)}
	| BEGIN_ENUMERATE item+ END_ENUMERATE		{($1, Ast.Enumerate $2)}
	| BEGIN_QUOTE block+ END_QUOTE			{($1, Ast.Quote $2)}
	| BEGIN_CODE RAW END_CODE			{($1, Ast.Code $2)}
	| BEGIN_VERBATIM RAW END_VERBATIM		{($1, Ast.Verbatim $2)}
	| PARHEAD inline+				{($1, Ast.Parhead $2)}

item:
	| BEGIN_PAR inline+ END_PAR			{($1, [($1, Ast.Paragraph $2)])}

inline:
	| PLAIN						{let (comm, txt) = $1 in (comm, Ast.Plain txt)}
	| BOLD_MARK inline+ BOLD_MARK			{($1, Ast.Bold $2)}
	| EMPH_MARK inline+ EMPH_MARK			{($1, Ast.Emph $2)}
	| MONO_MARK inline+ MONO_MARK			{($1, Ast.Mono $2)}

