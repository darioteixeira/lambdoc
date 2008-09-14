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
/* Environment commands and operators.						*/
/* All environment commands are composed of a begin/end pair.			*/
/* Envrionment operators are [$ $] and <$ $>.					*/
/********************************************************************************/

%token <Document_ast.command_t> BEGIN_ITEMIZE
%token <Document_ast.command_t> END_ITEMIZE

%token <Document_ast.command_t> BEGIN_ENUMERATE
%token <Document_ast.command_t> END_ENUMERATE

%token <Document_ast.command_t> BEGIN_QUOTE
%token <Document_ast.command_t> END_QUOTE

%token <Document_ast.command_t> BEGIN_ALGORITHM 
%token <Document_ast.command_t> END_ALGORITHM

%token <Document_ast.command_t> BEGIN_EQUATION
%token <Document_ast.command_t> END_EQUATION

%token <Document_ast.command_t> BEGIN_FIGURE
%token <Document_ast.command_t> END_FIGURE

%token <Document_ast.command_t> BEGIN_TABLE
%token <Document_ast.command_t> END_TABLE

%token <Document_ast.command_t> BEGIN_BIB
%token <Document_ast.command_t> END_BIB

%token <Document_ast.command_t> BEGIN_VERBATIM
%token <Document_ast.command_t> END_VERBATIM

%token <Document_ast.command_t> BEGIN_MATH
%token <Document_ast.command_t> END_MATH

%token <Document_ast.command_t> BEGIN_SUBPAGE
%token <Document_ast.command_t> END_SUBPAGE

%token <Document_ast.command_t> BEGIN_TABULAR
%token <Document_ast.command_t> END_TABULAR

%token <Document_ast.operator_t> BEGIN_MATHTEX
%token <Document_ast.operator_t> END_MATHTEX

%token <Document_ast.operator_t> BEGIN_MATHML
%token <Document_ast.operator_t> END_MATHML


/********************************************************************************/
/* Simple commands.								*/
/********************************************************************************/

%token <Document_ast.command_t> SECTION
%token <Document_ast.command_t> SUBSECTION
%token <Document_ast.command_t> SUBSUBSECTION
%token <Document_ast.command_t> TOC
%token <Document_ast.command_t> BIBLIOGRAPHY
%token <Document_ast.command_t> NOTES

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

%token <Document_ast.command_t> APPENDIX
%token <Document_ast.command_t> RULE
%token <Document_ast.command_t> SETTING
%token <Document_ast.command_t> CAPTION
%token <Document_ast.command_t> LOAD
%token <Document_ast.command_t> HEAD
%token <Document_ast.command_t> BODY
%token <Document_ast.command_t> FOOT
%token <Document_ast.command_t> AUTHOR
%token <Document_ast.command_t> TITLE
%token <Document_ast.command_t> RESOURCE
%token <Document_ast.command_t> NOTE
%token <Document_ast.command_t> NEW_ITEM


/********************************************************************************/
/* Starting symbol.								*/
/********************************************************************************/

%type <Document_ast.t> document
%start document


/********************************************************************************/
/* Begin grammar specification and declare top-level rules.			*/
/********************************************************************************/

%%

document:
	| super_block* EOF					{$1}


/********************************************************************************/
/* Blocks.									*/
/********************************************************************************/

super_block:
	| top_block						{Top_block $1}
	| nestable_block					{Nestable_block $1}


top_block:
	| heading						{Heading $1}
	| APPENDIX						{Appendix $1}
	| RULE							{Rule $1}
	| SETTING BEGIN PLAIN END BEGIN PLAIN END		{Setting ($1, $3, $6)}


heading:
	| SECTION BEGIN super_node+ END				{Section ($1, $3)}
	| SUBSECTION BEGIN super_node+ END			{Subsection ($1, $3)}
	| SUBSUBSECTION BEGIN super_node+ END			{Subsubsection ($1, $3)}
	| TOC							{Toc $1} 
	| BIBLIOGRAPHY						{Bibliography $1}
	| NOTES							{Notes $1}


nestable_block:
	| NEW_PARAGRAPH super_node+				{Paragraph ($1, $2)}
	| BEGIN_MATH PLAIN END_MATH				{Math ($1, $2)}
	| BEGIN_TABULAR tabular_block END_TABULAR		{Tabular ($1, $2)}
	| BEGIN_VERBATIM textual_node+ END_VERBATIM		{Preformat ($1, $2)}
	| BEGIN_ITEMIZE items END_ITEMIZE			{Itemize ($1, $2)}
	| BEGIN_ENUMERATE items END_ENUMERATE			{Enumerate ($1, $2)}
	| BEGIN_QUOTE nestable_block+ END_QUOTE			{Quote ($1, $2)}
	| BEGIN_ALGORITHM algorithm_block+ END_ALGORITHM	{Algorithm ($1, $2)}
	| BEGIN_EQUATION equation_block+ END_EQUATION		{Equation ($1, $2)}
	| BEGIN_FIGURE figure_block+ END_FIGURE			{Figure ($1, $2)}
	| BEGIN_TABLE table_block+ END_TABLE			{Table ($1, $2)}
	| BEGIN_BIB bib_block+ END_BIB				{Bib ($1, $2)}
	| NOTE BEGIN super_node+ END				{Note ($1, $3)}


items:
	| NEW_ITEM nestable_block+				{[Item ($1, $2)]}
	| items NEW_ITEM nestable_block+			{List.append $1 [Item ($2, $3)]}


/********************************************************************************/
/* Definition of floaters and bibliography entries.				*/
/********************************************************************************/

algorithm_block:
	| CAPTION BEGIN super_node+ END				{`Caption ($1, $3)}
	| BEGIN_VERBATIM textual_node+ END_VERBATIM		{`Verbatim ($1, $2)}


equation_block:
	| CAPTION BEGIN super_node+ END				{`Caption ($1, $3)}
	| BEGIN_MATH PLAIN END_MATH				{`Math ($1, $2)}


figure_block:
	| CAPTION BEGIN super_node+ END				{`Caption ($1, $3)}
	| LOAD BEGIN PLAIN END					{`Load ($1, $3)}
	| BEGIN_VERBATIM textual_node+ END_VERBATIM		{`Verbatim ($1, $2)}
	| BEGIN_SUBPAGE super_block+ END_SUBPAGE		{`Subpage ($1, $2)}


table_block:
	| CAPTION BEGIN super_node+ END				{`Caption ($1, $3)}
	| BEGIN_TABULAR tabular_block END_TABULAR		{`Tabular ($1, $2)}


bib_block:
	| AUTHOR BEGIN super_node+ END				{`Author ($1, $3)}
	| TITLE BEGIN super_node+ END				{`Title ($1, $3)}
	| RESOURCE BEGIN super_node+ END			{`Resource ($1, $3)}


/********************************************************************************/
/* Rules for tabular environment.						*/
/********************************************************************************/

tabular_block:
	| head_block? body_block+ foot_block?			{{thead = $1; tfoot = $3; tbodies = $2;}}
	| row+ body_block* foot_block?				{{thead = None; tfoot = $3; tbodies = (None, $1) :: $2;}}

head_block:
	| HEAD row+						{(Some $1, $2)}

body_block:
	| BODY row+						{(Some $1, $2)}

foot_block:
	| FOOT row+						{(Some $1, $2)}

row:
	| NEW_PARAGRAPH columns ROW_END				{($1, $2)}

columns:
	| super_node+						{[$1]}
	| columns COLUMN_SEP super_node+			{List.append $1 [$3]}


/********************************************************************************/
/* Nodes.									*/
/********************************************************************************/

super_node:
	| nonlink_node						{Nonlink_node $1}
	| link_node						{Link_node $1}


textual_node:
	| PLAIN							{Plain $1}
	| ENTITY						{Entity $1}


nonlink_node:
	| textual_node						{Textual $1}
	| BEGIN_MATHTEX PLAIN END_MATHTEX			{Mathtex ($1, $2)}
	| BEGIN_MATHML PLAIN END_MATHML				{Mathml ($1, $2)}
	| BOLD BEGIN super_node+ END				{Bold ($1, $3)}
	| EMPH BEGIN super_node+ END				{Emph ($1, $3)}
	| MONO BEGIN super_node+ END				{Mono ($1, $3)}
	| CAPS BEGIN super_node+ END				{Caps ($1, $3)}
	| THRU BEGIN super_node+ END				{Thru ($1, $3)}
	| SUP BEGIN super_node+ END				{Sup ($1, $3)}
	| SUB BEGIN super_node+ END				{Sub ($1, $3)}
	| BOX BEGIN super_node+ END				{Box ($1, $3)}


link_node:
	| LINK BEGIN PLAIN END BEGIN nonlink_node+ END		{Link ($1, $3, $6)}
	| SEE BEGIN PLAIN END					{See ($1, $3)}
	| CITE BEGIN PLAIN END					{Cite ($1, $3)}
	| REF BEGIN PLAIN END					{Ref ($1, $3)}
	| SREF BEGIN PLAIN END					{Sref ($1, $3)}
	| MREF BEGIN PLAIN END BEGIN nonlink_node+ END		{Mref ($1, $3, $6)}

