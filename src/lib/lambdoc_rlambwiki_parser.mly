(********************************************************************************)
(*  Lambdoc_rlambwiki_parser.mly
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

%{
open Lambdoc_reader
%}


(********************************************************************************)
(* Tokens.                                                                      *)
(********************************************************************************)

%token <Lambdoc_reader_ast.command * string> PLAIN
%token <Lambdoc_reader_ast.command * string> ENTITY
%token <Lambdoc_reader_ast.command * string list> CITE
%token <Lambdoc_reader_ast.command * string list > SEE

%token <Lambdoc_reader_ast.command> BEGIN_BOLD
%token <Lambdoc_reader_ast.command> BEGIN_EMPH
%token <Lambdoc_reader_ast.command> BEGIN_CAPS
%token <Lambdoc_reader_ast.command> BEGIN_MONO
%token <Lambdoc_reader_ast.command> BEGIN_SUP
%token <Lambdoc_reader_ast.command> BEGIN_SUB
%token <Lambdoc_reader_ast.command> BEGIN_INS
%token <Lambdoc_reader_ast.command> BEGIN_DEL
%token <Lambdoc_reader_ast.command> BEGIN_LINK

%token <Lambdoc_reader_ast.command * int> BEGIN_SECTION
%token <Lambdoc_reader_ast.command> BEGIN_PAR
%token <Lambdoc_reader_ast.command> BEGIN_ITEMIZE
%token <Lambdoc_reader_ast.command> BEGIN_ENUMERATE
%token <Lambdoc_reader_ast.command> BEGIN_QUOTE
%token <Lambdoc_reader_ast.command> BEGIN_SBIB
%token <Lambdoc_reader_ast.command> BEGIN_NOTE

%token END_BOLD
%token END_EMPH
%token END_CAPS
%token END_MONO
%token END_SUP
%token END_SUB
%token END_INS
%token END_DEL
%token END_LINK LINK_SEP

%token END_SECTION
%token END_PAR
%token END_ITEMIZE
%token END_ENUMERATE
%token END_QUOTE
%token END_SBIB
%token END_NOTE

%token <Lambdoc_reader_ast.command * string> SOURCE
%token <Lambdoc_reader_ast.command * string> VERBATIM
%token <Lambdoc_reader_ast.command> RULE
%token <Lambdoc_reader_ast.command> ITEM

%token EOF


(********************************************************************************)
(* Type declarations.                                                           *)
(********************************************************************************)

%type <Lambdoc_reader_ast.t> document


(********************************************************************************)
(* Begin grammar specification and declare rules.                               *)
(********************************************************************************)

%start document

%%

document:
    | block* EOF                                    {$1}

block:
    | BEGIN_SECTION inline+ END_SECTION             {let (comm, level) = $1 in (comm, Ast.Section (level, $2))}
    | BEGIN_PAR inline+ END_PAR                     {($1, Ast.Paragraph $2)}
    | BEGIN_ITEMIZE item+ END_ITEMIZE               {($1, Ast.Itemize $2)}
    | BEGIN_ENUMERATE item+ END_ENUMERATE           {($1, Ast.Enumerate $2)}
    | BEGIN_QUOTE block+ END_QUOTE                  {($1, Ast.Quote $2)}
    | BEGIN_SBIB BEGIN_PAR inline+ END_PAR END_SBIB {($1, Ast.Shortbib $3)}
    | BEGIN_NOTE block+ END_NOTE                    {($1, Ast.Note $2)}
    | SOURCE                                        {let (comm, raw) = $1 in (comm, Ast.Source raw)}
    | VERBATIM                                      {let (comm, raw) = $1 in (comm, Ast.Verbatim raw)}
    | RULE                                          {($1, Ast.Rule)}

item:
    | ITEM block+                                   {($1, $2)}

inline:
    | textual                                       {$1}
    | BEGIN_BOLD inline* END_BOLD                   {($1, Ast.Bold $2)}
    | BEGIN_EMPH inline* END_EMPH                   {($1, Ast.Emph $2)}
    | BEGIN_CAPS inline* END_CAPS                   {($1, Ast.Caps $2)}
    | BEGIN_MONO inline* END_MONO                   {($1, Ast.Mono $2)}
    | BEGIN_SUP inline* END_SUP                     {($1, Ast.Sup $2)}
    | BEGIN_SUB inline* END_SUB                     {($1, Ast.Sub $2)}
    | BEGIN_INS inline* END_INS                     {($1, Ast.Ins $2)}
    | BEGIN_DEL inline* END_DEL                     {($1, Ast.Del $2)}
    | BEGIN_LINK raw END_LINK                       {($1, Ast.Link ($2, None))}
    | BEGIN_LINK raw LINK_SEP textual+ END_LINK     {($1, Ast.Link ($2, Some $4))}
    | CITE                                          {let (comm, refs) = $1 in (comm, Ast.Cite refs)}
    | SEE                                           {let (comm, refs) = $1 in (comm, Ast.See refs)}

textual:
    | PLAIN                                         {let (comm, txt) = $1 in (comm, Ast.Plain txt)}
    | ENTITY                                        {let (comm, ent) = $1 in (comm, Ast.Entity ent)}

raw:
    | PLAIN                                         {let (comm, txt) = $1 in txt}

