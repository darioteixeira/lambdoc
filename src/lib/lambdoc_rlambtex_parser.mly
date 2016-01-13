(********************************************************************************)
(*  Lambdoc_rlambtex_parser.mly
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

%{
open Lambdoc_reader
%}


(********************************************************************************)
(* Environment operators.  These are used in an inline context.                 *)
(* Presently the only existing environment operators are [$ $] and <$ $>.       *)
(********************************************************************************)

%token <Lambdoc_reader_ast.command> BEGIN_MATHTEXINL    (* Operator [$ *)
%token <Lambdoc_reader_ast.command> END_MATHTEXINL      (* Operator $] *)
%token <Lambdoc_reader_ast.command> BEGIN_MATHMLINL     (* Operator <$ *)
%token <Lambdoc_reader_ast.command> END_MATHMLINL       (* Operator $> *)

%token BEGIN_MATHTEXINL_DUMMY
%token BEGIN_MATHMLINL_DUMMY


(********************************************************************************)
(* Opening tokens for environment commands.                                     *)
(********************************************************************************)

%token <Lambdoc_reader_ast.command> BEGIN_ABSTRACT
%token <Lambdoc_reader_ast.command> BEGIN_ITEMIZE
%token <Lambdoc_reader_ast.command> BEGIN_ENUMERATE
%token <Lambdoc_reader_ast.command> BEGIN_DESCRIPTION
%token <Lambdoc_reader_ast.command> BEGIN_QANDA
%token <Lambdoc_reader_ast.command> BEGIN_VERSE
%token <Lambdoc_reader_ast.command> BEGIN_QUOTE
%token <Lambdoc_reader_ast.command> BEGIN_TABULAR
%token <Lambdoc_reader_ast.command> BEGIN_SUBPAGE
%token <Lambdoc_reader_ast.command> BEGIN_PULLQUOTE
%token <Lambdoc_reader_ast.command> BEGIN_EQUATION
%token <Lambdoc_reader_ast.command> BEGIN_PRINTOUT
%token <Lambdoc_reader_ast.command> BEGIN_TABLE
%token <Lambdoc_reader_ast.command> BEGIN_FIGURE
%token <Lambdoc_reader_ast.command> BEGIN_BIB
%token <Lambdoc_reader_ast.command> BEGIN_NOTE
%token <Lambdoc_reader_ast.command> BEGIN_MATHTEXBLK
%token <Lambdoc_reader_ast.command> BEGIN_MATHMLBLK
%token <Lambdoc_reader_ast.command> BEGIN_VERBATIM
%token <Lambdoc_reader_ast.command> BEGIN_SOURCE
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BEGIN_BLKPAT_LIT
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BEGIN_BLKPAT_FRAG
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BEGIN_CUSTOM


(********************************************************************************)
(* Closing tokens for environment commands.                                     *)
(********************************************************************************)

%token END_ABSTRACT
%token END_ITEMIZE
%token END_ENUMERATE
%token END_DESCRIPTION
%token END_QANDA
%token END_VERSE
%token END_QUOTE
%token END_TABULAR
%token END_SUBPAGE
%token END_PULLQUOTE
%token END_EQUATION
%token END_PRINTOUT
%token END_TABLE
%token END_FIGURE
%token END_BIB
%token END_NOTE
%token END_MATHTEXBLK
%token END_MATHMLBLK
%token END_VERBATIM
%token END_SOURCE
%token END_BLKPAT_LIT
%token END_BLKPAT_FRAG
%token END_CUSTOM


(********************************************************************************)
(* Simple commands.                                                             *)
(********************************************************************************)

%token <Lambdoc_reader_ast.command> LINEBREAK
%token <Lambdoc_reader_ast.command> GLYPH
%token <Lambdoc_reader_ast.command> BOLD
%token <Lambdoc_reader_ast.command> EMPH
%token <Lambdoc_reader_ast.command> CODE
%token <Lambdoc_reader_ast.command> CAPS
%token <Lambdoc_reader_ast.command> INS
%token <Lambdoc_reader_ast.command> DEL
%token <Lambdoc_reader_ast.command> SUP
%token <Lambdoc_reader_ast.command> SUB
%token <Lambdoc_reader_ast.command> MBOX
%token <Lambdoc_reader_ast.command> SPAN
%token <Lambdoc_reader_ast.command> LINK
%token <Lambdoc_reader_ast.command> SEE
%token <Lambdoc_reader_ast.command> CITE
%token <Lambdoc_reader_ast.command> DREF
%token <Lambdoc_reader_ast.command> SREF
%token <Lambdoc_reader_ast.command> MREF

%token <Lambdoc_reader_ast.command> PARAGRAPH
%token <Lambdoc_reader_ast.command> PICTURE
%token <Lambdoc_reader_ast.command> PART
%token <Lambdoc_reader_ast.command> APPENDIX
%token <Lambdoc_reader_ast.command * int> SECTION
%token <Lambdoc_reader_ast.command> BIBLIOGRAPHY
%token <Lambdoc_reader_ast.command> NOTES
%token <Lambdoc_reader_ast.command> TOC
%token <Lambdoc_reader_ast.command * int> TITLE
%token <Lambdoc_reader_ast.command> RULE
%token <Lambdoc_reader_ast.command> MACRODEF
%token <Lambdoc_reader_ast.command> BOXOUTDEF
%token <Lambdoc_reader_ast.command> THEOREMDEF
%token <Lambdoc_reader_ast.command> ITEM
%token <Lambdoc_reader_ast.command> QUESTION
%token <Lambdoc_reader_ast.command> RQUESTION
%token <Lambdoc_reader_ast.command> ANSWER
%token <Lambdoc_reader_ast.command> RANSWER
%token <Lambdoc_reader_ast.command> THEAD
%token <Lambdoc_reader_ast.command> TFOOT
%token <Lambdoc_reader_ast.command> TBODY
%token <Lambdoc_reader_ast.command> BIB_AUTHOR
%token <Lambdoc_reader_ast.command> BIB_TITLE
%token <Lambdoc_reader_ast.command> BIB_RESOURCE
%token <Lambdoc_reader_ast.command> MACROARG

%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_EMPTY
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_SEQ
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_RAW
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_RAW_RAW
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_RAW_SEQ
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> INLPAT_RAW_SEQOPT
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BLKPAT_EMPTY
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BLKPAT_SEQ
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BLKPAT_RAW
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> BLKPAT_RAW_RAW
%token <Lambdoc_reader_ast.command * Lambdoc_core_basic.ident> MACROCALL


(********************************************************************************)
(* Miscelaneous tokens.                                                         *)
(********************************************************************************)

%token <Lambdoc_reader_ast.command> SPACE
%token <Lambdoc_reader_ast.command> PAR_BREAK
%token <Lambdoc_reader_ast.command> ROW_END
%token <Lambdoc_reader_ast.command> CELL_MARK
%token <Lambdoc_reader_ast.command * string> TEXT
%token <Lambdoc_reader_ast.command * string> ENTITY
%token OPEN CLOSE EOF


(********************************************************************************)
(* Priority.                                                                    *)
(********************************************************************************)

%nonassoc _inline_SPACE_ _inline_list_EMPTY_
%nonassoc SPACE
%nonassoc TEXT ENTITY LINEBREAK GLYPH BOLD EMPH CODE CAPS INS DEL SUP SUB MBOX SPAN LINK SEE CITE DREF SREF MREF MACROARG MACROCALL INLPAT_EMPTY INLPAT_SEQ INLPAT_RAW INLPAT_RAW_RAW INLPAT_RAW_SEQ INLPAT_RAW_SEQOPT BEGIN_MATHTEXINL_DUMMY BEGIN_MATHMLINL_DUMMY


(********************************************************************************)
(* Global settings.                                                             *)
(********************************************************************************)

%parameter <C: Lambdoc_rlambtex_context.S>

%start main

%type <Lambdoc_reader_ast.t> main


(********************************************************************************)
(* Rules.                                                                       *)
(********************************************************************************)

%%

main:
    | frag EOF                                                          {$1}


(********************************************************************************)
(* Block rules.                                                                 *)
(********************************************************************************)

frag:
    | spaceish? blockish*                                               {$2}

spaceish:
    | SPACE                                                             {()}
    | PAR_BREAK                                                         {()}

blockish:
    | block spaceish?                                                   {$1}

block:
    | inline_without_space inline_list                                  {(fst $1, Ast.Paragraph ($1 :: $2))}
    | simple_block                                                      {$1}
    | env_block                                                         {$1}

simple_block:
    | PARAGRAPH inline_bundle                                           {($1, Ast.Paragraph $2)}
    | PICTURE raw_bundle raw_bundle                                     {($1, Ast.Picture ($2, $3))}
    | PART inline_bundle                                                {($1, Ast.Part $2)}
    | APPENDIX                                                          {($1, Ast.Appendix)}
    | SECTION inline_bundle                                             {(fst $1, Ast.Section (snd $1, $2))}
    | BIBLIOGRAPHY                                                      {($1, Ast.Bibliography)}
    | NOTES                                                             {($1, Ast.Notes)}
    | TOC                                                               {($1, Ast.Toc)}
    | TITLE inline_bundle                                               {(fst $1, Ast.Title (snd $1, $2))}
    | RULE                                                              {($1, Ast.Rule)}
    | MACRODEF raw_bundle raw_bundle inline_bundle                      {($1, Ast.Macrodef ($2, $3, $4))}
    | BOXOUTDEF raw_bundle boxoutdef                                    {($1, Ast.Boxoutdef ($2, fst $3, snd $3))}
    | THEOREMDEF raw_bundle theoremdef                                  {let (caption, counter) = $3 in ($1, Ast.Theoremdef ($2, caption, counter))}
    | sim_blkpat                                                        {let (comm, tag, blkpat) = $1 in (comm, Ast.Extcomm_blk (tag, blkpat))}

sim_blkpat:
    | BLKPAT_EMPTY                                                      {(fst $1, snd $1, Ast.Blkpat_empty)}
    | BLKPAT_SEQ inline_bundle                                          {(fst $1, snd $1, Ast.Blkpat_seq $2)}
    | BLKPAT_RAW raw_bundle                                             {(fst $1, snd $1, Ast.Blkpat_raw $2)}
    | BLKPAT_RAW_RAW raw_bundle raw_bundle                              {(fst $1, snd $1, Ast.Blkpat_raw_raw ($2, $3))}

env_block:
    | BEGIN_ITEMIZE anon_item_frag* END_ITEMIZE                         {($1, Ast.Itemize $2)}
    | BEGIN_ENUMERATE anon_item_frag* END_ENUMERATE                     {($1, Ast.Enumerate $2)}
    | BEGIN_DESCRIPTION desc_item_frag* END_DESCRIPTION                 {($1, Ast.Description $2)}
    | BEGIN_QANDA qanda_frag* END_QANDA                                 {($1, Ast.Qanda $2)}
    | BEGIN_VERSE frag END_VERSE                                        {($1, Ast.Verse $2)}
    | BEGIN_QUOTE frag END_QUOTE                                        {($1, Ast.Quote $2)}
    | BEGIN_TABULAR tabular END_TABULAR                                 {($1, Ast.Tabular $2)}
    | BEGIN_SUBPAGE frag END_SUBPAGE                                    {($1, Ast.Subpage $2)}
    | BEGIN_PULLQUOTE inline_bundle? frag END_PULLQUOTE                 {($1, Ast.Pullquote ($2, $3))}
    | BEGIN_EQUATION wrapper END_EQUATION                               {let (cap, blk) = $2 in ($1, Ast.Equation (cap, blk))}
    | BEGIN_PRINTOUT wrapper END_PRINTOUT                               {let (cap, blk) = $2 in ($1, Ast.Printout (cap, blk))}
    | BEGIN_TABLE wrapper END_TABLE                                     {let (cap, blk) = $2 in ($1, Ast.Table (cap, blk))}
    | BEGIN_FIGURE wrapper END_FIGURE                                   {let (cap, blk) = $2 in ($1, Ast.Figure (cap, blk))}
    | BEGIN_ABSTRACT frag END_ABSTRACT                                  {($1, Ast.Abstract $2)}
    | BEGIN_BIB bib_author bib_title bib_resource END_BIB               {($1, Ast.Bib {Ast.author = $2; Ast.title = $3; Ast.resource = $4})}
    | BEGIN_NOTE frag END_NOTE                                          {($1, Ast.Note $2)}
    | set_literal BEGIN_MATHTEXBLK set_general TEXT END_MATHTEXBLK      {($2, Ast.Mathtex_blk (snd $4))}
    | set_literal BEGIN_MATHMLBLK set_general TEXT END_MATHMLBLK        {($2, Ast.Mathml_blk (snd $4))}
    | set_literal BEGIN_SOURCE set_general TEXT END_SOURCE              {($2, Ast.Source (snd $4))}
    | set_literal BEGIN_VERBATIM set_general TEXT END_VERBATIM          {($2, Ast.Verbatim (snd $4))}
    | env_blkpat                                                        {let (comm, tag, blkpat) = $1 in (comm, Ast.Extcomm_blk (tag, blkpat))}
    | BEGIN_CUSTOM inline_bundle? frag END_CUSTOM                       {(fst $1, Ast.Custom (snd $1, $2, $3))}

env_blkpat:
    | set_literal BEGIN_BLKPAT_LIT set_general TEXT END_BLKPAT_LIT      {(fst $2, snd $2, Ast.Blkpat_lit (snd $4))}
    | BEGIN_BLKPAT_FRAG frag END_BLKPAT_FRAG                            {(fst $1, snd $1, Ast.Blkpat_frag $2)}

anon_item_frag:
    | ITEM frag                                                         {($1, $2)}

desc_item_frag:
    | ITEM inline_bundle frag                                           {($1, $2, $3)}

qanda_frag:
    | QUESTION inline_bundle? frag                                      {($1, Ast.New_questioner $2, $3)}
    | RQUESTION frag                                                    {($1, Ast.Same_questioner, $2)}
    | ANSWER inline_bundle? frag                                        {($1, Ast.New_answerer $2, $3)}
    | RANSWER frag                                                      {($1, Ast.Same_answerer, $2)}

wrapper:
    | inline_bundle? spaceish? block spaceish?                          {($1, $3)}

bib_author:
    | BIB_AUTHOR inline_bundle SPACE?                                   {($1, $2)}

bib_title:
    | BIB_TITLE inline_bundle SPACE?                                    {($1, $2)}

bib_resource:
    | BIB_RESOURCE inline_bundle SPACE?                                 {($1, $2)}

boxoutdef:
    | /* empty */                                                       {(None, None)}
    | inline_bundle                                                     {(Some $1, None)}
    | inline_bundle raw_bundle                                          {(Some $1, Some $2)}

theoremdef:
    | inline_bundle                                                     {($1, None)}
    | inline_bundle raw_bundle                                          {($1, Some $2)}



(********************************************************************************)
(* Rules for tabular environment.                                               *)
(********************************************************************************)

tabular:
    | head? body+ foot?                                                 {{Ast.thead = $1; Ast.tfoot = $3; Ast.tbodies = $2;}}
    | row row* body* foot?                                              {{Ast.thead = None; Ast.tfoot = $4; Ast.tbodies = (fst $1, $1 :: $2) :: $3;}}

head: THEAD SPACE row+                                                  {($1, $3)}
foot: TFOOT SPACE row+                                                  {($1, $3)}
body: TBODY SPACE row+                                                  {($1, $3)}
row:  cell+ ROW_END                                                     {($2, $1)}
cell: CELL_MARK option(inline+)                                         {($1, $2)}


(********************************************************************************)
(* Inline context.                                                              *)
(********************************************************************************)

inline_without_space:
    | TEXT                                                              {(fst $1, Ast.Plain (snd $1))}
    | ENTITY                                                            {(fst $1, Ast.Entity (snd $1))}
    | LINEBREAK                                                         {($1, Ast.Linebreak)}
    | GLYPH raw_bundle raw_bundle                                       {($1, Ast.Glyph ($2, $3))}
    | BOLD inline_bundle                                                {($1, Ast.Bold $2)}
    | EMPH inline_bundle                                                {($1, Ast.Emph $2)}
    | CODE inline_bundle                                                {($1, Ast.Code $2)}
    | CAPS inline_bundle                                                {($1, Ast.Caps $2)}
    | INS inline_bundle                                                 {($1, Ast.Ins $2)}
    | DEL inline_bundle                                                 {($1, Ast.Del $2)}
    | SUP inline_bundle                                                 {($1, Ast.Sup $2)}
    | SUB inline_bundle                                                 {($1, Ast.Sub $2)}
    | MBOX inline_bundle                                                {($1, Ast.Mbox $2)}
    | SPAN inline_bundle                                                {($1, Ast.Span $2)}
    | LINK raw_bundle inline_bundle?                                    {($1, Ast.Link ($2, $3))}
    | SEE raw_bundle*                                                   {($1, Ast.See $2)}
    | CITE raw_bundle*                                                  {($1, Ast.Cite $2)}
    | DREF raw_bundle inline_bundle?                                    {($1, Ast.Dref ($2, $3))}
    | SREF raw_bundle inline_bundle?                                    {($1, Ast.Sref ($2, $3))}
    | MREF raw_bundle inline_bundle                                     {($1, Ast.Mref ($2, $3))}
    | MACROARG raw_bundle                                               {($1, Ast.Macroarg $2)}
    | MACROCALL inline_bundle*                                          {(fst $1, Ast.Macrocall (snd $1, $2))}
    | set_mathtexinl BEGIN_MATHTEXINL TEXT set_general END_MATHTEXINL   {($2, Ast.Mathtex_inl (snd $3))}
    | set_mathmlinl BEGIN_MATHMLINL TEXT set_general END_MATHMLINL      {($2, Ast.Mathml_inl (snd $3))}
    | sim_inlpat                                                        {let (comm, tag, inlpat) = $1 in (comm, Ast.Extcomm_inl (tag, inlpat))}

sim_inlpat:
    | INLPAT_EMPTY                                                      {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_empty)}
    | INLPAT_SEQ inline_bundle                                          {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_seq $2)}
    | INLPAT_RAW raw_bundle                                             {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw $2)}
    | INLPAT_RAW_RAW raw_bundle raw_bundle                              {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_raw ($2, $3))}
    | INLPAT_RAW_SEQ raw_bundle inline_bundle                           {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_seq ($2, $3))}
    | INLPAT_RAW_SEQOPT raw_bundle inline_bundle?                       {let (comm, tag) = $1 in (comm, tag, Ast.Inlpat_raw_seqopt ($2, $3))}

inline:
    | SPACE %prec _inline_SPACE_                                        {($1, Ast.Plain " ")}
    | SPACE TEXT                                                        {($1, Ast.Plain (" " ^ snd $2))}
    | inline_without_space                                              {$1}

inline_bundle:
    | OPEN inline_list CLOSE                                            {$2}

inline_list:
    | (* empty *) %prec _inline_list_EMPTY_                             {[]}
    | inline inline_list                                                {$1 :: $2}

raw_bundle:
    | set_raw OPEN set_general TEXT CLOSE                               {snd $4}


(********************************************************************************)
(* Set lexing context via side-effect.                                          *)
(********************************************************************************)

set_general:
    | /* empty */                                                       {C.(set General)}

set_raw:
    | /* empty */                                                       {C.(set Raw)}

set_mathtexinl:
    | BEGIN_MATHTEXINL_DUMMY                                            {C.(set Mathtexinl)}

set_mathmlinl:
    | BEGIN_MATHMLINL_DUMMY                                             {C.(set Mathmlinl)}

set_literal:
    | /* empty */                                                       {C.(set Literal)}

