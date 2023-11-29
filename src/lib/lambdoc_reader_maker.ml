module Ast = Lambdoc_reader_ast
module Compiler = Lambdoc_reader_compiler
module Extension = Lambdoc_reader_extension
module Preprocessor = Lambdoc_reader_preprocessor

open Lambdoc_document
open Invalid


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type READABLE =
sig
    type options

    val ast_from_string:
        ?options:options ->
        linenum_offset:int ->
        inline_extdefs:Extension.extdef list ->
        block_extdefs:Extension.extdef list ->
        string ->
        [ `Okay of Ast.t | `Error of Error.reading list ]
end


module type READER =
sig
    module Ext: Extension.S

    type options

    val ambivalent_from_string:
        ?options:options ->
        ?postprocessor:Error.localized list Ext.Foldmapper.t ->
        ?extcomms:Ext.extcomm list ->
        ?linenum_offset:int ->
        ?verify_utf8:bool ->
        ?expand_entities:bool ->
        ?idiosyncrasies:Idiosyncrasies.t ->
        string ->
        Ambivalent.t Ext.IO.t
end


module type FULL =
sig
    type options

    module Make: functor (Ext: Extension.S) -> READER with module Ext = Ext and type options = options

    module Trivial: READER with module Ext = Extension.Trivial and type options = options
end


(********************************************************************************)
(** {1 Public modules and functors}                                             *)
(********************************************************************************)

module Make (Readable: READABLE) (Ext: Extension.S) : READER with
    module Ext = Ext
    and type options = Readable.options =
struct
    module Ext = Ext
    module Comp = Compiler.Make (Ext)

    type options = Readable.options

    open Extension
    open Ext

    let extdef_of_extcomm (accum_inl, accum_blk) (tag, def) = match def with
        | Inlextcomm inlfun ->
            let inlsyn = match inlfun with
                | Inlfun_empty _             -> Syn_empty
                | Inlfun_seq _               -> Syn_seq
                | Inlfun_raw (a, _)          -> Syn_raw a
                | Inlfun_raw_seq (a, _)      -> Syn_raw_seq a
                | Inlfun_raw_seqopt (a, _)   -> Syn_raw_seqopt a
                | Inlfun_raw_raw (a1, a2, _) -> Syn_raw_raw (a1, a2)
            in ((tag, inlsyn) :: accum_inl, accum_blk)
        | Blkextcomm (blkfun, _) ->
            let blksyn = match blkfun with
                | Blkfun_empty _             -> Syn_empty
                | Blkfun_seq _               -> Syn_seq
                | Blkfun_lit _               -> Syn_lit
                | Blkfun_frag _              -> Syn_frag
                | Blkfun_raw (a, _)          -> Syn_raw a
                | Blkfun_raw_raw (a1, a2, _) -> Syn_raw_raw (a1, a2)
            in (accum_inl, (tag, blksyn) :: accum_blk)

    let ambivalent_from_string
        ?options
        ?postprocessor
        ?(extcomms = [])
        ?(linenum_offset = 0)
        ?(verify_utf8 = true)
        ?(expand_entities = true)
        ?(idiosyncrasies = Idiosyncrasies.default)
        source =
            let verified = if verify_utf8 then Preprocessor.verify_utf8 source else `Okay in
            match verified with
                | `Error (sane_str, error_lines) ->
                    let localized = List.map (fun line -> (Some line, None, Error.Malformed_code_point)) error_lines in
                    let errors = Comp.contextualize_errors ~sort:false sane_str localized in
                    IO.return (Ambivalent.Invalid (Invalid.make errors))
                | `Okay ->
                    let (inline_extdefs, block_extdefs) = List.fold_left extdef_of_extcomm ([], []) extcomms in
                    match Readable.ast_from_string ?options ~linenum_offset ~inline_extdefs ~block_extdefs source with
                        | `Okay ast ->
                            Comp.compile ?postprocessor ~extcomms ~expand_entities ~idiosyncrasies ~source ast
                        | `Error xs ->
                            let localized = List.map (fun (line, ident, msg) -> (line, ident, Error.Reading_error msg)) xs in
                            let errors = Comp.contextualize_errors ~sort:false source localized in
                            IO.return (Ambivalent.Invalid (Invalid.make errors))
                        | exception exc ->
                            IO.fail exc
end

