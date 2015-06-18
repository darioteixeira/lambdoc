(********************************************************************************)
(*  Lambdoc_rlambxml_dtd.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module String = BatString

open Lambdoc_reader.Extension


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(*  Create a new DTD according to the given inline/block extensions.  Note
    that the textual definition of the base DTD (contained in "lambxml.dtd")
    is patched to take into account the inline/block extensions.  Only then
    is the patched DTD fed into function {!Pxp_dtd_parser.parse_dtd_entity}.
    To avoid repatching and reparsing a previously seen DTD, a hash table
    cache is used.

    The alternative design -- parsing and saving the base DTD, and cloning it
    upon each request before modifying it using the PXP API -- was abandoned
    because there is no way to clone a DTD object in PXP.
*)
let make =
    let dtd_cache = Hashtbl.create 4 in
    let config = {Pxp_types.default_config with Pxp_types.encoding = `Enc_utf8} in
    let source = include_file "lambxml.dtd" in
    fun ~inline_extdefs ~block_extdefs ->
        try
            Hashtbl.find dtd_cache (inline_extdefs, block_extdefs)
        with Not_found ->
            let entry_of_extdef (accum_by, accum_source) (ident, syntax) =
                let (cm, attrs) = match syntax with
                    | Syn_empty            -> ("EMPTY", [])
                    | Syn_seq              -> ("%Seq;", [])
                    | Syn_lit              -> ("(#PCDATA)", [])
                    | Syn_frag             -> ("%Frag;", [])
                    | Syn_raw a            -> ("EMPTY", [a])
                    | Syn_raw_raw (a1, a2) -> ("EMPTY", [a1; a2])
                    | Syn_raw_seq a        -> ("%Seq;", [a])
                    | Syn_raw_seqopt a     -> ("%Seq;", [a]) in
                let xattrs = List.map (Printf.sprintf "%s CDATA #IMPLIED ") attrs |> String.concat " " in
                let decl = Printf.sprintf "<!ELEMENT %s %s>\n<!ATTLIST %s %%Common; %s>\n" ident cm ident xattrs in
                (accum_by ^ "|" ^ ident, accum_source ^ decl) in
            let (inline_by, source) = List.fold_left entry_of_extdef ("", source) inline_extdefs in
            let (block_by, source) = List.fold_left entry_of_extdef ("", source) block_extdefs in
            let source = String.nreplace ~str:source ~sub:"$INLINE" ~by:inline_by in
            let source = String.nreplace ~str:source ~sub:"$BLOCK" ~by:block_by in
            let dtd = Pxp_dtd_parser.parse_dtd_entity config (Pxp_types.from_string source) in
            Hashtbl.replace dtd_cache (inline_extdefs, block_extdefs) dtd;
            dtd

