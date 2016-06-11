(********************************************************************************)
(*  Lambcmd.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt
open Tyxml
open Lambdoc_prelude
open Lambdoc_document
open Lambdoc_reader
open Invalid


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Lwt_io =
struct
    include Lwt
    
    let fold_right = Lwt_list.fold_right_s
end

module Reader_extension = Lambdoc_reader.Extension.Make (Lwt_io)

module Tyxml_backend =
struct
    include Html
    module Svg = Svg
end


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let get_book comm maybe_credential raw_isbn =
    let open Ast in
    if String.starts_with raw_isbn "isbn:"
    then match maybe_credential with
        | Some credential ->
            begin try%lwt
                let isbn = Bookaml_isbn.of_string (String.chop ~left:5 raw_isbn) in
                let%lwt book = Bookaml_amazon_ocsigen.book_from_isbn_exn ~credential isbn in
                Lwt.return (`Okay book)
            with
                | Bookaml_isbn.Bad_isbn_length _ ->
                    Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "bad ISBN length")])
                | Bookaml_isbn.Bad_isbn_checksum _ ->
                    Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "bad ISBN checksum")])
                | Bookaml_isbn.Bad_isbn_character _ ->
                    Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "bad ISBN character")])
                | Bookaml_amazon.No_match _ ->
                    Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "no matching book found")])
            end
        | None ->
            Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "please provide Amazon credentials")])
    else
        Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "unknown protocol. Only '#isbn:#' is supported")])

let book_extcomm maybe_credential =
    let open Reader_extension in
    let f comm raw maybe_astseq = match%lwt get_book comm maybe_credential raw with
        | `Error msgs ->
            Lwt.return (`Error msgs)
        | `Okay book ->
            let astseq = match maybe_astseq with
                | Some astseq -> astseq
                | None        -> [(comm, Ast.Emph [(comm, Ast.Plain book.title)])] in
            match book.page with
                | Some page -> Lwt.return (`Okay ([(comm, Ast.Link (page, Some astseq))], []))
                | None      -> Lwt.return (`Okay (astseq, []))
    in ("book", Inlextcomm (Inlfun_raw_seqopt ("href", f)))

let bookpic_extcomm maybe_credential =
    let open Ast in
    let open Reader_extension in
    let f comm raw = match%lwt get_book comm maybe_credential raw with
        | `Error msgs ->
            Lwt.return (`Error msgs)
        | `Okay book -> match book.image_medium with
            | Some img ->
                let sprint = function
                    | Some s -> Ast.Plain s
                    | None   -> Ast.Entity "mdash" in
                let trows =
                    [
                    (comm,
                        [
                        (comm, Some [(comm, Ast.Plain "Title:")]);
                        (comm, Some [match book.page with
                            | Some url -> (comm, Ast.Link (url, Some [(comm, Ast.Plain book.title)]))
                            | None     -> (comm, Ast.Plain book.title)
                        ])]);
                    (comm,
                        [
                        (comm, Some [(comm, Ast.Plain "Author:")]);
                        (comm, Some [(comm, sprint book.author)])
                        ]);
                    (comm,
                        [
                        (comm, Some [(comm, Ast.Plain "Publisher:")]);
                        (comm, Some [(comm, sprint book.publisher)])
                        ]);
                    (comm,
                        [
                        (comm, Some [(comm, Ast.Plain "Publication date:")]);
                        (comm, Some [(comm, sprint book.pubdate)])
                        ]);
                    ] in
                let tbody = (comm, trows) in
                let blocks =
                    [
                    (comm, Ast.Picture (img.url, "cover for \"" ^ book.title ^ "\"", Some book.title));
                    (comm, Ast.Tabular {thead = None; tfoot = None; tbodies = [tbody]});
                    ] in
                Lwt.return (`Okay (blocks, []))
            | None ->
                Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "cannot find picture for book")])
    in ("bookpic", Blkextcomm (Blkfun_raw ("href", f), [`Embeddable_blk]))

let make_extcomms maybe_credential =
    [
    book_extcomm maybe_credential;
    bookpic_extcomm maybe_credential;
    ]

(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
    let open Html in
    let page =
        (html
            (head
                (title (pcdata the_title))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    Format.asprintf "%a" (Html.pp ()) page

let main () =
    let arguments = Arguments.parse () in
    let input_str = Pervasives.input_all arguments.input_chan in
	arguments.input_cleaner arguments.input_chan;
    let idiosyncrasies =
        let base =
            if arguments.unrestricted
            then Idiosyncrasies.unrestricted
            else Idiosyncrasies.default in
        {
        base with
            Idiosyncrasies.max_macro_depth = arguments.max_macro_depth;
            Idiosyncrasies.max_inline_depth = arguments.max_inline_depth;
            Idiosyncrasies.max_block_depth = arguments.max_block_depth;
        } in
    let maybe_credential = match (arguments.amazon_locale, arguments.amazon_associate_tag, arguments.amazon_access_key, arguments.amazon_secret_key) with
        | (Some locale, Some associate_tag, Some access_key, Some secret_key) ->
            Some (Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key)
        | _ ->
            None in
    let extcomms = make_extcomms maybe_credential in
    begin match arguments.input_markup with
        | `Lambtex ->
            let module M = Lambdoc_rlambtex_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Lambwiki ->
            let module M = Lambdoc_rlamblite_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~options:`Lambwiki ~extcomms ~idiosyncrasies input_str
        | `Lambxml ->
            let module M = Lambdoc_rlambxml_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Markdown ->
            let module M = Lambdoc_rlamblite_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~options:`Markdown ~extcomms ~idiosyncrasies input_str
        | `Sexp ->
            Lwt.return (Ambivalent.deserialize input_str)
	end >>= fun doc ->
    begin match arguments.output_markup with
        | `Sexp  ->
            Lwt.return (Ambivalent.serialize doc)
        | `Html ->
            let module Html_writer = Lambdoc_whtml_writer.Make (Tyxml_backend) in
            let valid_options = Html_writer.({default_valid_options with translations = arguments.language}) in
            let xhtml = Html_writer.write_ambivalent ~valid_options doc in
            Lwt.return (string_of_xhtml arguments.title xhtml)
	end >>= fun output_str ->
    Pervasives.output_string arguments.output_chan output_str;
    Pervasives.output_char arguments.output_chan '\n';
    arguments.input_cleaner arguments.input_chan;
    arguments.output_cleaner arguments.output_chan;
    Lwt.return (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)


let () =
    main () |> Lwt_main.run |> exit

