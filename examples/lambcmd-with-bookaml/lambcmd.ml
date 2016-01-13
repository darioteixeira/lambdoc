(********************************************************************************)
(*  Lambcmd.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Options
open Lambdoc_core
open Lambdoc_reader

module String = BatString


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Lwt_monad = struct include Lwt let fold_right = Lwt_list.fold_right_s end

module Reader_extension = Lambdoc_reader.Extension.Make (Lwt_monad)

module Tyxml_backend =
struct
    include Html5.M
    module Svg = Svg.M
end


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

(********************************************************************************)
(** {2 Functions pertaining to the Lambdoc extension}                           *)
(********************************************************************************)

let get_book comm maybe_credential raw_isbn =
    let open Ast in
    if String.starts_with raw_isbn "isbn:"
    then match maybe_credential with
        | Some credential ->
            begin try_lwt
                let isbn = Bookaml_isbn.of_string (String.lchop ~n:5 raw_isbn) in
                lwt book = Bookaml_amazon_ocsigen.book_from_isbn_exn ~credential isbn in
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
    let f comm raw maybe_astseq = match_lwt get_book comm maybe_credential raw with
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
    let f comm raw = match_lwt get_book comm maybe_credential raw with
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
                        (comm, None, Some [(comm, Ast.Plain "Title:")]);
                        (comm, None, Some [match book.page with
                            | Some url -> (comm, Ast.Link (url, Some [(comm, Ast.Plain book.title)]))
                            | None     -> (comm, Ast.Plain book.title)
                        ])]);
                    (comm,
                        [
                        (comm, None, Some [(comm, Ast.Plain "Author:")]);
                        (comm, None, Some [(comm, sprint book.author)])
                        ]);
                    (comm,
                        [
                        (comm, None, Some [(comm, Ast.Plain "Publisher:")]);
                        (comm, None, Some [(comm, sprint book.publisher)])
                        ]);
                    (comm,
                        [
                        (comm, None, Some [(comm, Ast.Plain "Publication date:")]);
                        (comm, None, Some [(comm, sprint book.pubdate)])
                        ]);
                    ] in
                let tbody = (comm, trows) in
                let blocks =
                    [
                    (comm, Ast.Picture (img.url, "cover for \"" ^ book.title ^ "\""));
                    (comm, Ast.Tabular ("Rl", {thead = None; tfoot = None; tbodies = [tbody]}));
                    ] in
                Lwt.return (`Okay (blocks, []))
            | None ->
                Lwt.return (`Error [(Some comm.comm_linenum, comm.comm_tag, Error.Extension_error "cannot find picture for book")])
    in ("bookpic", Blkextcomm (Blkfun_raw ("href", f), [`Embeddable_blk]))


(********************************************************************************)
(** {2 Lambcmd functions}                                                       *)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
    let open Html5.M in
    let page = (html
            (head
                (title (pcdata the_title))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    let buf = Buffer.create 1024 in
    Html5.P.print ~output:(Buffer.add_string buf) page;
    Buffer.contents buf


let main () =
    let options = Options.parse () in
    let input_str = BatPervasives.input_all options.input_chan in
    let idiosyncrasies =
        let base =
            if options.unrestricted
            then Idiosyncrasies.unrestricted
            else Idiosyncrasies.default in
        {
        base with
            Idiosyncrasies.max_macro_depth = options.max_macro_depth;
            Idiosyncrasies.max_inline_depth = options.max_inline_depth;
            Idiosyncrasies.max_block_depth = options.max_block_depth;
        } in
    let maybe_credential = match (options.amazon_locale, options.amazon_associate_tag, options.amazon_access_key, options.amazon_secret_key) with
        | (Some locale, Some associate_tag, Some access_key, Some secret_key) ->
            Some (Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key)
        | _ ->
            None in
    let extcomms = [book_extcomm maybe_credential; bookpic_extcomm maybe_credential] in
    lwt doc = match options.input_markup with
        | `Lambtex ->
            let module M = Lambdoc_rlambtex_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Lambwiki ->
            let module M = Lambdoc_rlambwiki_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Lambxml ->
            let module M = Lambdoc_rlambxml_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Markdown ->
            let module M = Lambdoc_rmarkdown_reader.Make (Reader_extension) in
            M.ambivalent_from_string ~extcomms ~idiosyncrasies input_str
        | `Sexp ->
            Lwt.return (Lambdoc_core.Ambivalent.deserialize input_str) in
    lwt output_str = match options.output_markup with
        | `Sexp  ->
            Lwt.return (Lambdoc_core.Ambivalent.serialize doc)
        | `Html5 ->
            let module Html5_writer = Lambdoc_whtml5_writer.Make (Tyxml_backend) in
            let valid_options = Html5_writer.({default_valid_options with translations = options.language}) in
            let xhtml = Html5_writer.write_ambivalent ~valid_options doc in
            Lwt.return (string_of_xhtml options.title xhtml) in
    output_string options.output_chan output_str;
    options.input_cleaner options.input_chan;
    options.output_cleaner options.output_chan;
    Lwt.return (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)


let () =
    main () |> Lwt_main.run |> exit

