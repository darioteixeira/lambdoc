(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Part 4 of the Lambdoc+Ocsigen tutorial.

    This instalment of the tutorial illustrates the creation of a command
    extension.  The custom command is named [banner] and takes a sequence
    of raw text as parameter (in Lambtex, for instance, this command takes
    the form [\banner{...}].  The command's output is a verbatim block
    containing the result of feeding the raw parameter to the Unix command
    [banner].  Note that this command generates a new block context, and
    may be used inside a figure or any context that accepts an embeddable
    block.

    The banner extension is implemented by function {!banner_extcomm}.  Note
    that it lives inside the Lwt monad, to avoid blocking on I/O during the
    invocation of the Unix command [banner].  (On Debian-based systems make
    sure to install the [sysvbanner] package.)
*)

open Eliom_content
open Html5.F
open Lambdoc_reader


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Lwt_monad = struct include Lwt let fold_right = Lwt_list.fold_right_s end

module Reader_extension = Lambdoc_reader.Extension.Make (Lwt_monad)

module Lambtex_reader = Lambdoc_rlambtex_reader.Make (Reader_extension)

module Eliom_backend =
struct
    include Eliom_content.Html5.F.Raw
    module Svg = Eliom_content.Svg.F.Raw
end

module Lambdoc_writer = Lambdoc_whtml5_writer.Make (Eliom_backend)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let banner_extcomm =
    let open Reader_extension in
    let f comm raw =
        lwt banner = Lwt_process.pread ("", [| "banner"; raw |]) in
        Lwt.return (`Okay ([comm, Ast.Verbatim banner], [])) in
    ("banner", Blkextcomm (Blkfun_raw ("txt", f), [`Figure_blk; `Embeddable_blk]))


let sample =
    let ch = Pervasives.open_in "sample.lambtex" in
    let sample = BatPervasives.input_all ch in
    Pervasives.close_in ch;
    sample


let make_page content =
    let css_uri = make_uri (Eliom_service.static_dir ()) ["css"; "lambdoc.css"] in
    (html
        (head
            (title (pcdata "Lambdoc + Ocsigen : Basic example"))
            [css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()])
        (body content))


let main_service = Eliom_service.Http.service 
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()


let rec step1_handler () () =
    let step2_service = Eliom_registration.Html5.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:main_service
        ~post_params:(Eliom_parameter.string "source")
        step2_handler in
    let step2_form e_source =
        [
        label ~a:[a_for e_source] [pcdata "Source:"];
        textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
        br ();
        button ~button_type:`Submit [pcdata "Submit"];
        ] in
    Lwt.return (make_page [post_form step2_service step2_form ()])


and step2_handler () source =
    lwt doc = Lambtex_reader.ambivalent_from_string ~extcomms:[banner_extcomm] source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)


let () =
    Eliom_registration.Html5.register main_service step1_handler

