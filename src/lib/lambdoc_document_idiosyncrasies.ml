(********************************************************************************)
(*  Lambdoc_document_idiosyncrasies.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Feature = Lambdoc_document_feature
module Valid = Lambdoc_document_valid

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type action = [ `Accept | `Deny ] [@@deriving sexp]

type 'a classifier = [ `Any | `Only of 'a | `Member of 'a list | `Not of 'a classifier ] [@@deriving sexp]

type feature_rule = Feature.t classifier * action [@@deriving sexp]

type classname_rule = (Feature.t classifier * Valid.classname classifier) * action [@@deriving sexp]

type t =
    {
    feature_ruleset: feature_rule list;
    feature_default: action [@default `Accept];
    classname_ruleset: classname_rule list;
    classname_default: action [@default `Accept];
    max_macro_depth: int option;
    max_inline_depth: int option;
    max_block_depth: int option;
    } [@@deriving sexp, make]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let unrestricted =
    make ()

let restricted =
    make ~classname_default:`Deny ~max_macro_depth:1 ~max_inline_depth:1 ~max_block_depth:1 ()

let default =
    let classname_ruleset =
        [
        ((`Only `Feature_span, `Any), `Accept);
        ((`Only `Feature_paragraph, `Member ["initial"; "indent"; "noindent"]), `Accept);
        ((`Only `Feature_itemize, `Member ["disc"; "circle"; "square"; "none"]), `Accept);
        ((`Only `Feature_enumerate, `Member ["decimal"; "lower-roman"; "upper-roman"; "lower-alpha"; "upper-alpha"; "none"]), `Accept);
        ((`Only `Feature_source, `Member ["plain"; "boxed"; "zebra"; "console"]), `Accept);
        ((`Only `Feature_verbatim, `Member (Array.init 10 (Printf.sprintf "mult%d") |> Array.to_list)), `Accept);
        ((`Only `Feature_picture, `Only "frame"), `Accept);
        ((`Member [`Feature_verbatim; `Feature_picture; `Feature_pullquote; `Feature_custom; `Feature_equation; `Feature_printout; `Feature_table; `Feature_figure], `Member ["center"; "right"; "left"]), `Accept);
        ] in
    make ~classname_ruleset ~classname_default:`Deny ~max_macro_depth:2 ~max_inline_depth:4 ~max_block_depth:4 ()

