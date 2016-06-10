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

type feature_ruleset = (Feature.t classifier * action) list [@@deriving sexp]

type classname_ruleset = ((Feature.t classifier * Valid.classname classifier) * action) list [@@deriving sexp]

type t =
    {
    feature_ruleset: feature_ruleset;
    feature_default: action;
    classname_ruleset: classname_ruleset;
    classname_default: action;
    max_macro_depth: int option;
    max_inline_depth: int option;
    max_block_depth: int option;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

let make
    ?(feature_ruleset = [])
    ?(feature_default = `Accept)
    ?(classname_ruleset = [])
    ?(classname_default = `Accept)
    ?(max_macro_depth = None)
    ?(max_inline_depth = None)
    ?(max_block_depth = None) () =
    {feature_ruleset; feature_default; classname_ruleset; classname_default; max_macro_depth; max_inline_depth; max_block_depth}


(********************************************************************************)
(** {2 Built-in idiosyncrasies}                                                 *)
(********************************************************************************)

let unrestricted =
    make ()

let restricted =
    make ~classname_default:`Deny ~max_macro_depth:(Some 1) ~max_inline_depth:(Some 1) ~max_block_depth:(Some 1) ()

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
    make ~classname_ruleset ~classname_default:`Deny ~max_macro_depth:(Some 2) ~max_inline_depth:(Some 4) ~max_block_depth:(Some 4) ()

