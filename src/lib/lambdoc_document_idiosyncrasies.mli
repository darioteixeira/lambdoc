(** Definition of document idiosyncrasies.  These represent additional
    restrictions over the set of allowed document elements and classnames.
*)

module Feature = Lambdoc_document_feature
module Valid = Lambdoc_document_valid


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

val unrestricted: t (** Maximally unrestrictive: all features and classnames allowed, and there are no depth limits *)
val restricted: t   (** Maximally restrictive: no features and no classnames allowed, and maximum depth of 1 *)
val default: t      (** Unrestrictive on features, but only allows the classnames shipped with default CSS and has reasonable depth limits *)

