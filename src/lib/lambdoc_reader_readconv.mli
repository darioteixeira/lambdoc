(** Utility functions for converting {!Ast} values to {!Lambdoc_document_valid} values.
*)

open Lambdoc_document
open Valid
open Invalid


(********************************************************************************)
(** {1 Submodule definitions}                                                   *)
(********************************************************************************)

(********************************************************************************)
(** {2 Entities}                                                                *)
(********************************************************************************)

module Entity_input:
sig
    val expand: string -> [ `Okay of string * string | `Error of Error.msg ]
end


(********************************************************************************)
(** {2 Identifiers}                                                             *)
(********************************************************************************)

module Identifier_input:
sig
    val matches_classname: string -> bool
    val matches_label: string -> bool
    val matches_macrodef: string -> bool
    val matches_customdef: string -> bool
    val matches_counter: string -> bool
end


(********************************************************************************)
(** {2 Literal values (verbatim and source environments)}                       *)
(********************************************************************************)

module Literal_input:
sig
    val trim: string -> string
end


(********************************************************************************)
(** {2 Order values}                                                            *)
(********************************************************************************)

module Order_input:
sig
    exception Invalid_order_format of string
    exception Invalid_order_levels of string * Level.section * int

    type ordinal_counter
    type hierarchical_counter

    val ordinal_counter: unit -> ordinal_counter ref
    val hierarchical_counter: unit -> hierarchical_counter ref

    val auto_ordinal: ordinal_counter ref -> [> `Auto_given of Order.ordinal ]
    val auto_hierarchical: Level.section -> hierarchical_counter ref -> [> `Auto_given of Order.hierarchical ]
    val user_ordinal: string -> [> `User_given of Order.ordinal ]
    val user_hierarchical: Level.section -> string -> [> `User_given of Order.hierarchical ]
    val no_order: unit -> [> `None_given ]
end


(********************************************************************************)
(** {2 Math values}                                                             *)
(********************************************************************************)

module Math_input:
sig
    val from_mathtex: string -> Math.t
    val from_mathml: string -> Math.t
end

