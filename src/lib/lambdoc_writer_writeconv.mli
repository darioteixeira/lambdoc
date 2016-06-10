(********************************************************************************)
(*  Lambdoc_writer_writeconv.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Utility functions for converting {!Lambdoc_document_valid} values to strings.
*)

open Lambdoc_document.Valid


(********************************************************************************)
(** {1 Submodule definitions}                                                   *)
(********************************************************************************)

(********************************************************************************)
(** {2 Tabular values}                                                          *)
(********************************************************************************)

module Tabular_output:
sig
    val string_of_alignment: Tabular.alignment -> string
end


(********************************************************************************)
(** {2 Order values}                                                            *)
(********************************************************************************)

module Order_output:
sig
    type ordinal_converter = Order.ordinal -> string
    type hierarchical_converter = (int -> string) list

    val format_arabic: ordinal_converter
    val format_roman: ordinal_converter
    val format_mainbody: hierarchical_converter
    val format_appendixed: hierarchical_converter

    val maybe_string_of_ordinal: ordinal_converter -> (Order.ordinal, 'b) Order.t -> string option
    val maybe_string_of_hierarchical: hierarchical_converter -> (Order.hierarchical, 'b) Order.t -> string option
end


(********************************************************************************)
(** {2 Math values}                                                             *)
(********************************************************************************)

module Math_output:
sig
    exception Mathtex_undefined
    exception Mathml_undefined

    val get_mathtex: Math.t -> string
    val get_mathml: Math.t -> string
end

