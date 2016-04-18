(********************************************************************************)
(*  Lambdoc_whtml_writer.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


module Make:
    functor (Html: Html_sigs.NoWrap) ->
    Maker.WRITER with
        type t = Html_types.div Html.elt and
        type valid_options = Lambdoc_whtml_writable.valid_options and
        type invalid_options = Lambdoc_whtml_writable.invalid_options

