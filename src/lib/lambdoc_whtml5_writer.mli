(********************************************************************************)
(*  Lambdoc_whtml5_writer.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


module Make:
    functor (Html5: Html5_sigs.NoWrap) ->
    Maker.WRITER with
        type t = Html5_types.div Html5.elt and
        type valid_options = Lambdoc_whtml5_writable.valid_options and
        type invalid_options = Lambdoc_whtml5_writable.invalid_options

