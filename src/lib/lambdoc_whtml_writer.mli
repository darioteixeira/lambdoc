open Lambdoc_writer


module Make:
    functor (Html: Html_sigs.NoWrap) ->
    Maker.WRITER with
        type t = Html_types.div Html.elt and
        type valid_options = Lambdoc_whtml_writable.valid_options and
        type invalid_options = Lambdoc_whtml_writable.invalid_options

