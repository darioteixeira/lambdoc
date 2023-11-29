open Lambdoc_writer


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type valid_options =
    {
    numbered_paragraphs: bool;
    translations: Translations.t;
    namespace: Html_types.nmtoken;
    prefix: Html_types.nmtoken;
    base_classes: Html_types.nmtokens;
    extra_classes: Html_types.nmtokens;
    }

type invalid_options =
    {
    prefix: Html_types.nmtoken;
    base_classes: Html_types.nmtokens;
    extra_classes: Html_types.nmtokens;
    }


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make:
    functor (Html: Html_sigs.NoWrap) ->
    Lambdoc_writer.Maker.WRITABLE with
        type t = Html_types.div Html.elt and
        type valid_options = valid_options and
        type invalid_options = invalid_options

