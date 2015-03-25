(********************************************************************************)
(*	Lambdoc_whtml5_writable.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type valid_options_t =
	{
	numbered_paragraphs: bool;
	translations: Translations.t;
	namespace: Html5_types.nmtoken;
	prefix: Html5_types.nmtoken;
	base_classes: Html5_types.nmtokens;
	extra_classes: Html5_types.nmtokens;
	}

type invalid_options_t =
	{
	prefix: Html5_types.nmtoken;
	base_classes: Html5_types.nmtokens;
	extra_classes: Html5_types.nmtokens;
	}


(********************************************************************************)
(**	{1 Public functors}							*)
(********************************************************************************)

module Make:
	functor (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) ->
	Lambdoc_writer.Maker.WRITABLE with
		type t = Html5_types.div Html5.elt and
		type valid_options_t = valid_options_t and
		type invalid_options_t = invalid_options_t

