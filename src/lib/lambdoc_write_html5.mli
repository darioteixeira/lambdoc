(********************************************************************************)
(*	Lambdoc_write_html5.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

(**     A functorial interface is used because the user may wish to use this module
	together with Eliom (for instance).  This particular use case is achieved by
	feeding Eliom's [Html5.F.Raw] and [Eliom_content.Svg.F.Raw] to the functor,
	as exemplified by the code below:
	{v
	module My_writer = Lambdoc_write_html5.Make_simple
	(struct
		include Eliom_content.Html5.F.Raw
		module Svg = Eliom_content.Svg.F.Raw
	end)
	v}
*)
module Make:
	functor (Ext: Extension.S) ->
	functor (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) ->
sig
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

	include Writer.S with
		type t = Html5_types.div Html5.elt and
		type 'a monad_t = 'a Ext.Monad.t and
		type linkdata_t = Ext.linkdata_t and
		type imagedata_t = Ext.imagedata_t and
		type extinldata_t = Ext.extinldata_t and
		type extblkdata_t = Ext.extblkdata_t and
		type wconfig_t = Ext.wconfig_t and
		type valid_options_t := valid_options_t and
		type invalid_options_t := invalid_options_t
end


module Make_simple: module type of Make (Lambdoc_writer.Extension.Unitary)

