(********************************************************************************)
(*	Writer.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Eliom_content
open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Lookup function that converts a book ISBN into an actual URI.
*)
type book_lookup_t = Book.isbn_t -> Html5.F.uri


(**	Lookup function that converts a book ISBN and cover specification into an actual URI.
*)
type cover_lookup_t = Book.isbn_t -> Book.coversize_t -> Html5.F.uri


(**	Lookup function that converts an image alias into an actual URI.
*)
type image_lookup_t = Alias.t -> Html5.F.uri


(**	The module type that all wannabe document writers must export.
*)
module type S =
sig
	type t

	val write_valid:
		?numbered_paragraphs:bool ->
		?translations:Translations.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?namespace:Html5_types.nmtoken ->
		?prefix:Html5_types.nmtoken ->
		?base_classes:Html5_types.nmtokens ->
		?extra_classes:Html5_types.nmtokens ->
		Valid.t -> t

	val write_invalid:
		?prefix:Html5_types.nmtoken ->
		?base_classes:Html5_types.nmtokens ->
		?extra_classes:Html5_types.nmtokens ->
		Invalid.t -> t

	val write_ambivalent:
		?numbered_paragraphs:bool ->
		?translations:Translations.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?namespace:Html5_types.nmtoken ->
		?prefix:Html5_types.nmtoken ->
		?base_classes:Html5_types.nmtokens ->
		?extra_classes:Html5_types.nmtokens ->
		Ambivalent.t -> t
end

