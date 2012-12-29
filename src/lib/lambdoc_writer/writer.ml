(********************************************************************************)
(*	Writer.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
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

	val write_ambivalent_manuscript:
		?translations:Translations.t ->
		?settings:Settings.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?extra_classes:Html5_types.nmtokens ->
		Ambivalent.manuscript_t -> t

	val write_ambivalent_composition:
		?translations:Translations.t ->
		?settings:Settings.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?extra_classes:Html5_types.nmtokens ->
		Ambivalent.composition_t -> t

	val write_valid_manuscript:
		?translations:Translations.t ->
		?settings:Settings.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?extra_classes:Html5_types.nmtokens ->
		Valid.manuscript_t -> t

	val write_valid_composition:
		?translations:Translations.t ->
		?settings:Settings.t ->
		?book_lookup:book_lookup_t ->
		?cover_lookup:cover_lookup_t ->
		?image_lookup:image_lookup_t ->
		?extra_classes:Html5_types.nmtokens ->
		Valid.composition_t -> t

	val write_invalid_manuscript:
		?extra_classes:Html5_types.nmtokens ->
		Invalid.manuscript_t -> t

	val write_invalid_composition:
		?extra_classes:Html5_types.nmtokens ->
		Invalid.composition_t -> t
end

