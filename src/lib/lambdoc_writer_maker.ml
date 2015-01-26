(********************************************************************************)
(*	Lambdoc_writer_maker.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Lambdoc_core
open Lambdoc_writer


(********************************************************************************)
(**	{1 Public signatures}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type WRITABLE =
sig
	type t
	type valid_options_t
	type invalid_options_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val from_valid:
		?valid_options:valid_options_t ->
		link_dict:Extension.link_dict_t ->
		image_dict:Extension.image_dict_t ->
		Valid.t ->
		t

	val from_invalid:
		?invalid_options:invalid_options_t ->
		Invalid.t ->
		t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	type t
	type valid_options_t
	type invalid_options_t
	type 'a monad_t
	type link_writer_t
	type image_writer_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val write_valid:
		?valid_options:valid_options_t ->
		?link_writers:link_writer_t list ->
		?image_writers:image_writer_t list ->
		Valid.t ->
		t monad_t

	val write_invalid:
		?invalid_options:invalid_options_t ->
		Invalid.t ->
		t monad_t

	val write_ambivalent:
		?valid_options:valid_options_t ->
		?invalid_options:invalid_options_t ->
		?link_writers:link_writer_t list ->
		?image_writers:image_writer_t list ->
		Ambivalent.t ->
		t monad_t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make (Writable: WRITABLE) (Ext: Extension.S) : S with
	type t = Writable.t and
	type valid_options_t = Writable.valid_options_t and
	type invalid_options_t = Writable.invalid_options_t and
	type 'a monad_t = 'a Ext.Monad.t and
	type link_writer_t = Ext.link_writer_t and
	type image_writer_t = Ext.image_writer_t =
struct
	type t = Writable.t
	type valid_options_t = Writable.valid_options_t
	type invalid_options_t = Writable.invalid_options_t
	type 'a monad_t = 'a Ext.Monad.t
	type link_writer_t = Ext.link_writer_t
	type image_writer_t = Ext.image_writer_t

	let default_valid_options = Writable.default_valid_options
	let default_invalid_options = Writable.default_invalid_options

	let write_valid ?valid_options ?(link_writers = []) ?(image_writers = []) doc =
		let open Ext in
		let (>>=) = Monad.bind in
		let process_hdata writers hdata =
			let dict = Hashtbl.create (Hashtbl.length hdata) in
			let hdata = Hashtbl.fold (fun k v accum -> (k, v) :: accum) hdata [] in
			let process_hdatum (href, payload) =
				let rec loop = function
					| []	   -> Monad.return href
					| hd :: tl -> hd href payload >>= function
						| Some href' -> Monad.return href'
						| None	     -> loop tl in
				loop writers >>= fun href' ->
				Hashtbl.add dict href href';
				Monad.return ()  in
			Monad.iter process_hdatum hdata >>= fun () ->
			Monad.return dict in
		process_hdata link_writers Valid.(doc.links) >>= fun link_dict ->
		process_hdata image_writers Valid.(doc.images) >>= fun image_dict ->
		Monad.return (Writable.from_valid ?valid_options ~link_dict ~image_dict doc)

	let write_invalid ?invalid_options doc =
		Ext.Monad.return (Writable.from_invalid ?invalid_options doc)

	let write_ambivalent ?valid_options ?invalid_options ?link_writers ?image_writers = function
		| Ambivalent.Valid doc   -> write_valid ?valid_options ?link_writers ?image_writers doc
		| Ambivalent.Invalid doc -> write_invalid ?invalid_options doc
end

