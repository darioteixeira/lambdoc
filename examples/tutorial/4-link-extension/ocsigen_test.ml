(********************************************************************************)
(*	Ocsigen_test.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Part 4 of the Lambdoc+Ocsigen tutorial.

	This fourth part of the tutorial illustrates the creation of a custom
	extension attached to links.  The module [Extension] defines a Lambdoc
	extension wrapped under the Lwt monad.  Whereas images pass through the
	extension unmodified and custom inline/block commands are altogether
	undefined, we check if links are using the protocol "user", in which
	case they are specially processed (the actual processing is outside
	the scope of this example, though).
*)

open Eliom_content
open Html5.F

module String = BatString


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Extension =
struct
	module Monad = struct include Lwt let iter = Lwt_list.iter_p end

	type linkdata_t = [ `User of string | `Other of string ]
	type imagedata_t = unit
	type extinldata_t = unit
	type extblkdata_t = unit
	type rconfig_t = unit
	type wconfig_t = unit

	let find_user name =
		(* Insert code to check if user actually exists in the system *)
		Lwt.return (`Okay (`User name))

	let linkify_user name =
		(* Insert code to create a link to the user's home page *)
		name

	let extinldefs = []	(* We do not define any custom inline commands *)

	let extblkdefs = []	(* We do not define any custom block commands *)

	let read_link ?rconfig href = match href with
		| x when String.starts_with x "user:" -> find_user (String.lchop ~n:5 x)
		| x				      -> Lwt.return (`Okay (`Other x))

	let read_image ?rconfig href =
		Lwt.return (`Okay ())

	let read_extinl ?rconfig tag extcomm =
		assert false	(* This should never be called, because we haven't defined any custom inline commands *)

	let read_extblk ?rconfig tag extcomm =
		assert false	(* This should never be called, because we haven't defined any custom block commands *)

	let write_link ?wconfig href = function
		| `User u ->
			let open Lambdoc_core in
			let href = linkify_user u in
			let seq = [Inline.plain "Estimeed User "; Inline.bold [Inline.plain u]] in
			Lwt.return (href, Some seq)
		| `Other x ->
			Lwt.return (href, None)

	let write_image ?wconfig href _ =
		Lwt.return href

	let write_extinl ?wconfig tag extinl data =
		assert false	(* This should never be called, because we haven't defined any custom inline commands *)

	let write_extblk ?wconfig tag extblk data =
		assert false	(* This should never be called, because we haven't defined any custom block commands *)
end


module Lambtex_reader = Lambdoc_read_lambtex.Make (Extension)
module Lambwiki_reader = Lambdoc_read_lambwiki.Make (Extension)
module Lambxml_reader = Lambdoc_read_lambxml.Make (Extension)
module Markdown_reader = Lambdoc_read_markdown.Make (Extension)


module Eliom_backend =
struct
	include Eliom_content.Html5.F.Raw
	module Svg = Eliom_content.Svg.F.Raw
end


module Lambdoc_writer = Lambdoc_write_html5.Make (Extension) (Eliom_backend)


module Markup = Litiom_choice.Make
(struct
	type t = [ `Lambtex | `Lambwiki | `Lambxml | `Markdown ]

	let of_string = function
		| "lambtex"  -> `Lambtex
		| "lambwiki" -> `Lambwiki
		| "lambxml"  -> `Lambxml
		| "markdown" -> `Markdown
		| x	     -> invalid_arg ("Markup.of_string: " ^ x)

	let to_string = function
		| `Lambtex  -> "lambtex"
		| `Lambwiki -> "lambwiki"
		| `Lambxml  -> "lambxml"
		| `Markdown -> "markdown"

	let describe = to_string

	let all = [ `Lambtex; `Lambwiki; `Lambxml; `Markdown ]
end)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_page content =
	let css_uri = make_uri (Eliom_service.static_dir ()) ["css"; "lambdoc.css"] in
	(html
		(head
			(title (pcdata "Lambdoc + Ocsigen : Basic example"))
			[css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()])
		(body content))


let main_service = Eliom_service.Http.service 
	~path:[""]
	~get_params:Eliom_parameter.unit
	()


let rec step1_handler () () =
	let step2_service = Eliom_registration.Html5.register_post_coservice
		~scope:Eliom_common.default_session_scope
		~fallback:main_service
		~post_params:Eliom_parameter.(Markup.param "markup" ** string "source")
		step2_handler in
	let step2_form (e_markup, e_source) =
		[
		label ~a:[a_for e_markup] [pcdata "Markup:"];
		Markup.choose ~name:e_markup ~value:`Lambtex ();
		br ();
		label ~a:[a_for e_source] [pcdata "Source:"];
		textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:"Lorem ipsum" ();
		br ();
		button ~button_type:`Submit [pcdata "Submit"];
		] in
	Lwt.return (make_page [post_form step2_service step2_form ()])


and step2_handler () (markup, source) =
	let reader = match markup with
		| `Lambtex  -> Lambtex_reader.ambivalent_from_string
		| `Lambwiki -> Lambwiki_reader.ambivalent_from_string
		| `Lambxml  -> Lambxml_reader.ambivalent_from_string
		| `Markdown -> Markdown_reader.ambivalent_from_string in
	let feature_ruleset = [`Only `Feature_bold, `Deny] in
	let idiosyncrasies = Lambdoc_core.Idiosyncrasies.make ~feature_ruleset () in
	lwt doc = reader ~idiosyncrasies source in
	lwt xdoc = Lambdoc_writer.write_ambivalent doc in
	let contents =
		[
		(xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt);
		p [a main_service [pcdata "Start again"] ()];
		] in
	Lwt.return (make_page contents)


let () =
	Eliom_registration.Html5.register main_service step1_handler

