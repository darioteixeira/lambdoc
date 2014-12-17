(********************************************************************************)
(*	Ocsigen_test.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Part 1 of the Lambdoc+Ocsigen tutorial.

	All examples in this tutorial illustrate a 2-step service: the first step
	prompts the user with a textarea where the input text (formatted using
	some supported markup) may be entered; the second step then shows the
	rendered Lambdoc document (note that in this example, the only supported
	markup is Lambtex).

	In this example, the built-in [Trivial] Lambtex reader is used for parsing
	(this reader uses an extension constructed from the identity monad).  For
	outputting Html5 values, the [Make_trivial] Html5 writer is given Eliom's
	[Html5.F.Raw].  Note that the [Make_trivial] functor is used because an
	extension built from the identity monad suffices.
*)

open Eliom_content
open Html5.F


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Eliom_backend =
struct
	include Eliom_content.Html5.F.Raw
	module Svg = Eliom_content.Svg.F.Raw
end


module Lambdoc_writer = Lambdoc_write_html5.Make_trivial (Eliom_backend)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let sample =
	let ch = Pervasives.open_in "sample.lambtex" in
	let sample = BatPervasives.input_all ch in
	Pervasives.close_in ch;
	sample


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
		~post_params:(Eliom_parameter.string "source")
		step2_handler in
	let step2_form e_source =
		[
		label ~a:[a_for e_source] [pcdata "Source:"];
		textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
		br ();
		button ~button_type:`Submit [pcdata "Submit"];
		] in
	Lwt.return (make_page [post_form step2_service step2_form ()])


and step2_handler () source =
	let doc = Lambdoc_read_lambtex.Trivial.ambivalent_from_string source in
	let xdoc = Lambdoc_writer.write_ambivalent doc in
	let contents =
		[
		(xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt);
		p [a main_service [pcdata "Start again"] ()];
		] in
	Lwt.return (make_page contents)


let () =
	Eliom_registration.Html5.register main_service step1_handler

