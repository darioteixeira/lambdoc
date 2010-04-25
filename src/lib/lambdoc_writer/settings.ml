(********************************************************************************)
(*	Settings.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document settings.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	default_bullet: Bullet.t;
	default_numbering: Numbering.t;
	image_lookup: Alias.t -> XHTML.M.uri;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make bullet numbering image_lookup =
	{
	default_bullet = bullet;
	default_numbering = numbering;
	image_lookup = image_lookup;
	}


let default = make Bullet.Disc Numbering.Decimal XHTML.M.uri_of_string

