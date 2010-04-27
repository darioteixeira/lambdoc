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
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make bullet numbering =
	{
	default_bullet = bullet;
	default_numbering = numbering;
	}


let default = make Bullet.Disc Numbering.Decimal

