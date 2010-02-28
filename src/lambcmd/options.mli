(********************************************************************************)
(*	Options.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	debug: bool;
	title: string;
	category: Category.t;
	input_markup: Markup.input_t;
	output_markup: Markup.output_t;
	input_chan: Pervasives.in_channel;
	output_chan: Pervasives.out_channel;
	input_cleaner: Pervasives.in_channel -> unit;
	output_cleaner: Pervasives.out_channel -> unit;
	}


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val parse: unit -> t

