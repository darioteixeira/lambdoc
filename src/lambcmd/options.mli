type t =
	{
	debug: bool;
	category: Category.t;
	input_markup: Markup.input_t;
	output_markup: Markup.output_t;
	input_chan: Pervasives.in_channel;
	output_chan: Pervasives.out_channel;
	}

val parse: unit -> t

