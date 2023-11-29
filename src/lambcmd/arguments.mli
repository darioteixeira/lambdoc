open Lambdoc_prelude


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    title: string;
    language: Language.t;

    unrestricted: bool;
    max_macro_depth: int option;
    max_inline_depth: int option;
    max_block_depth: int option;

    input_markup: Markup.input;
    output_markup: Markup.output;

    input_chan: Pervasives.in_channel;
    output_chan: Pervasives.out_channel;
    input_cleaner: Pervasives.in_channel -> unit;
    output_cleaner: Pervasives.out_channel -> unit;
    }


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

val parse: unit -> t

