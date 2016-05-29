(********************************************************************************)
(*  Language.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t = Lambdoc_writer.Translations.t


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val converter: t Cmdliner.Arg.converter
val default: t

