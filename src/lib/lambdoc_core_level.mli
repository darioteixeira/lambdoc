(********************************************************************************)
(*  Lambdoc_core_level.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definition of the hierarchy levels used in documents.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(** Definition of hierarchy levels for sections.  We support a
    six-level hierarchy, equivalent to XHTML's H1 to H6.
*)
type section_t = private int with sexp

(** Definition of hierarchy levels for titles.  We support a
    two-level hierarchy, equivalent to XHTML's H1 and H2.
    These can be interpreted as "title" and "subtitle".
*)
type title_t = private int with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** Maximum accepted hierarchical level.
*)
val max_section: int

(** Maximum accepted title level.
*)
val max_title: int

(** Constructor for {!section_t}.  We force the use
    of this constructor by making the type private.
*)
val section: int -> section_t

(** Constructor for {!title_t}.  We force the use
    of this constructor by making the type private.
*)
val title: int -> title_t

