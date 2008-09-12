(********************************************************************************)
(**	Converts documents into XHTML.  The XHTML representation used is
	the one offered by Ocsigen's XHTML.M module.  This allows the direct
	use of the output of this module from within Ocsigen programmes.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Document_valid
open Document_invalid
open Document_ambivalent


(********************************************************************************)
(*	{2 Public functions}							*)
(********************************************************************************)

(**	Converts an ambivalent manuscript (valid or invalid) into XHTML.
*)
val ambivalent_manuscript_to_xhtml:
	Ambivalent.manuscript_t -> [> `Div ] XHTML.M.elt

(**	Converts a valid manuscript into XHTML.
*)
val valid_manuscript_to_xhtml:
	Valid.manuscript_t -> [> `Div ] XHTML.M.elt

(**	Converts an invalid manuscript into XHTML.
*)
val invalid_manuscript_to_xhtml:
	Invalid.manuscript_t -> [> `Div ] XHTML.M.elt

(**	Converts an ambivalent composition (valid or invalid) into XHTML.
*)
val ambivalent_composition_to_xhtml:
	Ambivalent.composition_t -> [> `Div ] XHTML.M.elt

(**	Converts a valid composition into XHTML.
*)
val valid_composition_to_xhtml:
	Valid.composition_t -> [> `Div ] XHTML.M.elt

(**	Converts an invalid composition into XHTML.
*)
val invalid_composition_to_xhtml:
	Invalid.composition_t -> [> `Div ] XHTML.M.elt

