(********************************************************************************)
(**	Definitios pertaining to internal document references and numbering.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(**	{2 Label module}							*)
(********************************************************************************)

(**	The [Label] module encapsulates label-related definitions.
*)
module Label =
struct
	(**	Label identifiers can either be [`Auto_label] (when they're automatically
		specified by the system) or [`User_label] (when they're manually attributed
		by the user).
	*)
	type t =
		[ `Auto_label of ref_t
		| `User_label of ref_t
		] with sexp
end


(********************************************************************************)
(**	{2 Order module}							*)
(********************************************************************************)

(**	The [Order] module encapsulates order-related definitions.
*)
module Order:
sig
	(**	There are two different ordering schemes: [Numeric] and [Appendic].
		The former is used for main body sections, figures, etc, and is
		entirely nummeric; the latter is used for appendix sections, and
		uses an alphabetic character for the first level.
	*)
	type ordering_scheme_t =
		| Numeric of int list
		| Appendic of int list
		with sexp


	(**	A block has several variants available for its ordering.  Different
		types of blocks allow a different subset of these ordering variants:
		{ul
			{li [`Auto_order]: an automatically attributed ordering;}
			{li [`User_order]: a fixed ordering given by the user;}
			{li [`None_order]: the ordering should skip the block.}}
	*)

	type auto_order_t = [`Auto_order of ordering_scheme_t] with sexp
	type user_order_t = [`User_order of string] with sexp
	type none_order_t = [`None_order] with sexp

	(**	Ordering variants allowed for user sectional blocks.  Though all
		variants are allowed, there are two other restrictions to take
		into account: [`User_order] variants are only allowed in non-top
		level blocks, and non-top level blocks may only use [`User_order]
		or [`None] variants (these two are not enforced by the type-system).
	*)
	type user_sectional_order_t =
		[ auto_order_t
		| user_order_t
		| none_order_t
		] with sexp

	(**	Variants allowed for the preset sectional blocks (these are the TOC,
		the bibliography, and the list of notes).  Note that only the [`None]
		variant is allowed.
	*)
	type preset_sectional_order_t =
		[ none_order_t
		] with sexp

	(**	Variants allowed for floater blocks.  Note that [`None] variants are
		not allowed.  There are furthermore two additional restrictions to
		take into account: [`Auto_order] variants are only allowed outside
		of subpages, and figures inside of subpages may only use [`User_order]
		variants (these two are not enforced by the type-system).
	*)
	type floater_order_t =
		[ auto_order_t
		| user_order_t
		| none_order_t
		] with sexp

	(**	Ordering variants allowed for ghost blocks (bibliography entries
		and notes).  Only automatic numbering is allowed.
	*)
	type ghost_order_t =
		[ auto_order_t
		] with sexp

	(**	The various types of floater orders.
	*)
	type floater_t =
		| Algorithm_floater
		| Equation_floater
		| Figure_floater
		| Table_floater
		with sexp

	(**	The various variations of orderings for visible blocks.
	*)
	type block_order_t =
		private
		| Body_sectional_order of user_sectional_order_t
		| Appendix_sectional_order of user_sectional_order_t
		| Preset_sectional_order of preset_sectional_order_t
		| Floater_order of floater_t * floater_order_t
		with sexp

	(**	At the highest level, an ordered block can either be visible
		(if it can be reference by [\ref], [\sref], or [\mref]), a
		bibliography block (referenced by [\cite]), or a note block
		(referenced by [\see]).
	*)
	type t =
		private
		| Block_order of block_order_t
		| Bib_order of ghost_order_t
		| Note_order of ghost_order_t
		with sexp

	(**	Constructor functions.
	*)

	val body_sectional_order: user_sectional_order_t -> t
	val appendix_sectional_order: user_sectional_order_t -> t
	val preset_sectional_order: preset_sectional_order_t -> t
	val algorithm_order: floater_order_t -> t
	val equation_order: floater_order_t -> t
	val figure_order: floater_order_t -> t
	val table_order: floater_order_t -> t
	val bib_order: ghost_order_t -> t
	val note_order: ghost_order_t -> t
end =
struct
	type ordering_scheme_t =
		| Numeric of int list
		| Appendic of int list
		with sexp

	type auto_order_t = [`Auto_order of ordering_scheme_t] with sexp
	type user_order_t = [`User_order of string] with sexp
	type none_order_t = [`None_order] with sexp

	type user_sectional_order_t =
		[ auto_order_t
		| user_order_t
		| none_order_t
		] with sexp

	type preset_sectional_order_t =
		[ none_order_t
		] with sexp

	type floater_order_t =
		[ auto_order_t
		| user_order_t
		| none_order_t
		] with sexp

	type ghost_order_t =
		[ auto_order_t
		] with sexp

	type floater_t =
		| Algorithm_floater
		| Equation_floater
		| Figure_floater
		| Table_floater
		with sexp

	type block_order_t =
		| Body_sectional_order of user_sectional_order_t
		| Appendix_sectional_order of user_sectional_order_t
		| Preset_sectional_order of preset_sectional_order_t
		| Floater_order of floater_t * floater_order_t
		with sexp

	type t =
		| Block_order of block_order_t
		| Bib_order of ghost_order_t
		| Note_order of ghost_order_t
		with sexp

	let body_sectional_order o = Block_order (Body_sectional_order o)
	let appendix_sectional_order o = Block_order (Appendix_sectional_order o)
	let preset_sectional_order o = Block_order (Preset_sectional_order o)
	let algorithm_order o = Block_order (Floater_order (Algorithm_floater, o))
	let equation_order o = Block_order (Floater_order (Equation_floater, o))
	let figure_order o = Block_order (Floater_order (Figure_floater,  o))
	let table_order o = Block_order (Floater_order (Table_floater, o))
	let bib_order o = Bib_order o
	let note_order o = Note_order o
end


(********************************************************************************)
(**	{2 Label_dict module}							*)
(********************************************************************************)

(**	The label dictionary contains a mapping between the labels used in the
	document and the ordering of the corresponding block.  Note that all
	labels share the same namespace.  Users are therefore encouraged to
	use the informal LaTeX convention of prefixing each label with [fig:],
	[tab:], [sec:], etc.
*)
module Label_dict =
struct
	type t = (Label.t, Order.t) Hashtbl.t with sexp
end

