(********************************************************************************)
(*	Heading.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning heading elements.
*)

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Definition of the ordering types for the various kinds of headings
*)
type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp


(**	Part content.
*)
type part_content_t =
	[ `Custom of Inline.seq_t
	| `Appendix
	] with sexp


(**	Section content.
*)
type section_content_t =
	[ `Custom of Inline.seq_t
	| `Bibliography
	| `Notes
	| `Toc
	] with sexp


(**	Section locations.
*)
type section_location_t =
	[ `Mainbody
	| `Appendixed
	] with sexp


(**	Heading blocks.
*)
type heading_t =
	[ `Part of Label.t * part_order_t * part_content_t
	| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
	| `Parhead of Inline.seq_t
	] with sexp


(**	Phantom-type wrapper around {!heading_t}.
*)
type (+'a, +'b, +'c, +'d, +'e) t = private [< heading_t ] with sexp


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val part: Label.t -> part_order_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Part_blk ]) t

val appendix: Label.t ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Part_blk ]) t

val section: Label.t -> section_order_t -> section_location_t -> hierarchical_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Section_blk ]) t

val bibliography: Label.t ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Section_blk ]) t

val notes: Label.t ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Section_blk ]) t

val toc: Label.t ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Section_blk ]) t

val parhead: ('a, _) Inline.t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Parhead_blk ]) t

val get_heading: (_, _, _, _, _) t ->
	heading_t

