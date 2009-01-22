type request_t =
	| Manuscript_from_string of string
	| Composition_from_string of string
	| 

type reply_t =
	| Manuscript of Lambdoc_core.manuscript_t
	| Composition of Lambdoc_core.composition_t
	| XHTML of [ `Div ] XHTML.M.elt

