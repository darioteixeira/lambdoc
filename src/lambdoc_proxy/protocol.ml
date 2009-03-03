type request_t =
	| Manuscript_from_string of string
	| Composition_from_string of string

type reply_t =
	| Manuscript of Lambdoc_core.Ambivalent.manuscript_t
	| Composition of Lambdoc_core.Ambivalent.composition_t

