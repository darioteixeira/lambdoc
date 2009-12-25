open Options

type processor_t =
	| Manuscript_io of (string -> Lambdoc_core.Ambivalent.manuscript_t) * (Lambdoc_core.Ambivalent.manuscript_t -> string)
	| Composition_io of (string -> Lambdoc_core.Ambivalent.composition_t) * (Lambdoc_core.Ambivalent.composition_t -> string)


let options = Options.parse ()


let string_of_xhtml x = ""


let processor = match options.category with
	| `Manuscript ->
		let reader = match options.input_markup with
			| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string str)
			| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string str)
			| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string str)
			| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_manuscript str)
		and writer = match options.output_markup with
			| `Sexp  -> Lambdoc_core.Ambivalent.serialize_manuscript
			| `Xhtml -> (fun doc -> string_of_xhtml (Lambdoc_write_xhtml.Main.write_ambivalent_manuscript doc))
		in Manuscript_io (reader, writer)
	| `Composition ->
		let reader = match options.input_markup with
			| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_composition_from_string str)
			| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_composition_from_string str)
			| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_composition_from_string str)
			| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_composition str)
		and writer = match options.output_markup with
			| `Sexp  -> Lambdoc_core.Ambivalent.serialize_composition
			| `Xhtml -> (fun doc -> string_of_xhtml (Lambdoc_write_xhtml.Main.write_ambivalent_composition doc))
		in Composition_io (reader, writer)

