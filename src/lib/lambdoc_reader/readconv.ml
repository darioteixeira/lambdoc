(********************************************************************************)
(*	Readconv.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for converting {!Ast} values to {!Lambdoc_core} values.
*)

open Lambdoc_core

module String = BatString


(********************************************************************************)
(**	{1 Submodule definitions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Basic values}							*)
(********************************************************************************)

module Basic_input =
struct
	let entity_list =
		[
		("Aacute",	193);
		("aacute",	225);
		("Acirc",	194);
		("acirc",	226);
		("acute",	180);
		("AElig",	198);
		("aelig",	230);
		("Agrave",	192);
		("agrave",	224);
		("alefsym",	8501);
		("Alpha",	913);
		("alpha",	945);
		("amp",		38);
		("and",		8743);
		("ang",		8736);
		("apos",	39);
		("Aring",	197);
		("aring",	229);
		("asymp",	8776);
		("Atilde",	195);
		("atilde",	227);
		("Auml",	196);
		("auml",	228);
		("bdquo",	8222);
		("Beta",	914);
		("beta",	946);
		("brvbar",	166);
		("bull",	8226);
		("cap",		8745);
		("Ccedil",	199);
		("ccedil",	231);
		("cedil",	184);
		("cent",	162);
		("Chi",		935);
		("chi",		967);
		("circ",	710);
		("clubs",	9827);
		("cong",	8773);
		("copy",	169);
		("crarr",	8629);
		("cup",		8746);
		("curren",	164);
		("dagger",	8224);
		("Dagger",	8225);
		("darr",	8595);
		("dArr",	8659);
		("deg",		176);
		("Delta",	916);
		("delta",	948);
		("diams",	9830);
		("divide",	247);
		("Eacute",	201);
		("eacute",	233);
		("Ecirc",	202);
		("ecirc",	234);
		("Egrave",	200);
		("egrave",	232);
		("empty",	8709);
		("emsp",	8195);
		("ensp",	8194);
		("Epsilon",	917);
		("epsilon",	949);
		("equiv",	8801);
		("Eta",		919);
		("eta",		951);
		("ETH",		208);
		("eth",		240);
		("Euml",	203);
		("euml",	235);
		("euro",	8364);
		("exist",	8707);
		("fnof",	402);
		("forall",	8704);
		("frac12",	189);
		("frac14",	188);
		("frac34",	190);
		("frasl",	8260);
		("Gamma",	915);
		("gamma",	947);
		("ge",		8805);
		("gt",		62);
		("harr",	8596);
		("hArr",	8660);
		("hearts",	9829);
		("hellip",	8230);
		("Iacute",	205);
		("iacute",	237);
		("Icirc",	206);
		("icirc",	238);
		("iexcl",	161);
		("Igrave",	204);
		("igrave",	236);
		("image",	8465);
		("infin",	8734);
		("int",		8747);
		("Iota",	921);
		("iota",	953);
		("iquest",	191);
		("isin",	8712);
		("Iuml",	207);
		("iuml",	239);
		("Kappa",	922);
		("kappa",	954);
		("Lambda",	923);
		("lambda",	955);
		("lang",	9001);
		("laquo",	171);
		("larr",	8592);
		("lArr",	8656);
		("lceil",	8968);
		("ldquo",	8220);
		("le",		8804);
		("lfloor",	8970);
		("lowast",	8727);
		("loz",		9674);
		("lrm",		8206);
		("lsaquo",	8249);
		("lsquo",	8216);
		("lt",		60);
		("macr",	175);
		("mdash",	8212);
		("micro",	181);
		("middot",	183);
		("minus",	8722);
		("Mu",		924);
		("mu",		956);
		("nabla",	8711);
		("nbsp",	160);
		("ndash",	8211);
		("ne",		8800);
		("ni",		8715);
		("not",		172);
		("notin",	8713);
		("nsub",	8836);
		("Ntilde",	209);
		("ntilde",	241);
		("Nu",		925);
		("nu",		957);
		("Oacute",	211);
		("oacute",	243);
		("Ocirc",	212);
		("ocirc",	244);
		("OElig",	338);
		("oelig",	339);
		("Ograve",	210);
		("ograve",	242);
		("oline",	8254);
		("Omega",	937);
		("omega",	969);
		("Omicron",	927);
		("omicron",	959);
		("oplus",	8853);
		("or",		8744);
		("ordf",	170);
		("ordm",	186);
		("Oslash",	216);
		("oslash",	248);
		("Otilde",	213);
		("otilde",	245);
		("otimes",	8855);
		("Ouml",	214);
		("ouml",	246);
		("para",	182);
		("part",	8706);
		("permil",	8240);
		("perp",	8869);
		("Phi",		934);
		("phi",		966);
		("Pi",		928);
		("pi",		960);
		("piv",		982);
		("plusmn",	177);
		("pound",	163);
		("prime",	8242);
		("Prime",	8243);
		("prod",	8719);
		("prop",	8733);
		("Psi",		936);
		("psi",		968);
		("quot",	34);
		("radic",	8730);
		("rang",	9002);
		("raquo",	187);
		("rarr",	8594);
		("rArr",	8658);
		("rceil",	8969);
		("rdquo",	8221);
		("real",	8476);
		("reg",		174);
		("rfloor",	8971);
		("Rho",		929);
		("rho",		961);
		("rlm",		8207);
		("rsaquo",	8250);
		("rsquo",	8217);
		("sbquo",	8218);
		("Scaron",	352);
		("scaron",	353);
		("sdot",	8901);
		("sect",	167);
		("shy",		173);
		("Sigma",	931);
		("sigma",	963);
		("sigmaf",	962);
		("sim",		8764);
		("spades",	9824);
		("sub",		8834);
		("sube",	8838);
		("sum",		8721);
		("sup1",	185);
		("sup2",	178);
		("sup3",	179);
		("sup",		8835);
		("supe",	8839);
		("szlig",	223);
		("Tau",		932);
		("tau",		964);
		("there4",	8756);
		("Theta",	920);
		("theta",	952);
		("thetasym",	977);
		("thinsp",	8201);
		("THORN",	222);
		("thorn",	254);
		("tilde",	732);
		("times",	215);
		("trade",	8482);
		("Uacute",	218);
		("uacute",	250);
		("uarr",	8593);
		("uArr",	8657);
		("Ucirc",	219);
		("ucirc",	251);
		("Ugrave",	217);
		("ugrave",	249);
		("uml",		168);
		("upsih",	978);
		("Upsilon",	933);
		("upsilon",	965);
		("Uuml",	220);
		("uuml",	252);
		("weierp",	8472);
		("Xi",		926);
		("xi",		958);
		("Yacute",	221);
		("yacute",	253);
		("yen",		165);
		("yuml",	255);
		("Yuml",	376);
		("Zeta",	918);
		("zeta",	950);
		("zwj",		8205);
		("zwnj",	8204);
		]


	let entity_map =
		let map = Hashtbl.create (List.length entity_list) in
		List.iter (fun (k, v) -> Hashtbl.add map k v) entity_list;
		map


	let utf8_of_codepoint num =
		BatUTF8.of_char (BatUChar.chr num)


	let expand_entity =
		let rex = Pcre.regexp "^(?:(?:#x([a-zA-Z0-9]+))|(?:#([a-zA-Z0-9]+))|([[a-zA-Z0-9]+))$" in
		fun ent ->
			try match Pcre.get_opt_substrings (Pcre.exec ~rex ent) with
				| [| _; Some x; None; None |] ->
					(try `Okay (ent, utf8_of_codepoint (int_of_string ("0x" ^ x))) with _ -> `Error (Error.Invalid_entity_hexa ent))
				| [| _; None; Some x; None |] ->
					(try `Okay (ent, utf8_of_codepoint (int_of_string x)) with _ -> `Error (Error.Invalid_entity_deci ent))
				| [| _; None; None; Some x |] ->
					(try `Okay (ent, utf8_of_codepoint (Hashtbl.find entity_map x)) with _-> `Error (Error.Invalid_entity_name ent))
				| _ ->
					`Error (Error.Invalid_entity_name ent)
			with
				| _ ->
					`Error (Error.Invalid_entity_name ent)


	let matches_ident =
		let rex = Pcre.regexp "^[a-zA-Z][a-zA-Z0-9\\.:-_]*$" in
		fun str -> Pcre.pmatch ~rex str
end


(********************************************************************************)
(**	{2 Literal values (verbatim and source environments)}			*)
(********************************************************************************)

module Literal_input =
struct
	let trim =
		let left_rex = Pcre.regexp "^(\\s*[\n\r]+)*(.)" in
		let right_rex = Pcre.regexp "(\\s*[\n\r]+)*\\s*$" in
		fun str ->
			let left_trimmed = Pcre.replace_first ~rex:left_rex ~templ:"$2" str in
			Pcre.replace_first ~rex:right_rex ~templ:"" left_trimmed
end


(********************************************************************************)
(**	{2 Tabular values}							*)
(********************************************************************************)

module Tabular_input =
struct
	open Tabular

	let colspec_of_string = function
		| "c" -> (Center, Normal)
		| "C" -> (Center, Strong)
		| "l" -> (Left, Normal)
		| "L" -> (Left, Strong)
		| "r" -> (Right, Normal)
		| "R" -> (Right, Strong)
		| "j" -> (Justify, Normal)
		| "J" -> (Justify, Strong)
		| _   -> invalid_arg "colspec_of_string"

	let cellspec_of_string =
		let rex = Pcre.regexp "^(?<colspan>[0-9]+)(?<colspec>[a-zA-Z]+)(?<hline>(_|\\^|_\\^|\\^_)?)$" in
		fun str ->
			let subs = Pcre.exec ~rex str in
			let colspan = int_of_string (Pcre.get_named_substring rex "colspan" subs)
			and colspec = colspec_of_string (Pcre.get_named_substring rex "colspec" subs)
			and (overline, underline) = match Pcre.get_named_substring rex "hline" subs with
				| "^_" | "_^" -> (true, true)
				| "^"	      -> (true, false)
				| "_"	      -> (false, true)
				| _	      -> (false, false)
			in (colspec, colspan, overline, underline)
end


(********************************************************************************)
(**	{2 Order values}							*)
(********************************************************************************)

module Order_input =
struct
	open Basic
	open Order

	exception Invalid_order_format of string
	exception Invalid_order_levels of string * Level.hierarchical_t * int


	type ordinal_counter_t = ordinal_t
	type hierarchical_counter_t = int * int * int


	let make_ordinal_counter () = ref 0


	let make_hierarchy_counter () = ref (0, 0, 0)


	let auto_ordinal counter =
		let () = incr counter in
		`Auto_given !counter


	let auto_hierarchical level counter =
		let (l1, l2, l3) = match (level, !counter) with
			| (`Level1, (l1, _, _))		-> (l1+1, 0, 0)
			| (`Level2, (l1, l2, _))	-> (l1, l2+1, 0)
			| (`Level3, (l1, l2, l3))	-> (l1, l2, l3+1) in
		counter := (l1, l2, l3);
		match level with
			| `Level1 -> `Auto_given (Level1_order l1)
			| `Level2 -> `Auto_given (Level2_order (l1, l2))
			| `Level3 -> `Auto_given (Level3_order (l1, l2, l3))


	let user_ordinal str =
		try `User_given (int_of_string str)
		with Failure "int_of_string" -> raise (Invalid_order_format str)


	let user_hierarchical level str =
		try match (level, String.nsplit str ".") with
			| (`Level1, [l1])	  -> `User_given (Level1_order (int_of_string l1))
			| (`Level2, [l1; l2])	  -> `User_given (Level2_order (int_of_string l1, int_of_string l2))
			| (`Level3, [l1; l2; l3]) -> `User_given (Level3_order (int_of_string l1, int_of_string l2, int_of_string l3))
			| (expected, found)	  -> raise (Invalid_order_levels (str, expected, List.length found))
		with
			Failure "int_of_string" -> raise (Invalid_order_format str)


	let no_order () = `None_given
end


(********************************************************************************)
(**	{2 Math values}								*)
(********************************************************************************)

module Math_input =
struct
	open Math

	let from_mathtex mathtex =
		let mathml = Blahcaml.safe_mathml_from_tex mathtex in
		Both (mathtex, mathml)

	let from_mathml mathml =
		let sane = Blahcaml.sanitize_mathml mathml in
		Mathml sane
end

