(********************************************************************************)
(* Ocamlbuild plugin for Lambdoc project.					*)
(********************************************************************************)

open Ocamlbuild_plugin


(* Menhir options *)
let menhir_opts = S
	[
	(*
	A"--strict";
	A"--dump";
	A"--explain";
	A"--graph";
	A"--trace";
	A"--log-automaton"; A"0";
	A"--log-code"; A"0";
	A"--log-grammar"; A"0";
	*)
	];;


(* Ocamldoc options *)
let ocamldoc_opts = S
	[
	(*A"-intro"; A"intro.txt";*)
	];;


let _ = dispatch begin function
	| After_rules ->

		(* Add 'lib' to include directories when building pack *)
		flag ["ocaml"; "pack"] (S[A"-I"; A"lib"]);

		(* Add dependency to lambxml.dtd *)
		dep ["ocamldep"; "file:lib/lambdoc_read_lambxml/dtd.ml"] ["lib/lambdoc_read_lambxml/lambxml.dtd"];

		(* Flag Menhir options. *)
		flag ["menhir"] menhir_opts;

		(* Flag Ocamldoc options. *)
		flag ["ocaml"; "doc"] ocamldoc_opts;

		(* Other miscelaneous custom options. *)
		flag ["ocaml"; "use_thread"; "compile"] (S[A "-thread"]);
		flag ["ocaml"; "use_thread"; "link"] (S[A "-thread"]);
		flag ["ocaml"; "use_thread"; "infer_interface"] (S[A "-thread"]);
		flag ["ocaml"; "use_nocommandline"; "link"] (S[A "-predicates"; A "nocommandline"]);
	| _ -> ()
end

