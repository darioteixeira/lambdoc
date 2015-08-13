(********************************************************************************)
(*  Test_lambtex.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


let ambivalent =
    let module M =
    struct
        type t = Ambivalent.t

        let pp fmt doc =
            Format.pp_print_string fmt (Ambivalent.serialize doc)

        let equal = (=)
    end in
    (module M: Alcotest.TESTABLE with type t = M.t)


let inline_helper assertion source seq () =
    let module Reader = Lambdoc_rlambtex_reader.Trivial in
    let doc1 = Ambivalent.valid (Valid.make [Block.paragraph seq]) in
    let doc2 = Reader.ambivalent_from_string ~postprocessor:Reader.Ext.Foldmapper.amnesiac source in
    Alcotest.(check ambivalent) assertion doc1 doc2


let inline_testset =
    [
    ("Plain text", `Quick, inline_helper "plain" "hello" Inline.[plain "hello"]);
    ("Entity text", `Quick, inline_helper "entity" "&mdash;" Inline.[plain "—"]);
    ("Line break", `Quick, inline_helper "linebreak" "\\br" Inline.[linebreak ()]);
    ("Bold text", `Quick, inline_helper "bold" "\\bold{hello}" Inline.[bold [plain "hello"]]);
    ("Emphasized text", `Quick, inline_helper "emph" "\\emph{hello}" Inline.[emph [plain "hello"]]);
    ("Monospaced text", `Quick, inline_helper "code" "\\code{hello}" Inline.[code [plain "hello"]]);
    ("Caps text", `Quick, inline_helper "caps" "\\caps{hello}" Inline.[caps [plain "hello"]]);
    ("Ins text", `Quick, inline_helper "ins" "\\ins{hello}" Inline.[ins [plain "hello"]]);
    ("Del text", `Quick, inline_helper "del" "\\del{hello}" Inline.[del [plain "hello"]]);
    ("Superscript text", `Quick, inline_helper "sup" "\\sup{hello}" Inline.[sup [plain "hello"]]);
    ("Subscript text", `Quick, inline_helper "sub" "\\sub{hello}" Inline.[sub [plain "hello"]]);
    ("Mbox text", `Quick, inline_helper "mbox" "\\mbox{hello}" Inline.[mbox [plain "hello"]]);
    ("Span text", `Quick, inline_helper "span" "\\span{hello}" Inline.[span [plain "hello"]]);
    ("Link with text", `Quick, inline_helper "link_seq" "\\link{http://ocaml.org/}{OCaml}" Inline.[link "http://ocaml.org/" (Some [plain "OCaml"])]);
    ("Link without text", `Quick, inline_helper "link_noseq" "\\link{http://ocaml.org/}" Inline.[link "http://ocaml.org/" None]);
    ]


let tests =
    [
    ("Inline elements", inline_testset);
    ]


let () =
    Alcotest.run "Lambtex tests" tests

