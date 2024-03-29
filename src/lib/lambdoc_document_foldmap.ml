module Valid = Lambdoc_document_valid

open Lambdoc_prelude
open Valid
open Block
open Heading
open Inline
open Tabular
open Qanda
open Wrapper


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module IO: Monad.S

    type 'a t =
        {
        valid:       'a t -> 'a -> Valid.t -> ('a * Valid.t) IO.t;

        attr:        'a t -> 'a -> Attr.t -> ('a * Attr.t) IO.t;

        inline:      'a t -> 'a -> Inline.t -> ('a * Inline.t) IO.t;
        block:       'a t -> 'a -> Block.t -> ('a * Block.t) IO.t;
        seq:         'a t -> 'a -> Inline.seq -> ('a * Inline.seq) IO.t;
        frag:        'a t -> 'a -> Block.frag -> ('a * Block.frag) IO.t;

        plain:       'a t -> 'a -> Attr.t -> string -> ('a * Inline.t) IO.t;
        entity:      'a t -> 'a -> Attr.t -> entity -> ('a * Inline.t) IO.t;
        linebreak:   'a t -> 'a -> Attr.t -> ('a * Inline.t) IO.t;
        math_inl:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Inline.t) IO.t;
        code:        'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Inline.t) IO.t;
        glyph:       'a t -> 'a -> Attr.t -> href -> string -> string option -> ('a * Inline.t) IO.t;
        bold:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        emph:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mono:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        caps:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        ins:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        del:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sup:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sub:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mbox:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        span:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        link:        'a t -> 'a -> Attr.t -> href -> Inline.seq option -> ('a * Inline.t) IO.t;
        see:         'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        cite:        'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        dref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        sref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        mref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq -> ('a * Inline.t) IO.t;

        paragraph:   'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Block.t) IO.t;
        itemize:     'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        enumerate:   'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        description: 'a t -> 'a -> Attr.t -> (Inline.seq * Block.frag) list -> ('a * Block.t) IO.t;
        qanda:       'a t -> 'a -> Attr.t -> (Qanda.t * Block.frag) list -> ('a * Block.t) IO.t;
        verse:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        quote:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        math_blk:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Block.t) IO.t;
        source:      'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Block.t) IO.t;
        tabular:     'a t -> 'a -> Attr.t -> Tabular.t -> ('a * Block.t) IO.t;
        subpage:     'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        verbatim:    'a t -> 'a -> Attr.t -> string -> ('a * Block.t) IO.t;
        picture:     'a t -> 'a -> Attr.t -> href -> string -> string option -> int option -> ('a * Block.t) IO.t;
        pullquote:   'a t -> 'a -> Attr.t -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        boxout:      'a t -> 'a -> Attr.t -> Custom.boxout -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        theorem:     'a t -> 'a -> Attr.t -> Custom.theorem -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        equation:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        printout:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        table:       'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        figure:      'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        heading:     'a t -> 'a -> Attr.t -> Heading.t -> ('a * Block.t) IO.t;
        autogen:     'a t -> 'a -> Attr.t -> Autogen.t -> ('a * Block.t) IO.t;
        title:       'a t -> 'a -> Attr.t -> Level.title -> Inline.seq -> ('a * Block.t) IO.t;
        abstract:    'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        rule:        'a t -> 'a -> Attr.t -> ('a * Block.t) IO.t;
        }

    val aux_list: ('a t -> 'a -> 'b -> ('a * 'c) IO.t) -> 'a t -> 'a -> 'b list -> ('a * 'c list) IO.t
    val aux_maybe: ('a t -> 'a -> 'b -> ('a * 'c) IO.t) -> 'a t -> 'a -> 'b option -> ('a * 'c option) IO.t
    val aux_seq: (?attr:Attr.t -> Inline.seq -> Inline.t) -> 'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t
    val aux_frag: (?attr:Attr.t -> Block.frag -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t
    val aux_frags: (?attr:Attr.t -> Block.frag list -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t
    val aux_wrapper: (?attr:Attr.t -> Wrapper.t -> Block.t -> Block.t) -> 'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t

    val identity: 'a t
    val amnesiac: 'a t
end


(********************************************************************************)
(** {1 Public modules and functors}                                             *)
(********************************************************************************)

module Make (M: Monad.S) =
struct
    module IO = M

    type 'a t =
        {
        valid:       'a t -> 'a -> Valid.t -> ('a * Valid.t) IO.t;
            
        attr:        'a t -> 'a -> Attr.t -> ('a * Attr.t) IO.t;

        inline:      'a t -> 'a -> Inline.t -> ('a * Inline.t) IO.t;
        block:       'a t -> 'a -> Block.t -> ('a * Block.t) IO.t;
        seq:         'a t -> 'a -> Inline.seq -> ('a * Inline.seq) IO.t;
        frag:        'a t -> 'a -> Block.frag -> ('a * Block.frag) IO.t;

        plain:       'a t -> 'a -> Attr.t -> string -> ('a * Inline.t) IO.t;
        entity:      'a t -> 'a -> Attr.t -> entity -> ('a * Inline.t) IO.t;
        linebreak:   'a t -> 'a -> Attr.t -> ('a * Inline.t) IO.t;
        math_inl:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Inline.t) IO.t;
        code:        'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Inline.t) IO.t;
        glyph:       'a t -> 'a -> Attr.t -> href -> string -> string option -> ('a * Inline.t) IO.t;
        bold:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        emph:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mono:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        caps:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        ins:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        del:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sup:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sub:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mbox:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        span:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        link:        'a t -> 'a -> Attr.t -> href -> Inline.seq option -> ('a * Inline.t) IO.t;
        see:         'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        cite:        'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        dref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        sref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        mref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq -> ('a * Inline.t) IO.t;
        
        paragraph:   'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Block.t) IO.t;
        itemize:     'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        enumerate:   'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        description: 'a t -> 'a -> Attr.t -> (Inline.seq * Block.frag) list -> ('a * Block.t) IO.t;
        qanda:       'a t -> 'a -> Attr.t -> (Qanda.t * Block.frag) list -> ('a * Block.t) IO.t;
        verse:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        quote:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        math_blk:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Block.t) IO.t;
        source:      'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Block.t) IO.t;
        tabular:     'a t -> 'a -> Attr.t -> Tabular.t -> ('a * Block.t) IO.t;
        subpage:     'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        verbatim:    'a t -> 'a -> Attr.t -> string -> ('a * Block.t) IO.t;
        picture:     'a t -> 'a -> Attr.t -> href -> string -> string option -> int option -> ('a * Block.t) IO.t;
        pullquote:   'a t -> 'a -> Attr.t -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        boxout:      'a t -> 'a -> Attr.t -> Custom.boxout -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        theorem:     'a t -> 'a -> Attr.t -> Custom.theorem -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        equation:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        printout:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        table:       'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        figure:      'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        heading:     'a t -> 'a -> Attr.t -> Heading.t -> ('a * Block.t) IO.t;
        autogen:     'a t -> 'a -> Attr.t -> Autogen.t -> ('a * Block.t) IO.t;
        title:       'a t -> 'a -> Attr.t -> Level.title -> Inline.seq -> ('a * Block.t) IO.t;
        abstract:    'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        rule:        'a t -> 'a -> Attr.t -> ('a * Block.t) IO.t;
        }

    let (>>=) = IO.bind

    let aux_list f fm acc xs =
        let aux x (acc_alpha, acc_xs) =
            f fm acc_alpha x >>= fun (acc_alpha, x) ->
            IO.return (acc_alpha, x :: acc_xs) in
        IO.fold_right aux xs (acc, [])

    let aux_maybe f fm acc = function
        | Some x ->
            f fm acc x >>= fun (acc, x) ->
            IO.return (acc, Some x)
        | None ->
            IO.return (acc, None)

    let aux_seq (cons:?attr:Attr.t -> Inline.seq -> Inline.t) fm acc attr seq =
        fm.seq fm acc seq >>= fun (acc, seq) ->
        fm.attr fm acc attr >>= fun (acc, attr) ->
        IO.return (acc, cons ~attr seq)

    let aux_frag (cons: ?attr:Attr.t -> Block.frag -> Block.t) fm acc attr frag =
        fm.frag fm acc frag >>= fun (acc, frag) ->
        fm.attr fm acc attr >>= fun (acc, attr) ->
        IO.return (acc, cons ~attr frag)

    let aux_frags (cons: ?attr:Attr.t -> Block.frag list -> Block.t) fm acc attr frags =
        aux_list fm.frag fm acc frags >>= fun (acc, frags) ->
        fm.attr fm acc attr >>= fun (acc, attr) ->
        IO.return (acc, cons ~attr frags)

    let aux_wrapper (cons: ?attr:Attr.t -> Wrapper.t -> Block.t -> Block.t) fm acc attr wrapper blk =
        begin match wrapper with
            | Ordered (label, order, maybe_seq) ->
                aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
                IO.return (acc, Ordered (label, order, maybe_seq))
            | Unordered (label, seq) ->
                fm.seq fm acc seq >>= fun (acc, seq) ->
                IO.return (acc, Unordered (label, seq))
        end >>= fun (acc, wrapper) ->
        fm.block fm acc blk >>= fun (acc, blk) ->
        fm.attr fm acc attr >>= fun (acc, attr) ->
        IO.return (acc, cons ~attr wrapper blk)

    let identity =
        {
        valid = (fun fm acc valid ->
            fm.frag fm acc valid.content >>= fun (acc, content) ->
            IO.return (acc, {valid with content}));

        attr = (fun fm acc attr ->
            IO.return (acc, attr));
            
        inline = (fun fm acc {inl; attr} -> match inl with
            | Plain txt                 -> fm.plain fm acc attr txt
            | Entity ent                -> fm.entity fm acc attr ent
            | Linebreak                 -> fm.linebreak fm acc attr
            | Math_inl data             -> fm.math_inl fm acc attr data
            | Code data                 -> fm.code fm acc attr data
            | Glyph (href, alt, title)  -> fm.glyph fm acc attr href alt title
            | Bold seq                  -> fm.bold fm acc attr seq
            | Emph seq                  -> fm.emph fm acc attr seq
            | Mono seq                  -> fm.mono fm acc attr seq
            | Caps seq                  -> fm.caps fm acc attr seq
            | Ins seq                   -> fm.ins fm acc attr seq
            | Del seq                   -> fm.del fm acc attr seq
            | Sup seq                   -> fm.sup fm acc attr seq
            | Sub seq                   -> fm.sub fm acc attr seq
            | Mbox seq                  -> fm.mbox fm acc attr seq
            | Span seq                  -> fm.span fm acc attr seq
            | Link (href, maybe_seq)    -> fm.link fm acc attr href maybe_seq
            | See pointers              -> fm.see fm acc attr pointers
            | Cite pointers             -> fm.cite fm acc attr pointers
            | Dref (pointer, maybe_seq) -> fm.dref fm acc attr pointer maybe_seq
            | Sref (pointer, maybe_seq) -> fm.sref fm acc attr pointer maybe_seq
            | Mref (pointer, seq)       -> fm.mref fm acc attr pointer seq);

        block = (fun fm acc {blk; attr} -> match blk with
            | Paragraph seq                     -> fm.paragraph fm acc attr seq
            | Itemize frags                     -> fm.itemize fm acc attr frags
            | Enumerate frags                   -> fm.enumerate fm acc attr frags
            | Description dfrags                -> fm.description fm acc attr dfrags
            | Qanda qafrags                     -> fm.qanda fm acc attr qafrags
            | Verse frag                        -> fm.verse fm acc attr frag
            | Quote frag                        -> fm.quote fm acc attr frag
            | Math_blk data                     -> fm.math_blk fm acc attr data
            | Source data                       -> fm.source fm acc attr data
            | Tabular data                      -> fm.tabular fm acc attr data
            | Subpage frag                      -> fm.subpage fm acc attr frag
            | Verbatim txt                      -> fm.verbatim fm acc attr txt
            | Picture (href, alt, title, width) -> fm.picture fm acc attr href alt title width
            | Pullquote (maybe_seq, frag)       -> fm.pullquote fm acc attr maybe_seq frag
            | Boxout (data, maybe_seq, frag)    -> fm.boxout fm acc attr data maybe_seq frag
            | Theorem (data, maybe_seq, frag)   -> fm.theorem fm acc attr data maybe_seq frag
            | Equation (wrapper, blk)           -> fm.equation fm acc attr wrapper blk
            | Printout (wrapper, blk)           -> fm.printout fm acc attr wrapper blk
            | Table (wrapper, blk)              -> fm.table fm acc attr wrapper blk
            | Figure (wrapper, blk)             -> fm.figure fm acc attr wrapper blk
            | Heading data                      -> fm.heading fm acc attr data
            | Autogen data                      -> fm.autogen fm acc attr data
            | Title (level, seq)                -> fm.title fm acc attr level seq
            | Abstract frag                     -> fm.abstract fm acc attr frag
            | Rule                              -> fm.rule fm acc attr);

        seq = (fun fm acc seq ->
            aux_list fm.inline fm acc seq);
        frag = (fun fm acc frag ->
            aux_list fm.block fm acc frag);
    
        plain = (fun fm acc attr txt ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.plain ~attr txt));
        entity = (fun fm acc attr ent ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.entity ~attr ent));
        linebreak = (fun fm acc attr ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.linebreak ~attr ()));
        math_inl = (fun fm acc attr data ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.math_inl ~attr data));
        code = (fun fm acc attr data ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.code ~attr data));
        glyph = (fun fm acc attr href alt title ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.glyph ~attr href alt title));
        bold = (fun fm acc attr seq ->
            aux_seq Inline.bold fm acc attr seq);
        emph = (fun fm acc attr seq ->
            aux_seq Inline.emph fm acc attr seq);
        mono = (fun fm acc attr seq ->
            aux_seq Inline.mono fm acc attr seq);
        caps = (fun fm acc attr seq ->
            aux_seq Inline.caps fm acc attr seq);
        ins = (fun fm acc attr seq ->
            aux_seq Inline.ins fm acc attr seq);
        del = (fun fm acc attr seq ->
            aux_seq Inline.del fm acc attr seq);
        sup = (fun fm acc attr seq ->
            aux_seq Inline.sup fm acc attr seq);
        sub = (fun fm acc attr seq ->
            aux_seq Inline.sub fm acc attr seq);
        mbox = (fun fm acc attr seq ->
            aux_seq Inline.mbox fm acc attr seq);
        span = (fun fm acc attr seq ->
            aux_seq Inline.span fm acc attr seq);
        link = (fun fm acc attr href maybe_seq ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.link ~attr href maybe_seq));
        see = (fun fm acc attr pointers ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.see ~attr pointers));
        cite = (fun fm acc attr pointers ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.cite ~attr pointers));
        dref = (fun fm acc attr pointer maybe_seq ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.dref ~attr pointer maybe_seq));
        sref = (fun fm acc attr pointer maybe_seq ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.sref ~attr pointer maybe_seq));
        mref = (fun fm acc attr pointer seq ->
            fm.seq fm acc seq >>= fun (acc, seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Inline.mref ~attr pointer seq));

        paragraph = (fun fm acc attr seq ->
            fm.seq fm acc seq >>= fun (acc, seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.paragraph ~attr seq));
        itemize = (fun fm acc attr frags ->
            aux_frags Block.itemize fm acc attr frags);
        enumerate = (fun fm acc attr frags ->
            aux_frags Block.enumerate fm acc attr frags);
        description = (fun fm acc attr dfrags ->
            let aux (seq, frag) (acc_alpha, acc_xs) =
                fm.seq fm acc_alpha seq >>= fun (acc_alpha, seq) ->
                fm.frag fm acc_alpha frag >>= fun (acc_alpha, frag) ->
                IO.return (acc_alpha, (seq, frag) :: acc_xs) in
            IO.fold_right aux dfrags (acc, []) >>= fun (acc, dfrags) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.description ~attr dfrags));
        qanda = (fun fm acc attr qafrags ->
            let aux (qanda, frag) (acc_alpha, acc_xs) =
                begin match qanda with
                    | New_questioner maybe_seq ->
                        aux_maybe fm.seq fm acc_alpha maybe_seq >>= fun (acc_alpha, maybe_seq) ->
                        IO.return (acc_alpha, New_questioner maybe_seq)
                    | New_answerer maybe_seq ->
                        aux_maybe fm.seq fm acc_alpha maybe_seq >>= fun (acc_alpha, maybe_seq) ->
                        IO.return (acc_alpha, New_answerer maybe_seq)
                    | Same_questioner ->
                        IO.return (acc_alpha, Same_questioner)
                    | Same_answerer ->
                        IO.return (acc_alpha, Same_answerer)
                end >>= fun (acc_alpha, qanda) ->
                fm.frag fm acc_alpha frag >>= fun (acc_alpha, frag) ->
                IO.return (acc_alpha, (qanda, frag) :: acc_xs) in
            IO.fold_right aux qafrags (acc, []) >>= fun (acc, qafrags) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.qanda ~attr qafrags));
        verse = (fun fm acc attr frag ->
            aux_frag Block.verse fm acc attr frag);
        quote = (fun fm acc attr frag ->
            aux_frag Block.quote fm acc attr frag);
        math_blk = (fun fm acc attr data ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.math_blk ~attr data));
        source = (fun fm acc attr data ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.source ~attr data));
        tabular = (fun fm acc attr data ->
            let aux_cell fm acc {attr; cellfmt; seq} =
                aux_maybe fm.seq fm acc seq >>= fun (acc, maybe_seq) ->
                fm.attr fm acc attr >>= fun (acc, attr) ->
                IO.return (acc, Tabular.make_cell ~attr ?cellfmt maybe_seq) in
            let aux_row fm acc row =
                aux_list aux_cell fm acc row in
            let aux_group fm acc group =
                aux_list aux_row fm acc group in
            aux_maybe aux_group fm acc data.thead >>= fun (acc, thead) ->
            aux_maybe aux_group fm acc data.tfoot >>= fun (acc, tfoot) ->
            aux_list aux_group fm acc data.tbodies >>= fun (acc, tbodies) ->
            let data = {data with thead; tfoot; tbodies} in
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.tabular ~attr data));
        subpage = (fun fm acc attr frag ->
            aux_frag Block.subpage fm acc attr frag);
        verbatim = (fun fm acc attr txt ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.verbatim ~attr txt));
        picture = (fun fm acc attr href alt title width ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.picture ~attr href alt title width));
        pullquote = (fun fm acc attr maybe_seq frag ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.frag fm acc frag >>= fun (acc, frag) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.pullquote ~attr maybe_seq frag));
        boxout = (fun fm acc attr data maybe_seq frag ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.frag fm acc frag >>= fun (acc, frag) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.boxout ~attr data maybe_seq frag));
        theorem = (fun fm acc attr data maybe_seq frag ->
            aux_maybe fm.seq fm acc maybe_seq >>= fun (acc, maybe_seq) ->
            fm.frag fm acc frag >>= fun (acc, frag) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.theorem ~attr data maybe_seq frag));
        equation = (fun fm acc attr wrapper blk ->
            aux_wrapper Block.equation fm acc attr wrapper blk);
        printout = (fun fm acc attr wrapper blk ->
            aux_wrapper Block.printout fm acc attr wrapper blk);
        table = (fun fm acc attr wrapper blk ->
            aux_wrapper Block.table fm acc attr wrapper blk);
        figure = (fun fm acc attr wrapper blk ->
            aux_wrapper Block.figure fm acc attr wrapper blk);
        heading = (fun fm acc attr data ->
            begin match data with
                | Part (label, order, content) ->
                    begin match content with
                        | Custom_part seq ->
                            fm.seq fm acc seq >>= fun (acc, seq) ->
                            IO.return (acc, Custom_part seq)
                        | x ->
                            IO.return (acc, x)
                    end >>= fun (acc, content) ->
                    IO.return (acc, Part (label, order, content))
                | Section (label, order, location, level, content) ->
                    begin match content with
                        | Custom_section seq ->
                            fm.seq fm acc seq >>= fun (acc, seq) ->
                            IO.return (acc, Custom_section seq)
                        | x ->
                            IO.return (acc, x)
                    end >>= fun (acc, content) ->
                    IO.return (acc, Section (label, order, location, level, content))
            end >>= fun (acc, data) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.heading ~attr data));
        autogen = (fun fm acc attr data ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.autogen ~attr data));
        title = (fun fm acc attr level seq ->
            fm.seq fm acc seq >>= fun (acc, seq) ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.title ~attr level seq));
        abstract = (fun fm acc attr frag ->
            aux_frag Block.abstract fm acc attr frag);
        rule = (fun fm acc attr ->
            fm.attr fm acc attr >>= fun (acc, attr) ->
            IO.return (acc, Block.rule ~attr ()));
        }

    let amnesiac =
        {
        identity with attr = (fun fm acc attr -> IO.return (acc, Attr.(make ~classnames:attr.classnames ())));
        }
            
end

module Identity = Make (Monad.Identity)

