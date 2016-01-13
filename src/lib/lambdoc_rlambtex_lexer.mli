(********************************************************************************)
(*  Lambdoc_rlambtex_lexer.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Lexer for the Lambtex reader.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(** A lexing buffer.
*)
type buffer

(** The set of all lexemes output by the various lexers.
*)
type lexeme =
    | Begin_env of string
    | End_env of string
    | Begin_mathtexinl
    | End_mathtexinl
    | Begin_mathmlinl
    | End_mathmlinl
    | Open
    | Close
    | Simple of string
    | Cell_mark of string
    | Row_end
    | Par_break
    | Space
    | Text of string
    | Entity of string
    | Eof

(** Together with the lexeme proper we also return
    the number of newlines found within the lexeme.
*)
type pair = lexeme * int

(** A lexing outcome consists of an optional previous lexing pair
    (if there was leftover text in the buffer), and the current pair.
*)
type outcome =
    {
    previous: pair option;
    current: pair;
    }


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** Make a new lexing buffer.
*)
val make_buffer: Sedlexing.lexbuf -> buffer

(** General document lexer.
*)
val general: buffer -> outcome

(** Special lexer for raw environments.  Pretty much every character
    is returned as text; the exceptions are the EOF character, escaped
    characters, and the special "\}" termination tag (note that the backslash
    is there to satisfy OCamldoc and is not part of the terminator).
*)
val raw: buffer -> outcome

(** Special lexer for mathtex environments in an inline context.
    No attempt is made to interpret the characters in the stream,
    except for the EOF character and the environment terminator $].
*)
val mathtexinl: buffer -> outcome

(** Special lexer for mathml environments in an inline context.
    No attempt is made to interpret the characters in the stream,
    except for the EOF character and the environment terminator $>.
*)
val mathmlinl: buffer -> outcome

(** Special parameterised lexer for verbatim-like environments.
    The string parameter indicates the termination string.
    No attempt is made to interpret the characters in the stream,
    except for the EOF character and the termination string.
*)
val literal: string -> buffer -> outcome

