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
    | Simple of string
    | Text of string
    | Entity of string
    | Cell_mark of string
    | Row_end
    | Mathtex_op
    | Open
    | Close
    | Eop
    | Eof

(** Together with the lexeme proper we also return the number
    of newlines found before the lexeme and within the lexeme.
*)
type triple = lexeme * int * int

(** A lexing outcome consists of an optional previous lexing pair
    (if there was leftover text in the buffer), and the current pair.
*)
type outcome =
    {
    previous: triple option;
    current: triple;
    }


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** Make a new lexing buffer.
*)
val make_buffer: Sedlexing.lexbuf -> buffer

(** Lexer for block context.  Whitespace is not significant.
*)
val block: buffer -> outcome

(** Lexer for inline context.  Whitespace is significant.
*)
val inline: buffer -> outcome

(** Special lexer for raw environments.  Pretty much every character
    is returned as text; the exceptions are the EOF character, escaped
    characters, and the special "\}" termination tag (note that the backslash
    is there to satisfy OCamldoc and is not part of the terminator).
*)
val raw: buffer -> outcome

(** Special lexer for mathtex environments in an inline context.
    No attempt is made to interpret the characters in the stream,
    except for the EOF character and the environment terminator $$.
*)
val mathtex_inl: buffer -> outcome

(** Special parameterised lexer for verbatim-like environments.
    The string parameter indicates the termination string.
    No attempt is made to interpret the characters in the stream,
    except for the EOF character and the termination string.
*)
val literal: string -> buffer -> outcome

