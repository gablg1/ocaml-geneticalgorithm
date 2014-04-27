type token =
  | TokEof of (Lm_location.loc)
  | TokEol of (Lm_location.loc)
  | TokWhite of (string * Lm_location.loc)
  | TokLeftParen of (string * Lm_location.loc)
  | TokRightParen of (string * Lm_location.loc)
  | TokArrow of (string * Lm_location.loc)
  | TokComma of (string * Lm_location.loc)
  | TokColon of (string * Lm_location.loc)
  | TokDoubleColon of (string * Lm_location.loc)
  | TokNamedColon of (string * Lm_location.loc)
  | TokDollar of (string * Omake_ast.apply_strategy * Lm_location.loc)
  | TokEq of (string * Lm_location.loc)
  | TokArray of (string * Lm_location.loc)
  | TokDot of (string * Lm_location.loc)
  | TokId of (string * Lm_location.loc)
  | TokKey of (string * Lm_location.loc)
  | TokKeyword of (string * Lm_location.loc)
  | TokCatch of (string * Lm_location.loc)
  | TokClass of (string * Lm_location.loc)
  | TokOp of (string * Lm_location.loc)
  | TokInt of (string * Lm_location.loc)
  | TokFloat of (string * Lm_location.loc)
  | TokString of (string * Lm_location.loc)
  | TokBeginQuote of (string * Lm_location.loc)
  | TokEndQuote of (string * Lm_location.loc)
  | TokBeginQuoteString of (string * Lm_location.loc)
  | TokEndQuoteString of (string * Lm_location.loc)
  | TokStringQuote of (string * Lm_location.loc)
  | TokVar of (Omake_ast.apply_strategy * string * Lm_location.loc)
  | TokVarQuote of (Omake_ast.apply_strategy * string * Lm_location.loc)

val deps :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Omake_ast.exp * Omake_ast.exp * Lm_location.loc) list
val shell :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Omake_ast.body_flag * Omake_ast.exp
val string :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Omake_ast.body_flag * Omake_ast.exp
