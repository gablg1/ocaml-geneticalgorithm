type token =
  | TokEof of (Lm_location.loc)
  | TokValues of (Omake_value_type.value list * Lm_location.loc)
  | TokDefine of (string * Lm_location.loc)
  | TokLeftParen of (string * Lm_location.loc)
  | TokRightParen of (string * Lm_location.loc)
  | TokLessThan of (string * Lm_location.loc)
  | TokGreaterThan of (string * Lm_location.loc)
  | TokGreaterGreaterThan of (string * Lm_location.loc)
  | TokAmp of (string * Lm_location.loc)
  | TokPipe of (string * Lm_location.loc)
  | TokSemiColon of (string * Lm_location.loc)
  | TokAnd of (string * Lm_location.loc)
  | TokOr of (string * Lm_location.loc)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Omake_env.value_pipe
