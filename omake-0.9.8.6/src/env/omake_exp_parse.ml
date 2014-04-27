type token =
  | TokEof
  | TokLeftParen of (Lm_location.loc)
  | TokRightParen of (Lm_location.loc)
  | TokLeftBrack of (Lm_location.loc)
  | TokRightBrack of (Lm_location.loc)
  | TokPlus of (Lm_location.loc)
  | TokMinus of (Lm_location.loc)
  | TokStar of (Lm_location.loc)
  | TokSlash of (Lm_location.loc)
  | TokMod of (Lm_location.loc)
  | TokHat of (Lm_location.loc)
  | TokPipe of (Lm_location.loc)
  | TokAmp of (Lm_location.loc)
  | TokLsl of (Lm_location.loc)
  | TokLsr of (Lm_location.loc)
  | TokAsr of (Lm_location.loc)
  | TokAnd of (Lm_location.loc)
  | TokOr of (Lm_location.loc)
  | TokDot of (Lm_location.loc)
  | TokComma of (Lm_location.loc)
  | TokSemi of (Lm_location.loc)
  | TokLe of (Lm_location.loc)
  | TokLt of (Lm_location.loc)
  | TokEq of (Lm_location.loc)
  | TokNeq of (Lm_location.loc)
  | TokGt of (Lm_location.loc)
  | TokGe of (Lm_location.loc)
  | TokColonColon of (Lm_location.loc)
  | TokArrow of (Lm_location.loc)
  | TokId of (Lm_symbol.symbol * Lm_location.loc)
  | TokKey of (Lm_symbol.symbol * Lm_location.loc)
  | TokCatch of (Lm_location.loc)
  | TokInt of (int * Lm_location.loc)
  | TokFloat of (float * Lm_location.loc)
  | TokExp of (Omake_ast.exp)

open Parsing;;
let _ = parse_error;;
# 26 "omake_exp_parse.mly"
open Lm_location

open Omake_pos
open Omake_ast
open Omake_symbol
open Omake_ast_util
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_exp_parse" end)
open Pos;;

(*
 * Different types of identifiers.
 *)
type id =
   SimpleId of var
 | SuperId  of var * var
 | MethodId of var list

(*
 * Identifier stands for an application.
 *)
let make_id_exp (id, loc) =
   let e =
      match id with
         SimpleId v -> ApplyExp (NormalApply, v, [], loc)
       | SuperId (v1, v2) -> SuperApplyExp (NormalApply, v1, v2, [], loc)
       | MethodId vars -> MethodApplyExp (NormalApply, vars, [], loc)
   in
      e, loc

(*
 * Unary operations.
 *)
let make_unary_exp v (e, loc) =
   ApplyExp (NormalApply, v, [ExpArg e], loc), loc

let make_binary_exp v (e1, loc1) (e2, loc2) =
   let loc = union_loc loc1 loc2 in
      ApplyExp (NormalApply, v, [ExpArg e1; ExpArg e2], loc), loc

(*
 * If the function is a null application, add the args.
 *)
let apply_var = Lm_symbol.add ".fun"

let make_apply_exp (e, loc) args =
   match e with
      ApplyExp (strategy, v, [], _) ->
         ApplyExp (strategy, v, args, loc), loc
    | _ ->
         (* Create a temporary private variable *)
         SequenceExp ([VarDefExp ([apply_var], DefineString, DefineNormal, e, loc);
                       ApplyExp (NormalApply, apply_var, args, loc)], loc), loc

(*
 * Function parameters from an argument list.
 *)
let get_fun_param = function
    ExpArg (ApplyExp (NormalApply, v, [], loc)) ->
        NormalParam (v, loc)
  | ExpArg e
  | KeyArg (_, e)
  | ArrowArg (_, e) ->
        raise (OmakeException (loc_exp_pos (loc_of_exp e), StringError "illegal parameter"))

let get_fun_params = List.map get_fun_param
# 109 "omake_exp_parse.ml"
let yytransl_const = [|
  257 (* TokEof *);
    0|]

let yytransl_block = [|
  258 (* TokLeftParen *);
  259 (* TokRightParen *);
  260 (* TokLeftBrack *);
  261 (* TokRightBrack *);
  262 (* TokPlus *);
  263 (* TokMinus *);
  264 (* TokStar *);
  265 (* TokSlash *);
  266 (* TokMod *);
  267 (* TokHat *);
  268 (* TokPipe *);
  269 (* TokAmp *);
  270 (* TokLsl *);
  271 (* TokLsr *);
  272 (* TokAsr *);
  273 (* TokAnd *);
  274 (* TokOr *);
  275 (* TokDot *);
  276 (* TokComma *);
  277 (* TokSemi *);
  278 (* TokLe *);
  279 (* TokLt *);
  280 (* TokEq *);
  281 (* TokNeq *);
  282 (* TokGt *);
  283 (* TokGe *);
  284 (* TokColonColon *);
  285 (* TokArrow *);
  286 (* TokId *);
  287 (* TokKey *);
  288 (* TokCatch *);
  289 (* TokInt *);
  290 (* TokFloat *);
  291 (* TokExp *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\006\000\006\000\005\000\005\000\007\000\007\000\008\000\008\000\
\009\000\009\000\004\000\004\000\010\000\010\000\010\000\012\000\
\012\000\013\000\011\000\011\000\014\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\000\000\002\000\001\000\003\000\000\000\001\000\
\001\000\001\000\000\000\001\000\001\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\003\000\
\004\000\056\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\029\000\042\000\041\000\036\000\000\000\033\000\
\000\000\031\000\000\000\000\000\000\000\044\000\000\000\000\000\
\048\000\051\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\027\000\034\000\000\000\
\052\000\000\000\000\000\049\000"

let yydgoto = "\002\000\
\010\000\052\000\012\000\053\000\015\000\049\000\016\000\046\000\
\047\000\054\000\055\000\056\000\057\000\058\000"

let yysindex = "\010\000\
\051\255\000\000\051\255\051\255\051\255\242\254\000\000\000\000\
\000\000\000\000\103\001\000\000\129\001\181\001\007\255\245\254\
\004\255\239\254\241\254\000\000\040\255\051\255\051\255\051\255\
\051\255\051\255\051\255\051\255\051\255\051\255\051\255\051\255\
\051\255\051\255\051\255\051\255\051\255\051\255\051\255\051\255\
\051\255\000\000\000\000\000\000\000\000\000\000\051\255\000\000\
\000\255\000\000\249\254\181\001\017\255\000\000\243\254\023\255\
\000\000\000\000\155\001\243\000\243\000\004\255\004\255\004\255\
\029\002\233\001\003\002\084\001\084\001\084\001\207\001\207\001\
\081\002\081\002\055\002\055\002\081\002\081\002\181\001\015\255\
\051\255\000\000\040\255\051\255\040\255\000\000\000\000\181\001\
\000\000\181\001\243\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\044\255\000\000\086\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\036\255\000\000\046\255\
\142\255\000\000\000\000\000\000\058\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\115\255\000\000\142\000\169\000\000\000\000\000\060\255\063\255\
\000\000\000\000\000\000\250\255\021\000\169\255\196\255\223\255\
\031\001\052\001\047\255\048\000\075\000\102\000\058\001\079\001\
\129\000\156\000\237\000\006\001\183\000\210\000\057\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\196\000\
\000\000\254\254\066\255\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\243\255\000\000\253\255\252\255"

let yytablesize = 865
let yytable = "\011\000\
\050\000\013\000\014\000\017\000\018\000\021\000\083\000\022\000\
\044\000\045\000\001\000\043\000\048\000\019\000\050\000\084\000\
\081\000\050\000\080\000\082\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\037\000\003\000\085\000\004\000\087\000\079\000\005\000\014\000\
\035\000\014\000\039\000\014\000\003\000\040\000\004\000\037\000\
\037\000\005\000\014\000\014\000\043\000\038\000\045\000\014\000\
\014\000\046\000\014\000\014\000\047\000\006\000\051\000\091\000\
\007\000\008\000\009\000\014\000\038\000\038\000\089\000\088\000\
\006\000\092\000\090\000\007\000\008\000\009\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\000\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\000\000\030\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\006\000\032\000\
\006\000\000\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\009\000\006\000\009\000\000\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\000\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\010\000\009\000\010\000\000\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\000\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\010\000\011\000\000\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\000\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\007\000\011\000\007\000\000\000\007\000\007\000\
\007\000\000\000\000\000\000\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\000\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\008\000\007\000\008\000\
\000\000\008\000\008\000\008\000\000\000\000\000\000\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\015\000\008\000\015\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\000\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\016\000\015\000\016\000\000\000\016\000\
\000\000\000\000\000\000\000\000\000\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\000\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\017\000\016\000\
\017\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\020\000\017\000\020\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\020\000\020\000\020\000\000\000\000\000\
\054\000\020\000\020\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\021\000\020\000\021\000\000\000\
\021\000\054\000\000\000\000\000\000\000\000\000\021\000\021\000\
\021\000\000\000\054\000\053\000\021\000\021\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\024\000\
\021\000\024\000\000\000\024\000\053\000\000\000\000\000\000\000\
\000\000\024\000\024\000\024\000\000\000\053\000\055\000\024\000\
\024\000\000\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\025\000\024\000\025\000\000\000\025\000\055\000\
\000\000\000\000\000\000\000\000\025\000\025\000\025\000\000\000\
\055\000\000\000\025\000\025\000\000\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\025\000\022\000\025\000\022\000\
\000\000\022\000\000\000\000\000\021\000\000\000\022\000\022\000\
\022\000\022\000\025\000\026\000\027\000\022\000\022\000\000\000\
\022\000\022\000\000\000\000\000\022\000\022\000\023\000\000\000\
\023\000\022\000\023\000\000\000\000\000\000\000\000\000\000\000\
\023\000\023\000\023\000\000\000\000\000\000\000\023\000\023\000\
\000\000\023\000\023\000\000\000\000\000\023\000\023\000\012\000\
\000\000\012\000\023\000\012\000\000\000\000\000\000\000\000\000\
\000\000\012\000\012\000\012\000\000\000\000\000\000\000\012\000\
\012\000\000\000\012\000\012\000\013\000\000\000\013\000\000\000\
\013\000\000\000\018\000\012\000\018\000\000\000\018\000\013\000\
\000\000\000\000\000\000\000\000\013\000\013\000\000\000\013\000\
\013\000\000\000\018\000\018\000\000\000\018\000\018\000\019\000\
\013\000\019\000\000\000\019\000\000\000\021\000\018\000\022\000\
\000\000\023\000\024\000\025\000\026\000\027\000\000\000\019\000\
\019\000\000\000\019\000\019\000\000\000\000\000\000\000\020\000\
\021\000\000\000\022\000\019\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\000\000\000\000\000\000\036\000\037\000\038\000\039\000\
\040\000\041\000\021\000\042\000\022\000\000\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\000\000\000\000\000\000\036\000\037\000\
\038\000\039\000\040\000\041\000\021\000\000\000\022\000\086\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\000\000\000\000\000\000\
\036\000\037\000\038\000\039\000\040\000\041\000\021\000\000\000\
\022\000\000\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\000\000\
\000\000\000\000\036\000\037\000\038\000\039\000\040\000\041\000\
\021\000\000\000\022\000\000\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\038\000\039\000\
\040\000\041\000\021\000\000\000\022\000\000\000\023\000\024\000\
\025\000\026\000\027\000\028\000\000\000\030\000\031\000\032\000\
\033\000\000\000\000\000\000\000\000\000\000\000\036\000\037\000\
\038\000\039\000\040\000\041\000\021\000\000\000\022\000\000\000\
\023\000\024\000\025\000\026\000\027\000\028\000\000\000\000\000\
\031\000\032\000\033\000\000\000\000\000\000\000\000\000\000\000\
\036\000\037\000\038\000\039\000\040\000\041\000\021\000\000\000\
\022\000\000\000\023\000\024\000\025\000\026\000\027\000\000\000\
\000\000\000\000\031\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\038\000\039\000\040\000\041\000\
\021\000\000\000\022\000\000\000\023\000\024\000\025\000\026\000\
\027\000\000\000\000\000\000\000\031\000\032\000\033\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\000\000\000\000\
\040\000\041\000\021\000\000\000\022\000\000\000\023\000\024\000\
\025\000\026\000\027\000\000\000\000\000\000\000\031\000\032\000\
\033\000"

let yycheck = "\001\000\
\003\001\003\000\004\000\005\000\019\001\002\001\020\001\004\001\
\020\001\021\001\001\000\005\001\030\001\028\001\030\001\029\001\
\024\001\020\001\019\001\003\001\022\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\041\000\
\005\001\002\001\020\001\004\001\030\001\047\000\007\001\001\001\
\005\001\003\001\005\001\005\001\002\001\005\001\004\001\020\001\
\021\001\007\001\012\001\013\001\003\001\005\001\003\001\017\001\
\018\001\003\001\020\001\021\001\003\001\030\001\031\001\085\000\
\033\001\034\001\035\001\029\001\020\001\021\001\083\000\081\000\
\030\001\085\000\084\000\033\001\034\001\035\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\029\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\001\001\029\001\
\003\001\255\255\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\001\001\029\001\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\029\001\003\001\255\255\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\001\001\
\029\001\003\001\255\255\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\029\001\003\001\255\255\005\001\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\029\001\003\001\
\255\255\005\001\006\001\007\001\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\029\001\003\001\255\255\005\001\255\255\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\001\001\029\001\003\001\255\255\005\001\
\255\255\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\001\001\029\001\
\003\001\255\255\005\001\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\001\001\029\001\003\001\255\255\005\001\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\255\255\255\255\
\003\001\017\001\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\029\001\003\001\255\255\
\005\001\020\001\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\255\255\029\001\003\001\017\001\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\001\001\
\029\001\003\001\255\255\005\001\020\001\255\255\255\255\255\255\
\255\255\011\001\012\001\013\001\255\255\029\001\003\001\017\001\
\018\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\029\001\003\001\255\255\005\001\020\001\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\255\255\
\029\001\255\255\017\001\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\029\001\003\001\
\255\255\005\001\255\255\255\255\002\001\255\255\004\001\011\001\
\012\001\013\001\008\001\009\001\010\001\017\001\018\001\255\255\
\020\001\021\001\255\255\255\255\024\001\025\001\001\001\255\255\
\003\001\029\001\005\001\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\255\255\255\255\255\255\017\001\018\001\
\255\255\020\001\021\001\255\255\255\255\024\001\025\001\001\001\
\255\255\003\001\029\001\005\001\255\255\255\255\255\255\255\255\
\255\255\011\001\012\001\013\001\255\255\255\255\255\255\017\001\
\018\001\255\255\020\001\021\001\001\001\255\255\003\001\255\255\
\005\001\255\255\001\001\029\001\003\001\255\255\005\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\255\255\020\001\
\021\001\255\255\017\001\018\001\255\255\020\001\021\001\001\001\
\029\001\003\001\255\255\005\001\255\255\002\001\029\001\004\001\
\255\255\006\001\007\001\008\001\009\001\010\001\255\255\017\001\
\018\001\255\255\020\001\021\001\255\255\255\255\255\255\001\001\
\002\001\255\255\004\001\029\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\002\001\255\255\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\002\001\255\255\
\004\001\255\255\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\002\001\255\255\004\001\255\255\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\002\001\255\255\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\002\001\255\255\004\001\255\255\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\014\001\015\001\016\001\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\002\001\255\255\
\004\001\255\255\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\014\001\015\001\016\001\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\002\001\255\255\004\001\255\255\006\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\014\001\015\001\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\255\255\255\255\
\026\001\027\001\002\001\255\255\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\015\001\
\016\001"

let yynames_const = "\
  TokEof\000\
  "

let yynames_block = "\
  TokLeftParen\000\
  TokRightParen\000\
  TokLeftBrack\000\
  TokRightBrack\000\
  TokPlus\000\
  TokMinus\000\
  TokStar\000\
  TokSlash\000\
  TokMod\000\
  TokHat\000\
  TokPipe\000\
  TokAmp\000\
  TokLsl\000\
  TokLsr\000\
  TokAsr\000\
  TokAnd\000\
  TokOr\000\
  TokDot\000\
  TokComma\000\
  TokSemi\000\
  TokLe\000\
  TokLt\000\
  TokEq\000\
  TokNeq\000\
  TokGt\000\
  TokGe\000\
  TokColonColon\000\
  TokArrow\000\
  TokId\000\
  TokKey\000\
  TokCatch\000\
  TokInt\000\
  TokFloat\000\
  TokExp\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 173 "omake_exp_parse.mly"
    ( let e, _ = _1 in e )
# 490 "omake_exp_parse.ml"
               : Omake_ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int * Lm_location.loc) in
    Obj.repr(
# 178 "omake_exp_parse.mly"
   ( let i, loc = _1 in
	       IntExp (i, loc), loc
	  )
# 499 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float * Lm_location.loc) in
    Obj.repr(
# 182 "omake_exp_parse.mly"
   ( let x, loc = _1 in
	       FloatExp (x, loc), loc
	  )
# 508 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp) in
    Obj.repr(
# 186 "omake_exp_parse.mly"
          ( let e = _1 in
               e, loc_of_exp e
          )
# 517 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'id) in
    Obj.repr(
# 190 "omake_exp_parse.mly"
   ( make_id_exp _1 )
# 524 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 192 "omake_exp_parse.mly"
   ( make_unary_exp neg_fun_sym _2 )
# 532 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 194 "omake_exp_parse.mly"
   ( make_binary_exp add_fun_sym _1 _3 )
# 541 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 196 "omake_exp_parse.mly"
   ( make_binary_exp sub_fun_sym _1 _3 )
# 550 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 198 "omake_exp_parse.mly"
   ( make_binary_exp mul_fun_sym _1 _3 )
# 559 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 200 "omake_exp_parse.mly"
   ( make_binary_exp div_fun_sym _1 _3 )
# 568 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 202 "omake_exp_parse.mly"
   ( make_binary_exp mod_fun_sym _1 _3 )
# 577 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 204 "omake_exp_parse.mly"
   ( make_binary_exp lxor_fun_sym _1 _3 )
# 586 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 206 "omake_exp_parse.mly"
   ( make_binary_exp lor_fun_sym _1 _3 )
# 595 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 208 "omake_exp_parse.mly"
   ( make_binary_exp land_fun_sym _1 _3 )
# 604 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 210 "omake_exp_parse.mly"
   ( make_binary_exp lsl_fun_sym _1 _3 )
# 613 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 212 "omake_exp_parse.mly"
   ( make_binary_exp lsr_fun_sym _1 _3 )
# 622 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 214 "omake_exp_parse.mly"
   ( make_binary_exp asr_fun_sym _1 _3 )
# 631 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 216 "omake_exp_parse.mly"
   ( make_binary_exp and_fun_sym _1 _3 )
# 640 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 218 "omake_exp_parse.mly"
   ( make_binary_exp or_fun_sym _1 _3 )
# 649 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 220 "omake_exp_parse.mly"
          ( make_binary_exp le_fun_sym _1 _3 )
# 658 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 222 "omake_exp_parse.mly"
          ( make_binary_exp lt_fun_sym _1 _3 )
# 667 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 224 "omake_exp_parse.mly"
          ( make_binary_exp equal_fun_sym _1 _3 )
# 676 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 226 "omake_exp_parse.mly"
          ( make_binary_exp nequal_fun_sym _1 _3 )
# 685 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 228 "omake_exp_parse.mly"
          ( make_binary_exp gt_fun_sym _1 _3 )
# 694 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 230 "omake_exp_parse.mly"
          ( make_binary_exp ge_fun_sym _1 _3 )
# 703 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'opt_args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 232 "omake_exp_parse.mly"
          ( make_apply_exp _1 _3 )
# 713 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Omake_ast.exp * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Omake_ast.exp * Lm_location.loc) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 234 "omake_exp_parse.mly"
   ( make_binary_exp nth_fun_sym _3 _1 )
# 723 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Omake_ast.exp * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 236 "omake_exp_parse.mly"
   ( _2 )
# 732 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'opt_exp_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 238 "omake_exp_parse.mly"
   ( let loc = union_loc _1 _3 in
	       ArrayExp (_2, loc), loc
	  )
# 743 "omake_exp_parse.ml"
               : Omake_ast.exp * Lm_location.loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_symbol.symbol * Lm_location.loc) in
    Obj.repr(
# 245 "omake_exp_parse.mly"
   ( let id, loc = _1 in
               SimpleId id, loc
          )
# 752 "omake_exp_parse.ml"
               : 'id))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lm_symbol.symbol * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lm_symbol.symbol * Lm_location.loc) in
    Obj.repr(
# 249 "omake_exp_parse.mly"
   ( let v1, loc1 = _1 in
            let v2, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               SuperId (v1, v2), loc
          )
# 765 "omake_exp_parse.ml"
               : 'id))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lm_symbol.symbol * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rev_path_id) in
    Obj.repr(
# 255 "omake_exp_parse.mly"
   ( let v1, loc1 = _1 in
            let vars, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               MethodId (v1 :: vars), loc
          )
# 778 "omake_exp_parse.ml"
               : 'id))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_symbol.symbol * Lm_location.loc) in
    Obj.repr(
# 264 "omake_exp_parse.mly"
   ( let v, loc = _1 in
               [v], loc
          )
# 787 "omake_exp_parse.ml"
               : 'rev_path_id))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_path_id) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lm_symbol.symbol * Lm_location.loc) in
    Obj.repr(
# 268 "omake_exp_parse.mly"
   ( let path, loc1 = _1 in
            let v, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               v :: path, loc
          )
# 800 "omake_exp_parse.ml"
               : 'rev_path_id))
; (fun __caml_parser_env ->
    Obj.repr(
# 280 "omake_exp_parse.mly"
   ( [] )
# 806 "omake_exp_parse.ml"
               : 'opt_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rev_exp_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_semi_or_comma) in
    Obj.repr(
# 282 "omake_exp_parse.mly"
   ( List.rev _1 )
# 814 "omake_exp_parse.ml"
               : 'opt_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 287 "omake_exp_parse.mly"
   ( let e, _ = _1 in [e] )
# 821 "omake_exp_parse.ml"
               : 'rev_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_exp_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'semi_or_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 289 "omake_exp_parse.mly"
          ( let e, _ = _3 in
               e :: _1
          )
# 832 "omake_exp_parse.ml"
               : 'rev_exp_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 296 "omake_exp_parse.mly"
     ( () )
# 838 "omake_exp_parse.ml"
               : 'opt_semi_or_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'semi_or_comma) in
    Obj.repr(
# 298 "omake_exp_parse.mly"
     ( () )
# 845 "omake_exp_parse.ml"
               : 'opt_semi_or_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 303 "omake_exp_parse.mly"
     ( _1 )
# 852 "omake_exp_parse.ml"
               : 'semi_or_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 305 "omake_exp_parse.mly"
     ( _1 )
# 859 "omake_exp_parse.ml"
               : 'semi_or_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 313 "omake_exp_parse.mly"
   ( [] )
# 865 "omake_exp_parse.ml"
               : 'opt_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 315 "omake_exp_parse.mly"
   ( _1 )
# 872 "omake_exp_parse.ml"
               : 'opt_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_args) in
    Obj.repr(
# 319 "omake_exp_parse.mly"
          ( List.rev _1 )
# 879 "omake_exp_parse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_arrow_args) in
    Obj.repr(
# 321 "omake_exp_parse.mly"
          ( List.rev _1 )
# 886 "omake_exp_parse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_arrow_args) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rev_args) in
    Obj.repr(
# 323 "omake_exp_parse.mly"
          ( List.rev_append _1 (List.rev _3) )
# 895 "omake_exp_parse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_arg) in
    Obj.repr(
# 328 "omake_exp_parse.mly"
          ( [_1] )
# 902 "omake_exp_parse.ml"
               : 'rev_arrow_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_arrow_args) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arrow_arg) in
    Obj.repr(
# 330 "omake_exp_parse.mly"
          ( _3 :: _1 )
# 911 "omake_exp_parse.ml"
               : 'rev_arrow_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_args) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 335 "omake_exp_parse.mly"
          ( let e, _ = _3 in
               ArrowArg (get_fun_params (List.rev _1), e)
          )
# 922 "omake_exp_parse.ml"
               : 'arrow_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 342 "omake_exp_parse.mly"
   ( [_1] )
# 929 "omake_exp_parse.ml"
               : 'rev_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_args) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 344 "omake_exp_parse.mly"
          ( _3 :: _1 )
# 938 "omake_exp_parse.ml"
               : 'rev_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 348 "omake_exp_parse.mly"
   ( let e, _ = _1 in
               ExpArg e
          )
# 947 "omake_exp_parse.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_symbol.symbol * Lm_location.loc) in
    Obj.repr(
# 352 "omake_exp_parse.mly"
   ( let key, loc = _1 in
	        KeyArg (key, NullExp loc)
          )
# 956 "omake_exp_parse.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lm_symbol.symbol * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Omake_ast.exp * Lm_location.loc) in
    Obj.repr(
# 356 "omake_exp_parse.mly"
   ( let key, _ = _1 in
            let e, _ = _3 in
	        KeyArg (key, e)
          )
# 968 "omake_exp_parse.ml"
               : 'arg))
(* Entry ast_exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let ast_exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Omake_ast.exp)
