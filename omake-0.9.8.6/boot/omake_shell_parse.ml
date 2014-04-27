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

open Parsing;;
let _ = parse_error;;
# 30 "omake_shell_parse.mly"
open Lm_printf
open Lm_symbol
open Lm_location

open Omake_env
open Omake_pos
open Omake_shell_type
open Omake_command_type
open Omake_value_type

module Pos = MakePos (struct let name = "Omake_shell_parse" end)
open Pos

(*
 * If the command is a node, detect it here.
 *)
let collect_redirect chan =
   match chan with
      [ValNode node] ->
         RedirectNode node
    | _ ->
         RedirectArg chan

(*
 * Build a command from a sequence of words.
 *)
let null_command loc =
   { cmd_loc     = loc;
     cmd_env     = [];
     cmd_exe     = ();
     cmd_argv    = [];
     cmd_stdin   = RedirectNone;
     cmd_stdout  = RedirectNone;
     cmd_stderr  = false;
     cmd_append  = false
   }

let command_of_values argv loc =
   { cmd_loc     = loc;
     cmd_env     = [];
     cmd_exe     = ();
     cmd_argv    = argv;
     cmd_stdin   = RedirectNone;
     cmd_stdout  = RedirectNone;
     cmd_stderr  = false;
     cmd_append  = false
   }

(*
 * Diversions.
 *)
let rec set_stdin_inner pipe file =
   match pipe with
      PipeApply (loc, apply) ->
         let apply = { apply with apply_stdin = file } in
            PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command = { command with cmd_stdin = file } in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         raise (Invalid_argument "Omake_shell_parse.set_stdin: internal error")
    | PipeGroup (loc, group) ->
         let group = { group with group_stdin = file } in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdin_inner pipe file)

let rec set_stdout_inner pipe file stderr append =
   match pipe with
      PipeApply (loc, apply) ->
         let apply =
            { apply with apply_stdout = file;
                         apply_stderr = stderr;
                         apply_append = append
            }
         in
            PipeApply (loc, apply)
    | PipeCommand (loc, command) ->
         let command =
            { command with cmd_stdout = file;
                           cmd_stderr = stderr;
                           cmd_append = append
            }
         in
            PipeCommand (loc, command)
    | PipeCond (loc, _, _, _)
    | PipeCompose (loc, _, _, _) ->
         raise (Invalid_argument "Omake_shell_parse.set_stdout: internal error")
    | PipeGroup (loc, group) ->
         let group =
            { group with group_stdout = file;
                         group_stderr = stderr;
                         group_append = append
            }
         in
            PipeGroup (loc, group)
    | PipeBackground (loc, pipe) ->
         PipeBackground (loc, set_stdout_inner pipe file stderr append)

let set_stdin pipe file =
   set_stdin_inner pipe (collect_redirect file)

let set_stdout pipe file stderr append =
   set_stdout_inner pipe (collect_redirect file) stderr append
# 125 "omake_shell_parse.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* TokEof *);
  258 (* TokValues *);
  259 (* TokDefine *);
  260 (* TokLeftParen *);
  261 (* TokRightParen *);
  262 (* TokLessThan *);
  263 (* TokGreaterThan *);
  264 (* TokGreaterGreaterThan *);
  265 (* TokAmp *);
  266 (* TokPipe *);
  267 (* TokSemiColon *);
  268 (* TokAnd *);
  269 (* TokOr *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\005\000\005\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\003\000\003\000\003\000\004\000\
\002\000\003\000\003\000\003\000\003\000\004\000\004\000\001\000\
\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\019\000\000\000\020\000\000\000\003\000\
\017\000\000\000\000\000\001\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\018\000\010\000\011\000\000\000\
\012\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\014\000\015\000\000\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\009\000\010\000"

let yysindex = "\008\000\
\033\255\000\000\000\000\000\000\017\255\000\000\032\255\000\000\
\000\000\020\255\071\255\000\000\020\255\002\255\008\255\000\000\
\255\254\017\255\017\255\017\255\000\000\000\000\000\000\020\255\
\000\000\020\255\000\000\017\255\096\255\088\255\080\255\104\255\
\000\000\000\000\096\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\059\255\084\255\045\255\052\255\
\000\000\000\000\064\255"

let yygindex = "\000\000\
\000\000\251\255\000\000\248\255\000\000"

let yytablesize = 117
let yytable = "\011\000\
\004\000\021\000\005\000\004\000\023\000\025\000\027\000\028\000\
\001\000\004\000\024\000\029\000\030\000\031\000\032\000\033\000\
\026\000\034\000\004\000\016\000\005\000\004\000\035\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\012\000\003\000\004\000\000\000\005\000\013\000\014\000\015\000\
\016\000\017\000\018\000\019\000\020\000\005\000\000\000\000\000\
\000\000\005\000\000\000\000\000\006\000\005\000\005\000\005\000\
\006\000\005\000\000\000\007\000\006\000\006\000\006\000\007\000\
\008\000\000\000\000\000\007\000\008\000\007\000\000\000\000\000\
\008\000\000\000\008\000\022\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\004\000\013\000\014\000\015\000\
\004\000\000\000\000\000\019\000\004\000\013\000\014\000\015\000\
\000\000\017\000\018\000\019\000\020\000\013\000\014\000\015\000\
\000\000\017\000\000\000\019\000\020\000\013\000\014\000\015\000\
\000\000\000\000\000\000\019\000\020\000"

let yycheck = "\005\000\
\002\001\010\000\004\001\002\001\013\000\014\000\015\000\009\001\
\001\000\002\001\009\001\017\000\018\000\019\000\020\000\024\000\
\009\001\026\000\002\001\001\001\004\001\002\001\028\000\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\001\001\001\001\002\001\255\255\004\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\001\001\255\255\255\255\
\255\255\005\001\255\255\255\255\001\001\009\001\010\001\011\001\
\005\001\013\001\255\255\001\001\009\001\010\001\011\001\005\001\
\001\001\255\255\255\255\009\001\005\001\011\001\255\255\255\255\
\009\001\255\255\011\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\001\001\006\001\007\001\008\001\
\005\001\255\255\255\255\012\001\009\001\006\001\007\001\008\001\
\255\255\010\001\011\001\012\001\013\001\006\001\007\001\008\001\
\255\255\010\001\255\255\012\001\013\001\006\001\007\001\008\001\
\255\255\255\255\255\255\012\001\013\001"

let yynames_const = "\
  "

let yynames_block = "\
  TokEof\000\
  TokValues\000\
  TokDefine\000\
  TokLeftParen\000\
  TokRightParen\000\
  TokLessThan\000\
  TokGreaterThan\000\
  TokGreaterGreaterThan\000\
  TokAmp\000\
  TokPipe\000\
  TokSemiColon\000\
  TokAnd\000\
  TokOr\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 190 "omake_shell_parse.mly"
          ( let pipe, _ = _1 in
               pipe
          )
# 246 "omake_shell_parse.ml"
               : Omake_env.value_pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lm_location.loc) in
    Obj.repr(
# 194 "omake_shell_parse.mly"
          ( let loc = _1 in
               PipeCommand (loc, null_command loc)
          )
# 255 "omake_shell_parse.ml"
               : Omake_env.value_pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'command) in
    Obj.repr(
# 203 "omake_shell_parse.mly"
          ( let command, loc = _1 in
               PipeCommand (loc, command), loc
          )
# 264 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pipe) in
    Obj.repr(
# 207 "omake_shell_parse.mly"
          ( let pipe1, loc1 = _1 in
            let pipe2, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeSequence, pipe1, pipe2), loc
          )
# 277 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pipe) in
    Obj.repr(
# 213 "omake_shell_parse.mly"
          ( let pipe1, loc1 = _1 in
            let pipe2, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeAnd, pipe1, pipe2), loc
          )
# 290 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pipe) in
    Obj.repr(
# 219 "omake_shell_parse.mly"
          ( let pipe1, loc1 = _1 in
            let pipe2, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               PipeCond (loc, PipeOr, pipe1, pipe2), loc
          )
# 303 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pipe) in
    Obj.repr(
# 225 "omake_shell_parse.mly"
          ( let pipe1, loc1 = _1 in
            let pipe2, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
               PipeCompose (loc, false, pipe1, pipe2), loc
          )
# 316 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'pipe) in
    Obj.repr(
# 231 "omake_shell_parse.mly"
          ( let pipe1, loc1 = _1 in
            let pipe2, loc2 = _4 in
            let loc = union_loc loc1 loc2 in
               PipeCompose (loc, true, pipe1, pipe2), loc
          )
# 330 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string * Lm_location.loc) in
    Obj.repr(
# 237 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let _, loc2 = _2 in
            let loc = union_loc loc1 loc2 in
               PipeBackground (loc, pipe), loc
          )
# 342 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string * Lm_location.loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pipe) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * Lm_location.loc) in
    Obj.repr(
# 243 "omake_shell_parse.mly"
          ( let _, loc1 = _1 in
            let _, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
            let pipe, _ = _2 in
            let group =
               { group_stdin  = RedirectNone;
                 group_stdout = RedirectNone;
                 group_stderr = false;
                 group_append = false;
                 group_pipe = pipe
               }
            in
               PipeGroup (loc, group), loc
          )
# 364 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 258 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let file, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdin pipe file in
               pipe, loc
          )
# 378 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 265 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let file, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file false false in
               pipe, loc
          )
# 392 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 272 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let file, loc2 = _3 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file false true in
               pipe, loc
          )
# 406 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 279 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let file, loc2 = _4 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file true false in
               pipe, loc
          )
# 421 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pipe) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string * Lm_location.loc) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string * Lm_location.loc) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 286 "omake_shell_parse.mly"
          ( let pipe, loc1 = _1 in
            let file, loc2 = _4 in
            let loc = union_loc loc1 loc2 in
            let pipe = set_stdout pipe file true true in
               pipe, loc
          )
# 436 "omake_shell_parse.ml"
               : 'pipe))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_command) in
    Obj.repr(
# 298 "omake_shell_parse.mly"
          ( let rev_argv, loc = _1 in
            let command = command_of_values (List.rev rev_argv) loc in
               command, loc
          )
# 446 "omake_shell_parse.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 306 "omake_shell_parse.mly"
          ( let values, loc = _1 in
               [values], loc
          )
# 455 "omake_shell_parse.ml"
               : 'rev_command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rev_command) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'word) in
    Obj.repr(
# 310 "omake_shell_parse.mly"
          ( let values1, loc1 = _1 in
            let values2, loc2 = _2 in
               values2 :: values1, union_loc loc1 loc2
          )
# 466 "omake_shell_parse.ml"
               : 'rev_command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Omake_value_type.value list * Lm_location.loc) in
    Obj.repr(
# 320 "omake_shell_parse.mly"
   ( _1 )
# 473 "omake_shell_parse.ml"
               : 'word))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Omake_env.value_pipe)
