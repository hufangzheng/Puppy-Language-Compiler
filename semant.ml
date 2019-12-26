open Errormsg

type tenv = Env.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp; ty: Types.ty}

module A = Absyn

(* 检查表达式类型是否为INT *)
let checkInt ({exp; ty}, pos) =
  if ty = Types.INT then ()
  else
    let msg = "expect a type of int." in
    display_error (TypeError, pos, msg);
    raise_error TypeError pos msg

(* 检查exp的起始函数 *)
let rec transExp (venv, tenv, exp) =

(* 检查exp，避免重复传入venv和tenv环境变量，当需要改变环境变量时，递归调用transExp *)
  let rec trep = function
  | A.IntExp num           -> {exp = (); ty = Types.INT}
  | A.NilExp pos           -> {exp = (); ty = Types.NIL}
  | A.StringExp (str, pos) -> {exp = (); ty = Types.STRING}
  | A.BreakExp pos         -> {exp = (); ty = Types.UNIT}
  | A.OpExp {left; op; right; pos} -> (
      let l = trep left and r = trep right in (
      match op with
      | A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp -> (
        checkInt(l, pos);
        checkInt(r, pos);
      )
      | A.EqOp | A.NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp -> (
        Types.check_ty l.ty r.ty
      )
      )
      {exp = (); ty = Types.INT}
  )
  | A.LValue {l; pos} -> (
      let rec trlval l pos = (
      match l with
      | A.Ident {name; pos}      -> (match (Symbol.look venv name) with
                                   | Some(Env.VarEntry{ty}) -> {exp = (); ty = ty}
                                   | Some(Env.FunEntry{formals, result}) -> failwith "This is a function, simple variable expected."
                                   | None -> {exp = (); ty = Types.INT})
      | A.RecordAccess {record; name; pos} -> (
          let {exp; ty} = trlval record pos in
          match ty with
          | Types.Record (fields, unique) ->
            let (s, t) = (List.find (fun (sym, ty) -> Symbol.name sym = Symbol.name name) fields) in
            {exp = (); ty = t}
          | _ -> failwith "This variable is not a record type."
      )
      | A.ArrayAccess {array; exp; pos} -> (
          let {_; arrty} = trlval array pos in
          let {_; indexty} = trep exp in
            match arrty with
            | Types.ARRAY (ty, unique) ->
              (if indexty = Types.INT then {exp = (); ty = ty})
            | _ -> failwith "The index of array is not INT."
      )
      ) in trlval l pos
      ) in
  trep exp

let rec trdec venv tenv decl =
  match decl with
  | VarDec {name; _; typ; init; pos} -> (
      let {init_exp; init_ty} = transExp venv tenv init in
      match (var_dec_ty, init_ty) with
      | (Some var_ty, None) -> (
          let varty = Symbol.look tenv typ in     (* 从类型环境tenv中查找变量声明指定的类型 *)
          Symbol.enter venv name Env.VarEntry{ty = varty}
        )
      | (Some var_ty, _)    -> (
          let varty = Symbol.look tenv typ in
          if Types.check_ty var_ty init_ty then (Symbol.enter venv name Env.VarEntry{ty = varty})
          else
            failwith "This type is "
        )
  )
