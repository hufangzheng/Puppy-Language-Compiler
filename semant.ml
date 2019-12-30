open Errormsg
open Printf
open Absyn

type tenv = Env.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp; ty: Types.ty}

module A = Absyn

(* 检查表达式类型是否为INT *)
let checkInt ({exp; ty}, pos) =
  if ty = Types.INT then ()
  else
    failwith "expect a type of int."

(* 在type环境中查询类型名（ty_symbol）所绑定的类型，找到后返回真实类型 *)
let find_ty tenv ty_symbol =
  match Symbol.look tenv ty_symbol with
  | Some ty -> Types.actual_ty ty
  | None ->
      failwith (sprintf "This type is not in scope: %s" (Symbol.name ty_symbol))

(* 检查exp的起始函数 *)
let rec transExp venv tenv exp =
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
        if Types.check_ty l.ty r.ty then ()
        else
          failwith "The types of two operators are not matched."
      )
      );
      {exp = (); ty = Types.INT}
  )
  | A.LetExp {decs; body; pos} -> check_let venv tenv decs body
  | A.LValue {l; pos} -> trlval l
  | A.AssignExp {lvalue; exp; pos} -> trassign venv tenv lvalue exp
  | A.CallExp {func; args; pos} -> {exp = (); ty = trcall venv tenv func args}
  | A.RecordExp {rec_exp_fields; typ = typ_id; pos} -> {exp = (); ty = trrecord venv tenv rec_exp_fields typ_id}
  | A.SeqExp exps -> {exp = (); ty = Types.INT}
  | A.IfExp {test; then'; else'; pos} -> {exp = (); ty = Types.INT}
  | A.WhileExp {test; body; pos} -> {exp = (); ty = Types.INT}
  | A.ForExp {var; lo; hi; body; pos; _} -> {exp = (); ty = Types.INT}
  | A.BreakExp pos -> {exp = (); ty = Types.UNIT}
  | A.ArrayExp {typ; size; init; pos} -> {exp = (); ty = Types.INT}

(* 检查声明 transiton declartion *)
and trdec venv tenv decl =
  match decl with
  (* 检查变量的声明 *)
  | A.VarDec {name; escape; typ; init; pos} -> (
      let {exp = _; ty = init_ty} = transExp venv tenv init in
      let venv' =           (* 新的var environment *)
      match (typ, init_ty) with
      | (Some var_ty, Types.NIL) -> (
          let varty = find_ty tenv (fst var_ty) in     (* 从类型环境tenv中查找变量声明指定的类型，var_ty为(symbol * pos)元组，使用fst函数获取第一个元素 *)
          match varty with
          | Types.RECORD _ as f -> Symbol.enter venv name (Env.VarEntry{ty = f})
          | _ -> failwith "type nil can only be assigned with record variable."
        )
      | (Some var_ty, _)    -> (
          let varty = find_ty tenv (fst var_ty) in
          if Types.check_ty varty init_ty then Symbol.enter venv name (Env.VarEntry{ty = init_ty})
          else
            failwith (sprintf "This type is %s, %s expected." (Types.string_of_ty varty) (Types.string_of_ty init_ty))
        )
      | (None, Types.NIL)   -> failwith "type nil need to be assigned with type-defined variable."
      | (None, _)           -> Symbol.enter venv name (Env.VarEntry{ty = init_ty})
      in
      (venv', tenv)         (* 最终返回新的环境变量 *)
    )
  (* 检查类型的声明
   * 将新声明的类型加入到类型环境tenv中
   *)
  | A.TypeDec {name; ty; pos} -> (
      let tenv' = Symbol.enter tenv name (trans_absty_to_typesty tenv ty) in
      (venv, tenv')
)
  (* 检查函数的声明 *)
  | A.FunctionDec fundec -> (
      let find_result_type result =
        match result with
        | None -> Types.UNIT
        | Some t -> find_ty tenv (fst t)
      in
      (* 查询单个field的类型，返回(参数名, 参数类型)对 *)
      let transparam {name; typ; _} = (name, find_ty tenv typ) in
      (* 求形参列表中每个field的类型，返回[(n1, t1); (n2, t2); (n3, t3)...]列表 *)
      let find_params_type params = List.map transparam params in
      (* 将函数加入到初始的变量环境中 *)
      let add_func venv {name; params; result; body; _} =
        let result_type = find_result_type result in
        let params_type = find_params_type params in
        let params' = List.map snd params_type in
        Symbol.enter venv name (Env.FunEntry{formals = params'; result = result_type}) in
      let venv' = List.fold_left add_func venv fundec in
      List.iter
        (fun {name; params; result; body; _} ->
           (* 将单个形参加入到变量环境中 *)
           let enterparam venv (name, ty) =
             Symbol.enter venv name (Env.VarEntry{ty = ty}) in
           (* 将所有形参加入到变量环境中 *)
           let params_type = find_params_type params in
           let venv'' = List.fold_left enterparam venv' params_type in
           transExp venv'' tenv body;
           ();
        )
        fundec;
        (venv', tenv)
)

(* 将抽象语法树的类型Absyn.ty转换为Types.ty类型
 * Absyn.NameTy : 在类型环境tenv中查找名称symbol对应的类型并返回
 * Absyn.RecordTy : 检查field列表中的每一项的类型，并创建一个Types.RECORD类型返回
 * Absyn.ArrayTy : 检查数组元素的类型，并创建一个Types.ARRAY类型返回
 *)
and trans_absty_to_typesty tenv ty =
  match ty with
  | A.NameTy (symbol, pos) -> find_ty tenv symbol
  | A.RecordTy field ->
    let rec_field_list =
      List.map (fun {name; escape; typ; _} -> (name, find_ty tenv typ)) field
    in
    Types.RECORD (rec_field_list, ref ())
  | A.ArrayTy (symbol, pos) ->
    let arr_ele_ty =
      find_ty tenv symbol
    in
    Types.ARRAY (arr_ele_ty, ref ())

(* 检查Let表达式 *)
and check_let venv tenv decls body =
    let (venv', tenv') =
      List.fold_left (fun (venv, tenv) decl -> trdec venv tenv decl) (venv, tenv) decls
    in
    transExp venv' tenv' body

and trlval l = (
      match l with
      | A.Ident {name; pos}      -> (match (Symbol.look venv name) with
                                   | Some(Env.VarEntry{ty}) -> {exp = (); ty = ty}
                                   | Some(Env.FunEntry{formals; result}) -> failwith "This is a function, simple variable expected."
                                   | None -> {exp = (); ty = Types.INT})
      | A.RecordAccess {record; name; pos} -> (
          let {exp; ty} = trlval record in
          match ty with
          | Types.RECORD (fields, unique) ->
            let (s, t) = (List.find (fun (sym, ty) -> Symbol.name sym = Symbol.name name) fields) in
            {exp = (); ty = t}
          | _ -> failwith "This variable is not a record type."
      )
      | A.ArrayAccess {array; exp; pos} -> (
          let {exp = _; ty = arrty} = trlval array in
          let {exp = _; ty = indexty} = trep exp in
            match arrty with
            | Types.ARRAY (ty, unique) ->
              if indexty = Types.INT then {exp = (); ty = ty}
              else
                failwith "The type of index of array is not int."
            | _ -> failwith "This variable is not a array."
      )
      )

and trassign venv tenv lvalue exp =
  let {exp = _; ty = lvalue_ty} = trlval lvalue in
  let {exp = _; ty = exp_ty} = transExp venv tenv exp in
  match Types.check_ty lvalue_ty exp_ty with
  | true -> {exp = (); ty = Types.UNIT}
  | false -> sprintf (failwith "This expression has type %s, but type %s expected.") (Types.string_of_ty exp_ty) (Types.string_of_ty lvalue_ty)

(* 对函数调用表达式CallExp的检查
 * 在变量环境venv中查找函数名，并判断是否与FunEntry进行了绑定
 * 若是，检查传入的参数的类型是否与FunEntry中的参数类型一致
 * 若相一致，返回函数的类型
 *)
and trcall venv tenv func args =
  match Symbol.look venv func with
  | None -> failwith "This function is not exist."
  | Some (Env.VarEntry _) -> failwith (sprintf "The %s is not a function." (Symbol.name func))
  | Some (Env.FunEntry {formals = formals_ty; result}) ->
    let args_ty = List.map (fun exp -> (trep exp).ty) args in
    List.iter2
      (fun expected actual ->
         if not(Types.check_ty expected actual) then
           failwith (sprintf "Argument type error.\nIn function %s: this type is %s, %s expected." (Symbol.name func) (Types.string_of_ty actual) (Types.string_of_ty expected))
      )
      formals_ty args_ty;
      result

(* 检查record表达式RecordExp
 * 先在类型环境中根据typ_id检查是否存在该record变量
 * 若存在，返回值为Types.RECORD类型，比较该类型的fields中每个数据域的类型与
 * 表达式中的rec_fields的类型是否相同
 * 若是，返回Types.RECORD类型
 *)
and trrecord venv tenv rec_fields typ_id =
  let rec_type = find_ty tenv typ_id in
  match rec_type with
  | Types.RECORD (fields, unique) ->
      let rec_act_ty = List.map (fun (symbol, exp, _) -> (trep exp).ty) rec_fields in
      let rec_formal_ty = List.map snd fields in
      if List.for_all2 (fun expected actual -> Types.check_ty expected actual) rec_formal_ty rec_act_ty then
        rec_type
      else
        failwith "The record fields are not matched."
  | _ -> failwith (sprintf "%s is not a record." (Symbol.name typ_id))

in
trep exp
