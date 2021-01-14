type proj_t = Left | Right | Value
exception Should_not_be_relational of string
type 'a relational = Rel of 'a * 'a | Simple of 'a

(* module Rexpr : REXPR = *)
module Rexpr =
struct

  type 'a t = 'a relational

  let is_relational = function
    | Rel _ -> true
    | _ -> false

  let mk_simple expr = Simple expr
  let mk_rel expr expr' = Rel (expr, expr')

  let deduplicate = function
    | Rel (expr, expr') ->
      if expr <> expr' then Rel (expr, expr')
      else mk_simple expr
    | rexpr -> rexpr

  let deduplicate_eq equals = function
    | Rel (expr, expr') as rexpr ->
      if equals expr expr'
      then mk_simple expr
      else rexpr
    | rexpr -> rexpr

  let left = function
    | Rel (expr, _) -> expr
    | Simple expr -> expr

  let r_left = function
    | Rel (expr, _) -> Simple expr
    | Simple expr -> Simple expr
  
  let right = function
    | Rel (_, expr) -> expr
    | Simple expr -> expr

  let r_right = function
    | Rel (_, expr) -> Simple expr
    | Simple expr -> Simple expr
  
  let value = function
    | Rel _ -> raise @@ Should_not_be_relational
        "[Rel_expr][value] expects a simple expression."
    | Simple expr -> expr

  let proj p rexpr =
    match p with
    | Left -> left rexpr
    | Right -> right rexpr
    | Value -> value rexpr
  
  let val_equals f rexpr rexpr' =
    let equals' p r_e r_e' = f (proj p r_e) (proj p r_e') in
    equals' Left rexpr rexpr' && equals' Right rexpr rexpr'

  let equals f rexpr rexpr' =
    val_equals f rexpr rexpr'

  let hash f rexpr =
    Hashtbl.hash (f @@ left rexpr, f @@ right rexpr)

  let apply f = function
    | Rel (expr,expr') ->
      let x = f expr
      and x' = f expr'
      in mk_rel x x'
    | Simple expr ->
      let x = f expr
      in mk_simple x

  let apply_r_func f rexpr =
    match f with
    | Rel (f,f') ->
      let x = f (left rexpr)
      and x' = f' (right rexpr)
      in mk_rel x x'
    | Simple f -> apply f rexpr

  let apply2 f rexpr1 rexpr2 =
    apply_r_func (apply f rexpr1) rexpr2

  let apply3 f rexpr1 rexpr2 rexpr3 =
    apply_r_func (apply2 f rexpr1 rexpr2) rexpr3
  
  let apply_lr f rexpr =
    f (left rexpr) (right rexpr)

  let apply_left f = function
    | Rel (expr, expr') -> Rel (f expr, expr')
    | Simple _ -> failwith "Cannot apply_left on relational expression"

  let apply_right f = function
    | Rel (expr, expr') -> Rel (expr, f expr')
    | Simple _ -> failwith "Cannot apply_right on relational expression"
  
  let fold f rexpr x =
    match rexpr with
    | Rel (expr,expr') ->
      f expr x |> f expr'
    | Simple expr ->
      f expr x

  let fold2 f rexpr1 rexpr2 x =
    match rexpr1,rexpr2 with
    | (Simple expr1, Simple expr2) ->
      f expr1 expr2 x
    | _ ->
      f (left rexpr1) (left rexpr2) x
      |> f (right rexpr1) (right rexpr2)

  let to_string f  = function
    | Rel (expr, expr') -> "<" ^ f expr ^ " | " ^ f expr' ^ ">"
    | Simple expr -> "<" ^ f expr ^ ">"

  let pp_rexpr pp_expr ppf = function
    | Rel (expr, expr') -> Format.fprintf ppf "<%a | %a>" pp_expr expr pp_expr expr'
    | Simple expr -> Format.fprintf ppf "<%a>" pp_expr expr
end


(* module DatedRexpr : REXPR = *)
module DatedRexpr =
struct

  type 'a t = {
    date: int option;
    value: 'a Rexpr.t;
  }
  
  let is_relational rexpr = Rexpr.is_relational rexpr.value
  let get_value rexpr = rexpr.value

  let mk_simple ?(date=None) expr =
    { value = Rexpr.mk_simple expr; date; }
      
  let mk_rel ?(date=None) expr expr' =
    { value = Rexpr.mk_rel expr expr'; date; }

  let from_rexpr ?(date=None) rexpr =
    { value = rexpr; date; }

  (* let to_rexpr rexpr = rexpr.value *)

  let deduplicate rexpr =
    { rexpr with value = Rexpr.deduplicate rexpr.value }
    
  let deduplicate_eq equals rexpr =
    { rexpr with value = Rexpr.deduplicate_eq equals rexpr.value }

  let left rexpr = Rexpr.left rexpr.value
  let right rexpr = Rexpr.right rexpr.value
  let r_left rexpr = mk_simple ~date:rexpr.date @@ left rexpr
  let r_right rexpr = mk_simple ~date:rexpr.date @@ right rexpr
  let value rexpr = Rexpr.value rexpr.value
  let proj p rexpr = Rexpr.proj p rexpr.value

  let val_equals f rexpr rexpr' =
    Rexpr.equals f rexpr.value rexpr'.value
  
  let equals f rexpr rexpr' =
    val_equals f rexpr rexpr' && rexpr.date = rexpr'.date
  
  let hash f rexpr =
    Hashtbl.hash (Rexpr.hash f rexpr.value, rexpr.date)
  
  let apply f rexpr =
    { rexpr with value = Rexpr.apply f rexpr.value }

  (* let oldest d1 d2 =
   *   match d1, d2 with
   *   | None, None -> None
   *   | Some d, None | None, Some d -> None
   *   | Some d1, Some d2 -> Some (min d1 d2) *)

  let youngest d1 d2 =
    match d1, d2 with
    | None, None -> None
    | Some d, None | None, Some d -> Some d
    | Some d1, Some d2 -> Some (max d1 d2)
  
  let apply2 f rexpr1 rexpr2 =
    let value = Rexpr.apply2 f rexpr1.value rexpr2.value in
    let date = youngest rexpr1.date rexpr2.date in
    { value; date }

  let apply3 f rexpr1 rexpr2 rexpr3 =
    let value = Rexpr.apply3 f rexpr1.value rexpr2.value rexpr3.value in
    let date = youngest (youngest rexpr1.date rexpr2.date) rexpr3.date in 
    { value; date }

  let apply_lr f rexpr =
    Rexpr.apply_lr f rexpr.value

  let apply_left f rexpr =
      { rexpr with value = Rexpr.apply_left f rexpr.value }

  let apply_right f rexpr =
      { rexpr with value = Rexpr.apply_right f rexpr.value }
  
  let fold f rexpr x =
    Rexpr.fold f rexpr.value x

  let fold2 f rexpr1 rexpr2 x =
    Rexpr.fold2 f rexpr1.value rexpr2.value x

  let to_string f rexpr =
    Rexpr.to_string f rexpr.value ^
    match rexpr.date with
    | Some d -> Format.sprintf "(%d)" d
    | None -> "(inf)"

  let pp_rexpr pp_expr ppf rexpr =
    Rexpr.pp_rexpr pp_expr ppf rexpr.value;
    match rexpr.date with
    | Some d -> Format.fprintf ppf "(%d)" d
    | None -> Format.fprintf ppf "(inf)"

  let get_date rexpr =
    rexpr.date

  let set_date date rexpr =
    { rexpr with date }
end

(* Hide variant modules *)

type 'a t = Rexpr of 'a Rexpr.t | DRexpr of 'a DatedRexpr.t 

type rel_memory = Formula.ax_term t
type rel_pc = Formula.bl_term t
type rel_bv = Formula.bv_term t

let is_relational = function
  | Rexpr rexpr -> Rexpr.is_relational rexpr
  | DRexpr rexpr -> DatedRexpr.is_relational rexpr

let get_value = function
  | Rexpr rexpr -> rexpr
  | DRexpr rexpr -> DatedRexpr.get_value rexpr

let mk_simple ?(date=None) expr =
  match date with
  | None -> Rexpr (Rexpr.mk_simple expr)
  | Some _ -> DRexpr (DatedRexpr.mk_simple ~date expr )

let mk_rel ?(date=None) expr expr' =
  match date with
  | None -> Rexpr (Rexpr.mk_rel expr expr')
  | Some _ -> DRexpr (DatedRexpr.mk_rel ~date expr expr')

let deduplicate = function
  | Rexpr rexpr -> Rexpr (Rexpr.deduplicate rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.deduplicate rexpr)

let deduplicate_eq equals = function
  | Rexpr rexpr -> Rexpr (Rexpr.deduplicate_eq equals rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.deduplicate_eq equals rexpr)

let left = function
  | Rexpr rexpr -> Rexpr.left rexpr
  | DRexpr rexpr -> DatedRexpr.left rexpr

let right = function
  | Rexpr rexpr -> Rexpr.right rexpr
  | DRexpr rexpr -> DatedRexpr.right rexpr

let r_left = function
  | Rexpr rexpr -> Rexpr (Rexpr.r_left rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.r_left rexpr)

let r_right = function
  | Rexpr rexpr -> Rexpr (Rexpr.r_right rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.r_right rexpr)

let value = function
  | Rexpr rexpr -> Rexpr.value rexpr
  | DRexpr rexpr -> DatedRexpr.value rexpr

let proj p = function
  | Rexpr rexpr -> Rexpr.proj p rexpr
  | DRexpr rexpr -> DatedRexpr.proj p rexpr

let to_dated_rexpr = function
  | Rexpr rexpr -> DatedRexpr.from_rexpr rexpr
  | DRexpr rexpr -> rexpr

let val_equals f rexpr1 rexpr2 =
  match rexpr1, rexpr2 with
  | Rexpr rexpr1, Rexpr rexpr2 ->
    Rexpr.val_equals f rexpr1 rexpr2
  | _ ->
    let drexpr1 = to_dated_rexpr rexpr1 in
    let drexpr2 = to_dated_rexpr rexpr2 in
    DatedRexpr.val_equals f drexpr1 drexpr2

let equals f rexpr1 rexpr2 =
  match rexpr1, rexpr2 with
  | Rexpr rexpr1, Rexpr rexpr2 ->
    Rexpr.equals f rexpr1 rexpr2
  | _ ->
    let drexpr1 = to_dated_rexpr rexpr1 in
    let drexpr2 = to_dated_rexpr rexpr2 in
    DatedRexpr.equals f drexpr1 drexpr2

let hash f =
  function 
  | Rexpr rexpr -> Rexpr.hash f rexpr
  | DRexpr rexpr -> DatedRexpr.hash f rexpr

let apply f = function
  | Rexpr rexpr -> Rexpr (Rexpr.apply f rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.apply f rexpr)

let apply2 f rexpr1 rexpr2 =
  match rexpr1, rexpr2 with
  | Rexpr rexpr, Rexpr rexpr' ->
    Rexpr (Rexpr.apply2 f rexpr rexpr')
  | _ ->
    let drexpr1 = to_dated_rexpr rexpr1 in
    let drexpr2 = to_dated_rexpr rexpr2 in
    DRexpr (DatedRexpr.apply2 f drexpr1 drexpr2)

let apply3 f rexpr1 rexpr2 rexpr3 =
  match rexpr1, rexpr2, rexpr3 with
  | Rexpr rexpr1, Rexpr rexpr2, Rexpr rexpr3 ->
    Rexpr (Rexpr.apply3 f rexpr1 rexpr2 rexpr3)
  | _ ->
    let drexpr1 = to_dated_rexpr rexpr1 in
    let drexpr2 = to_dated_rexpr rexpr2 in
    let drexpr3 = to_dated_rexpr rexpr3 in
    DRexpr (DatedRexpr.apply3 f drexpr1 drexpr2 drexpr3)

let apply_lr f = function
  | Rexpr rexpr -> Rexpr.apply_lr f rexpr
  | DRexpr rexpr -> DatedRexpr.apply_lr f rexpr

let apply_left f = function
  | Rexpr rexpr -> Rexpr (Rexpr.apply_left f rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.apply_left f rexpr)

let apply_right f = function
  | Rexpr rexpr -> Rexpr (Rexpr.apply_right f rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.apply_right f rexpr)

let fold f rexpr x =
  match rexpr with
  | Rexpr rexpr -> Rexpr.fold f rexpr x
  | DRexpr rexpr -> DatedRexpr.fold f rexpr x

let fold2 f rexpr1 rexpr2 x =
  match rexpr1, rexpr2 with
  | Rexpr rexpr1, Rexpr rexpr2 ->
    Rexpr.fold2 f rexpr1 rexpr2 x
  | _ ->
    let drexpr1 = to_dated_rexpr rexpr1 in
    let drexpr2 = to_dated_rexpr rexpr2 in
    DatedRexpr.fold2 f drexpr1 drexpr2 x

let to_string f = function
  | Rexpr rexpr -> Rexpr.to_string f rexpr
  | DRexpr rexpr -> DatedRexpr.to_string f rexpr

let pp_rexpr pp_expr ppf = function
  | Rexpr rexpr -> Rexpr.pp_rexpr pp_expr ppf rexpr
  | DRexpr rexpr -> DatedRexpr.pp_rexpr pp_expr ppf rexpr 

let get_date = function
  | Rexpr _ -> None
  | DRexpr rexpr -> DatedRexpr.get_date rexpr

let set_date date = function
  | Rexpr rexpr -> DRexpr (DatedRexpr.from_rexpr ~date rexpr)
  | DRexpr rexpr -> DRexpr (DatedRexpr.set_date date rexpr)
