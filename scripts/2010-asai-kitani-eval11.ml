(*
#use "eval11.ml";;

# eval11 (Lam("x",Var("x")));;
- : v = VFun <fun>

*)

type t = Var of string                        (* term *)
       | Lam of string * t | App of t * t
       | Shift of string * t | Reset of t

type xs = string list                (* argument list *)

exception UnboundVariable

(* offset : string -> xs -> int *)
let offset x xs =
  let rec loop xs n = match xs with
      [] -> raise UnboundVariable
    | a::rest ->
        if x = a then n else loop rest (n + 1)
  in loop xs 0

type i = IAccess of int                (* instruction *)
       | IPush_closure of i list | IReturn
       | IPush_env | IPop_env | ICall
       | IShift of i list | IReset of i list

type v = VFun of (s -> c -> d -> v)          (* value *)
       | VCont of (c * s)
       | VEnv of v list
       | VK of c
and  c = i list                       (* continuation *)
and  d = (c * s) list                         (* dump *)
and  s = v list                         (* data stack *)

(* run_d11 : d -> v -> v *)
let rec run_d11 d v = match d with
    [] -> v
  | (c,s)::d' -> run_c11 c (v::s) d'

(* run_c11 : i list -> s -> d -> v *)
and run_c11 c s d = match c with
    [] -> (match s with [v] -> run_d11 d v)
  | IAccess(n)::c ->
     (match s with (VEnv(vs)::s) ->
        run_c11 c ((List.nth vs n)::s) d)
  | IPush_closure(code)::c ->
     (match s with (VEnv(vs)::s) ->
       run_c11 c
         (VFun(fun (v::s') c' d' ->
           run_c11 (code@c') (VEnv(v::vs)::s') d')
          ::s) d)
  | IReturn::c -> (match s with (v::VK(c')::s) ->
     run_c11 c' (v::s) d)
  | IPush_env::c -> (match s with (VEnv(vs)::s) ->
     run_c11 c (VEnv(vs)::VEnv(vs)::s) d)
  | IPop_env::c ->
     (match s with (v0::VEnv(vs)::s) ->
       run_c11 c (VEnv(vs)::v0::s) d)
  | ICall::c -> (match s with (v1::v0::s) ->
     match v0 with                          (* dummy *)
         VFun(f) -> f (v1::VK(c)::s) [] d 
       | VCont(c',s') ->
           run_c11 c' (v1::s') ((c,s)::d))
  | IShift(code)::c ->
     (match s with (VEnv(vs)::s) ->
       run_c11 code [VEnv(VCont(c,s)::vs)] d)
  | IReset(code)::c ->
     (match s with (VEnv(vs)::s) ->
       run_c11 code [VEnv(vs)] ((c,s)::d))

(* f11 : t -> xs -> i list *)
let rec f11 t xs = match t with
    Var(x) -> [IAccess (offset x xs)]
  | Lam(x,t) ->
      [IPush_closure((f11 t (x::xs))@[IReturn])]
  | App(t0,t1) ->
      [IPush_env]@(f11 t0 xs)@[IPop_env]
      @(f11 t1 xs)@[ICall]
  | Shift(x,t) -> [IShift(f11 t (x::xs))]
  | Reset(t) -> [IReset(f11 t xs)]

(* eval11 : t -> v *)
let eval11 t = run_c11 (f11 t []) [VEnv([])] []
