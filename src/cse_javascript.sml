load "ListPair";

(*                                                                   *)
(*  multiple-argument lambda calculus                                *)
(*  plus sequences                                                   *)
(*  plus conditionals                                                *)
(*  plus blocks                                                      *)
(*  plus let declarations                                            *)
(*  plus function declarations                                       *)
(*  plus return statements                                           *)
(*                                                                   *)

datatype term = NUM of real
              | UND
              | BOO of bool 
              | VAR of string
              | LAM of string list * term
              | APP of term * (term list)
              | SEQ of term list
              | CON of term * term * term
              | BLK of term
              | LET of string * term
              | FUN of string * string list * term
              | RET of term

datatype value = REAL of real
               | BOOL of bool
               | CLOS of (string * value ref) list list *
	                 string list * term
               | PRIM of string
               | UNAS  (* "unassigned" initial value for locals    *)
               | UNDE  (* undefined value: default return value    *)

(*                                                                 *)
(* environments                                                    *)
(*                                                                 *)

type frame = (string * value ref) list

type env = frame list

val empty = nil

fun lookupframe x nil
  = NONE
  | lookupframe x ((x', v) :: f')
  = if x = x' then SOME (! v) else lookupframe x f'

exception FrameError of string * (string * value ref) list

fun replaceframe x v ((x', v') :: f)
  = if x = x' then v' := v else (replaceframe x v f)
  | replaceframe x v e
  = raise FrameError ("no pattern applies in replaceframe", e)

fun extend xs vs e
    = ListPair.map (fn (x, v) => (x, ref v)) (xs, vs) :: e

fun lookup x (f :: e')
    = case lookupframe x f of
        NONE => lookup x e'
      | SOME v => v

fun replace x v (f :: e')
    = case lookupframe x f of
        NONE => replace x v e'
      | SOME _ => replaceframe x v f

(*                                                                 *)
(* control is list of directives                                   *)
(*                                                                 *)

datatype directive = T of term         (* Term                     *)
                   | A of int          (* Apply                    *)
                   | P                 (* Pop                      *)
                   | E of env          (* Environment              *)
                   | B of term * term  (* Branch                   *)
                   | S of string       (* aSsignment               *)
                   | R                 (* Reset control            *)
                   | M                 (* Mark control             *)

exception ApplyPrimError of string

(*                                                                 *)
(* a small selection of primitive functions to run examples        *)
(*                                                                 *)

fun applyprim "+" (v1:real) (v2:real) = REAL (v1 + v2)
  | applyprim "-" (v1:real) (v2:real) = REAL (v1 - v2)
  | applyprim "*" (v1:real) (v2:real) = REAL (v1 * v2)
  | applyprim "<=" (v1:real) (v2:real) = BOOL (v1 <= v2)
  | applyprim s v1 v2 = raise ApplyPrimError(s)

(*                                                                 *)
(* find the declarations in a list of statements/expressions       *)
(* note that branches of conditionals are blocks with their        *)
(* own scope                                                       *)
(*                                                                 *)

fun scan (SEQ ts)         = foldr (fn (x,y) => scan x @ y) nil ts
  | scan (LET (x, t))     = [x]
  | scan (FUN (f, xs, t)) = [f]
  | scan other            = nil

exception Error of string * value list

(* run function                                                    *)

(*                                                                 *)
(* base case                                                       *)
(*                                                                 *)
fun run                                  nil          (v :: s) e
  = v  
(*                                                                 *)
(* primitive expressions                                           *)
(*                                                                 *)
  | run                      (T (NUM n) :: c)               s  e
  = run                                    c     (REAL n :: s) e
  | run                          (T UND :: c)               s  e
  = run                                    c       (UNDE :: s) e 
  | run                      (T (BOO b) :: c)               s  e
  = run                                    c     (BOOL b :: s) e 
  | run                      (T (VAR x) :: c)               s  e
  = run                                    c (lookup x e :: s) e 
(*                                                                 *)
(* compound expressions and statements                             *)
(*                                                                 *)
  | run                     (T (LAM (x, t)) :: c)           s  e
  = run         c         (CLOS (e, x, SEQ [t, RET UND]) :: s) e 
  | run               (T (APP (f, ts)) :: c)                s  e
  = run        (T f :: map (fn t => T t) ts
                       @ A (length ts) :: c)                s  e 
  | run                    (T (SEQ ts) :: c)                s  e
  = run (foldr
         (fn (t, ts') =>
             if null ts' then [T t]
             else T t :: P :: ts')
         nil ts                         @ c)                s  e
  | run     (T (CON (pred, cons, alt)) :: c)                s  e
  = run       (T pred :: B (cons, alt) :: c)                s  e
  | run                (T (LET (x, t)) :: c)                s  e
  = run                    (T t :: S x :: c)                s  e 
  | run                     (T (BLK t) :: c)                s  e
  = let val locals = scan t
        val unassigneds =
        map (fn x => UNAS) locals
    in
    run                    (T t :: E e :: c)                s
                                    (extend locals unassigneds e) 
    end
  | run        (T (FUN (x, params, t)) :: c)                s  e
  = run  (T (LET (x, LAM (params, t))) :: c)                s  e
  | run                     (T (RET t) :: c)                s  e
  = run (T t :: R :: c)                                     s  e 
(*                                                                 *)
(* other directives                                                *)
(*                                                                 *)
(* Environment directive                                           *)
  | run (E e' :: c)                                         s  e
  = run c                                                   s  e'
(* R: Return: keep popping directives until M is found             *)
(*                                   M is found                    *)
  | run                        (R :: M :: c)                s  e
  = run                                   c                 s  e
(*                                   M is not yet found            *)
  | run                        (R :: d :: c)                s  e
  = run                             (R :: c)                s  e
(* Branch directive                                                *)
  | run                 (B (cons, alt) :: c)  (BOOL true :: s) e
  = run (T cons :: c)                                       s  e 
  | run                 (B (cons, alt) :: c) (BOOL false :: s) e
  = run                         (T alt :: c)                s  e
(* Pop directive                                                   *)
  | run                             (P :: c)          (v :: s) e
  = run                                   c                 s  e
(* Set directive                                                   *)
  | run                           (S x :: c)                s  e
  = (replace x (hd s) e;
    run                                   c                 s  e)
(* Application directive                                           *)
(*                           apply primitive operation             *)
  | run                           (A n :: c)
                             (REAL i :: REAL j :: PRIM p :: s) e
  = run                                   c
                                        (applyprim p j i :: s) e 
(*                           apply closure                         *)
  | run                           (A n :: c)                s  e
  = let val CLOS (e', params, t) = List.nth(s, n)
    in
    run (T t :: 
         (case c of
           (R :: c')     =>               c   (* tail call         *)
         | (E e'' :: c') =>          M :: c   (* e not needed      *)
         | otherwise     =>   M :: E e :: c   (* general case      *)
         ))
                                       (List.drop (s, n + 1))
                       (extend params (rev (List.take (s, n))) e')
    end
  | run c                                                   s  e
  = raise Error ("no pattern applies in run", s) 

val e_init = extend
             [     "+",      "-",      "*",      "/",      "<="]
             [PRIM "+", PRIM "-", PRIM "*", PRIM "/", PRIM "<="]
             empty

fun evaluate t = run  [T t] nil e_init

(*                                                                 *)
(* testing                                                         *)
(*                                                                 *)

val test1 = NUM 10.0
val res1 = evaluate test1

val test2 = LAM (["x", "y"], NUM 10.0)
val res2 = evaluate test2

val test3 = APP (VAR "+", [NUM 4.0, NUM 5.0])
val res3 = evaluate test3

val test4 = APP (test2, [test1, test1]
val res4 = evaluate test4

val test5 = SEQ [NUM 1.0, NUM 2.0, NUM 3.0]
val res5 = evaluate test5

val test6 = CON (APP (VAR "<=", [NUM 9.0, NUM 10.0]),
                 NUM 1.0,
                 NUM 2.0)
val res6 = evaluate test6

val test7 = APP (VAR "-", [NUM 7.0, NUM 4.0])
val res7 = evaluate test7

val test8 = BLK (SEQ [LET ("x", NUM 1.0), VAR "x"])
val res8 = evaluate test8

val test9 = BLK (SEQ (LET ("x", NUM 2.0)
                     ::
                     (BLK (SEQ [LET ("x", NUM 1.0), VAR "x"]))
                     ::
                     VAR "x"
                     ::
                     nil))
val res9 = evaluate test9

val fac = LAM (["n"], RET 
                       (CON 
                        (APP (VAR "<=",
                             [ VAR "n",
                               NUM 1.0 ]),
                        NUM 1.0,
                        APP (VAR "*",
                             [ VAR "n",
                               APP (VAR "factorial",
                                    [ APP (VAR "-",
                                          [ VAR "n",
                                           NUM 1.0])])]))))

val test10 = BLK (SEQ [LET ("factorial", fac),
                       APP (VAR "factorial",
                           [ NUM 4.0])])

val res10 = evaluate test10;
