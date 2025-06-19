(* CSE machine *)

datatype term = NUM of int
              | VAR of string
              | LAM of string * term
              | APP of term * term

datatype value = INT of int
               | SUC
               | CLO of environment * string * term
withtype environment = (string * value) list

datatype directive = T of term (* evaluate term in current env *)
                   | A (* apply function to arg, both on stash *)
                   | E of environment (* restore env           *)

val empty_environment       = nil
fun extend x v e
  = (x, v) :: e
fun lookup x ((x', v) :: e)
  = if x = x' then v else lookup x e

fun run                     nil                  (v :: nil) e 
  = v
  | run         (T (VAR x) :: c)                         s  e 
  = run                       c           (lookup x e :: s) e
  | run    (T (LAM (x, t)) :: c)                         s  e 
  = run                       c        (CLO (e, x, t) :: s) e
  | run  (T (APP (t0, t1)) :: c)                         s  e 
  = run (T t0 :: T t1 :: A :: c)                         s  e 
  | run              (E e' :: c)                         s  e 
  = run                       c                          s  e'
  | run                 (A :: c) (v :: CLO (e', x, t) :: s) e 
  = run        (T t :: E e :: c)                         s  (extend x v e') 

  | run         (T (NUM n) :: c)                         s  e 
  = run                       c                (INT n :: s) e
  | run                 (A :: c)        (INT n :: SUC :: s) e 
  = run                       c          (INT (n + 1) :: s) e 

val e_init = extend "succ" SUC empty_environment

fun evaluate t
  = run             (T t :: nil)                       nil  e_init

val test1 = NUM 10
val res1 = evaluate test1

val test2 = LAM ("x", NUM 10)
val res2 = evaluate test2

val test3 = APP (test2, test1)
val res3 = evaluate test3

val test4 = APP (VAR "succ", NUM 4)
val res3 = evaluate test4


(* val test5 = (evaluate (APP (APP (LAM ("x", LAM ("y", VAR "y")), NUM 0), VAR "x"))); *)


