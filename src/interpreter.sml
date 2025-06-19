(* recursive interpreter *)

datatype term = NUM of int
              | VAR of string
              | LAM of string * term
              | APP of term * term

datatype value = INT of int
               | SUC
               | CLO of environment * string * term
withtype environment = (string * value) list

val empty_environment       = nil
fun extend x v e
  = (x, v) :: e
fun lookup x ((x', v) :: e)
  = if x = x' then v else lookup x e

fun apply SUC (INT n)
  = INT (n + 1)
  | apply (CLO (e, x, t)) v
  = eval t (extend x v e)

and eval (NUM n) e 
  = INT n
  | eval (VAR x) e 
  = lookup x e
  | eval (LAM (x, t))  e 
  = CLO (e, x, t)
  | eval (APP (t0, t1)) e 
  = apply (eval t0 e) (eval t1 e)

val e_init = extend "succ" SUC empty_environment

fun evaluate t
  = eval t e_init

val test1 = NUM 10
val res1 = evaluate test1

val test2 = LAM ("x", NUM 10)
val res2 = evaluate test2

val test3 = APP (test2, test1)
val res3 = evaluate test3

val test4 = APP (VAR "succ", NUM 4)
val res3 = evaluate test4
