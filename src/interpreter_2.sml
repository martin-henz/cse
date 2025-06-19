(* recursive interpreter *)

datatype term = NUM of int
              | VAR of string
              | LAM of string * term
              | APP of term * term
              | CAL
              | ENV of environment
and     value = INT of int
              | SUC
              | CLO of environment * string * term
withtype environment = (string * value) list

datatype directive = T of term
                   | A
                   | E of environment

val empty_environment       = nil
fun extend x v e
  = (x, v) :: e
fun lookup x ((x', v) :: e)
  = if x = x' then v else lookup x e

fun eval                    nil                               s  e
  =  s
  | eval           (NUM n :: ts)                              s  e 
  = eval                     ts                     (INT n :: s) e
  | eval           (VAR x :: ts)                              s  e 
  = eval                     ts              ((lookup x e) :: s) e
  | eval      (LAM (x, t) :: ts)                              s  e 
  = eval                     ts           ((CLO (e, x, t)) :: s) e
  | eval  ((APP (t0, t1)) :: ts)                              s  e 
  = eval (t0 :: t1 :: CAL :: ts)                              s  e
  | eval             (CAL :: ts)             (INT n :: SUC :: s) e 
  = eval                     ts               (INT (n + 1) :: s) e
  | eval             (CAL :: ts)    (v :: (CLO (e', x, t)) :: s) e 
  = eval      (t :: ENV e :: ts)                              s  (extend x v e')
  | eval          (ENV e' :: ts)                              s  e 
  = eval                     ts                               s  e'
  
val e_init = extend "succ" SUC empty_environment

fun evaluate t
  = hd (eval [t] nil e_init)

val test1 = NUM 10
val res1 = evaluate test1

val test2 = LAM ("x", NUM 10)
val res2 = evaluate test2

val test3 = APP (test2, test1)
val res3 = evaluate test3

val test4 = APP (VAR "succ", NUM 4)
val res3 = evaluate test4
