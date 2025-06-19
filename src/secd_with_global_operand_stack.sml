(* SECD machine with global operand stack *)

load "Int";

datatype term = NUM of int
              | VAR of string
              | LAM of string * term
              | APP of term * term

datatype value = INT of int
               | SUC
	       | CLO of environment * string * term
withtype environment = (string * value) list

datatype directive = T of term
                   | A



val empty = nil
fun extend x v e
  = (x, v) :: e
fun lookup x ((x', v) :: e)
  = if x = x' then v else lookup x e


fun stash_to_string nil
  = ""
  | stash_to_string (INT i :: s)
  = (Int.toString i) ^ ", " ^ (stash_to_string s)
  | stash_to_string (SUC :: s)
  = "SUC, " ^ (stash_to_string s)
  | stash_to_string (CLO (e, x, t) :: s)
  = "CLO, " ^ (stash_to_string s)

fun control_to_string nil
  = ""
  | control_to_string (A :: c)
  = "A, " ^ (control_to_string c)
  | control_to_string (T t :: c)
  = "T, " ^ (control_to_string c)



fun run                 (v :: nil) e                       nil             nil
  = v
  | run                         s  e                       nil (( e', c') :: d)
  = run                         s  e'                        c'              d
  | run                         s  e           (T (NUM n) :: c)              d
  = run               (INT n :: s) e                         c               d
  | run                         s  e           (T (VAR x) :: c)              d
  = run          (lookup x e :: s) e                         c               d
  | run                         s  e      (T (LAM (x, t)) :: c)              d
  = run       (CLO (e, x, t) :: s) e                         c               d
  | run                         s  e    (T (APP (t0, t1)) :: c)              d
  = run                         s  e   (T t0 :: T t1 :: A :: c)              d
  | run        (INT n :: SUC :: s) e                   (A :: c)              d
  = run         (INT (n + 1) :: s) e                         c               d
  | run (v :: CLO (e', x, t) :: s) e                   (A :: c)              d
  = run                         s  (extend x v e') (T t :: nil)   ((e, c) :: d)
  | run                         s  e                         c                d
  = raise Fail ("Unmatched case: stash: " ^ (stash_to_string s) ^ "; control:" ^ (control_to_string c)    )

val e_init = extend "succ" SUC empty

fun evaluate t
  = run                       nil  e_init          (T t :: nil)            nil

val test1 = NUM 10
val res1 = evaluate test1

val test2 = LAM ("x", NUM 10)
val res2 = evaluate test2

val test3 = APP (test2, test1)
val res3 = evaluate test3


val test4 = APP (VAR "succ", NUM 4)

val res4 = evaluate test4

