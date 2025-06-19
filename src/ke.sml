(* CSE machine *)

(* Variant of the CEK machine where C is folded into K *)

datatype term = NUM of int
              | VAR of string
              | LAM of string * term
              | APP of term * term

type 'a environment = (string * 'a) list

datatype value = INT of int
               | SUC
               | CLO of value environment * string * term

val empty_environment       = nil
fun extend x v e            = (x, v) :: e
fun lookup x ((x', v) :: e) = if x = x' then v else lookup x e

datatype directive = T of term
                   | V of value
                   | A  (* APP turns into A *)
                   | F  (* A turns into F   *)
                   | E of value environment

fun run                                  (T (VAR x) :: nil) e
  = lookup x e
  | run                             (T (LAM (x, t)) :: nil) e
  = CLO (e, x, t)
  | run                                  (V (INT n) :: nil) e
  = INT n
  | run                (T (VAR x) :: E e' :: T t :: A :: k) e
  = run                 (T t :: (V (lookup x e)) :: F :: k) e'
  | run          (T (LAM (x, t')) :: E e' :: T t :: A :: k) e
  = run             (T t :: (V (CLO (e, x, t'))) :: F :: k) e'
  | run                             (T (APP (t0, t1)) :: k) e
  = run                     (T t0 :: E e :: T t1 :: A :: k) e
  | run                                    (T (NUM n) :: k) e
  = run                                    (V (INT n) :: k) e
  | run       (T (VAR x) :: (V (CLO (e', y, t))) :: F :: k) e
  = run                                          (T t :: k) (extend y (lookup x e) e')
  | run (T (LAM (x, t')) :: (V (CLO (e', y, t))) :: F :: k) e
  = run                                          (T t :: k) (extend y (CLO (e, x, t')) e')
  | run       (V (INT n) :: (V (CLO (e', x, t))) :: F :: k) e
  = run                                          (T t :: k) (extend x (INT n) e')
  | run                    (V (INT n) :: (V SUC) :: F :: k) e
  = run                              (V (INT (n + 1)) :: k) e

val e_init = extend "succ" SUC empty_environment

fun evaluate t
  = run                                        (T t :: nil) e_init

val test1 = NUM 10
val res1 = evaluate test1

val test2 = LAM ("x", NUM 10)
val res2 = evaluate test2

val test3 = APP (test2, test1)
val res3 = evaluate test3

val test4 = APP (VAR "succ", NUM 4)
val res3 = evaluate test4
