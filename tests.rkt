#lang racket

;; ============================================================
;; CS 441 - Fall 2026 - Program 1 tests
;; Run with:  raco test tests.rkt      (program1.rkt in same folder)
;; ============================================================

(require rackunit
         "program1.rkt")

;; check-eval: evaluate expr in env and compare to expected AST
(define (check-eval msg expr env expected)
  (check-equal? (eval-expr expr env) expected msg))

;; check-num: like check-eval for a numeric result, compared with =
;; so 5/2 and 2.5 are both accepted for a division test
(define (check-num msg expr env expected)
  (match (eval-expr expr env)
    [`(lit ,value)
     (check-true (and (number? value) (= value expected)) msg)]
    [other (fail (format "~a: expected a literal, got ~a" msg other))]))

(define empty-env (hash))

;; ============================================================
;; PROFESSOR'S TESTS
;; ============================================================

;; ---- Identities -------------------------------------------

(check-eval "x + 0, x=42"
            '(binary-op "+" (var "x") (lit 0)) #hash(("x" . 42))
            '(lit 42))

(check-eval "x + 0, x unbound"
            '(binary-op "+" (var "x") (lit 0)) #hash()
            '(lit maybe))

(check-eval "y - 0, y=-15.5"
            '(binary-op "-" (var "y") (lit 0)) #hash(("y" . -15.5))
            '(lit -15.5))

(check-eval "(10/0) - 0: div by zero -> maybe"
            '(binary-op "-" (binary-op "/" (lit 10) (lit 0)) (lit 0)) #hash()
            '(lit maybe))

(check-eval "z - z -> 0"
            '(binary-op "-" (var "z") (var "z")) #hash(("z" . 99))
            '(lit 0))

(check-eval "yes - yes: booleans don't subtract"
            '(binary-op "-" (lit yes) (lit yes)) #hash()
            '(lit maybe))

(check-eval "score * 1, score=88.7"
            '(binary-op "*" (var "score") (lit 1)) #hash(("score" . 88.7))
            '(lit 88.7))

(check-eval "maybe * 1: can't multiply a boolean-ish value"
            '(binary-op "*" (lit maybe) (lit 1)) #hash()
            '(lit maybe))

(check-eval "total * 0 -> 0"
            '(binary-op "*" (var "total") (lit 0)) #hash(("total" . 500))
            '(lit 0))

(check-eval "6A - 3A with A=10"
            '(binary-op "-"
                        (binary-op "*" (lit 6) (var "A"))
                        (binary-op "*" (lit 3) (var "A")))
            #hash(("A" . 10))
            '(lit 30))

(check-eval "6A - 3A with A unbound"
            '(binary-op "-"
                        (binary-op "*" (lit 6) (var "A"))
                        (binary-op "*" (lit 3) (var "A")))
            #hash()
            '(lit maybe))

;; ---- Boolean operations, no maybe -------------------------

(check-eval "yes and no" '(binary-op "and" (lit yes) (lit no)) #hash() '(lit no))
(check-eval "yes or no"  '(binary-op "or" (lit yes) (lit no)) #hash() '(lit yes))
(check-eval "not no"     '(not (lit no)) #hash() '(lit yes))

;; ---- Boolean operations with maybe ------------------------

(check-eval "no and maybe"
            '(binary-op "and" (lit no) (lit maybe)) #hash() '(lit no))

(check-eval "yes or maybe"
            '(binary-op "or" (lit yes) (lit maybe)) #hash() '(lit yes))

(check-eval "maybe and yes stays maybe (not 'no)"
            '(binary-op "and" (lit maybe) (lit yes)) #hash() '(lit maybe))

(check-eval "unbound_flag and no -> no"
            '(binary-op "and" (var "unbound_flag") (lit no)) #hash()
            '(lit no))

(check-eval "(5 + \"hello\") or yes -> yes"
            '(binary-op "or"
                        (binary-op "+" (lit 5) (lit "hello"))
                        (lit yes))
            #hash()
            '(lit yes))

(check-eval "not (10/0) -> maybe"
            '(not (binary-op "/" (lit 10) (lit 0))) #hash()
            '(lit maybe))

;; ---- Relational -------------------------------------------

(check-eval "x < 100, x=50"
            '(binary-op "<" (var "x") (lit 100)) #hash(("x" . 50))
            '(lit yes))

(check-eval "50 == \"fifty\": type mismatch"
            '(binary-op "==" (lit 50) (lit "fifty")) #hash()
            '(lit maybe))

(check-eval "(y > 10) or yes, y unbound -> yes"
            '(binary-op "or"
                        (binary-op ">" (var "y") (lit 10))
                        (lit yes))
            #hash()
            '(lit yes))

;; ---- Arithmetic / strings (last group, typos fixed) -------

(check-eval "(20 / (8 - 4)) + 5 = 10"
            '(binary-op "+"
                        (binary-op "/" (lit 20) (binary-op "-" (lit 8) (lit 4)))
                        (lit 5))
            #hash()
            '(lit 10))

(check-eval "X + Y with X unbound -> maybe"
            '(binary-op "+" (var "X") (var "Y")) (hash "Y" 10)
            '(lit maybe))

(check-eval "string concatenation, nested"
            '(binary-op "~" (lit "this ")
                        (binary-op "~" (lit "and ") (lit "that")))
            #hash()
            '(lit "this and that"))

(check-eval "\"number \" ~ 5 -> maybe"
            '(binary-op "~" (lit "number ") (lit 5)) #hash()
            '(lit maybe))

(check-eval "\"number \" + 5 -> maybe"
            '(binary-op "+" (lit "number ") (lit 5)) #hash()
            '(lit maybe))

;; ============================================================
;; EXTRA TESTS
;; ============================================================

;; ---- Literals and variables -------------------------------

(check-eval "number literal" '(lit 5) empty-env '(lit 5))
(check-eval "string literal" '(lit "hi") empty-env '(lit "hi"))
(check-eval "maybe literal"  '(lit maybe) empty-env '(lit maybe))
(check-eval "bound variable" '(var "a") (hash "a" 7) '(lit 7))
(check-eval "unbound variable" '(var "a") empty-env '(lit maybe))
(check-eval "variable bound to a boolean"
            '(var "flag") (hash "flag" 'yes) '(lit yes))

;; ---- Full 3VL truth tables --------------------------------

(for* ([left '(yes no maybe)]
       [right '(yes no maybe)])
  (define expected-and
    (cond [(or (eq? left 'no) (eq? right 'no)) 'no]
          [(or (eq? left 'maybe) (eq? right 'maybe)) 'maybe]
          [else 'yes]))
  (define expected-or
    (cond [(or (eq? left 'yes) (eq? right 'yes)) 'yes]
          [(or (eq? left 'maybe) (eq? right 'maybe)) 'maybe]
          [else 'no]))
  (check-eval (format "~a and ~a" left right)
              `(binary-op "and" (lit ,left) (lit ,right)) empty-env
              `(lit ,expected-and))
  (check-eval (format "~a or ~a" left right)
              `(binary-op "or" (lit ,left) (lit ,right)) empty-env
              `(lit ,expected-or)))

(check-eval "not yes"   '(not (lit yes)) empty-env '(lit no))
(check-eval "not maybe" '(not (lit maybe)) empty-env '(lit maybe))
(check-eval "not of a number is a type mismatch"
            '(not (lit 5)) empty-env '(lit maybe))
(check-eval "and on two reals is a type mismatch"
            '(binary-op "and" (lit 1.5) (lit 2.5)) empty-env '(lit maybe))
(check-eval "or on two strings is a type mismatch"
            '(binary-op "or" (lit "a") (lit "b")) empty-env '(lit maybe))

;; ---- Arithmetic and constant folding ----------------------

(check-eval "constant folding across branches: (2+3)*(4-1)"
            '(binary-op "*"
                        (binary-op "+" (lit 2) (lit 3))
                        (binary-op "-" (lit 4) (lit 1)))
            empty-env '(lit 15))

(check-num "10 / 4 (exact 5/2 or 2.5)"
           '(binary-op "/" (lit 10) (lit 4)) empty-env 5/2)

(check-eval "divide by zero"
            '(binary-op "/" (lit 5) (lit 0)) empty-env '(lit maybe))
(check-eval "divide by 0.0"
            '(binary-op "/" (lit 5) (lit 0.0)) empty-env '(lit maybe))
(check-eval "maybe propagates through arithmetic"
            '(binary-op "+" (binary-op "/" (lit 1) (lit 0)) (lit 3))
            empty-env '(lit maybe))

;; Example A from the assignment
(check-eval "Example A: (x + 10) / (y * 0)"
            '(binary-op "/"
                        (binary-op "+" (var "x") (lit 10))
                        (binary-op "*" (var "y") (lit 0)))
            empty-env '(lit maybe))

;; Example C from the assignment
(check-eval "Example C: 6A - 3A, A=2"
            '(binary-op "-"
                        (binary-op "*" (lit 6) (var "A"))
                        (binary-op "*" (lit 3) (var "A")))
            (hash "A" 2) '(lit 6))

;; ---- Identities: the rest of the rule list ----------------

(check-eval "0 + x"  '(binary-op "+" (lit 0) (var "x")) (hash "x" 8) '(lit 8))
(check-eval "1 * x"  '(binary-op "*" (lit 1) (var "x")) (hash "x" 8) '(lit 8))
(check-eval "0 * x, x unbound"
            '(binary-op "*" (lit 0) (var "x")) empty-env '(lit 0))
(check-eval "x - x, x unbound"
            '(binary-op "-" (var "x") (var "x")) empty-env '(lit 0))
(check-eval "0 + x, x unbound"
            '(binary-op "+" (lit 0) (var "x")) empty-env '(lit maybe))

;; Deep simplification: identities nested inside identities
(check-eval "1 * (x + 0), x=5"
            '(binary-op "*" (lit 1) (binary-op "+" (var "x") (lit 0)))
            (hash "x" 5) '(lit 5))
(check-eval "1 * (x + 0), x unbound"
            '(binary-op "*" (lit 1) (binary-op "+" (var "x") (lit 0)))
            empty-env '(lit maybe))
(check-eval "(1 * (x + 0)) + 0, x=5"
            '(binary-op "+"
                        (binary-op "*" (lit 1)
                                   (binary-op "+" (var "x") (lit 0)))
                        (lit 0))
            (hash "x" 5) '(lit 5))

;; Identities must not hide type mismatches
(check-eval "\"a\" + 0 is a type mismatch"
            '(binary-op "+" (lit "a") (lit 0)) empty-env '(lit maybe))
(check-eval "\"a\" * 0 is a type mismatch"
            '(binary-op "*" (lit "a") (lit 0)) empty-env '(lit maybe))
(check-eval "\"a\" - \"a\" is a type mismatch"
            '(binary-op "-" (lit "a") (lit "a")) empty-env '(lit maybe))
(check-eval "x * 1 where x is a string"
            '(binary-op "*" (var "x") (lit 1)) (hash "x" "abc") '(lit maybe))

;; ---- Strings and comparisons ------------------------------

(check-eval "concat two strings"
            '(binary-op "~" (lit "a") (lit "b")) empty-env '(lit "ab"))
(check-eval "3 == 3.0"
            '(binary-op "==" (lit 3) (lit 3.0)) empty-env '(lit yes))
(check-eval "3 /= 4"
            '(binary-op "/=" (lit 3) (lit 4)) empty-env '(lit yes))
(check-eval "\"a\" == \"a\""
            '(binary-op "==" (lit "a") (lit "a")) empty-env '(lit yes))
(check-eval "\"a\" /= \"a\""
            '(binary-op "/=" (lit "a") (lit "a")) empty-env '(lit no))
(check-eval "\"apple\" < \"banana\""
            '(binary-op "<" (lit "apple") (lit "banana")) empty-env '(lit yes))
(check-eval "\"b\" <= \"a\""
            '(binary-op "<=" (lit "b") (lit "a")) empty-env '(lit no))
(check-eval "5 >= 5"
            '(binary-op ">=" (lit 5) (lit 5)) empty-env '(lit yes))
(check-eval "5 > 5"
            '(binary-op ">" (lit 5) (lit 5)) empty-env '(lit no))
(check-eval "number < string is a type mismatch"
            '(binary-op "<" (lit 5) (lit "a")) empty-env '(lit maybe))
(check-eval "maybe == maybe is unknown, not yes"
            '(binary-op "==" (lit maybe) (lit maybe)) empty-env '(lit maybe))
(check-eval "unbound == unbound is unknown"
            '(binary-op "==" (var "p") (var "q")) empty-env '(lit maybe))

;; Example B from the assignment (all four cases)
(define example-b
  '(binary-op "and"
              (binary-op ">=" (var "score") (lit 75.0))
              (not (var "flag"))))
(check-eval "Example B: 80.0, flag no"
            example-b (hash "score" 80.0 "flag" 'no) '(lit yes))
(check-eval "Example B: 70.0, flag no"
            example-b (hash "score" 70.0 "flag" 'no) '(lit no))
(check-eval "Example B: score unbound, flag yes -> no dominates"
            example-b (hash "flag" 'yes) '(lit no))
(check-eval "Example B: score unbound, flag no -> maybe"
            example-b (hash "flag" 'no) '(lit maybe))

;; ---- Totality: bad input never crashes --------------------

(check-eval "unknown operator"
            '(binary-op "^" (lit 2) (lit 3)) empty-env '(lit maybe))
(check-eval "unknown node type"
            '(foo 1 2) empty-env '(lit maybe))
(check-eval "wrong number of operands"
            '(binary-op "+" (lit 1)) empty-env '(lit maybe))

;; ---- Fixed point: evaluating a result again changes nothing -

(define fixed-point-cases
  (list (cons '(binary-op "*" (lit 1) (binary-op "+" (var "x") (lit 0)))
              (hash "x" 5))
        (cons '(binary-op "-" (var "z") (var "z")) empty-env)
        (cons '(binary-op "and" (var "u") (lit no)) empty-env)
        (cons '(not (binary-op "/" (lit 10) (lit 0))) empty-env)
        (cons '(binary-op "~" (lit "a") (lit 5)) empty-env)))

(for ([test-case fixed-point-cases])
  (define result (eval-expr (car test-case) (cdr test-case)))
  (check-equal? (eval-step result (cdr test-case)) result
                (format "fixed point for ~a" (car test-case)))
  (check-equal? (eval-expr result (cdr test-case)) result))
