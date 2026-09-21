#lang racket

;; ============================================================
;; CS 441 - Fall 2026
;; Program 1 - The Expression Evaluator
;; ============================================================

(provide eval-expr eval-step)

;; ------------------------------------------------------------
;; Three-valued logic (Kleene)
;; ------------------------------------------------------------

(define (three-value? value)
  (or (equal? value 'yes)
      (equal? value 'no)
      (equal? value 'maybe)))

(define (three-not value)
  (cond
    [(equal? value 'yes) 'no]
    [(equal? value 'no) 'yes]
    [else 'maybe]))          ; 'maybe or a type mismatch

(define (three-and left right)
  (cond
    ;; 'no dominates AND
    [(or (equal? left 'no) (equal? right 'no)) 'no]
    ;; otherwise any 'maybe makes the result unknown
    [(or (equal? left 'maybe) (equal? right 'maybe)) 'maybe]
    [else 'yes]))            ; both are 'yes

(define (three-or left right)
  (cond
    ;; 'yes dominates OR
    [(or (equal? left 'yes) (equal? right 'yes)) 'yes]
    ;; otherwise any 'maybe makes the result unknown
    [(or (equal? left 'maybe) (equal? right 'maybe)) 'maybe]
    [else 'no]))             ; both are 'no

;; ------------------------------------------------------------
;; AST helper
;; ------------------------------------------------------------

(define (lit value)
  `(lit ,value))

;; ------------------------------------------------------------
;; Apply a binary operation to already-evaluated values.
;; Returns an AST literal.  Anything that doesn't fit (type
;; mismatch, divide by zero, unknown operator) becomes 'maybe.
;; ------------------------------------------------------------

(define (apply-binary-op op left right)
  (cond

    ;; Arithmetic
    [(member op '("+" "-" "*"))
     (if (and (number? left) (number? right))
         (lit (cond
                [(equal? op "+") (+ left right)]
                [(equal? op "-") (- left right)]
                [else            (* left right)]))
         (lit 'maybe))]

    [(equal? op "/")
     (if (and (number? left) (number? right) (not (zero? right)))
         (lit (/ left right))
         (lit 'maybe))]

    ;; String concatenation
    [(equal? op "~")
     (if (and (string? left) (string? right))
         (lit (string-append left right))
         (lit 'maybe))]

    ;; Equality / inequality (numbers or strings only; anything
    ;; else, including 'maybe, is a type mismatch -> 'maybe)
    [(equal? op "==")
     (cond
       [(and (number? left) (number? right))
        (lit (if (= left right) 'yes 'no))]
       [(and (string? left) (string? right))
        (lit (if (string=? left right) 'yes 'no))]
       [else (lit 'maybe)])]

    [(equal? op "/=")
     (cond
       [(and (number? left) (number? right))
        (lit (if (= left right) 'no 'yes))]
       [(and (string? left) (string? right))
        (lit (if (string=? left right) 'no 'yes))]
       [else (lit 'maybe)])]

    ;; Relational comparisons
    [(member op '("<" "<=" ">" ">="))
     (cond
       [(and (number? left) (number? right))
        (lit (if (cond
                   [(equal? op "<")  (< left right)]
                   [(equal? op "<=") (<= left right)]
                   [(equal? op ">")  (> left right)]
                   [else             (>= left right)])
                 'yes 'no))]
       [(and (string? left) (string? right))
        (lit (if (cond
                   [(equal? op "<")  (string<? left right)]
                   [(equal? op "<=") (string<=? left right)]
                   [(equal? op ">")  (string>? left right)]
                   [else             (string>=? left right)])
                 'yes 'no))]
       [else (lit 'maybe)])]

    ;; Three-valued AND / OR
    [(equal? op "and")
     (if (and (three-value? left) (three-value? right))
         (lit (three-and left right))
         (lit 'maybe))]

    [(equal? op "or")
     (if (and (three-value? left) (three-value? right))
         (lit (three-or left right))
         (lit 'maybe))]

    ;; Unknown operator
    [else (lit 'maybe)]))

;; ------------------------------------------------------------
;; Algebraic simplification (happens before children are folded)
;;
;;   x + 0 -> x      0 + x -> x      x - 0 -> x     x - x -> 0
;;   x * 1 -> x      1 * x -> x      x * 0 -> 0     0 * x -> 0
;;
;; A rule only fires if x evaluates to a number or to 'maybe
;; (an unknown).  If x is a string or a yes/no, the operation is a
;; type mismatch, so we skip the rule and let apply-binary-op
;; turn it into 'maybe.
;; ------------------------------------------------------------

(define (numeric-or-unknown? expr env)
  (match (eval-expr expr env)
    [`(lit ,value) (or (number? value) (equal? value 'maybe))]
    [_ #f]))

(define (simplify-binary-op op left right env)
  (define (ok? expr) (numeric-or-unknown? expr env))
  (cond
    [(and (equal? op "+") (equal? right '(lit 0)) (ok? left))  left]
    [(and (equal? op "+") (equal? left '(lit 0))  (ok? right)) right]
    [(and (equal? op "-") (equal? right '(lit 0)) (ok? left))  left]
    [(and (equal? op "-") (equal? left right)     (ok? left))  '(lit 0)]
    [(and (equal? op "*") (equal? right '(lit 1)) (ok? left))  left]
    [(and (equal? op "*") (equal? left '(lit 1))  (ok? right)) right]
    [(and (equal? op "*") (equal? right '(lit 0)) (ok? left))  '(lit 0)]
    [(and (equal? op "*") (equal? left '(lit 0))  (ok? right)) '(lit 0)]
    [else #f]))

;; ------------------------------------------------------------
;; Evaluate one step of an expression.  The result is still an
;; AST; eval-expr repeats this until a fixed point is reached.
;; ------------------------------------------------------------

(define (eval-step expr env)
  (match expr

    ;; Literal: already as simple as it gets
    [`(lit ,value) expr]

    ;; Variable: replace with its value, or 'maybe if unbound
    [`(var ,id)
     (if (hash-has-key? env id)
         (lit (hash-ref env id))
         (lit 'maybe))]

    ;; NOT
    [`(not ,subexpr)
     (match (eval-expr subexpr env)
       [`(lit ,value) (lit (three-not value))]
       [_             (lit 'maybe)])]

    ;; Binary operation
    [`(binary-op ,op ,left ,right)
     (let ([simplified (simplify-binary-op op left right env)])
       (if simplified
           simplified
           (match* ((eval-expr left env) (eval-expr right env))
             [(`(lit ,left-value) `(lit ,right-value))
              (apply-binary-op op left-value right-value)]
             [(_ _) (lit 'maybe)])))]

    ;; Invalid AST: degrade instead of crashing (eval-expr is total)
    [_ (lit 'maybe)]))

;; ------------------------------------------------------------
;; Main evaluator: keep stepping until nothing changes.
;; ------------------------------------------------------------

(define (eval-expr expr env)
  (let ([result (eval-step expr env)])
    (if (equal? result expr)
        result
        (eval-expr result env))))

;; ------------------------------------------------------------
;; Tests (run with: raco test program1.rkt)
;; ------------------------------------------------------------

(module+ test
  (require rackunit)

  (define empty-env (hash))

  ;; Example A: divide by zero / unbound variables
  (check-equal?
   (eval-expr '(binary-op "/"
                          (binary-op "+" (var "x") (lit 10))
                          (binary-op "*" (var "y") (lit 0)))
              empty-env)
   '(lit maybe))

  ;; Deep identities
  (check-equal?
   (eval-expr '(binary-op "*" (lit 1) (binary-op "+" (var "x") (lit 0)))
              (hash "x" 5))
   '(lit 5))
  (check-equal? (eval-expr '(binary-op "-" (var "x") (var "x")) empty-env)
                '(lit 0))

  ;; Constant folding
  (check-equal? (eval-expr '(binary-op "+" (lit 2) (lit 3)) empty-env)
                '(lit 5))

  ;; Type mismatches
  (check-equal? (eval-expr '(binary-op "+" (lit 5) (lit "a")) empty-env)
                '(lit maybe))
  (check-equal? (eval-expr '(binary-op "+" (lit "a") (lit 0)) empty-env)
                '(lit maybe))
  (check-equal? (eval-expr '(binary-op "and" (lit 1) (lit 2)) empty-env)
                '(lit maybe))

  ;; Truth table
  (check-equal? (eval-expr '(binary-op "and" (lit yes) (lit maybe)) empty-env) '(lit maybe))
  (check-equal? (eval-expr '(binary-op "and" (lit no)  (lit maybe)) empty-env) '(lit no))
  (check-equal? (eval-expr '(binary-op "and" (lit maybe) (lit maybe)) empty-env) '(lit maybe))
  (check-equal? (eval-expr '(binary-op "or" (lit yes) (lit maybe)) empty-env) '(lit yes))
  (check-equal? (eval-expr '(binary-op "or" (lit no)  (lit maybe)) empty-env) '(lit maybe))
  (check-equal? (eval-expr '(binary-op "or" (lit maybe) (lit maybe)) empty-env) '(lit maybe))
  (check-equal? (eval-expr '(not (lit maybe)) empty-env) '(lit maybe))

  ;; Example B
  (check-equal?
   (eval-expr '(binary-op "and"
                          (binary-op ">=" (var "score") (lit 75.0))
                          (not (var "flag")))
              (hash "score" 80.0 "flag" 'no))
   '(lit yes)))
