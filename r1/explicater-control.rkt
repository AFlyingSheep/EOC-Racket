#lang racket
(require "utilities.rkt")
(require racket/dict)
(provide explicate-tail)

;; explicate-control : Lvar^mon -> Cvar
; Noting that we has already removed complex operator
; So in Prim, the es is only Var or Int!

; explicate-tail: output Return_stmt and Seq
(define (explicate-tail e)
    (match e
    ; if it's atomic, return now!
    [(Var x) (values (Return (Var x)) (Return (Var x)))]
    [(Int n) (values (Return (Int n)) (Return (Int n)))]
    ; in tail, call expl-tail on body and catch the ret-stmt, 
    ; and call expl-assign on rhs
    [(Let x rhs body) 
        ; Get return value
        (define-values (ret-stmt ret) (explicate-tail body))
        ; about rhs
        (define cont-new (explicate-assign rhs x ret))
        (values ret-stmt cont-new)
    ]
    [(Prim op es) (values (Return (Prim op es)) (Return (Prim op es)))]
    [else (error "explicate-tail unhandled case" e)])
)

(define (explicate-assign e x cont)
    (match e
    [(Var xx) (Seq (Assign (Var x) (Var xx)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body) 
        ; the body's value will be assigned to x
        (define new-cont-body (explicate-assign body x cont))
        ; next, we should process rhs which will contain values for y
        (define new-cont (explicate-assign rhs y new-cont-body))
        new-cont
    ]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [else (error "explicate-assign unhandled case" e)]))

(define (explicate-control p)
    (match p
    [(Program info body) (Program info (explicate-tail body))]))


; unit test (without uniquify)
(define (Unit-test exp) 
  (define-values (a b) (explicate-tail (parse-exp exp)))
  (display b)
  (display "\n")
)

; (Unit-test 42)
; (Unit-test `(+ 42 68))
; (Unit-test `(let ([x 10]) x))
; (Unit-test `(let ([y (let ([x.1 20]) (let ([x.2 22]) (+ x.1 x.2)))]) y))
; (Unit-test 
;     `(let ([tmp0 (- 3)])
;         (let ([tmp1 (- 2)])
;             (let ([tmp2 (+ 1 tmp1)])
;                 (let ([tmp3 (+ tmp0 tmp2)])
;                     (let ([tmp4 (+ 1 2)])
;                         (let ([tmp5 (- 1)])
;                             (let ([tmp6 (- tmp4 tmp5)])
;                                 (+ tmp3 tmp6)
;                             )
;                         )
;                     )
;                 )
;             )
;         )
;     )
; )