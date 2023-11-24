#lang racket
(require "utilities.rkt")
(require racket/dict)
(provide explicate-control)

;; explicate-control : Lvar^mon -> Cvar
; Noting that we has already removed complex operator
; So in Prim, the es is only Var or Int!

; for symbol table, only append symbol when Let

; explicate-tail: output Return_stmt and Seq
(define (explicate-tail e symbol-table)
    (match e
    ; if it's atomic, return now!
    [(Var x) 
        (values (Return (Var x)) symbol-table)
    ]
    [(Int n) (values (Return (Int n)) symbol-table)]
    ; in tail, call expl-tail on body and catch the ret-stmt, 
    ; and call expl-assign on rhs
    [(Let x rhs body) 
        (define new-table.1 (append (list x) symbol-table))
        ; Get return value
        (define-values (ret new-table.2) (explicate-tail body new-table.1))
        ; about rhs
        (explicate-assign rhs x ret new-table.2)
    ]
    [(Prim op es) (values (Return (Prim op es)) symbol-table)]
    [else (error "explicate-tail unhandled case" e)])
)

(define (explicate-assign e x cont symbol-table)
    (match e
    [(Var xx) (values (Seq (Assign (Var x) (Var xx)) cont) symbol-table)]
    [(Int n) (values (Seq (Assign (Var x) (Int n)) cont) symbol-table)]
    [(Let y rhs body) 
        (define new-table (append (list y) symbol-table))
        ; the body's value will be assigned to x
        (define-values (new-cont-body new-table.1) (explicate-assign body x cont new-table))
        ; next, we should process rhs which will contain values for y
        (define-values (new-cont new-table.2) (explicate-assign rhs y new-cont-body new-table.1))
        (values new-cont new-table.2)
    ]
    [(Prim op es) (values (Seq (Assign (Var x) (Prim op es)) cont) symbol-table)]
    [else (error "explicate-assign unhandled case" e)]))

(define (explicate-control p)
    (match p
    [(Program info body) 
        (define-values (exp table) (explicate-tail body '()))
        (dict-set! info 'symbol-table table)
	(dict-set! info 'symbol-num (length table))
	(Program info exp)
    ]))

; unit test (without uniquify)
(define (Unit-test exp) 
  (display (explicate-control (Program (make-hash) (parse-exp exp))))
  (display "\n")
)

; (display (explicate-tail (parse-exp `(let ([m 3]) m))))

; (Unit-test 42)
; (Unit-test `(+ 42 68))
 (Unit-test `(let ([x 10]) x))
 (Unit-test `(let ([y (let ([x.1 20]) (let ([x.2 22]) (+ x.1 x.2)))]) y))
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
