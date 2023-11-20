#lang racket
(require "utilities.rkt")
(require racket/dict)

(define gensym
  (let ([counter 0])
    (lambda ([x 'g])
      (if (number? x)
        (set! counter x)
        (begin0 (string->unreadable-symbol
                 (format "~a~a" x counter))
          (set! counter (add1 counter)))))))

; rco-atom: input exp and return Symbol & list
(define (rco-atom exp) 
  (match exp
    ; If exp is an atomic, return itself and empty list
    [(Var x) (values (Var x) '())]
    [(Int n) (values (Int n) '())]
    ; If exp is Let, for rhs, apply rco-exp;
    [(Let x rhs body) 
      (define new-rhs (rco-exp rhs))
      (define-values (new-body body-ss) (rco-atom body))
      (values new-body (append `((,x . ,new-rhs)) body-ss))
    ]
    ; If exp is Prim, call rco-atom on it's es and return new atomic
    ; return: new symbol Var, &list(..., (NSVar . (Prim op new-es)))
    [(Prim op es) 
      (define-values (new-es sss)
        (for/lists (l1 l2) ([e es]) (rco-atom e))
      )
      (define ss (append* sss))
      (define tmp (gensym 'tmp))
      (values (Var tmp) 
        (append ss `((,tmp . ,(Prim op new-es))))
      )
    ]
  )
)

; unpack lists. Because we search atomic deeply, so the order of adding element
; is also the base to the top.
; We only need to construct from the first element to the last.
(define (unpack lists e)
  (match lists
    [`() e]
    [`((,x . ,value) . ,other)
      (Let x value (unpack other e))
    ]
  )
)

(define (rco-exp exp)
  (match exp
    ; If it's atomic, return it.
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    ; If it's Let, apply rco-exp on e and body.
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    ; If it's Prim, we should make sure es is atomic.
    ; We will unpack the symbol and list which created in rco-atom by make-lets
    [(Prim op es)
      (define-values (new-es sss)
        (for/lists (l1 l2) ([e es]) (rco-atom e)))
      ; (display new-es)
      ; (display "\n")
      (unpack (append* sss) (Prim op new-es))
    ]
  )
)

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-opera* exp) 
  (match exp
    [(Program info e) (Program info (rco-exp e))]
  )
)

; unit test (without uniquify)
(define (Unit-test exp) 
  (display (remove-complex-opera* (Program '() (parse-exp exp))))
  (display "\n")
)

(Unit-test '(+ 1 2))
(Unit-test '(+ (- 3) 2))
(Unit-test '(- (+ 2 3)))
(Unit-test '(+ 1 (+ 3 4)))
(Unit-test '(+ (+ 1 2) (+ 3 4)))
(Unit-test '(+ (+ (- 3) (+ 1 (- 2))) (- (+ 1 2) (- 1))))

; . 's test
; (define (my_match exp)
;   (match exp
;     [`() #f]
;     [`(,x . ,right_exp)
;       (display right_exp)
;       (display "\n")
;       (my_match right_exp)
;     ]
;   )
; )

; (define lll (append (list 1 2 3 4)))
; (display "\n")
; (display lll)
; (display "\n")
; (my_match lll)

