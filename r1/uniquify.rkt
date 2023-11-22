#lang racket
(require "utilities.rkt")
(require racket/dict)

;;;exp ::= int | (read) | (- exp) | (+ exp exp)
;;;      | var | (let ([var exp]) exp)
;;;R1 ::= (program exp)

;; uniquify : Lvar -> Lvar
(define (uniquify-exp env)
  (lambda (e sym-table)
    (match e
      ; if it's a Var, we should loop up the dic,
      ; if it's not appear, ERROR!
      ; else return the x
      [(Var x) (match (dict-has-key? sym-table x) 
        [#f (error "Symbol Not Found!")]
        [#t (Var (dict-ref sym-table x))])]

      [(Int n) (Int n)]

      ; if it's 'Let, we should checkout whether x is in the symbol
      ; if true, we generate a new sysbom name which be pointed by origin symbol
      ; so when we get a Var, we can check it's changed name.
      [(Let x e body) (match (dict-has-key? sym-table x)
        [#f 
          (dict-set! sym-table x x)
          (let
            ([ret-value 
                (Let (dict-ref sym-table x) 
                     ((uniquify-exp env) e sym-table) 
                     ((uniquify-exp env) body sym-table)
                )
            ])
            (dict-remove! sym-table x)
            ret-value)
          
        ]
        [#t 
          (let*
            ( [old-x (dict-ref sym-table x)]
              [new-x (gensym old-x)]
              [ret-value
                (Let new-x
                  ((uniquify-exp env) e (dict-set-ret sym-table x new-x))
                  ((uniquify-exp env) body (dict-set-ret sym-table x new-x))
                )
              ]
            )
            (dict-set! sym-table x old-x)
            ret-value)
        ]
                
                )]

      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e sym-table)))])))

(define (dict-set-ret dict x y) (dict-set! dict x y) dict)

(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e (make-hash)))]))

; expression
; `apple : #<Var: apple>
; `42 / 42 : #<Int: 42>

; unit test
(define (Unit-test exp) 
  (display (uniquify (Program '() ((uniquify-exp `()) (parse-exp exp) (make-hash)))))
  (display "\n")
)

(Unit-test 42)
; (Unit-test `apple)  ; error!
(Unit-test `(+ 42 68))
(Unit-test `(let ([x 32]) (+ (let ([x 10]) x) x)))
(Unit-test 
  `(let ([x 32]) 
   (+ 
     (let ([x 10]) 
     (+ 
      (+
        (let ([x 5]) x) 
        (let ([x 6]) x)
      )
     x)) 
    x
    ))
)
