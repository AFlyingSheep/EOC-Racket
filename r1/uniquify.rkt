#lang racket
(require "utilities.rkt")
(require racket/dict)

;;;exp ::= int | (read) | (- exp) | (+ exp exp)
;;;      | var | (let ([var exp]) exp)
;;;R1 ::= (program exp)

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
; (define (test_program body) 
;     (uniquify-exp body (make-hash))
; )
; (display (test_program `(program () 42)))
; (display "\n")

; expression
; `apple : #<Var: apple>
; `42 / 42 : #<Int: 42>

; unit test

(display ((uniquify-exp `()) (parse-exp `(let ([x 32]) (+ (let ([x 10]) x) x))) (make-hash)))
(display "\n")
(display ((uniquify-exp `()) (parse-exp 
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
    ))) (make-hash)))
(display "\n")