#lang racket
(struct Int (value))
(struct Prim (op args))
(struct Program (info body))


(define eight (Int 8))
(define neg-eight (Prim '- (list eight)))

(define rd (Prim '- '()))

(define (leaf arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
  )
)

(define (is_exp ast)
 (match ast
  [(Int n) #t]
  [(Prim 'read '()) #t]
  [(Prim '- (list e)) (is_exp e)]
  [(Prim '+ (list e1 e2))
   (and (is_exp e1) (is_exp e2))]
  [(Prim '- (list e1 e2))
   (and (is_exp e1) (is_exp e2))]
  [else #f]
  )
 )

(define (is_Lint ast)
  (match ast
    [(Program '() e) (is_exp e)]
    [else #f]
  )
)

(is_Lint (Program '() (Prim '+ (list (Int 100) (Int 32)))))

(displayln rd)

