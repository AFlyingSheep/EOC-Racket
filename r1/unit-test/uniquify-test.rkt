#lang racket
(require "../utilities.rkt")
(require racket/dict)
(require "../uniquify.rkt")

; Unit tests
; expression
; `apple : #<Var: apple>
; `42 / 42 : #<Int: 42>

; unit test
(define (Unit-test exp) 
  (display (uniquify (Program '() (parse-exp exp))))
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
