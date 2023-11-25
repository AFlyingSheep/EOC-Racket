#lang racket
(require "../utilities.rkt")
(require "../remove-complex-opera.rkt")

; unit test (without uniquify)
(define (Unit-test exp) 
  (display (remove-complex-opera (Program '() (parse-exp exp))))
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

