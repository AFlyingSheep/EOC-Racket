#lang racket
(require rackunit)

(define cal
    (lambda (exp)
    (match exp
        [(? number? x) x]

        [`(,op ,num1 ,num2)
            (let ([v1 (cal num1)]
                  [v2 (cal num2)])
            (match op
                ['+ (+ v1 v2)]
                ['- (- v1 v2)]
                ['* (* v1 v2)]
                ['/ (/ v1 v2)]
            ))
        ]
        [else #f]
    ))
)


(check-equal? (cal '(* (+ 1 2) (+ 3 4))) 21)
(check-equal? (cal '(* (* (* (* (* 2 2) 2) 2) 2) (* (* (* (* (* 2 2) 2) 2) 2) 2))) 2048)

(display "Pass.\n")