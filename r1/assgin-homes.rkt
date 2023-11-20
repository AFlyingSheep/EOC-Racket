#lang racket
(require "utilities.rkt")
(require racket/dict)
(require "select-instructions.rkt")
(require "explicater-control.rkt")

;; assign-homes : x86var -> x86var

(define (create-homes env)
    (define sym-dict (make-hash))
    (define count-value -8)
    (for ([sym env])
        ; (define count (- count 8))
        (dict-set! sym-dict sym (Deref 'rbp count-value))
        (set! count-value (- count-value 8))
    )
    sym-dict
)

(define (exchange-symbol sym sym-dict)
    (match (symbol? sym)
        [#t (dict-ref sym-dict sym)]
        [#f sym]
    )
)

(define (handout-stmt stmt sym-dict)
    (match stmt
        [(Instr op (list arg1 arg2))
            (Instr op (list (exchange-symbol arg1 sym-dict)
                            (exchange-symbol arg2 sym-dict)))
        ]
        [(Instr op (list arg))
            (Instr op (list (exchange-symbol arg sym-dict)))
        ]
        [else stmt]
    )
)

(define (assign-homes-impl instructions symbol-table)
    (define modified-insts
    (for/list ([ins (in-list instructions)])
        (handout-stmt ins symbol-table)
    )
    )
    modified-insts
)

(define (assign-homes p)
  (match p
    [(Program info e) (Program info (assign-homes-impl e (create-homes info)))]
  )
)

(define (Unit-test exp)
  (define eexp (explicate-control (Program '() (parse-exp exp))))
  (display eexp)
  (display "\n")
  (define eeexp (select-instructions eexp))
  (display eeexp)
  (display "\n")
  (define eeeexp (assign-homes eeexp))
  (display eeeexp)
  (display "\n")
)

(Unit-test 42)
(Unit-test `(+ 21 42))
(Unit-test `(- 42))
(Unit-test `(let ([x (read)]) x))
(Unit-test `(let ([y (let ([x.1 20]) (let ([x.2 22]) (+ x.1 x.2)))]) y))
(Unit-test 
    `(let ([tmp0 (- 3)])
        (let ([tmp1 (- 2)])
            (let ([tmp2 (+ 1 tmp1)])
                (let ([tmp3 (+ tmp0 tmp2)])
                    (let ([tmp4 (+ 1 2)])
                        (let ([tmp5 (read)])
                            (let ([tmp6 (- tmp4 tmp5)])
                                (+ tmp3 tmp6)
                            )
                        )
                    )
                )
            )
        )
    )
)
(Unit-test `(let ([y (let ([x1 (- 20)]) (let ([x2 22]) (+ x1 x2)))]) y))