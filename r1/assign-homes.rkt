#lang racket
(require "utilities.rkt")
(require racket/dict)
(require "select-instructions.rkt")
(require "explicater-control.rkt")
(require "patch-instructions.rkt")
(provide assign-homes) 
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
	;(newline)(newline)(write ins)(newline)(newline)
        (handout-stmt ins symbol-table)
    )
    )
    modified-insts
)

(define (assign-homes p)
  (match p
    [(Program info e) (Program info (assign-homes-impl e (create-homes (dict-ref info 'symbol-table))))]
  )
)

(define (display-ins ins)
  (for/list ([i ins])
    (write i)
  )
)

(define (display-prog p)
  (match p
    [(Program info e) (display-ins e)]
  )
)