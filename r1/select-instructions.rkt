#lang racket
(require "utilities.rkt")
(require racket/dict)
(require "explicater-control.rkt")
(provide select-instructions)

;; select-instructions : Cvar -> x86var

(define (atom-to-x86 atom) 
    (match atom
        [(Var n) n]
        [(Int n) (Imm n)]
        [else (error "Not a atom!")]
    )
)

(define (normal-assign lhs rhs)
    (match rhs
    [(Prim '+ (list a1 a2))
        (list (Instr 'movq (list (atom-to-x86 a1) lhs))
              (Instr 'addq (list (atom-to-x86 a2) lhs))
        )
    ]
    [(Prim '- (list a1 a2))
        (list (Instr 'movq (list (atom-to-x86 a1) lhs))
              (Instr 'subq (list (atom-to-x86 a2) lhs))
        )
    ]
    [(Prim '- (list a))
        (list (Instr 'movq (list (atom-to-x86 a) lhs))
              (Instr 'negq (list lhs))
        )
    ] 
    [(Prim 'read '())
        (list (Callq 'read_int 0) (Instr 'movq (list (Reg 'rax) lhs)))
    ]
    )
)

(define (stmt-to-x86 stmt)
    (match stmt
        [(Assign (Var lhs) rhs)
            (match rhs
                [(Int n) (list (Instr 'movq (list (Imm n) lhs)))]
                [(Var v) (list (Instr 'movq (list v lhs)))]
                ; if rhs is a prim, we should check it has the same symbol on '+
                [(Prim '+ es)
                    (match es
                        [(list (Var v1) (Int i)) #:when (equal? v1 lhs)
                            (list (Instr 'addq (list(Imm i lhs))))
                        ]
                        [(list (Int i) (Var v1)) #:when (equal? v1 lhs)
                            (list (Instr 'addq (list(Imm i lhs))))
                        ]
                        [else (normal-assign lhs rhs)]
                    )
                ]
                [else (normal-assign lhs rhs)]
            )
        ]
        [(Assign (Reg x) rhs)
            (normal-assign (Reg x) rhs)
        ]
    )
)

(define (select-instructions* p i-list)

  (match p
    ; If it's Seq, handout current element and also return Seq
    [(Seq s tail)
        ; handout the s
        (define new-list (append i-list (stmt-to-x86 s)))
        (select-instructions* tail new-list)
    ]
    [(Return e)
        (match e
            [(Prim _ _) (append i-list (normal-assign (Reg 'rax) e) (list (Jmp 'conclusion)))]
            [(Var _) (append i-list (list (Instr 'movq (list (atom-to-x86 e) (Reg 'rax)))
                           (Jmp 'conclusion)))]
            [(Int _) (append (list (Instr 'movq (list (atom-to-x86 e) (Reg 'rax)))
                           (Jmp 'conclusion)))]
        )
    ]
  )
)

(define (select-instructions exp)
  (match exp
    [(Program info e) (Program info (select-instructions* e '()))]
  )
)

; unit test (without uniquify)
(define (Unit-test exp) 
  (define eexp (explicate-control (Program '() (parse-exp exp))))
  (define eeexp (select-instructions eexp))
  (display eeexp)
  (display "\n")
)

; (Unit-test 42)
; (Unit-test `(+ 21 42))
; (Unit-test `(- 42))
; (Unit-test `(let ([x (read)]) x))
; (Unit-test `(let ([y (let ([x.1 20]) (let ([x.2 22]) (+ x.1 x.2)))]) y))
; (Unit-test 
;     `(let ([tmp0 (- 3)])
;         (let ([tmp1 (- 2)])
;             (let ([tmp2 (+ 1 tmp1)])
;                 (let ([tmp3 (+ tmp0 tmp2)])
;                     (let ([tmp4 (+ 1 2)])
;                         (let ([tmp5 (read)])
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
