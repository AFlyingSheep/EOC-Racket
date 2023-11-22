#lang racket
(require "utilities.rkt")
(require racket/dict)
(provide patch-instructions)
;; patch-instructions : x86var -> x86int

(define (patch-ins ins)
	(match ins
		[(Instr op (list (Deref r1 i1) (Deref r2 i2)))
		 	(list
				(Instr 'movq (list (Deref r1 i1) (Reg 'rax)))
				(Instr op (list (Reg 'rax) (Deref r2 i2)))
			)
		]
		[else (list ins)]
	)	
)

(define (patch-instructions* instructions)
	(define mod-instructions
	(for*/list ([ins (in-list instructions)] 
				[result (in-list (patch-ins ins))])
    result
	)
	)
	mod-instructions
)
(define (patch-instructions p)
	(match p
		[(Program info e) (Program info (patch-instructions* e))]
	)
)


