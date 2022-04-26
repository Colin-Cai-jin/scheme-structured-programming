#lang racket
(require "structured-program.rkt")

(define (make-prime-list n)
  (prog
    (define i 0)
    (define j 0)
    (define flag 0)
    (define result '())
    (for ((set! i 2) (<= i n) (set! i (+ i 1)))
	 (set! flag 0)
	 (for ((set! j 2) (<= (* j j) i) (set! j (+ j 1)))
	      (if (zero? (remainder i j))
		  (begin
		    (set! flag 1)
		    (break)
		    )
		  )
	      )
	 (if (zero? flag)
	     (goto line-b))
	 (line line-a)
	 (continue)
	 (line line-b)
	 (if (zero? flag)
	     (set! result (cons i result)))
	 )
    (return (reverse result))
   ))

(display (make-prime-list 100))
(newline)

