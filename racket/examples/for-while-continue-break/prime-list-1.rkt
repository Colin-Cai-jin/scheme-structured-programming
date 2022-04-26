#lang racket
(require "for-while-continue-break.rkt")

(define (prime-list n)
  (define r '())
  (define flag 0)
  (for (i 2 (< i n) (+ i 1))
       (set! flag 0)
       (for (j 2 (<= (* j j) i) (+ j 1))
	    (if (zero? (remainder i j))
		(begin
		  (set! flag 1)
		  (break))
		(void))
	    )
       (if (zero? flag)
	   (set! r (cons i r))
	   (void))
       )
  (reverse r)
  )
(display (prime-list 100))

