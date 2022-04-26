#lang racket
(require "structured-program-infix.rkt")

(define (fib n)
  (prog
    (if (n < 3)
	(return 1))
    (return ($fib (n - 1)) + ($fib (n - 2)))))

(define (fib-list n)
  (prog
    (var result = ())
    (var i)
    (for ((i = 1) (i <= n) (i = i + 1))
	 (result = result >-< : ($fib i)))
    (return result)))

(define (fib-it a b n)
  (prog
    (var c)
    (while (n > 0)
	   (n = n - 1)
	   (c = a + b)
	   (a = b)
	   (b = c))
    (return b)))

(define (fib2 n)
  (prog
    (if (n < 3)
	(return 1))
    (return ($fib-it 1 1 (n - 2)))))

(define (fib-list2 n)
  (prog
    (var result = ())
    (var i)
    (for ((i = 1) (i <= n) (i = i + 1))
	 (result = result >-< : ($fib2 i)))
    (return result)))

;Test
(prog
  ($printf "~a\n" ($fib-list 40))
  ($printf "~a\n" ($fib-list2 40)))

