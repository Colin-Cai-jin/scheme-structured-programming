#lang racket
(require "structured-program-infix.rkt")

(function (create-prime-list-v1 n)
  (var i k flag)
  (var result = ())
  (for ((k = 2) (k <= n) (k = k + 1))
       (flag = #t)
       (for ((i = 0) (i < sizeof result) (i = i + 1))
	    ($printf "~a % ~a\n" k (result(i))) ;result(i) must add brackets
	    (if (k % result(i) == 0)
		(begin
		  (flag = #f)
		  (break))))
       (if flag
	   (result = result >-< (: k))))
  (return result))

(function (create-prime-list-v2 n)
  (var result = ())
  (var table = () (n));array of length n.It does not support >-<
  (var i j)
  (for ((i = 0) (i < n) (i = i + 1))
       (table(i) = 0))
  (for ((i = 2) (i * i <= n) (i = i + 1))
       (for ((j = i * 2) (j <= n) (j = j + i))
	    (table(j - 1) = 1)))
  (for ((i = 1) (i < n) (i = i + 1))
       (if (table(i) == 0)
	   (result = result >-< (: (i + 1)))))
  (return result))

;Test
(prog
  ($printf "~a\n" ($create-prime-list-v1 100))
  ($printf "~a\n" ($create-prime-list-v2 100)))
