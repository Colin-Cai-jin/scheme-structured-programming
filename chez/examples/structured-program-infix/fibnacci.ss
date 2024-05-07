(import (structured-program-infix))

(function (fib n)
  (if (n < 3)
      (return 1))
  (return ($fib (n - 1)) + ($fib (n - 2))))

(function (fib-list n)
  (var result = ())
  (var i)
  (for ((i = 1) (i <= n) (i = i + 1))
       (result = result >-< : ($fib i)))
  (return result))

(function (fib-it a b n)
  (var c)
  (while (n > 0)
	 (n = n - 1)
	 (c = a + b)
	 (a = b)
	 (b = c))
  (return b))

(function (fib2 n)
  (if (n < 3)
      (return 1))
  (return ($fib-it 1 1 (n - 2))))

(function (fib-list2 n)
  (var result = ())
  (var i)
  (for ((i = 1) (i <= n) (i = i + 1))
       (result = result >-< : ($fib2 i)))
  (return result))

;Test
(prog
  ($printf "~a\n" ($fib-list 40))
  ($printf "~a\n" ($fib-list2 40)))
