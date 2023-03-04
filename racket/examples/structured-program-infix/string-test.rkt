#lang racket
(require "structured-program-infix.rkt")

(define (strcat s1 s2)
  (prog
    (var i)
    (var r = s1)
    (for ((i = 0) (i < sizeof s2) (i = i + 1))
	 (r = r >-< s2 (i)))
    (return r))
  )

(define (strrev s)
  (prog
    (var i ret len)
    (len = sizeof s)
    (ret = "")
    (for ((i = len - 1) (i >= 0) (i = i - 1))
	 (ret = ret >-< s(i)))
    (return ret))
  )

(define (strstr s1 s2)
  (prog
    (var lens1 = sizeof s1)
    (var lens2 = sizeof s2)
    (var ret = ())
    (var i j k flag tmp)
    (for ((i = 0) (i < lens1) (i = i + 1))
	 (flag = #t)
	 (tmp = "")
	 (for ((j = 0) (j < lens2) (j = j + 1))
	      (k = i + j)
	      (if (k >= lens1)
		  (begin
		    (flag = #f)
		    (break)))
	      (tmp = tmp >-< s1(k)))
	 (if (flag and tmp == s2)
	     (ret = ret >-< : i)))
    (return ret))
  )

;Test
(prog
  (var s1 s2)
  ($printf "input two string(with quotation marks)\n")
  (s1 = ($read))
  (s2 = ($read))
  ($printf "strcat(\"~a\", \"~a\") = \"~a\"\n" s1 s2 ($strcat s1 s2))
  ($printf "strrev(\"~a\") = \"~a\"\n" s1 ($strrev s1))
  ($printf "strrev(\"~a\") = \"~a\"\n" s2 ($strrev s2))
  ($printf "strstr(\"~a\", \"~a\") = ~a\n" s1 s2 ($strstr s1 s2))
  )