#lang racket
(require "structured-program-infix.rkt")
;Draw one line.
;Pass all the apples(O) and don't pass X
;  O X O O O
;  
;  O O O O O
;  
;  O O O O O
;  
;  O O O O O
;  
;  O O O O O
;  
;  O O O O O

(define (is-in a arr)
  (prog
    (var i)
    (for ((i = 0) (i < sizeof arr) (i = i + 1))
	 (if (a == arr(i))
	     (return #t)))
    (return #f))
  )

(define (set-intersction a b)
  (prog
    (var ret = ())
    (var i)
    (for ((i = 0) (i < sizeof a) (i = i + 1))
	 (if ($is-in (a(i)) b)
	     (ret = ret >-< : a(i))))
    (return ret))
  )

(define (set-del-element arr x)
  (prog
    (var ret = ())
    (var i)
    (for ((i = 0) (i < sizeof arr) (i = i + 1))
	 (if (x != arr(i))
	     (ret = ret >-< : arr(i))))
    (return ret)))

(define (search-graph point graph)
  (prog
    (var i)
    (for ((i = 0) (i < sizeof graph) (i = i + 1))
	 (if (graph(i)(0) == point)
	     (return graph(i)(1))))
    (return #f)))

(define (get-all-path from_point others_points graph)
  (prog
    (var ret = ())
    (var i j points t)
    (if (sizeof others_points == 0)
	(return : : from_point))
    (points = ($set-intersction ($search-graph from_point graph) others_points))
    (for ((i = 0) (i < sizeof points) (i = i + 1))
	 (t = ($get-all-path (points(i)) ($set-del-element others_points (points(i))) graph))
	 (for ((j = 0) (j < sizeof t) (j = j + 1))
	      (ret = ret >-< : (: from_point >-< t(j)))))
    (return ret)))

(define (make-one-point-edges n)
  (prog
    (var ret1 = ())
    (var ret = (*)(2))
    (ret(0) = n)
    (if (n - 6 >= 0 and n - 6 != 6)
	(ret1 = ret1 >-< : (n - 6)))
    (if (n + 6 < 30 and n + 6 != 6)
	(ret1 = ret1 >-< : (n + 6)))
    (if (n + 1 < 30 and n + 1 != 6 and (n + 1) // 6 == n // 6)
	(ret1 = ret1 >-< : (n + 1)))
    (if (n - 1 >= 0 and n - 1 != 6 and (n - 1) // 6 == n // 6)
	(ret1 = ret1 >-< : (n - 1)))
    (ret(1) = ret1)
    (return ret))
  )

(define (make-graph)
  (prog
    (var i)
    (var graph = ())
    (for ((i = 0) (i < 30) (i = i + 1))
	 (if (i == 6)
	     (continue))
	 (graph = graph >-< : ($make-one-point-edges i))
	 )
    (return graph)))

(define (is-step-in-path step path)
  (prog
    (var i)
    (var a = step(0))
    (var b = step(1))
    (for ((i = 0) (i < sizeof path) (i = i + 1))
	 (if (path(i) == a)
	     (begin
	       (if (i - 1 >= 0 and path(i - 1) == b)
		   (return #t))
	       (if (i + 1 < sizeof path and path(i + 1) == b)
		   (return #t))
	       (return #f))))
    (return #f))
  )

(define (draw path)
  (prog
    (var i j)
    (for ((i = 0) (i < 12) (i = i + 1))
	 (if (i % 2 == 0)
	     (begin
	       ($printf "O")
	       (for ((j = 0) (j < 4) (j = j + 1))
		    (if (i == 0 and j == 0)
			($printf " X")
			(begin
			  (if ($is-step-in-path (: (i // 2 + j * 6) >-< : (i // 2 + (j + 1) * 6)) path)
			      ($printf "-O")
			      ($printf " O"))))))
	     (begin
	       (for ((j = 0) (j < 5) (j = j + 1))
		    (if ($is-step-in-path (: (i // 2 + j * 6) >-< : (i // 2 + j * 6 + 1)) path)
			(if (j == 0)
			    ($printf "|")
			    ($printf " |"))
			(if (j == 0)
			    ($printf " ")
			    ($printf "  "))))))
	 ($printf "\n"))
    ($printf "\n")))

(define (play display-func)
  (prog
    (var points = ())
    (var all_paths)
    (var i)
    (for ((i = 1) (i < 30) (i = i + 1))
	 (if (i != 6)
	     (points = points >-< : i)))
    (all_paths = ($get-all-path 0 points ($make-graph)))
    (for ((i = 0) (i < sizeof all_paths) (i = i + 1))
	 ($display-func (all_paths(i))))))

;Run
(prog
  ($play draw))
