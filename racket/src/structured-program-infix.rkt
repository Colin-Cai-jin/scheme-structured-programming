#lang racket
(provide function prog var
 	~assign
	~get-pair-of-n-d-array
	~mlist
	~mappend
	~make-vector ~make-list
	~infix-cal ~set-arr-value!
	~get-value-of-n-d-array
	)
(require (for-syntax "function-lib.rkt"))

(define-syntax (prog x)
  (syntax-case x ()
	       ((k body ...)
		(letrec ((cut
			   (lambda (? s)
			     (if (or (null? s) (not (? (car s))))
				 (cons '() s)
				 (let ((r (cut ? (cdr s))))
				   (cons (cons (car s) (car r)) (cdr r)))))))
		  (let* ((all-code (syntax->datum #'(body ...)))
			 (cut-result (cut (lambda (x) (eq? (car x) 'var)) all-code))
			 (defines (car cut-result))
			 (body (cdr cut-result)))
		    (if (null? defines)
			(datum->syntax #'k (trans-program-infix body))
			(datum->syntax #'k
				       `(let () ,@defines ,(trans-program-infix body)))))))))


(define (~mlist . s)
  (if (null? s)
  '()
  (mcons (car s) (apply ~mlist (cdr s)))))

;Get the right pair, then we can use set-car! to set the value
;(~get-pair-of-n-d-array '(0 1 2 3 4) 2) => (2 3 4)
;(~get-pair-of-n-d-array '((0 1 2) (3 4 5)) 1 2) => (5)
(define (~get-pair-of-n-d-array s . n)
  (cond
    ((and (null? (cdr n)) (zero? (car n))) s)
    ((zero? (car n)) (apply ~get-pair-of-n-d-array (mcar s) (cdr n)))
    (else (apply ~get-pair-of-n-d-array (mcdr s) (cons (- (car n) 1) (cdr n))))))

(define (~mappend s1 s2)
  (define (_reverse r s)
    (if (null? s)
        r
        (_reverse (mcons (mcar s) r) (mcdr s))))
  (_reverse s2 (_reverse '() s1)))

;Make a N dimention vecor
;(~make-vector '(2 2) 0) => #(#(0 0) #(0 0))
(define (~make-vector size . value)
    (if (null? (cdr size))
        (apply make-vector (car size) value)
        (let ((s (apply make-vector (car size) value)))
          (for-each
            (lambda (n)
	      (vector-set! s n (apply ~make-vector (cdr size) value)))
	    (range 0 (car size)))
	  s)))

;Make a N dimention list
;(~make-list '(2 2) 0) => ((0 0) (0 0))
(define (~make-list size . value)
  (define (make-mlist n v)
    (define (it r n)
      (if (zero? n)
	  r
	  (it (mcons v r) (- n 1))))
    (it '() n))
  (let ((v (if (null? value) 0 (car value))))
    (if (null? (cdr size))
	(make-mlist (car size) v)
	(make-mlist (car size) (apply ~make-list (cdr size) value)))))

;Get the value in a N demention array
;(~get-value-of-n-d-array '#(#(1 2) (3 4)) 1 1) => 4
(define (~get-value-of-n-d-array s . arg)
  (define (mlist-ref mlist n)
    (if (zero? n)
      (mcar mlist)
      (mlist-ref (mcdr mlist) (- n 1))))
  (if (null? arg)
      s
      (apply ~get-value-of-n-d-array
	     ((cond ((vector? s) vector-ref) ((string? s) (lambda (s n) (make-string 1 (string-ref s n)))) (else mlist-ref)) s (car arg))
	     (cdr arg))))

;For the variable define
;(var i j) => (define i 0)(define j 0)
;(var i = 1) => (define i (~infix-cal 1))
;(var i = 1 + 2) => (define i (~infix-cal 1 + 2))
;(define a = ()) => (define a '())
;(define a = () (4 5)) => (define a (~make-vector (list 4 5)))
;(define a = () (4 5) 0) => (define a (~make-vector (list 4 5) 0))
;(define a = (*) (4 5) 0) => (define a (~make-list (list 4 5) 0))
(define-syntax (var x)
  (syntax-case x (=)
    ((k v = d ...)
     (let ((desc (syntax->datum #'(d ...)))
	   (_v (syntax->datum #'v)))
       (letrec ((is-arr-define
		  (lambda (s)
		    (cond
		      ((null? s) #t)
		      ((eq? '* s) '*)
		      ((not (pair? s)) #f)
		      ((not (null? (cdr s))) #f)
		      (else (is-arr-define (car s)))))))
	 (let ((r (is-arr-define (car desc))))
	   (case r
	     ((#t)
	      (case (length desc)
		((1) (datum->syntax #'k `(define ,_v (quote ,(car desc)))))
		((2) (datum->syntax #'k `(define ,_v (~make-vector ,(cons 'list (cadr desc))))))
		((3) (datum->syntax #'k `(define ,_v (~make-vector ,(cons 'list (cadr desc)) ,(caddr desc)))))))
	     ((*)
	      (case (length desc)
		((1) (datum->syntax #'k `(define ,_v (quote ,(car desc)))))
		((2) (datum->syntax #'k `(define ,_v (~make-list ,(cons 'list (cadr desc))))))
		((3) (datum->syntax #'k `(define ,_v (~make-list ,(cons 'list (cadr desc)) ,(caddr desc)))))))
	     ((#f) (datum->syntax #'k `(define ,_v (~infix-cal ,@desc)))))))))
    ((k v ...)
     (let ((vs (syntax->datum #'(v ...))))
	   (datum->syntax
	     #'k
	     (cons 'begin (map (lambda (v) `(define ,v 0))  vs)))))))

;For the the variable assignment
;(~assign a = 1 + 2) => (set! a (~infix-cal 1 + 2))
;(~assign a(1 + 1)(2 + 1) = 1 + 2) => (~set-arr-value! a (~infix-cal 1 + 1) (~infix-cal 2 + 1) = (~infix-cal 1 + 2))
(define-syntax (~assign x)
  (syntax-case x ()
    ((k s ...)
     (let* ((args (syntax->datum #'(s ...)))
	    (v (car args))
	    (v-args (reverse (cdr (memq '= (reverse (cdr args))))))
	    (value (cdr (memq '= (cdr args)))))
     (if (null? v-args)
	 (datum->syntax #'k `(set! ,v (~infix-cal ,@value)))
	 (datum->syntax #'k `(~set-arr-value! ,v ,@(map (lambda (x) (cons '~infix-cal x)) v-args) = (~infix-cal ,@value))))))))

;For the value assignment of the array
(define-syntax ~set-arr-value!
  (syntax-rules (=)
    ((_ n-d-array n ... m = v)
     (let ((x (~get-value-of-n-d-array n-d-array n ...)))
       (cond
	 ((vector? x) (vector-set! x m v))
	 ((string? x) (set! x (let ((len (string-length x))) (string-append (substring x 0 m) v (substring x (+ m 1) len)))))
	 (else (set-mcar! (~get-pair-of-n-d-array x m) v)))))))

;Convert the infix expression to the s-expression
(define-syntax (~infix-cal x)
  (syntax-case x ()
    ((k a ...)
     (let ((result
	    (infix->prefix
	     (syntax->datum #'(a ...))
	     ;All the operations
	     ;((op) argument-count priority Lisp-expression)
	     '(
	     ((+) 2 5 +)
	     ((-) 2 5 -)
	     ((*) 2 3 *)
	     ((/) 2 3 /)
	     ((//) 2 3 quotient)
	     ((%) 2 3 remainder)
	     ((? :) 3 10 if)
	     ((-) 1 0 -)
	     ((==) 2 7 (lambda (a b) ((if (string? a) string=? eqv?) a b)))
	     ((>) 2 7 (lambda (a b) ((if (string? a) string>? >) a b)))
	     ((<) 2 7 (lambda (a b) ((if (string? a) string<? <) a b)))
	     ((>=) 2 7 (lambda (a b) ((if (string? a) string>=? >=) a b)))
	     ((<=) 2 7 (lambda (a b) ((if (string? a) string<=? <=) a b)))
	     ((!=) 2 7 (lambda (a b) (not ((if (string? a) string=? eqv?) a b))))
	     ((and) 2 9 and)
	     ((or) 2 9 or)
	     ((!) 1 8 not)
	     ((:) 1 0 ~mlist)
	     ((sizeof) 1 0 (lambda (x) (cond ((vector? x) (vector-length x)) ((string? x) (string-length x)) (else (let it((r 0)(y x)) (if (null? y) r (it (+ r 1) (mcdr y))))))))
	     ((**) 2 1 expt)
	     ((bit-and) 2 4 bitwise-and)
	     ((bit-or) 2 4 bitwise-ior)
	     ((bit-xor) 2 4 bitwise-xor)
	     ((bit-not) 1 2 bitwise-not)
	     ((>-<) 2 6 (lambda (x . s) (apply (cond ((string? x) string-append) (else ~mappend)) x s)))
	     ))))
     (datum->syntax
       #'k
      result)))))

(define-syntax (function x)
  (syntax-case x ()
    ((k f body ...)
     (with-syntax
       ((define (datum->syntax #'k 'define))
	(prog (datum->syntax #'k  'prog)))
     #'(define f (prog body ...))))))

