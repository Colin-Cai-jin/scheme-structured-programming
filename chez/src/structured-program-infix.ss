;#lang racket
(library (structured-program-infix)
(export prog var
 	~assign
	~get-pair-of-n-d-array
	~make-vector ~make-list
	~infix-cal ~set-arr-value! ~assign
	~get-vector-of-n-d-array
	~get-list-of-n-d-array
	)
(import (scheme))


(define-syntax (prog x)
  ;The same as `range` in Racket
  ;(range 5) => (0 1 2 3 4)
  ;(range 1 4) => (1 2 3)
  ;(range 1 10 2) => (1 3 5 7 9)
  ;(range 10 1 -2) => (10 8 6 4 2)
  (define (range . s)
    (define (_range from to step)
      (define (it+ r now)
	(if (>= now to)
	    r
	    (it+ (cons now r) (+ now step))))
      (define (it- r now)
	(if (<= now to)
	    r
	    (it- (cons now r) (+ now step))))
      (if (> step 0)
	  (it+ '() from)
	  (it- '() from)))
    (reverse
      (case (length s)
	((1) (_range 0 (car s) 1))
	((2) (apply _range (append s '(1))))
	((3) (apply _range s)))))
  ;The same as `fold-left` in R6RS
  (define (fold-left f init lst)
    (if (null? lst)
	init
	(fold-left f (f init (car lst)) (cdr lst))))
  ;Get the last element of `lst`
  ;(last '(1 2 3 4 5)) => 5
  (define (last lst)
    (if (null? (cdr lst))
	(car lst)
	(last (cdr lst))))
  ;Set subtraction
  ;(set- '(1 2 3 4) '(2 4 5 6)) => (1 3)
  (define (set- a b)
    (filter (lambda (x) (not (member x b))) a))
  ;Returns the list of the first `n` elements of `lst`
  ;(take '(a b c d e) 3) => (a b c)
  (define (take lst n)
    (define (_ r lst n)
      (if (zero? n)
	  (reverse r)
	  (_ (cons (car lst) r) (cdr lst) (- n 1))))
    (_ '() lst n))
  ;Returns `lst` except the first `n` elements
  ;(drop '(a b c d e) 3) => (d e)
  (define (drop lst n)
    (if (zero? n)
	lst
	(drop (cdr lst) (- n 1))))
  ;compose some functions to one function
  ;(list (+ 1 2 3 4)) => (10)
  ;((compose list +) 1 2 3 4) => (10)
  (define (compose . fs)
    (if (null? (cdr fs))
	(car fs)
	(lambda s
	  ((car fs)
	   (apply (apply compose (cdr fs)) s)))))
  ;curry
  ;(= 1 1) => #t
  ;((curry2 = 1) 1) => #t
  (define (curry n f . args) ;n : the number of arguments accepted by f
    (if (= n (length args))
	(apply f args)
	(lambda s (apply curry n f (append args s)))))
  (define (curryn n) (curry (+ n 2) curry n))
  (define curry2 (curryn 2))
  (define curry3 (curryn 3))
  (define curry4 (curryn 4))


  ;convert the code in the macro to the normal scheme code
  (define (trans lst)
    ;convert a tree to a list
    ;(flat '((1 2 3)(4 5 6)(7 (8 9)))) => (1 2 3 4 5 6 7 8 9)
    (define (flat lst)
      (cond
	((null? lst) '())
	((not (pair? lst)) (list lst))
	((pair? (car lst))
	 (append
	   (flat (caar lst))
	   (flat (cdar lst)) (flat (cdr lst))))
	(else (cons (car lst) (flat (cdr lst))))))
    ;all the symbols of `lst`
    (define str-list (map
		       (lambda (x)
			 (let ((s (symbol->string x)))
			   (if (char=? #\$ (string-ref s 0))
			       (string->symbol (substring s 1 (string-length s)))
			       x)))
		       (filter symbol? (flat lst))))
    ;It returns a function to create new names with a prefix of `prefix` which are not in `str-list`
    (define (get-new-name prefix str-list)
      (let ((i 1))
	(lambda ()
	  (let it ()
	    (let ((name
		    (string->symbol
		      (string-append prefix (number->string i)))))
	      (set! i (+ i 1))
	      (if (memq name str-list)
		  (it)
		  name))))))
    (define get-line-name (get-new-name "line-name-" str-list))
    (define get-func-name (get-new-name "func-" str-list))
    (define get-var-name (get-new-name "var-" str-list))

    ;replace the symbols in the list
    ;(replace-line-name '(a b (c d e)) '((a . A)(d . D)) => (A b (c D e))
    (define (replace-line-name code table)
      (cond
	((null? code) '())
	((not (pair? code))
	 (let ((x (assq code table)))
	   (if x
	       (cdr x)
	       code)))
	(else
	  (cons (replace-line-name (car code) table)
		(replace-line-name (cdr code) table)))))
    ;(del-quote-in-front-of-number '(test (quote 1) (test2 (quote a) (quote 2)))) => (test 1 (test2 (quote a) 2))
    (define (del-quote-in-front-of-number code)
      (cond
	((not (pair? code)) code)
	((and (eq? 'quote (car code))
	      (number? (cadr code)))
	 (cadr code))
	(else
	  (cons (del-quote-in-front-of-number (car code))
		(del-quote-in-front-of-number (cdr code))))))
    ;(add-infix-macro '(i = j + k) 'func-1) => (~assign i = j + k)
    ;(add-infix-macro '(i + j) 'func-1) => (~infix-cal i + j)
    ;(add-infix-macro '(func-1 line-1) 'func-1) => (func-1 line-1)
    (define (add-infix-macro code func-name)
      (define (is-assign? lst)
        (define (lists+eq? lst)
  	(cond
  	  ((null? lst) #f)
  	  ((eq? '= (car lst)) #t)
  	  ((list? (car lst)) (lists+eq? (cdr lst)))
  	  (else #f)))
        (cond
  	((not (symbol? (car lst))) #f)
  	((char=? #\$ (string-ref (symbol->string (car lst)) 0)) #f)
  	((lists+eq? (cdr lst)) #t)
  	(else #f)))
      (define (_convert lst)
        (if (not (list? lst))
  	  lst
  	  (case (car lst)
  	    ((begin if) (cons (car lst) (map _convert (cdr lst))))
  	    ((quote) lst)
  	    (else
  	      (cond
  		((eq? (car lst) func-name) lst)
  		((is-assign? lst)(cons '~assign lst))
  		(else (cons '~infix-cal lst)))))))
      (map (lambda (code-line) (cons (car code-line) (map _convert (cdr code-line)))) code))
    ;Delete all of the stats which never run
    (define (opt-del-never-run-stat prg)
      (define (traversal-tran-relations r lst)
	(cond
	  ((null? lst) r)
	  ((eq? (car lst) (car prg))
	   (if (eq? 'quote (cadr lst))
	       (traversal-tran-relations
		 (cons (caddr lst) r)
		 (cdddr lst))
	       (traversal-tran-relations
		 (cons (cadr lst) r)
		 (cddr lst))))
	  (else traversal-tran-relations r (cdr lst))))
      (define all-trans-relations
	(map 
	  (lambda (code-line)
	    (cons
	      (caar code-line)
	      (traversal-tran-relations '() (flat code-line))))
	  (caddr prg)))
      (define (layer-traversal r queue relations)
	(if (null? queue)
	    r
	    (let ((s (assq (car queue) relations)))
	      (layer-traversal
		(cons (car queue) r)
		(if s
		    (append (cdr queue) (set- (cdr s) (append r queue)))
		    (cdr queue))
		relations))))
      (let ((all-used-lines
	      (layer-traversal '() `(,(cadr prg)) all-trans-relations)))
	(list
	  (car prg)
	  (cadr prg)
	  (filter
	    (lambda (x) (memq (caar x) all-used-lines))
	    (caddr prg)))))
    ;Delete all the stats which only change the stat without doing anything else.
    (define (opt-del-empty-stat prg)
      (define (get-line-eq-classes func-name code)
	(define (add-eq pair classes)
	  (define (search match? x classes)
	    (cond
	      ((null? classes) #f)
	      ((match? x (car classes))
	       (list (car classes) '() (cdr classes)))
	      (else
		(let ((r (search match? x (cdr classes))))
		  (and r
		       `(,(car r)
			  (,(car classes) ,@(cadr r))
			  ,(caddr r)))))))
	  (let ((first (search
			 (lambda (x s) (eq? x (or (car s) (cadr s))))
			 (car pair)
			 classes))
		(second (search
			  (lambda (x s) (memq x s))
			  (cdr pair)
			  classes)))
	    (case (list (not (not first)) (not (not second)))
	      (((#t #t))
	       (let ((len-first-pre (length (cadr first)))
		     (len-second-pre (length (cadr second))))
		 (if (= len-first-pre len-second-pre);must be (car pair) = (cdr pair)
		     (append (cadr first) (list (cons #f (car first))) (caddr first))
		     (if (< len-first-pre len-second-pre)
			 (append
			   (cadr first)
			   (if (caar second)
			       (list (append (car first) (car second)))
			       (list (cons #f (append (car first) (cdar second)))))
			   (drop
			     (cadr second)
			     (- len-second-pre len-first-pre 1))
			   (caddr second))
			 (append
			   (cadr second)
			   (if (caar second)
			       (list (append (car first) (car second)))
			       (list (cons #f (append (car first) (cdar second)))))
			   (drop
			     (cadr first)
			     (- len-first-pre len-second-pre 1))
			   (caddr first))))))
	      (((#f #f))
	       (if (eq? (car pair) (cdr pair))
		   (cons (list #f (car pair)) classes)
		   (cons (list (cdr pair) (car pair)) classes)))
	      (((#t #f))
	       (append
		 (cadr first)
		 (list (cons (cdr pair) (car first)))
		 (caddr first)))
	      (((#f #t))
	       (append
		 (cadr second)
		 (list (append (car second) (list (car pair))))
		 (caddr second))))))
	(fold-left
	  (lambda (classes code-line)
	    (if (and
		  (null? (cddr code-line))
		  (pair? (cadr code-line))
		  (eq? func-name (caadr code-line)))
		(let ((goto-stat
			(if (pair? (cadadr code-line))
			    (cadr (cadadr code-line))
			    (cadadr code-line))))
		  (add-eq
		    (cons (caar code-line) goto-stat)
		    classes))
		classes))
	  '()
	  code))
      (define line-eq-classes
	(get-line-eq-classes (car prg) (caddr prg)))
      (define line-table
	(apply append
	       (map
		 (lambda (s)
		   (let ((s2 (if (car s) s (cdr s))))
		     (map (lambda (x) (cons x (car s2))) (cdr s2))))
		 line-eq-classes)))
      (list
	(car prg)
	(let ((s (assq (cadr prg) line-table))
	      (func-name (car prg)))
	  (if s
	      (cdr s)
	      (cadr prg)))
	(let ((func-name (car prg)))
	  (append
	    (replace-line-name
	      (filter
		(lambda (code-line)
		  (not
		    (and
		      (null? (cddr code-line))
		      (pair? (cadr code-line))
		      (eq? func-name (caadr code-line)))))
		(caddr prg))
	      line-table)
	    (map
	      (lambda (class)
		(let ((stat (cadr class)))
		`((,stat)
		  (,func-name
		    ,(if (symbol? stat)
			 (list 'quote stat)
			 stat)))))
	      (filter
		(lambda (x) (not (car x)))
		line-eq-classes))))))
    ;Combine the stats
    (define (opt-combine-stat prg)
      (define (replace-code v-code from-index to-index line-name)
       (define (replace code from to)
	 (if (list? code)
	     (if (eq? 'if (car code))
		 (cond
		   ((equal? from (caddr code))
		    (if (null? (cdr to))
			(list 'if (cadr code) (car to) (cadddr code))
			(list 'if (cadr code) (cons 'begin to) (cadddr code))))
		   ((equal? from (cadddr code))
		    (if (null? (cdr to))
			(list 'if (cadr code) (caddr code) (car to))
			(list 'if (cadr code) (caddr code) (cons 'begin to))))
		   (else
		     (list 'if (cadr code) (replace (caddr code) from to) (replace (cadddr code) from to))))
		 (if (equal? (last code) from)
		     (append (take code (- (length code) 1)) to)
		     (append (take code (- (length code) 1)) (list (replace (last code) from to)))))
	     code))
       (let ((from-segment (list (car prg)
			  (if (symbol? line-name)
			   (list 'quote line-name)
			   line-name)))
	     (code (vector-ref v-code to-index))
	     (to-segment (vector-ref v-code from-index)))
	(vector-set! v-code to-index (replace code from-segment to-segment))))
      (let* ((len (length (caddr prg)))
	     (v-line-name (list->vector (map caar (caddr prg))))
	     (v-line-used (make-vector len '(0 . 0)))
	     (v-code (list->vector (map cdr (caddr prg)))))
	(list
	  (car prg)
	  (cadr prg)
	  (begin
	    (for-each
	      (lambda (n)
		(let ((s (flat (vector-ref v-code n))))
		  (for-each
		    (lambda (m)
		      (let* ((line-name (vector-ref v-line-name m))
			     (used (vector-ref v-line-used m))
			     (count ((compose length filter)
				     (curry2 eq? line-name)
				     s)))
			(if (> count 0)
			    (vector-set!
			      v-line-used
			      m
			      (cons (+ (car used) count) n))
			    (void))))
		    (range len))))
	      (range len))
	    (for-each
	      (lambda (n)
		(if (or
		      (eq? (cadr prg) (vector-ref v-line-name n))
		      ((curry2 (compose not =) 1)
		       (car (vector-ref v-line-used n))))
		    (vector-set! v-line-used n #f)
		    (vector-set! v-line-used n (cdr (vector-ref v-line-used n)))))
	      (range len))
	    (for-each
	      (lambda (n)
		(let ((index-inserted (vector-ref v-line-used n)))
		  (if index-inserted
		      (begin
			(replace-code v-code n index-inserted (vector-ref v-line-name n))
			(for-each
			  (lambda (m)
			    (if (eq? n (vector-ref v-line-used m))
				(vector-set! v-line-used m index-inserted)
				(void)))
			  (range len)))
		      (void))))
	      (range len))
	    (fold-left
	      (lambda (r index)
		(if (vector-ref v-line-used index)
		    r
		    (cons
		      (cons
			(list (vector-ref v-line-name index))
			(vector-ref v-code index))
		      r)))
	      '()
	      (range (- len 1) -1 -1))))))
    ;Replace the stats with the numbers
    (define (opt-stat-to-number prg)
      (let ((table (map
		     (lambda (s n) (cons (caar s) n))
		     (caddr prg)
		     (range (length (caddr prg))))))
      (list (car prg)
	    (cdr (assq (cadr prg) table))
	    (del-quote-in-front-of-number
	      (replace-line-name (caddr prg) table)))))
    ;The optimizing function
    ;There are 4 passes here
    (define (optimize func~start~code)
      (fold-left
	(lambda (prg opt) (opt prg))
	func~start~code
	`(,opt-del-never-run-stat
	   ,opt-combine-stat
	   ,opt-del-empty-stat
	   ,opt-stat-to-number
	   )))
    ;Convert the source code to the stat machine
    ;names : (list func-name start-line end-line continue-line break-line)
    (define (it r names lst)
      (let ((func-name (list-ref names 0))
	    (start-line (list-ref names 1))
	    (end-line (list-ref names 2))
	    (continue-line (list-ref names 3))
	    (break-line (list-ref names 4)))
	(cond
	  ((null? lst)
	   (let ((s (last (car r))))
	     (if (eq? (car s) func-name)
		 r
		 `((,@(car r) (,func-name (quote ,end-line)))
		   ,@(cdr r)))))
	  ((null? r)
	   (if (not (pair? (car lst)))
	       (if (eq? 'begin (car lst))
		   (it '() names (cdr lst))
		   '());error
	       (it `(((,start-line))) names lst)))
	  (else
	    (case (caar lst)
	      ((line)
	       (it
		 `((,(cdar lst))
		   (,@(car r) (,func-name (quote ,(cadar lst))))
		   ,@(cdr r))
		 names
		 (cdr lst)))
	      ((goto)
	       (it
		 (let ((s `((,@(car r) (,func-name (quote ,(cadar lst))))
			    ,@(cdr r))))
		   (if (null? (cdr lst))
		       s
		       (cons `((,(get-line-name))) s)))
		 names
		 (cdr lst)))
	      ((continue)
	       (it
		 (let ((s `((,@(car r) (,func-name (quote ,continue-line)))
			    ,@(cdr r))))
		   (if (null? (cdr lst))
		       s
		       (cons `((,(get-line-name))) s)))
		 names
		 (cdr lst)))
	      ((break)
	       (it
		 (let ((s `((,@(car r) (,func-name (quote ,break-line)))
			    ,@(cdr r))))
		   (if (null? (cdr lst))
		       s
		       (cons `((,(get-line-name))) s)))
		 names
		 (cdr lst)))
	      ((return)
	       (it
		 `(((,(get-line-name)))
		   (,@(car r) ,(if (null? (cdar lst)) '(void) (if (null? (cddar lst)) (cadar lst) (cdar lst))))
		   ,@(cdr r))
		 names
		 (cdr lst)))
	      ((if)
	       (let* ((if-start-line (get-line-name))
		      (if-end-line (get-line-name))
		      (if-true-line (get-line-name))
		      (if-false-line (if (= 4 (length (car lst)))
					 (get-line-name)
					 if-end-line))
		      (make-code-list (lambda (s)
					(if (eq? 'begin (car s))
					    s
					    (list s))))
		      (true-code (it
				   '() 
				   (list
				     func-name
				     if-true-line
				     if-end-line
				     continue-line
				     break-line)
				   (make-code-list (caddar lst))))
		      (false-code (if (= 4 (length (car lst)))
				      (it
					'()
					(list
					  func-name
					  if-false-line
					  if-end-line
					  continue-line
					  break-line)
					(make-code-list (list-ref (car lst) 3)))
				      '())))
		 (it
		   `(((,if-end-line))
		     ,@false-code
		     ,@true-code
		     ((,if-start-line)
		      (if ,(cadar lst)
			  (,func-name (quote ,if-true-line))
			  (,func-name (quote ,if-false-line))))
		     (,@(car r) (,func-name (quote ,if-start-line)))
		     ,@(cdr r))
		   names
		   (cdr lst))))
	      ((while)
	       (let* ((while-start-line (get-line-name))
		      (while-end-line (get-line-name))
		      (while-body-line (get-line-name))
		      (while-body-code (it
					 '()
					 (list
					   func-name
					   while-body-line
					   while-start-line
					   while-start-line
					   while-end-line)
					 (cddar lst))))
		 (it
		   `(((,while-end-line))
		     ,@while-body-code
		     ((,while-start-line)
		      (if ,(cadar lst)
			  (,func-name (quote ,while-body-line))
			  (,func-name (quote ,while-end-line))))
		     (,@(car r) (,func-name (quote ,while-start-line)))
		     ,@(cdr r))
		   names
		   (cdr lst))))
	      ((for)
	       (let* ((for-cmp-line (get-line-name))
		      (for-body-line (get-line-name))
		      (for-end-line (get-line-name))
		      (for-step-line (get-line-name))
		      (head-body (cdar lst))
		      (for-body-code (it
				       '()
				       (list
					 func-name
					 for-body-line
					 for-step-line
					 for-step-line
					 for-end-line)
				       (let ((s (cdr head-body)))
					 (if (eq? 'begin (caar s))
					     (car s)
					     s)))))
		 (it
		   `(((,for-end-line))
		     ,@for-body-code
		     ((,for-step-line)
		      ,(caddar head-body) (,func-name (quote ,for-cmp-line)))
		     ((,for-cmp-line)
		      (if ,(cadar head-body)
			  (,func-name (quote ,for-body-line))
			  (,func-name (quote ,for-end-line))))
		     (,@(car r) ,(caar head-body) (,func-name (quote ,for-cmp-line)))
		     ,@(cdr r))
		   names
		   (cdr lst))))
	      (else
		(it
		  `((,@(car r) ,(car lst)) ,@(cdr r))
		  names
		  (cdr lst))))))))

    (let* ((stat-var-name (get-var-name))
	   (start-line-name (get-line-name))
	   (end-line-name (get-line-name))
	   (func-name (get-func-name))
	   (result (optimize
		     (list
		       func-name
		       start-line-name
		       (it
			 '()
			 (list
			   func-name
			   start-line-name
			   end-line-name
			   '()
			   '())
			 lst))))
	   (code (add-infix-macro (caddr result) func-name)))
      `(let ,func-name ((,stat-var-name 
			  ,(if (symbol? (cadr result))
			       (list 'quote (cadr result))
			       (cadr result))))
	 (case ,stat-var-name ,@code))))

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
			(datum->syntax #'k (trans body))
			(datum->syntax #'k
				       `(let () ,@defines ,(trans body)))))))))

;Get the right pair, then we can use set-car! to set the value
;(~get-pair-of-n-d-array '(0 1 2 3 4) 2) => (2 3 4)
;(~get-pair-of-n-d-array '((0 1 2) (3 4 5)) 1 2) => (5)
(define (~get-pair-of-n-d-array s . n)
  (cond
    ((and (null? (cdr n)) (zero? (car n))) s)
    ((zero? (car n)) (apply ~get-pair-of-n-d-array (car s) (cdr n)))
    (else (apply ~get-pair-of-n-d-array (cdr s) (cons (- (car n) 1) (cdr n))))))

;Make a N dimention vecor
;(~make-vector '(2 2) 0) => #(#(0 0) #(0 0))
(define (~make-vector size . value)
    (define (range a b)
      (define (it r start)
        (if (< start a)
            r
            (it (cons start r) (- start 1))))
      (if (>= a b)
	  '()
	  (it '() (- b 1))))
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
  (let ((v (if (null? value) 0 (car value))))
    (if (null? (cdr size))
	(make-list (car size) v)
	(make-list (car size) (apply ~make-list (cdr size) value)))))

;Get the value in a N demention vector
;(~get-vector-of-n-d-array '#(#(1 2)#(3 4)) 1 1) => 4
(define (~get-vector-of-n-d-array s . arg)
  (if (null? arg)
      s
      (apply ~get-vector-of-n-d-array
	     (vector-ref s (car arg))
	     (cdr arg))))

;Get the value in a N demention list
;(~get-list-of-n-d-array '((1 2) (3 4)) 1 1) => 4
(define (~get-list-of-n-d-array s . arg)
  (if (null? arg)
      s
      (apply ~get-list-of-n-d-array
	     (list-ref s (car arg))
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
     (if (vector? n-d-array)
	 (vector-set! (~get-vector-of-n-d-array n-d-array n ...) m v)
	 (set-car! (~get-pair-of-n-d-array n-d-array n ... m) v)))))

;Convert the infix expression to the s-expression
(define-syntax (~infix-cal x)

(define (infix->prefix expr ops)
  (define (take lst n)
    (define (_ r lst n)
      (if (zero? n)
	  (reverse r)
	  (_ (cons (car lst) r) (cdr lst) (- n 1))))
    (_ '() lst n))
  (define (drop lst n)
    (if (zero? n)
	lst
	(drop (cdr lst) (- n 1))))  
  (define (fold-left f init lst)
    (if (null? lst)
	init
	(fold-left f (f init (car lst)) (cdr lst))))
  ;Get the first element of `lst` satisfied with `?`
  ;Return #f if failed
  (define (get-first ? lst)
    (cond
      ((null? lst) #f)
      ((? (car lst)) (car lst))
      (else (get-first ? (cdr lst)))))
  ;(split-even-odd-pos-reverse '(1 2 3 4 5)) => ((5 3 1) . (4 2))
  (define (split-even-odd-pos-reverse s)
    (define (it even-r odd-r s b-even)
      (cond
	((null? s) (cons even-r odd-r))
	(b-even (it (cons (car s) even-r) odd-r (cdr s) #f))
	(else (it even-r (cons (car s) odd-r) (cdr s) #t))))
    (it '() '() s #t))
  ;(lambda (lst n) (>= (length lst) n))
  (define (length>=? lst n)
    (cond
      ((zero? n) #t)
      ((not (pair? lst)) #f)
      (else (length>=? (cdr lst) (- n 1)))))
  ;Greater value, less priority
  ;make a priority greater than all the operations
  (define infinit-priority `(op () () () ,(+ 1 (apply max (map caddr ops)))))
  ;shift/reduce
  (define (shift-reduce s expr)
    (define (shift s expr)
      (if (null? expr)
	  (if (equal? (car s) infinit-priority)
	      (cdadr s)
	      (shift-reduce (cons infinit-priority s) '()))
	  (shift-reduce (cons (car expr) s) (cdr expr))))
    (define (try-reduce s expr)
      (let* ((arg-cnt (list-ref (caddr s) 3))
	     (n (if (= 1 arg-cnt) 2 (- (* 2 arg-cnt) 1))))
	(if (length>=? s (+ n 1))
	    (let* ((remained (drop s (+ n 1)))
		   (combined (take (cdr s) n))
		   (splited-combined (split-even-odd-pos-reverse combined))
		   (symbols (map cadr (cdr splited-combined)))
		   (args (car splited-combined)))
	      (if (equal? symbols (list-ref (caddr s) 2))
		  (shift-reduce
		    (cons
		      (car s)
		      (cons
			(cons
			  'data
			  (cons
			    (list-ref (caddr s) 5)
			    (map cdr args)))
			remained))
		    expr)
		  (shift s expr)))
	    (shift s expr))))
    (cond
      ((null? s)
       (shift s expr))
      ((eq? 'op (caar s))
       (if (or (null? (cdr s)) (eq? 'op (caadr s)))
	   (shift s expr)
	   (if (and
		 (length>=? s 3)
		 (<= (list-ref (caddr s) 4)
		     (list-ref (car s) 4)))
	       (try-reduce s expr)
	       (shift s expr))))
      ((eq? 'data (caar s))
       (shift s expr))
      (else
	#f)))
  (let* ((b-first-symbol (symbol? (car expr)))
	 (first (and b-first-symbol (symbol->string (car expr))))
	 (first-len (and b-first-symbol (string-length first))))
    (if (and b-first-symbol
	     (> first-len 1)
	     (char=? #\$ (string-ref first 0)))
	(cons
	  (string->symbol (substring first 1 first-len))
	  (map
	    (lambda (x)
	      (if (list? x)
		  (infix->prefix x ops)
		  x))
	    (cdr expr)))
	(let* ((~expr (fold-left
		       (lambda (r x)
			 (if (list? x)
			     (cons
			       #f
			       (append
				 (cdr r)
				 (list (cons 'data (infix->prefix x ops)))))
			     (let ((desc
				     (get-first
				       (if (car r)
					   (lambda (op)
					     (and (memq x (car op))
						  (= (cadr op) 1)))
					   (lambda (op)
					     (and (memq x (car op))
						  (> (cadr op) 1))))
				       ops)))
			       (if desc
				   (cons
				     #t
				     (append
				       (cdr r)
				       (list (cons 'op (cons x desc)))))
				   (cons
				     #f
				     (append
				       (cdr r)
				       (list (cons 'data x))))))))
		       '(#t)
		       expr))
	      (combined-arr-expr
		(reverse
		  (map
		    (lambda (x)
		      (if (eq? 'op (car x))
			  x
			  (if (null? (cddr x))
			      (cadr x)
			      (cons
			       'data
			       (cons
				'(lambda s
				   (if (vector? (car s))
				     (apply ~get-vector-of-n-d-array s)
				     (apply ~get-list-of-n-d-array s)))
				(map cdr (reverse (cdr x))))))))
		    (cdr
		      (fold-left
			(lambda (r x)
			 (if (eq? 'op (car x))
			  (cons #f (cons x (cdr r)))
			  (if (car r)
			   (cons #t (cons (cons 'data-set (cons x (cdadr r))) (cddr r)))
			   (cons #t (cons (cons 'data-set (list x)) (cdr r))))))
			'(#f)
			(cdr ~expr)))))))
	  (shift-reduce '() combined-arr-expr)))))

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
	     ((==) 2 7 eqv?)
	     ((>) 2 7 >)
	     ((<) 2 7 <)
	     ((>=) 2 7 >=)
	     ((<=) 2 7 <=)
	     ((!=) 2 7 (lambda (a b) (not (eqv? a b))))
	     ((and) 2 9 and)
	     ((or) 2 9 or)
	     ((!) 1 8 not)
	     ((:) 1 0 list)
	     ((sizeof) 1 0 (lambda (x) (if (vector? x) (vector-length x) (length x))))
	     ((**) 2 1 expt)
	     ((bit-and) 2 4 bitwise-and)
	     ((bit-or) 2 4 bitwise-ior)
	     ((bit-xor) 2 4 bitwise-xor)
	     ((bit-not) 1 2 bitwise-not)
	     ((>-<) 2 6 append)
	     ))))
     (datum->syntax
       #'k
      result)))))

)

