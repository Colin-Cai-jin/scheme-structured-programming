;#lang racket
(library (structured-program)
(export prog)
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
    (define str-list (filter symbol? (flat lst)))
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
    (define (del-quote-in-front-of-number code)
      (cond
	((not (pair? code)) code)
	((and (eq? 'quote (car code))
	      (number? (cadr code)))
	 (cadr code))
	(else
	  (cons (del-quote-in-front-of-number (car code))
		(del-quote-in-front-of-number (cdr code))))))
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
    (define (opt-stat-to-number prg)
      (let ((table (map
		     (lambda (s n) (cons (caar s) n))
		     (caddr prg)
		     (range (length (caddr prg))))))
      (list (car prg)
	    (cdr (assq (cadr prg) table))
	    (del-quote-in-front-of-number
	      (replace-line-name (caddr prg) table)))))
    (define (optimize func~start~code)
      (fold-left
	(lambda (prg opt) (opt prg))
	func~start~code
	`(,opt-del-never-run-stat
	   ,opt-combine-stat
	   ,opt-del-empty-stat
	   ,opt-stat-to-number
	   )))
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
		   (,@(car r) ,(if (null? (cdar lst)) '(void) (cadar lst)))
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
			 lst)))))
      `(let ,func-name ((,stat-var-name 
			  ,(if (symbol? (cadr result))
			       (list 'quote (cadr result))
			       (cadr result))))
	 (case ,stat-var-name ,@(caddr result)))))

  (syntax-case x ()
	       ((k body ...)
		(letrec ((cut
			   (lambda (? s)
			     (if (or (null? s) (not (? (car s))))
				 (cons '() s)
				 (let ((r (cut ? (cdr s))))
				   (cons (cons (car s) (car r)) (cdr r)))))))
		  (let* ((all-code (syntax->datum #'(body ...)))
			 (cut-result (cut (lambda (x) (eq? (car x) 'define)) all-code))
			 (defines (car cut-result))
			 (body (cdr cut-result)))
		    (if (null? defines)
			(datum->syntax #'k (trans body))
			(datum->syntax #'k
				       `(let () ,@defines ,(trans body)))))))))


)


