(library (for-while-continue-break)
(export for while do/while)
(import (scheme))

(define-syntax for
  (lambda (x)
    (syntax-case x ()
      ((k (var init-value condition next) do-thing ...)
       (with-syntax
         ((break (datum->syntax #'k 'break))
          (continue (datum->syntax #'k 'continue)))
           #'(call/cc
             (lambda (break)
	       (let ((continue 0)(flag #t)(var init-value))
		 (call/cc (lambda (c) (set! continue c)))
		 (if flag
		     (set! flag #f)
		     (begin
		      (set! var next)))
		 (if condition
		     (begin
		 	do-thing ...
			(continue))
		 (void))))))))))


(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((k condition do-thing ...)
       (with-syntax
         ((break (datum->syntax #'k 'break))
          (continue (datum->syntax #'k 'continue)))
           #'(call/cc
             (lambda (break)
	       (let ((continue 0))
		 (call/cc (lambda (c) (set! continue c)))
		 (if condition
		     (begin
		       do-thing ...
		       (continue))
		     (void))))))))))

(define-syntax do/while
  (lambda (x)
    (syntax-case x (while)
      ((k do-thing ... (while condition))
       (with-syntax
         ((break (datum->syntax #'k 'break))
          (continue (datum->syntax #'k 'continue)))
           #'(call/cc
             (lambda (break)
	       (let ((continue 0))
		 (call/cc (lambda (c) (set! continue c)))
		 do-thing ...
		 (if condition
		  (continue)
		  (void))))))))))

)
