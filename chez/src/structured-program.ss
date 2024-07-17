;#lang racket
(library (structured-program)
(export prog)
(import (scheme))
(import (function-lib))


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
			 (cut-result (cut (lambda (x) (eq? (car x) 'define)) all-code))
			 (defines (car cut-result))
			 (body (cdr cut-result)))
		    (if (null? defines)
			(datum->syntax #'k (trans-program body))
			(datum->syntax #'k
				       `(let () ,@defines ,(trans-program body)))))))))


)


