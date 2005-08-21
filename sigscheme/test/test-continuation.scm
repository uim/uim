(load "test/unittest.scm")

(assert-equal? "call/cc test1" -3  (call-with-current-continuation
				 (lambda (exit)
				   (for-each (lambda (x)
					       (if (negative? x)
						   (exit x)))
					     '(54 0 37 -3 245 19))
				   #t)))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
     (lambda (return)
       (letrec ((re
		 (lambda (obj1)
		   (cond ((null? obj1) 0)
			 ((pair? obj1)
			  (+ (re (cdr obj1)) 1))
			 (else
			  (return #f))))))
      (re obj))))))

(assert-equal? "call/cc test2" 4  (list-length '(1 2 3 4)))
(assert-equal? "call/cc test3" #f (list-length '(a b . c)))

(total-report)
