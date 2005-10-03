(load "./test/unittest.scm")

; with-exception-handler
(with-exception-handler
 (lambda (x)
   (assert-equal? "with-exception-handler test #1" 'an-error x))
 (lambda ()
   (+ 1 (raise 'an-error))))

(assert-equal? "with-exception-handler test #2" 6
	       (with-exception-handler
		(lambda (x)
		  'not-reaches-here)
		(lambda ()
		  (+ 1 2 3))))

(assert-equal? "with-exception-handler test #2" 'success
	       (with-exception-handler
		(lambda (x)
		  'not-reaches-here)
		(lambda ()
		  'success)))


; guard
(assert-equal? "guard test #1" 'exception
	       (guard (condition
		       (else
			(assert-equal? "guard test #2" 'an-error condition)
			'exception))
		      (+ 1 (raise 'an-error))))

(assert-equal? "guard test #3" 3
	       (guard (condition
		       (else
			'exception))
		      (+ 1 2)))

(assert-equal? "guard test #4" 'success
	       (guard (condition
		       (else
			'exception))
		      'success))

(assert-equal? "guard test #5" 'exception
	       (guard (condition
		       (else
			'exception))
		      (+ 1 (raise 'error))))

(assert-equal? "guard test #6" 42
  (guard (condition
	  ((assq 'a condition) => cdr)
	  ((assq 'b condition))
	  (else
	   (display condition)
	   (newline)))
  (raise (list (cons 'a 42)))))

(assert-equal? "guard test #7" '(b . 23)
  (guard (condition
	  ((assq 'a condition) => cdr)
	  ((assq 'b condition))
	  (else
	   (display condition)
	   (newline)))
	 (raise (list (cons 'b 23)))))


; mixed use of with-exception-handler and guard
(assert-equal? "mixed exception handling test #1" 'guard-ret
	       (with-exception-handler (lambda (x)
					 (k 'with-exception-ret))
				       (lambda ()
					 (guard (condition
						 (else
						  'guard-ret))
						(raise 1)))))

(assert-equal? "mixed exception handling test #1" 'with-exception-ret
	       (with-exception-handler (lambda (x)
					 'with-exception-ret)
				       (lambda ()
					 (guard (condition
						 ((negative? condition)
						  'guard-ret))
						(raise 1)))))

;(assert-equal? "mixed exception handling test #1" 'positive
;  (call-with-current-continuation
;   (lambda (k)
;     (with-exception-handler (lambda (x)
;			       (k 'zero))
;			     (lambda ()
;			       (guard (condition
;				       ((positive? condition) 'positive)
;				       ((negative? condition) 'negative))
;				      (raise 1)))))))
;
;(assert-equal? "mixed exception handling test #2" 'negative
;  (call-with-current-continuation
;   (lambda (k)
;     (with-exception-handler (lambda (x)
;			       (k 'zero))
;			     (lambda ()
;			       (guard (condition
;				       ((positive? condition) 'positive)
;				       ((negative? condition) 'negative))
;				      (raise -1)))))))

;(assert-equal? "mixed exception handling test #3" 'zero
;  (call-with-current-continuation
;   (lambda (k)
;     (with-exception-handler (lambda (x)
;			       (k 'zero))
;			     (lambda ()
;			       (guard (condition
;				       ((positive? condition) 'positive)
;				       ((negative? condition) 'negative))
;				      (raise 0)))))))


(total-report)
