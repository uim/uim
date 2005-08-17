(define total-err-num  0)
(define total-test-num 0)
(define test-filename "unspecified")

(define total-report
  (lambda ()
    (begin
;      (print "total")
;      (print total-test-num)
      (if (= total-err-num 0)
	  (print "OK")
	  (begin
	    (print "[ ERROR NUM ]")
	    (print total-err-num))))))

(define report-error
  (lambda (errmsg)
    (begin
      (print "error : ")
      (print errmsg))))

(define assert
  (lambda (msg exp)
    (set! total-test-num (+ total-test-num 1))
    (if exp
	#t
	(begin
	  (set! total-err-num (+ total-err-num 1))
	  (report-error msg)
	  #f))))

(define assert-eq?
  (lambda (msg a b)
    (if (not (assert msg (eq? a b)))
	(begin
	  (print "assert-eq? : we expect ")
	  (print a)
	  (print " but got ")
	  (print b)))))

(define assert-equal?
  (lambda (msg a b)
    (if (not (assert msg (equal? a b)))
	(begin
	  (print "assert-equal? : we expect ")
	  (print a)
	  (print " but got ")
	  (print b)))))
