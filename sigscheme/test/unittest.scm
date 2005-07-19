(define total-err-num  0)
(define total-test-num 0)
(define test-filename "unspecified")

(define total-report
  (lambda ()
    (begin
;      (print "total")
;      (print total-test-num)
      (if (= total-err-num 0)
	  (print "OK\n")
	  (begin
	    (print "[ ERROR NUM ]\n")
	    (print total-err-num)
	    (print "\n"))))))

(define report-error
  (lambda (errmsg)
    (begin
      (print "error : ")
      (print errmsg)
      (print "\n"))))

(define assert
  (lambda (msg exp)
    (begin
      (set! total-test-num (+ total-test-num 1))
      (if (exp)
	  #t
	  (begin
	    (set! total-err-num (+ total-err-num 1))
	    (report-error msg)
	    #f)))))

(define assert-eq?
  (lambda (msg a b)
    (if (not (assert msg (eq? a b)))
	(begin
	  (print "assert-eq? : we expect ")
	  (print a)
	  (print " but got ")
	  (print b)
	  (print "\n")))))

(define assert-equal?
  (lambda (msg a b)
    (if (not (assert msg (equal? a b)))
	(begin
	  (print "assert-equal? : we expect ")
	  (print a)
	  (print " but got ")
	  (print b)
	  (print "\n")))))
