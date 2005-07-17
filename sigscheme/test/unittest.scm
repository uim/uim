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
	    (print "[ ERROR !! ]\n")
	    (print total-err-num)
	    (print "\n"))))))

(define report-error
  (lambda (errmsg)
    (begin
      (print "error")
      (print errmsg))))

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
    (assert msg (eq? a b))))
