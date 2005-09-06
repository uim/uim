(define total-err-num  0)
(define total-test-num 0)
(define test-filename "unspecified")

(define total-report
  (lambda ()
    (begin
      (if (= total-err-num 0)
	  (begin
	    (display "OK")
	    (newline))
	  (begin
	    (display "[ ERROR NUM : ")
	    (display total-err-num)
	    (display " ]")
	    (newline))))))

(define report-error
  (lambda (errmsg)
    (begin
      (display "error : ")
      (display errmsg)
      (newline))))

(define assert
  (lambda (msg exp)
    (set! total-test-num (+ total-test-num 1))
    (if exp
	#t
	(begin
	  (set! total-err-num (+ total-err-num 1))
	  (report-error msg)
	  #f))))

(define assert-true assert)

(define assert-false
  (lambda (msg exp)
    (assert msg (not exp))))

(define assert-eq?
  (lambda (msg a b)
    (if (not (assert msg (eq? a b)))
	(begin
	  (display "assert-eq? : we expect ")
	  (write a)
	  (display " but got ")
	  (write b)
	  (newline)))))

(define assert-equal?
  (lambda (msg a b)
    (if (not (assert msg (equal? a b)))
	(begin
	  (display "assert-equal? : we expect ")
	  (write a)
	  (display " but got ")
	  (write b)
	  (newline)))))

;; dummy definition to eval args for assert-error. real implementation needed.
(define assert-error
  (lambda (msg exp)
    #f))
