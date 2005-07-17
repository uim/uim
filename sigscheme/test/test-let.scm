(load "test/unittest.scm")

(assert-eq? "basic let test1" 0 (let ((n 0))
				 n))

(assert-eq? "basic let test2" 1 (let ((n 0))
				  (set! n 1)))

(define count
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1)))))

(assert-eq? "lexical scope test1" 1 (count))
(assert-eq? "lexical scope test2" 2 (count))

(total-report)
