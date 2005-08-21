(load "test/unittest.scm")

;; check delay and force
(assert-equal? "delay-force check" 6 (begin
				       (define foo (delay
						     (+ 1 2 3)))
				       (force foo)))

(total-report)
