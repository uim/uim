(load "./test/unittest.scm")

(define test
  (lambda (msg ret func)
    (assert-equal? msg ret (func))))
