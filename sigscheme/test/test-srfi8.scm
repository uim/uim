(load "test/unittest.scm")

(assert "receive test1"
	(receive (a b c)
		 (values #f #f #t)
		 (and (not a) (not b) c)))
(assert-equal? "receive test2"
               5
	       (receive (a b) (values 4 5)
                 b))
(assert-true   "receive test3"
	       (receive args (values)
		 (null? args)))
(assert-true   "receive test4"
	       (receive () (values)
                 #t))

(total-report)
