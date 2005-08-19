(load "test/unittest.scm")

(assert "receive test1"
	(receive (a b c)
		 (values #f #f #t)
		 (and (not a) (not b) c)))

(total-report)
