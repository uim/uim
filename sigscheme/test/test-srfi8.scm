(load "./test/unittest.scm")

(receive (a b c)
	 (values #f #t '())
	 (assert-equal? "receive test 1" #f a)
	 (assert-equal? "receive test 2" #t b)
	 (assert-equal? "receive test 3" '() c))

(assert-equal? "receive test4"
               5
	       (receive (a b) (values 4 5)
                 b))

(assert-true "receive test5"
	     (receive args (values)
		      (null? args)))

(assert-true "receive test6"
	     (receive () (values)
		      #t))

(define var 'global)
(receive (a b c var)
	 (values 'a 6 var 'local)
	 (assert-equal? "receive test 7" 'a a)
	 (assert-equal? "receive test 8" 6 b)
	 (assert-equal? "receive test 9" 'global c)
	 (assert-equal? "receive test 10" 'local var))


(total-report)
