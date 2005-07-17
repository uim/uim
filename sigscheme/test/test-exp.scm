(load "test/unittest.scm")

;; case
(assert-eq? "case check" #t (case (* 2 3)
			      ((2 3 4 7)   #f)
			      ((1 4 6 8 9) #t)))

(assert-eq? "case else check" 'elseworks (case 1
					   ((3) 'a)
					   ((4) 'b)
					   (else
					    'elseworks)))

(total-report)
