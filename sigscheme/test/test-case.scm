(load "test/unittest.scm")

(assert-eq? "basic case check1" 'case1 (case 1
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-eq? "basic case check2" 'case2 (case 2
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-eq? "basic else check"  'caseelse (case 3
					((1) 'case1)
					((2) 'case2)
					(else
					 'caseelse)))

(total-report)
