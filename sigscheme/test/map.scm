(print (map cadr '((1 2) (1 2) (1 2))))
(print (map + '(1 2 3) '(1 2 3)))
(print (map (lambda (x y) (+ x y))
	    '(1 2 3) '(1 2 3)))
(print (map print '(1 2 3)))

(print (map print '(1 2 3)))
(print (map (lambda (x) (+ x x)) '(1 2 3)))
