(define total
  (lambda (n val)
    (if (= n 0)
	val
	(total (- n 1) (+ val n)))))

(print (total 10 0))
