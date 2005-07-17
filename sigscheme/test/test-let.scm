(load "test/unittest.scm")

(assert-eq? "basic let test1" 0 (let ((n 0))
				 n))

(assert-eq? "basic let test2" 1 (let ((n 0))
				  (set! n 1)))

(assert-eq? "basic let test3" 1 (let ((n 0))
				  (set! n (+ n 1))))

(assert-eq? "basic let test4" 3 (let ((n1 2)
				      (n2 1))
				  (+ n1 n2)))

(assert-eq? "basic let* test1" 70 (let ((x 2) (y 3))
				    (let* ((x 7)
					   (z (+ x y)))
				      (* z x))))

(assert-eq? "basic letrec test1" #t (let ((even?
					   (lambda (n)
					     (if (zero? n)
						 #t
						 (odd? (- n 1)))))
					  (odd?
					   (lambda (n)
					     (if (zero? n)
						 #f
						 (even? (- n 1))))))
				      (even? 88)))


(define count
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1)))))

(assert-eq? "lexical scope test1" 1 (count))
(assert-eq? "lexical scope test2" 2 (count))

(total-report)
