(define (test f g n)
  (if (= n 0)
      f
      (let ((m (- n 1)))
	((f g f m) f g m)
	((g f g m) g f m)
	g)))

(test test test 10)
