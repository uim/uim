(define loop
  (lambda (i l)
    (let ((a 0)
	  (b 1)
	  (c 2))
      (if (< i l)
	  (loop (+ 1 i) l)
	  l))))

(print (loop 0 20000))
