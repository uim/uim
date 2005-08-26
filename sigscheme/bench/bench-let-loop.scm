(define loop
  (lambda (i l)
    (let ((a 0)
	  (b 1)
	  (c 2))
      (if (< i l)
	  (loop (+ 1 i) l)
	  l))))

(write (loop 0 20000))
(newline)
