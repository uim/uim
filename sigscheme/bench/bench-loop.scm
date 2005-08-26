(define loop
  (lambda (i l)
    (if (< i l)
	(loop (+ 1 i) l)
	l)))

(write (loop 0 8000))
(newline)
