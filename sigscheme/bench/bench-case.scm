(define loop
  (lambda (i l)
    (case 6
      ((1 2 3 4 5) #f)
      ((6)
       (if (< i l)
	   (loop (+ 1 i) l)
	   l)))))

(write (loop 0 20000))
(newline)
