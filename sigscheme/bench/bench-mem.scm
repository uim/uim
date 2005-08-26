(define *lifetime*  100)
(define *blocksize* 100)
  
(define *vec* (make-vector *lifetime*))

(define (foo i j)
  (if (< i *lifetime*)
      (begin
	(vector-set! *vec* i (make-vector *blocksize*))
	(foo (+ i 1) j))
      (if (< 0 j)
          (foo 0 (- j 1))
	  '())))

(write (foo 0 100))
(newline)
