;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/vector.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:39:09 1992                          */
;*    Last change :  Mon Jun  7 11:46:40 2004 (serrano)                */
;*                                                                     */
;*    On test les operations primitives sur les vecteurs               */
;*---------------------------------------------------------------------*/

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    Tvector optimization check                                       */
;*---------------------------------------------------------------------*/
(define *number-images* (vector #\0 #\1 #\2))
(define *foo*           (vector "toto" "toto"))

(define (prin-integer n)
   (let ((x (vector-ref *number-images* 2)))
      x))
 
(define (foo n)
   (vector-ref (if (equal? 5 n) *number-images* *foo*) 0)
   (prin-integer n)) 

;*---------------------------------------------------------------------*/
;*    test-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-vector)
   (test "vector?" (vector? '#()) #t)
   (test "vector?" (vector? '#(1)) #t)
   (test "ref" (vector-ref '#(1 2 3 4) 2) 3)
   (test "set" (let ((v (make-vector 1 '())))
		  (vector-set! v 0 'toto)
		  (vector-ref  v 0))
	 'toto)
   (test "length" (vector-length '#(1 2 3 4 5)) 5)
   (test "length" (vector-length (make-vector 5 'toto)) 5)
   (test "equal vector" (let ((v (make-vector 3 '())))
			   (vector-set! v 0 '(1 2 3))
			   (vector-set! v 1 '#(1 2 3))
			   (vector-set! v 2 'hello)
			   v)
	 '#((1 2 3) #(1 2 3) hello))
   (test "vector-fill" (let ((v (make-vector 3 1)))
			  (vector-fill! v 2)
			  (+ (vector-ref v 0)
			     (vector-ref v 1)
			     (vector-ref v 2)))
	 6)
   (test "tvector.1" (let ((t '#(1 2 3)))
			(vector-ref t 2))
	 3)
;   (test "tvector2"
;	 (string? (with-output-to-string
;		     (lambda ()
;			(print (make-array-of-int 1 1)))))
;	 #t)
   (test "vector-ref" (foo 10) #\2)
   (test "vector-ref" (vector-ref (let ((v (vector 0 1 2))) v) 2) 2))

(test-vector)

(total-report)
