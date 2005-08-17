;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/kwote.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:22:02 1992                          */
;*    Last change :  Fri Jul  6 09:37:50 2001 (serrano)                */
;*                                                                     */
;*    On test l'expansion des kwote                                    */
;*---------------------------------------------------------------------*/

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    test-quote ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-quote)
   (test "quote" `(list ,(+ 1 2) 4) '(list 3 4))
   (test "quote" (let ((name 'a)) `(list ,name ',name)) '(list a (quote a)))
   (test "quote" `(a ,(+ 1 2) ,@(map (lambda (x) (+ 10 x))
				     '(4 -5 6))
		     b)
	 '(a 3 14 5 16 b))
   (test "quote" `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '((cons))))
	 '((foo 7) cons))
   (test "quote" `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
	 '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
   (test "quote" (let ((name1 'x)
		       (name2 'y))
		    `(a `(b ,,name1 ,',name2 d) e))
	 '(a `(b ,x ,'y d) e))
   (test "quote" (quasiquote (list (unquote (+ 1 2)) 4))
	 '(list 3 4))
   (test "quote" '(quasiquote (list (unquote (+ 1 2)) 4))
	 '`(list ,(+ 1 2) 4))
   (test "quote" `#(1 2 ,(+ 1 2) ,(+ 2 2))
	 '#(1 2 3 4))
   (test "quote" `#(1 2 ,(+ 1 2) ,@(map (lambda (x) (+ 1 x)) '(3 4)) 6)
	 '#(1 2 3 4 5 6)))
 
(test-quote)

(total-report)
