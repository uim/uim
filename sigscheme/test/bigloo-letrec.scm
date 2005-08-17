;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/letrec.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 19:18:37 1992                          */
;*    Last change :  Fri Jul  6 09:38:02 2001 (serrano)                */
;*                                                                     */
;*    On test `letrec'                                                 */
;*---------------------------------------------------------------------*/

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1 y)
   (letrec ((x (number->string y))
	    (foo (lambda (string)
		    (string->symbol (string-append string x)))))
      foo))

;*---------------------------------------------------------------------*/
;*    plante1                                                          */
;*    -------------------------------------------------------------    */
;*    un test qui plantait a la compilation                            */
;*---------------------------------------------------------------------*/
(define (foo a)
   (letrec ((foo (lambda (x) (bar 0) (set! foo 8) 'done))
	    (bar (lambda (x) (if (= x 0)
				 'done
				 (foo x)))))
      (foo a)))

;*---------------------------------------------------------------------*/
;*    test-letrec ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-letrec)
   (test "letrec" ((test1 1) "TOTO") 'TOTO1)
   (test "letrec" (foo 10) 'done)
   (test "delay"  (procedure? (letrec ((foo (delay foo))) (force foo))) #t))

(test-letrec)

(total-report)
