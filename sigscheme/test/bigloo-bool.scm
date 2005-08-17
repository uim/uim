;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/bool.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:16:12 1992                          */
;*    Last change :  Wed Apr  1 14:05:49 1998 (serrano)                */
;*                                                                     */
;*    On test les operations booleenes.                                */
;*---------------------------------------------------------------------*/

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    predicat ...                                                     */
;*---------------------------------------------------------------------*/
(define (predicat x)
   (> x 5))

;*---------------------------------------------------------------------*/
;*    faux-predicat ...                                                */
;*---------------------------------------------------------------------*/
(define (faux-predicat x)
   (> x 5))

;*---------------------------------------------------------------------*/
;*    encore-faux ...                                                  */
;*---------------------------------------------------------------------*/
(define (encore-faux x)
   (> x 5))

;*---------------------------------------------------------------------*/
;*    local-pred-1 ...                                                 */
;*---------------------------------------------------------------------*/
(define (local-pred-1 x)
   (let ((pred (lambda (x) (< x 3))))
      (if (pred x) #t #f)))

;*---------------------------------------------------------------------*/
;*    local-pred-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (local-pred-2 x)
  (let* ((foo (lambda (x) (< x 3)))
	 (bar (lambda (x) (if (foo x) 3 4)))
	 (gee (lambda (x) (if (foo x) 3 4))))
    bar
    gee
    (if (foo x) #t #f)))

;*---------------------------------------------------------------------*/
;*    local-pred-3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (local-pred-3 x)
  (let ((pred (lambda (x) (< x 3))))
    (pred x)))

;*---------------------------------------------------------------------*/
;*    test-bool ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-bool)
   (test "or" (or #f #f) #f)
   (test "not" (not #f) #t)
   (test "and" (and #t #t) #t)
   (test "and" (and #t #f) #f)
   (test "if" (let ((x 1)) (if x x)) 1)
   (test "ifnot" (let ((x 1)) (if (not x) #t #f)) #f)
   (test "ifor" (let ((x 1) (y #f)) (if (or x y) x y)) 1)
   (test "ifand" (let ((x 1) (y #f)) (if (and x y) #t #f)) #f)
   (test "pred" (if (predicat 6) #t #f) #t)
   (test "faux" (if (faux-predicat 6) (faux-predicat 7) (faux-predicat 3)) #t)
   (test "encore-faux" (if (encore-faux 6) #t #f) #t)
   (test "local-pred-1" (local-pred-1 1) #t)
   (test "local-pred-2" (local-pred-2 1) #t)
   (test "local-pred-3" (if (local-pred-3 1) #t #f) #t))

(test-bool)

(total-report)
