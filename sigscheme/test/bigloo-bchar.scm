;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/bchar.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  1 09:29:00 1992                          */
;*    Last change :  Tue May  7 14:38:22 2002 (serrano)                */
;*                                                                     */
;*    On test les caracteres                                           */
;*---------------------------------------------------------------------*/

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    test-char ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-char)
   (test "char?" (char? #\a) #t)
   (test "char?" (char? 1) #f)
   (test "char=?" (let ((x #\a)) (char=? x #\a)) #t)
   (test "char=?" (let ((x #\b)) (char=? x #\a)) #f)
;   (test "char<?" (let ((x #\b)) (char<? x #\a)) #f)
;   (test "char<?" (let ((x #\b)) (char<? #\a x)) #t)
;   (test "char>?" (let ((x #\b)) (char>? x #\a)) #t)
;   (test "char>?" (let ((x #\b)) (char>? #\a x)) #f)
;   (let ((s "été"))
;      (test "char>?" (char>? (string-ref s 0) #a127) #t))
;   (test "char-ci=?" (let ((x #\A)) (char-ci=? x #\a)) #t)
;   (test "char-ci=?" (let ((x #\B)) (char-ci=? x #\a)) #f)
;   (test "char-ci<?" (let ((x #\B)) (char-ci<? x #\a)) #f)
;   (test "char-ci<?" (let ((x #\B)) (char-ci<? #\a x)) #t)
;   (test "char-ci>?" (let ((x #\B)) (char-ci>? x #\a)) #t)
;   (test "char-ci>?" (let ((x #\B)) (char-ci>? #\a x)) #f)
   (test "char-alphabetic?" (char-alphabetic? #\a) #t)
   (test "char-alphabetic?" (char-alphabetic? #\0) #f)
   (test "char-numeric?" (char-numeric? #\a) #f)
   (test "char-numeric?" (char-numeric? #\0) #t)
   (test "char-whitespace?" (char-whitespace? #\a) #f)
   (test "char-whitespace?" (char-whitespace? #\space) #t)
   (test "char-upper-case?" (char-upper-case? #\A) #t)
   (test "char-upper-case?" (char-upper-case? #\a) #f)
   (test "char-lower-case?" (char-lower-case? #\A) #f)
   (test "char-lower-case?" (char-lower-case? #\a) #t)
   (test "char->integer" (char->integer #\0) 48)
;   (test "char->integer" (char->integer #a200) 200)
   (test "integer->char" (integer->char 48) #\0)
   (test "char-upcase" (char-upcase #\a) #\A)
   (test "char-upcase" (char-upcase #\A) #\A)
   (test "char-downcase" (char-downcase #\a) #\a)
   (test "char-downcase" (char-downcase #\A) #\a)
   (test "unsigned char" (char->integer (integer->char 128)) 128))

(test-char)
			    
(total-report)
