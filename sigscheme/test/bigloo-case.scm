;;    A practical implementation for the Scheme programming language   
;;                                                                     
;;                                    ,--^,                            
;;                              _ ___/ /|/                             
;;                          ,;'( )__, ) '                              
;;                         ;;  //   L__.                               
;;                         '   \\   /  '                               
;;                              ^   ^                                  
;;                                                                     
;;               Copyright (c) 1992-2004 Manuel Serrano                
;;                                                                     
;;     Bug descriptions, use reports, comments or suggestions are      
;;     welcome. Send them to                                           
;;       bigloo@sophia.inria.fr                                        
;;       http://www.inria.fr/mimosa/fp/Bigloo                                 
;;                                                                     
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by 
;;   the Free Software Foundation; either version 2 of the License, or 
;;   (at your option) any later version. More precisely,
;;
;;      - The compiler and the tools are distributed under the terms of the
;;      GNU General Public License.
;;
;;      - The Bigloo run-time system and the libraries are distributed under 
;;      the terms of the GNU Library General Public License. The source code
;;      of the Bigloo runtime system is located in the ./runtime directory.
;;      The source code of the FairThreads library is located in the
;;      ./fthread directory.
;;
;;   This program is distributed in the hope that it will be useful,   
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of    
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     
;;   GNU General Public License for more details.                      
;;                                                                     
;;   You should have received a copy of the GNU General Public         
;;   License along with this program; if not, write to the Free        
;;   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,   
;;   MA 02111-1307, USA.                                               

;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/case.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 18 15:16:39 1992                          */
;*    Last change :  Mon May 19 06:11:19 2003 (serrano)                */
;*                                                                     */
;*    On test le case.                                                 */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

(load "./test/unittest-bigloo.scm")

;*---------------------------------------------------------------------*/
;*    test 1.                                                          */
;*---------------------------------------------------------------------*/
(define (test1 x)
   (case x
      ((tutu) 'tutu)
      ((toto) 'toto)
      (else
       'dummy
       'else)))

;*---------------------------------------------------------------------*/
;*    test 2.                                                          */
;*---------------------------------------------------------------------*/
(define (test2 x)
   (case (begin 1 2 x)
      ((tete tutu tyty) 1)
      ((toto) 2)
      ((tata) 3)))

;*---------------------------------------------------------------------*/
;*    test 3.                                                          */
;*---------------------------------------------------------------------*/
(define (test3 x)
   (case (begin 2 3 x)
      ((1 2 3 4) 1)
      ((5 6 7 8) 5)
      (else      0)))

;*---------------------------------------------------------------------*/
;*    test 4.                                                          */
;*---------------------------------------------------------------------*/
(define (test4 x)
   (case x
      ((tutu 1) "tutu ou 1")
      ((2 3)    "2 ou 3") 
      (else     "else")))

;*---------------------------------------------------------------------*/
;*    test 5.                                                          */
;*    -------------------------------------------------------------    */
;*    Ce test est important car il permet de tester la compilation     */
;*    des cases qui comportent des symboles qui ont meme nombre de     */
;*    hash                                                             */
;*---------------------------------------------------------------------*/
(define (test5 x)
   (case x
      ((SHOW show)
       'show)
      ((compute!)
       'compute!)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    test 6.                                                          */
;*---------------------------------------------------------------------*/
(define (test6 x)
   (case x
      ((#\o)
       #\o)
      ((#\d)
       #\d)
      ((#\x)
       #\x)))

;*---------------------------------------------------------------------*/
;*    test 7. ...                                                      */
;*---------------------------------------------------------------------*/
(define (test7 x)
   ;; set! et fibo on meme nombre de hash
   (case x
      ((set!)
       'set!)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    test.8                                                           */
;*    -------------------------------------------------------------    */
;*    This test used to make the compiler crashes.                     */
;*---------------------------------------------------------------------*/
;(define-macro push!
;   (lambda (stack o)
;      `(begin
;	  (set! ,stack (cons ,o ,stack))
;	  ,o)))
;
;(define (test.8 data)
;   (let ((elem-stack '()))
;      (push! elem-stack
;	     (read/rp (regular-grammar ()
;			 (else 2))
;		      (open-input-string data)))
;     elem-stack))

;*---------------------------------------------------------------------*/
;*    test-case ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-case)
   (test "case symbol" (test1 'tutu) 'tutu)
   (test "case symbol" (test1 'toto) 'toto)
   (test "case symbol" (test1 'tata) 'else)
   (test "case symbol" (test1 5) 'else)
   (test "case symbol" (test2 'tutu) 1)
   (test "case symbol" (test2 'toto) 2)
   (test "case symbol" (test2 'tata) 3)
;   (test "case symbol" (test2 5) #unspecified)
   (test "case integer" (test3 (+ 1 2)) 1)
   (test "case integer" (test3 'toto) 0)
   (test "case mixte" (test4 'tutu) "tutu ou 1")
   (test "case mixte" (test4 1) "tutu ou 1")
   (test "case mixte "(test4 3) "2 ou 3")
   (test "case mixte" (test4 'titi) "else")
   (test "case hash" (test5 'show) 'show)
   (test "case hash" (test5 'SHOW) 'show)
   (test "case hash" (test5 'compute!) 'compute!)
   (test "case hash" (test5 'toto) 'else)
;   (test "case char"  (test6 #\a) #unspecified)
   (test "case char" (test6 #\x) #\x)
   (test "case char" (test6 (string-ref "o" 0)) #\o)
   (test "case hash" (test7 'fibo) 'else))
   
(test-case)

(total-report)
