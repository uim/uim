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
;*    serrano/prgm/project/bigloo/recette/vector.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:39:09 1992                          */
;*    Last change :  Mon Jun  7 11:46:40 2004 (serrano)                */
;*                                                                     */
;*    On test les operations primitives sur les vecteurs               */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

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
