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
;*    serrano/prgm/project/bigloo/recette/kwote.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:22:02 1992                          */
;*    Last change :  Fri Jul  6 09:37:50 2001 (serrano)                */
;*                                                                     */
;*    On test l'expansion des kwote                                    */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

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
