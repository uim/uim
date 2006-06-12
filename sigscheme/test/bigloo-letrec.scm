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
;*    serrano/prgm/project/bigloo/recette/letrec.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 19:18:37 1992                          */
;*    Last change :  Fri Jul  6 09:38:02 2001 (serrano)                */
;*                                                                     */
;*    On test `letrec'                                                 */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

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
