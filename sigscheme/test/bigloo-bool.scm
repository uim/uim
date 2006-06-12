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
;*    serrano/prgm/project/bigloo/recette/bool.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:16:12 1992                          */
;*    Last change :  Wed Apr  1 14:05:49 1998 (serrano)                */
;*                                                                     */
;*    On test les operations booleenes.                                */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

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
