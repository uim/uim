#! /usr/bin/env sscm -C ISO-8859-1

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
;*    serrano/prgm/project/bigloo/recette/bchar.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  1 09:29:00 1992                          */
;*    Last change :  Tue May  7 14:38:22 2002 (serrano)                */
;*                                                                     */
;*    On test les caracteres                                           */
;*---------------------------------------------------------------------*/

;; ChangeLog
;;
;; 2005-08-18 kzk     Copied from Bigloo 2.6e and adapted to SigScheme

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
;   (test "char->integer" (char->integer #\0) 48)
;   (test "char->integer" (char->integer #a200) 200)
;   (test "integer->char" (integer->char 48) #\0)
   (test "char-upcase" (char-upcase #\a) #\A)
   (test "char-upcase" (char-upcase #\A) #\A)
   (test "char-downcase" (char-downcase #\a) #\a)
   (test "char-downcase" (char-downcase #\A) #\a)
;   (test "unsigned char" (char->integer (integer->char 128)) 128)
   )

(test-char)
			    
(total-report)
