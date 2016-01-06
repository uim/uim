;;;
;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;; yes/no dialog for skk-purge-from-jisyo
;; this is just a quick hack derived from skk-editor.scm

(define-record 'skk-dialog
  '((context      '())
    (left-string  '())
    (right-string '())))
(define skk-dialog-new-internal skk-dialog-new)

(define skk-dialog-new
  (lambda (sc)
    (let ((dc (skk-dialog-new-internal)))
      (skk-dialog-set-context! dc sc)
      dc)))

(define skk-dialog-flush
  (lambda (dc)
    (skk-dialog-set-left-string! dc '())
    (skk-dialog-set-right-string! dc '())))

(define skk-dialog-make-string
  (lambda (sl dir)
    (if (not (null? sl))
	(if dir
	    (string-append (skk-dialog-make-string (cdr sl) dir)
			   (car sl))
	    (string-append (car sl)
			   (skk-dialog-make-string (cdr sl) dir)))
	(if dir
	    "Really purge? (yes/no) " 
	    ""))))

(define skk-dialog-get-left-string
  (lambda (dc)
    (skk-dialog-make-string
     (skk-dialog-left-string dc) #t)))

(define skk-dialog-get-right-string
  (lambda (dc)
    (skk-dialog-make-string
     (skk-dialog-right-string dc) #f)))

(define skk-dialog-commit-char-list
  (lambda (dc sl)
    (if (not (null? sl))
	(begin
	  (skk-dialog-set-left-string!
	   dc
	   (cons (car sl)
		 (skk-dialog-left-string dc)))
	  (skk-dialog-commit-char-list
	   dc (cdr sl))))))

(define skk-dialog-commit
  (lambda (dc str)
    (skk-dialog-commit-char-list
     dc (reverse (string-to-list str)))))
				 
(define skk-dialog-commit-raw
  (lambda (dc key key-state)
    (let ((raw-str (im-get-raw-key-str key key-state))
	  (sc (skk-dialog-context dc))
	  (str
	   (string-append
	    (skk-dialog-get-left-string dc)
	    (skk-dialog-get-right-string dc))))
      (if raw-str
	  (skk-dialog-commit dc raw-str)
	  ;; not a string
	  (and
	   (if (skk-backspace-key? key key-state)
	       (let ((cur (skk-dialog-left-string dc)))
		 (if (not (null? cur))
		     (skk-dialog-set-left-string!
		      dc (cdr cur)))
		 #f)
	       #t)
	   (if (skk-return-key? key key-state)
	       (begin
		 (if (string=? str "Really purge? (yes/no) yes")
		     (begin
		       (skk-purge-candidate sc)
		 	(skk-dialog-flush dc)
		 	(skk-context-set-child-context! sc '())
		 	(skk-context-set-child-type! sc '())))
		 (if (string=? str "Really purge? (yes/no) no")
		     (begin
		 	(skk-dialog-flush dc)
		 	(skk-context-set-child-context! sc '())
		 	(skk-context-set-child-type! sc '())))
		 #f)
	       #t)
	   (if (skk-cancel-key? key key-state)
	       (begin
		 (skk-dialog-flush dc)
		 (skk-context-set-child-context! sc '())
		 (skk-context-set-child-type! sc '())
		 #f)
	       #t)
	   )))))
