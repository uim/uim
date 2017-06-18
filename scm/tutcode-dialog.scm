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

;; yes/no dialog for tutcode-purge-candidate
;; this is just a quick hack derived from skk-editor.scm

(define-record 'tutcode-dialog
  '((context      ())
    (left-string  ())
    (right-string ())))
(define tutcode-dialog-new-internal tutcode-dialog-new)

(define tutcode-dialog-new
  (lambda (sc)
    (let ((dc (tutcode-dialog-new-internal)))
      (tutcode-dialog-set-context! dc sc)
      dc)))

(define tutcode-dialog-flush
  (lambda (dc)
    (tutcode-dialog-set-left-string! dc ())
    (tutcode-dialog-set-right-string! dc ())))

(define tutcode-dialog-make-string
  (lambda (sl dir)
    (if (null? sl)
        (if dir
            "Really purge? (yes/no) " 
            "")
	(if dir
	    (string-append (tutcode-dialog-make-string (cdr sl) dir)
			   (car sl))
	    (string-append (car sl)
			   (tutcode-dialog-make-string (cdr sl) dir))))))

(define tutcode-dialog-get-left-string
  (lambda (dc)
    (tutcode-dialog-make-string
     (tutcode-dialog-left-string dc) #t)))

(define tutcode-dialog-get-right-string
  (lambda (dc)
    (tutcode-dialog-make-string
     (tutcode-dialog-right-string dc) #f)))

(define tutcode-dialog-commit-char-list
  (lambda (dc sl)
    (if (not (null? sl))
	(begin
	  (tutcode-dialog-set-left-string!
	   dc
	   (cons (car sl)
		 (tutcode-dialog-left-string dc)))
	  (tutcode-dialog-commit-char-list
	   dc (cdr sl))))))

(define tutcode-dialog-commit
  (lambda (dc str)
    (tutcode-dialog-commit-char-list
     dc (reverse (string-to-list str)))))
				 
(define tutcode-dialog-commit-raw
  (lambda (dc key key-state)
    (let ((raw-str (im-get-raw-key-str key key-state))
	  (sc (tutcode-dialog-context dc))
	  (str
	   (string-append
	    (tutcode-dialog-get-left-string dc)
	    (tutcode-dialog-get-right-string dc))))
      (if raw-str
	  (tutcode-dialog-commit dc raw-str)
	  ;; not a string
	  (cond
            ((tutcode-backspace-key? key key-state)
              (let ((cur (tutcode-dialog-left-string dc)))
               (if (not (null? cur))
                   (tutcode-dialog-set-left-string! dc (cdr cur)))))
	    ((tutcode-return-key? key key-state)
              (cond
                ((string=? str "Really purge? (yes/no) yes")
                  (tutcode-purge-candidate sc)
                  (tutcode-dialog-flush dc)
                  (tutcode-context-set-child-context! sc ())
                  (tutcode-context-set-child-type! sc ())
                  (tutcode-update-preedit sc))
                ((string=? str "Really purge? (yes/no) no")
                  (tutcode-dialog-flush dc)
                  (tutcode-context-set-child-context! sc ())
                  (tutcode-context-set-child-type! sc ())
                  (tutcode-update-preedit sc))))
            ((tutcode-cancel-key? key key-state)
              (tutcode-dialog-flush dc)
              (tutcode-context-set-child-context! sc ())
              (tutcode-context-set-child-type! sc ())
              (tutcode-update-preedit sc)))))))
