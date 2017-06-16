;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

;; SKKと同じ再帰学習をインラインで行うための簡易テキストエディタ。
;; skk-editor.scmからコピーしてtutcode.scm用に変更。

(define-record 'tutcode-editor
  '((context      ())
    (left-string  ())
    (right-string ())))
(define tutcode-editor-new-internal tutcode-editor-new)

(define tutcode-editor-new
  (lambda (sc)
    (let ((ec (tutcode-editor-new-internal)))
      (tutcode-editor-set-context! ec sc)
      ec)))

(define tutcode-editor-flush
  (lambda (ec)
    (tutcode-editor-set-left-string! ec ())
    (tutcode-editor-set-right-string! ec ())))

(define tutcode-editor-make-string
  (lambda (sl dir)
    (if (null? sl)
        ""
	(if dir
	    (string-append (tutcode-editor-make-string (cdr sl) dir)
			   (car sl))
	    (string-append (car sl)
			   (tutcode-editor-make-string (cdr sl) dir))))))

(define tutcode-editor-get-left-string
  (lambda (ec)
    (tutcode-editor-make-string
     (tutcode-editor-left-string ec) #t)))

(define tutcode-editor-get-right-string
  (lambda (ec)
    (tutcode-editor-make-string
     (tutcode-editor-right-string ec) #f)))

(define tutcode-editor-commit-char-list
  (lambda (ec sl)
    (if (not (null? sl))
	(begin
	  (tutcode-editor-set-left-string!
	   ec
	   (cons (car sl)
		 (tutcode-editor-left-string ec)))
	  (tutcode-editor-commit-char-list
	   ec (cdr sl))))))

(define tutcode-editor-commit
  (lambda (ec str)
    (tutcode-editor-commit-char-list
     ec (reverse (string-to-list str)))))

(define tutcode-editor-commit-raw
  (lambda (ec key key-state)
    (let ((raw-str (im-get-raw-key-str key key-state))
	  (sc (tutcode-editor-context ec))
	  (str
	   (string-append
	    (tutcode-editor-get-left-string ec)
	    (tutcode-editor-get-right-string ec))))
      (if raw-str
	  (tutcode-editor-commit ec raw-str)
	  ;; not a string
	  (cond
            ((tutcode-backspace-key? key key-state)
              (let ((cur (tutcode-editor-left-string ec)))
                (if (not (null? cur))
                    (tutcode-editor-set-left-string! ec (cdr cur)))))
	    ((generic-go-left-key? key key-state)
              (let ((cur (tutcode-editor-left-string ec)))
                (if (not (null? cur))
                    (begin
                      (tutcode-editor-set-left-string! ec (cdr cur))
                      (tutcode-editor-set-right-string!
                       ec (cons (car cur) (tutcode-editor-right-string ec)))))))
	    ((generic-go-right-key? key key-state)
              (let ((cur (tutcode-editor-right-string ec)))
                (if (not (null? cur))
                    (begin
                      (tutcode-editor-set-right-string! ec (cdr cur))
                      (tutcode-editor-set-left-string!
                       ec (cons (car cur) (tutcode-editor-left-string ec)))))))
	    ((tutcode-return-key? key key-state)
              (if (< 0 (string-length str))
                  (begin
                    (skk-lib-learn-word
                      tutcode-dic
                      (cons (string-list-concat (tutcode-context-head sc)) "")
                      ""
                      str
                      #f)
                    (tutcode-save-personal-dictionary #t)
                    (tutcode-commit-editor-context sc str))
                  (begin
                    (tutcode-editor-flush ec)
                    (tutcode-context-set-child-context! sc '())
                    (tutcode-context-set-child-type! sc '())
                    (if (> (tutcode-context-nr-candidates sc) 0)
                        (tutcode-back-to-converting-state sc)
                        (tutcode-back-to-yomi-state sc)))))
	    ((tutcode-cancel-key? key key-state)
              (tutcode-editor-flush ec)
              (tutcode-context-set-child-context! sc '())
              (tutcode-context-set-child-type! sc '())
              (if (> (tutcode-context-nr-candidates sc) 0)
                  (tutcode-back-to-converting-state sc)
                  (tutcode-back-to-yomi-state sc)))
	  )))))
