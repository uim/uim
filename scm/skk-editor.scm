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

;; SKKの再帰学習をインラインで行うための簡易テキストエディタ
;;

(define-record 'skk-editor
  '((context      '())
    (left-string  '())
    (right-string '())))
(define skk-editor-new-internal skk-editor-new)

(define skk-editor-new
  (lambda (sc)
    (let ((ec (skk-editor-new-internal)))
      (skk-editor-set-context! ec sc)
      ec)))

(define skk-editor-flush
  (lambda (ec)
    (skk-editor-set-left-string! ec '())
    (skk-editor-set-right-string! ec '())))

(define skk-editor-make-string
  (lambda (sl dir)
    (if (not (null? sl))
	(if dir
	    (string-append (skk-editor-make-string (cdr sl) dir)
			   (car sl))
	    (string-append (car sl)
			   (skk-editor-make-string (cdr sl) dir)))
	"")))

(define skk-editor-get-left-string
  (lambda (ec)
    (skk-editor-make-string
     (skk-editor-left-string ec) #t)))

(define skk-editor-get-right-string
  (lambda (ec)
    (skk-editor-make-string
     (skk-editor-right-string ec) #f)))

(define skk-editor-commit-char-list
  (lambda (ec sl)
    (if (not (null? sl))
	(begin
	  (skk-editor-set-left-string!
	   ec
	   (cons (car sl)
		 (skk-editor-left-string ec)))
	  (skk-editor-commit-char-list
	   ec (cdr sl))))))

(define skk-editor-commit
  (lambda (ec str)
    (skk-editor-commit-char-list
     ec (reverse (string-to-list str)))))
				 
(define skk-editor-commit-raw
  (lambda (ec key key-state)
    (let ((raw-str (im-get-raw-key-str key key-state))
	  (sc (skk-editor-context ec))
	  (str
	   (string-append
	    (skk-editor-get-left-string ec)
	    (skk-editor-get-right-string ec))))
      (if raw-str
	  (skk-editor-commit ec raw-str)
	  ;; not a string
	  (and
	   (if (skk-backspace-key? key key-state)
	       (let ((cur (skk-editor-left-string ec)))
		 (if (not (null? cur))
		     (skk-editor-set-left-string!
		      ec (cdr cur)))
		 #f)
	       #t)
	   (if (skk-go-left-key? key key-state)
	       (let ((cur (skk-editor-left-string ec)))
		 (if (not (null? cur))
		     (begin
		       (skk-editor-set-left-string!
			ec (cdr cur))
		       (skk-editor-set-right-string!
			ec (cons (car cur) (skk-editor-right-string ec)))))
		 #f)
	       #t)
	   (if (skk-go-right-key? key key-state)
	       (let ((cur (skk-editor-right-string ec)))
		 (if (not (null? cur))
		     (begin
		       (skk-editor-set-right-string!
			ec (cdr cur))
		       (skk-editor-set-left-string!
			ec (cons (car cur) (skk-editor-left-string ec)))))
		 #f)
	       #t)
	   (if (skk-return-key? key key-state)
	       (begin
		 (if (< 0 (string-length str))
		     (begin
		       (skk-lib-learn-word
                        skk-dic
                        (cons (skk-make-string (skk-context-head sc)
                                               skk-type-hiragana)
                              (skk-context-okuri-head sc))
                        (skk-make-string (skk-context-okuri sc)
                                         skk-type-hiragana)
                        str
                        skk-use-numeric-conversion?)
		       (skk-save-personal-dictionary)
		       (if skk-use-numeric-conversion?
			  (let ((numlst
				 (skk-lib-store-replaced-numstr
				  (skk-make-string (skk-context-head sc)
						   skk-type-hiragana))))
			    (set! str (skk-lib-merge-replaced-numstr
				       str numlst))))
		       (skk-commit-editor-context sc str))
		     (begin
		       (skk-editor-flush ec)
		       (skk-context-set-child-context! sc '())
		       (skk-context-set-child-type! sc '())
		       (if (> (skk-context-nr-candidates sc) 0)
			   (skk-back-to-converting-state sc)
			   (skk-back-to-kanji-state sc))))
		 #f)
	       #t)
	   (if (skk-cancel-key? key key-state)
	       (begin
		 (skk-editor-flush ec)
		 (skk-context-set-child-context! sc '())
		 (skk-context-set-child-type! sc '())
		 (if (> (skk-context-nr-candidates sc) 0)
		     (skk-back-to-converting-state sc)
		     (skk-back-to-kanji-state sc))
		 #f)
	       #t)
	   )))))
