;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

; hk-context manage japanese roma -> kana conversion.
; this conversion is table based, and you can switch inputting
; method (note, it's merely "inputting method", not "input method")



; to decrease of typing cost, internal procedure omit "context"
; table name is held in C code. Same table has same table id.
(define-record 'hk-context
  '((table-id          ())
    (left-string-list  ())
    (right-string-list ())))
(define hk-context-new-internal hk-context-new)

(define hk-context-new
  (lambda (table)
    (let ((res (hk-context-new-internal))
	  (table-id (open-table table)))
    (if (> 0 table-id)
	#f
	(begin
	  (hk-context-set-table-id! res table-id)
	  res)))))

(define hk-left-string-append
  (lambda (hkc str-list)
    (let ((l (car (hk-context-left-string-list hkc))))
      (hk-context-set-left-string-list! hkc '(l str-list))
)))


;;APIs

(define hk-get-left-string
  (lambda (hkc mode)
    (car (hk-context-left-string-list hkc))))

(define hk-get-right-string
  (lambda (hkc)
    (car (hk-context-right-string-list hkc))))

(define hk-push-key!
  (lambda (hkc str state)
    (let ((left (hk-left-string-list hkc))
	  (key-list (list (list str) (list str) (list str) (list str))))
      (if left
	  (hk-set-left-string-list! hkc (append left (list key-list)))
	  (hk-set-left-string-list! hkc (list key-list)))
      (hk-left-string-match hkc)
      )))

(define hk-make-match-list
  (lambda (left)
    (mapcar
     (lambda(l)
       (car (cadr l)))
     left)))
	
;;マッチング対象
;hennへん 
;henkへんk
;henaへな
;hennya へんや or へにゃ
(define hk-left-string-match
  (lambda (hkc)
    (let* ((left (hk-left-string-list hkc))
	   (match-list (mapcar
			(lambda(l)
			  (car (cadr l)))
			left))
	   (shifting (if (> (- (length left) 2) 0)
			 (list (nth (- (length left) 2) left))
			 ())))
      (if (hk-find-continual? hkc (caar (reverse left)))
	  (begin
	    (print (list (nth (- (length left) 2) left)))
	    (hk-left-string-reduce hkc 
				   (reverse (cddr (reverse left)))
				shifting;   (list (nth (- (length left) 2) left))
				   (list (nth (- (length left) 1) left))))
	  (begin
	  (print (list (nth (- (length left) 1) left)))
	    (hk-left-string-reduce hkc 
				   (reverse (cdr (reverse left)))
				   (list (nth (- (length left) 1) left))
				   ())))
      (print (hk-left-string-list hkc))
	  )))


(define hk-left-string-reduce
  (lambda (hkc left shifting rest)
    (let* ((complete (hk-find-complete-matched hkc (hk-make-match-list shifting))))
      (print shifting)
      (if shifting
	  (if (hk-find-longer-entry-exist?
	       hkc (hk-make-match-list shifting))
	      (begin
;		(print (reverse (cdr (reverse left))))
;		(print (append (list (car (reverse left))) shifting))
		(if left
		    (hk-left-string-reduce hkc
					   (reverse (cdr (reverse left)))
					   (append (list (car (reverse left))) shifting)
					   rest)
		    (hk-left-string-do-reducing hkc left shifting rest complete)))
	      (hk-left-string-do-reducing hkc left shifting rest complete))))))

(define hk-left-string-do-reducing
  (lambda (hkc left shifting rest complete)
    (if complete
	(begin
	  (hk-set-left-string-list! hkc 
				    (append left (list complete) rest)))
	(begin
	  (let ((short-complete (hk-find-complete-matched hkc (hk-make-match-list 
							       (cdr
								shifting)))))
					;	      (print short-complete)
	    (if short-complete
		(hk-set-left-string-list! hkc
					  (append left (list (car shifting)) (list short-complete) rest))
		(hk-set-left-string-list! hkc
					  (append left shifting rest)))
	    )))))

(define hk-make-str-list
  (lambda (str)
    (if str
	(mapcar 
	 (lambda (s)
	   (let ((splitted (string-split s " ")))
	     (if splitted
		 splitted
		 (list s))))
	 (string-split str "\t"))
	())))

;    (if (hk-find-continuable? hkc)
;	hk-left-string-matching-rec;文字列を追加して検索
;					;completeを呼びだして、置換
	
(define hk-left-string-matching-rec
  (lambda (hkc str left)  
()
))

(define hk-move-cursor
  (lambda (hkc direction)
    (if direction
	(begin
	  (if (hk-left-string ac)
	      (let ((c (car (hk-left-string hkc))))
		(hk-context-set-left-string!
		 hkc (cdr (hk-left-string hkc)))
		(hk-set-right-string! hkc
				      (cons c
					    (hk-right-string ac))))))
	(begin
	  (if (hk-right-string hkc)
	      (let ((c (car (hk-right-string hkc))))
		(hk-set-right-string!
		 hkc (cdr (hk-right-string hkc)))
		(hk-set-left-string! hkc
				     (cons c
					   (hk-left-string ac)))))))
    ))

(define hk-input-end
  (lambda (hkc)
   () ))

(define hk-backspace!
  (lambda (hkc)
    (if(> (length (hk-context-left-string hkc)) 0)
     ())
     ))



(define hk-find-complete-matched
  (lambda (hkc str-list)
    (let ((str #f)
	  (table-id (hk-context-table-id hkc)))
      (if str-list
	  (begin
	    (mapcar (lambda (s)
		  ;    (print str)
		      (if str
			  (set! str (string-append str " " s))
			  (set! str s)))
		    str-list)
	    (hk-make-str-list
	     (find-entry-matched-complete table-id (string-append str "\t"))))
	  #f
      ))))


(define hk-find-continual?
  (lambda (hkc str-list)
    (let ((str #f)
	  (table-id (hk-context-table-id hkc)))
      (if str-list
	  (begin
	    (mapcar (lambda (s)
		      (if str
			  (set! str (string-append str " " s))
			  (set! str s)))
		    str-list)
	    (find-entry-matched-complete table-id (string-append str " ")))
	  #f
	  ))))

(define hk-find-longer-entry-exist?
  (lambda (hkc str-list)
    (let ((str "")
	  (table-id (hk-context-table-id hkc)))
      (if str-list
	  (begin
	    (mapcar 
	     (lambda (s)
	       (set! str (string-append str " " s)))
	     str-list)
	    (find-entry-matched-continual table-id str))
	  ())
      )))

(define hk-delete!
  (lambda (hkc)
    ()
))

(define hk-flush!
  (lambda (hkc)
    (hk-context-set-left-string-list! hkc '())
    
))

