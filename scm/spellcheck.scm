;;;
;;; Copyright (c) 2003-2012 uim Project http://code.google.com/p/uim/
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

;;uim-spellcheck is dynamic spell checker.

(require-custom "generic-key-custom.scm")
(require-custom "spellcheck-custom.scm")


(define spell-context-rec-spec
  (append
   context-rec-spec
   '((state              spell-state-latin)
     (kana-mode          #t)
     (head               "")
     ;;(okuri              "")
     (tail               "")
     (unused-5th         ())
     (nth                ())
     (rk-context         ())
     (candidate-op-count ())
     (candidate-window   ())
     (candidates         ())
     (right-string       ())
     (left-string        ()) ;;カーソルの左側にあるpreedit文字列のリスト
     (mode               0)
     (last-word          "") ;;SPELLやPOBoxの用語でいうContext
     (immediate-commit   #t)))) ;;一時的にspell-preedit-immididate-commit?をオフにするかどうか
(define-record 'spell-context spell-context-rec-spec)
(define spell-context-new-internal spell-context-new)

(define spell-flush
  (lambda (sc)
    (spell-context-set-state! sc 'spell-state-no-preedit)
    (spell-context-set-head! sc '())
    (spell-context-set-immediate-commit! sc #t)
    (spell-context-set-tail! sc '())
    (spell-context-set-left-string! sc '())
    (spell-context-set-right-string! sc '())
    (spell-context-set-nth! sc #f)
    (spell-context-set-candidate-window! sc #f)))


(define spell-context-new
  (lambda (id im)
    (let ((sc (spell-context-new-internal id im)))
      (spell-context-set-head! sc ())
      (spell-flush sc)
      (spell-context-set-state! sc 'spell-state-latin)
      sc)))

(define spell-make-string
  (lambda (sc sl dir)  ;;dirは文字列の方向を表す(カーソルより右側の文字列は向きが逆になってる)
    (let ((kana?))
      (if (= 1 (spell-context-mode sc))
	  (set! kana? #t))
      (if (= 2 (spell-context-mode sc))
	  (set! kana? #f))
      (if sl
	  (if dir
	      (string-append (spell-make-string sc (cdr sl) dir)
			     (if kana?
				 (caar sl)
				 (cdar sl)))
	      (string-append (if kana?
				 (caar sl)
				 (cdar sl))
			     (spell-make-string sc (cdr sl) dir)))
	  ""))))

(define spell-context-kana-toggle
  (lambda (sc)
    (let ((s (spell-context-kana-mode sc)))
      (set! s (not s))
      (if s
	  (spell-context-set-mode! sc 1)
	  (spell-context-set-mode! sc 2))
      (spell-context-set-kana-mode! sc s))))

(define spell-get-string-by-mode
  (lambda (sc res)
    (if res
	(if (spell-context-kana-mode sc)
	    (car res)
	    (cdr res))
	#f)))

(define spell-get-nth-candidate
  (lambda (sc n)
    (if (> n (spell-get-nr-candidates sc))
	#f)
    (car (cdr (car (nthcdr n (spell-context-candidates sc)))))
    ))

(define spell-get-nr-candidates
  (lambda (sc)
    (length (spell-context-candidates sc))))

(define spell-get-current-candidate
  (lambda (sc)
    (spell-get-nth-candidate
     sc
     (spell-context-nth sc))))

(define spell-get-candidates! ;;もうちょっと関数名をどうにかしたい
  (lambda (sc preedit context)
	(let ((tmp))
	  (spell-lib-send-command (string-append "set_context\t"  context "\n"))
	  (spell-context-set-candidates!
	   sc
	   (spell-parse-cands
		(spell-lib-send-command (string-append "l\t"  preedit "\n"))))
	  )))

(define spell-make-assoc-list
  (lambda (lst)
	(map 
	 (lambda (str)
	   (string-split str "="))
	 lst)))

(define spell-commit-candidate
  (lambda (sc)
	(let ((nth (spell-context-nth sc)))
	  (set! assoc-list 
			(spell-make-assoc-list 
			 (cddar (nthcdr nth (spell-context-candidates sc)))))
	  (spell-learn-word sc assoc-list)
	  )))

(define spell-learn-word
  (lambda (sc assoc-list)
    (let ((key     (or (cadr (assoc "basekey"     assoc-list)) ""))
	  (value   (or (cadr (assoc "base"        assoc-list)) ""))
	  (part    (or (cadr (assoc "part"        assoc-list)) ""))
	  (context (or (spell-context-last-word sc) ""))
	  (suffix  (or (cadr (assoc "conjugation" assoc-list)) ""))
	  (rest    (or (cadr (assoc "suffix"      assoc-list)) "")))

	  (spell-lib-send-command
	   (string-append "learn_word\t" key "\t" value "\t" part "\t" context "\t" suffix "\t" rest "\n"))

	  (spell-context-set-last-word!
	   sc
	   (spell-get-current-candidate sc))
	  )))

(define spell-parse-cands
  (lambda (cands-string)
	(map
	 (lambda (str-line)
	   (string-split str-line "\t"))
	 (cdr (delq "" (string-split cands-string "\n"))))))

(define spell-begin-conversion
  (lambda (sc)
    (let ((res))
	  (spell-get-candidates!
	   sc 
	   (spell-make-string sc (spell-context-left-string sc) #t)
	   (spell-context-last-word sc))
      (set! res
	    (spell-get-nth-candidate sc 0))
      (if res
	  (begin
	    (spell-context-set-nth! sc 0)
	    (spell-context-set-state!
	     sc 'spell-state-converting))
	  (spell-flush sc))
      ())))

(define spell-update-preedit
  (lambda (sc)
    (let ((rkc (spell-context-rk-context sc))
	  (stat (spell-context-state sc)))
      (im-clear-preedit sc)
      (if (= stat 'spell-state-converting)
	  (begin
	    (im-pushback-preedit
	     sc preedit-reverse
	     (spell-get-current-candidate sc)))
	  (if (spell-has-preedit? sc)
	      (let ((hl (spell-make-string
			 sc (spell-context-left-string sc) #t))
		    (hr (spell-make-string
			 sc (spell-context-right-string sc) #f)))
		(if (string? hl)
		    (im-pushback-preedit
		     sc preedit-underline
		     hl))
		(im-pushback-preedit sc preedit-underline
				     (rk-pending rkc))
		(im-pushback-preedit sc preedit-cursor "")
		(if (string? hr)
		    (im-pushback-preedit
		     sc preedit-underline
		     hr)))))
      (im-update-preedit sc))))

(define spell-update-mode
  (lambda (sc)
    (let ((mode (spell-context-mode sc)))
      (im-update-mode sc mode))))

(define spell-update-candidate-window
  (lambda (sc)
    (if (and
	 (spell-has-preedit? sc)
	 (not (spell-context-candidate-window sc))
	 (or spell-always-show-window?
	     (> (spell-context-candidate-op-count sc)
		spell-candidate-op-count)))
	(begin
	  (spell-get-candidates!
	   sc
	   (spell-make-string sc (spell-context-left-string sc) #t)
	   (spell-context-last-word sc))
	  (im-activate-candidate-selector
	   sc (spell-get-nr-candidates sc) 0)
	  (spell-context-set-candidate-window! sc #t)
	  ))
    (if (not (spell-has-preedit? sc))
	(begin
	  (spell-context-set-candidate-window! sc #f)
	  (im-deactivate-candidate-selector sc)))
    (if (spell-context-candidate-window sc)
	(begin
	  (spell-get-candidates!
	   sc
	   (spell-make-string sc (spell-context-left-string sc) #t)
	   (spell-context-last-word sc))	  
	  (im-select-candidate sc (spell-context-nth sc))))
    ))

(define spell-has-preedit?
  (lambda (ac)
    (or
     (> (length (spell-context-left-string ac)) 0)
     (> (length (spell-context-right-string ac)) 0))))

(define spell-proc-input-no-preedit
  (lambda (sc key key-state)
    (let* ((key-str (charcode->string (ichar-downcase key)))
	   (rkc (spell-context-rk-context sc))
	   (res #f)
	   (direct (ja-direct (charcode->string key)))
	   (immediate-commit))
      (and
       (if (spell-cancel-key? key key-state)
	   (begin
	     (spell-flush sc)
	     #f)
	   #t)
       (if (spell-kana-toggle key key-state)
	   (begin 
	     (spell-context-kana-toggle sc)
	     (spell-update-mode sc)
	     #f)
	   #t)
       (if (spell-backspace-key? key key-state)
	   (if (not (rk-backspace rkc))
	       (begin
		 (im-commit-raw sc)
		 #f)
	       #f)
	   #t)
       (if (control-key-mask key-state)
	   (begin
	     (im-commit-raw sc)
	     #f)
	   #t)
       (if (and
	    (shift-key-mask key-state)
	    (ichar-alphabetic? key))
	   (begin
	     (spell-context-set-immediate-commit! sc #f)
	     #t)
	   #t)
       ;; direct key => commit
       (if direct
	   (begin
	     (im-commit sc direct)
	     #f)
	   #t)
       (if (symbol? key)
	   (begin
	     (spell-flush sc)
	     (spell-context-set-last-word! sc "")
	     (im-commit-raw sc)
	     #f)
	   #t)
       (spell-proc-input-with-preedit sc key key-state))
      #f)))

(define spell-proc-input-with-preedit
  (lambda (sc key key-state)
    (let* ((rkc (spell-context-rk-context sc))
	   (stat (spell-context-state sc))
	   (res))
      (and
       (if (spell-begin-conv-key? key key-state)
	   (begin
	     (spell-begin-conversion sc)
	     #f)
	   #t)
       (if (spell-cancel-key? key key-state)
	   (begin
	     (spell-flush sc)
	     #f)
	   #t)
       (if (spell-backspace-key? key key-state)
	   (begin
	     (if (not (rk-backspace rkc))
		 (if (spell-has-preedit? sc)
		     (spell-context-set-left-string!
		      sc (cdr (spell-context-left-string sc)))
		     (begin
		       (im-commit-raw sc)
		       (spell-flush sc))))
	     #f)
	   #t)
       ;; delete
       (if (spell-delete-key? key key-state)
	   (begin
	     (if (not (rk-delete rkc))
		 (if (spell-context-right-string sc)
		     (spell-context-set-right-string!
		      sc
		      (cdr (spell-context-right-string sc)))))
	     #f)
	   #t)
       ;;
       (if (spell-commit-key? key key-state)
	   (begin
	     (im-commit id (spell-make-string
			    sc (spell-context-left-string sc) (spell-context-kana-mode sc)))
	     (spell-flush sc)
	     (spell-update-mode id sc)
	     #f)
	   #t)
       ;; left
       (if (spell-go-left-key? key key-state)
	   (begin
	     (if (spell-context-left-string sc)
		 (let ((c (car (spell-context-left-string sc))))
		   (spell-context-set-left-string!
		    sc (cdr (spell-context-left-string sc)))
		   (spell-context-set-right-string! 
		    sc
		    (cons c (spell-context-right-string sc)))))
	     #f)
	   #t)
       ;; right
       (if (spell-go-right-key? key key-state)
	   (begin
	     (if (spell-context-right-string sc)
		 (let ((c (car (spell-context-right-string sc))))
		   (spell-context-set-right-string!
		    sc (cdr (spell-context-right-string sc)))
		   (spell-context-set-left-string!
		    sc
		    (cons c (spell-context-left-string sc)))))
	     #f)
	   #t)
       (if (ichar-numeric? key)
	   (begin
	     (spell-context-set-nth! sc (- key 49))
	     (im-commit sc (spell-get-nth-candidate sc (- key 49)))
	     (spell-commit-candidate sc)
	     (spell-flush sc)
	     (spell-update-mode id sc)
	     #f)
	   #t)
       ;; modifiers (shiftを除く) => ignore
       (if (and (modifier-key-mask key-state) (not (shift-key-mask key-state)))
	   (begin
	     (im-commit-raw sc)
	     #f)
	   #t)
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string (ichar-downcase key))))
	 (if res
	     (begin
	       (spell-context-set-left-string!
		sc
		(cons res
		      (spell-context-left-string sc)))
	       (spell-context-set-candidate-window! sc #f) ;FIXME:very duty hack
	       (if (and spell-preedit-immididate-commit? (spell-context-immediate-commit sc))
		   (begin 
		     (im-commit sc (spell-make-string sc (spell-context-left-string sc) #t))
		     (spell-flush sc)
		     #f)
		   #t)
	       ))))
      #f)))

(define spell-proc-state-converting
  (lambda (sc key key-state)
    (let ((res ()))
      (and
       (if (spell-next-candidate key key-state)
	   (begin
	     (spell-context-set-nth! sc
				     (+ 1 (spell-context-nth sc)))
	     (if (not (spell-get-current-candidate
		       sc (spell-context-nth sc)))
	     (spell-context-set-nth! sc 0))
	     (spell-context-set-candidate-op-count!
	      sc
	      (+ 1 (spell-context-candidate-op-count sc)))
	     #f)
	   #t)
       (if (spell-prev-candidate key key-state)
	   (begin
	     (if (> (spell-context-nth sc) 0)
		 (spell-context-set-nth! sc (- (spell-context-nth sc) 1))
		 (spell-context-set-nth! sc (- (spell-get-nr-candidates sc) 1)))
	     #f)
	   #t)
       (if (spell-cancel-key? key key-state)
	   (begin
	     (spell-flush sc)
	     #f)
	   #t)
       (if (spell-commit-key? key key-state)
	   (begin
	     (set! res (spell-get-current-candidate sc))
	     (spell-commit-candidate sc)
	     (spell-flush sc)
	     (spell-update-mode sc)
	     #f)
	   #t)
       (begin
	 (spell-update-mode sc)
	 (set! res (spell-get-current-candidate sc))
	 (spell-commit-candidate sc)
	 (spell-flush sc)
	 (let ((res2 (spell-proc-input-no-preedit sc key key-state)))
	   (set!
	    res
	    (string-append
	     res 
	     (spell-make-string sc
				(spell-context-tail sc)
				(spell-context-kana-mode sc))))
	   (if (string? res2)
	       (set! res
		     (string-append res res2))))))
      res)))

(define spell-proc-mode-latin
  (lambda (sc key key-state)
    (if
     (spell-on-key? key key-state)
     (begin
       (spell-context-set-mode! sc 1)
       (spell-update-mode sc))
     (im-commit-raw sc))
    ()))

(define spell-push-key
  (lambda (sc key key-state)
    (let* ((state (spell-context-state sc))
	   (mode (spell-context-mode sc))
	   (fun)
	   (res))
	    (if (spell-has-preedit? sc)
		(set! fun spell-proc-input-with-preedit)
		(set! fun spell-proc-input-no-preedit))
	    (if (= state 'spell-state-converting)
		(set! fun spell-proc-state-converting))))
      (set! res (fun sc key key-state))  
      (if res
	  (im-commit sc res))
      (spell-update-preedit sc)
      (spell-update-candidate-window sc)
      )

(define spell-init-handler
  (lambda (id im arg)
    #f))

(define spell-press-key-handler
  (lambda (sc key state)
    (spell-push-key sc key state)))

(define spell-release-key-handler
  (lambda (sc key state)
    #f))

(define spell-reset-handler
  (lambda (sc)
    #f))

(define spell-mode-handler
  (lambda (sc mode)
    (spell-flush sc)
    (spell-context-set-mode! sc mode)
    (if (= mode 1)
	(spell-context-set-kana-mode! sc #t))
    (if (= mode 2)
	(spell-context-set-kana-mode! sc #f))
    (spell-update-preedit sc)
    #f))

(define spell-get-candidate-handler
  (lambda (sc idx)
    (spell-get-nth-candidate sc idx)))

(define spell-set-candidate-index-handler
  (lambda (sc idx)
    (spell-context-set-nth! sc idx)
    (spell-update-preedit sc)))

(register-im
 'spell
 "ja"
 "EUC-JP"
 spell-im-name-label
 spell-im-short-desc
 #f
 spell-init-handler
 #f
 spell-mode-handler
 spell-press-key-handler
 spell-release-key-handler
 spell-reset-handler
 spell-get-candidate-handler
 spell-set-candidate-index-handler
 #f
 #f
 #f
 #f
 #f
 #f)
