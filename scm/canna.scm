;;; canna.scm: Canna for uim.
;;;
;;; Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/
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

(require "ustr.scm")
(require "japanese.scm")
(require "japanese-kana.scm")
(require "japanese-azik.scm")
(require-custom "generic-key-custom.scm")
(require-custom "canna-custom.scm")
(require-custom "canna-key-custom.scm")


;;; implementations

(define canna-init-lib-ok? #f)

(define canna-type-hiragana   ja-type-hiragana)
(define canna-type-katakana   ja-type-katakana)
(define canna-type-hankana    ja-type-hankana)
(define canna-type-latin      ja-type-latin)
(define canna-type-wide-latin ja-type-wide-latin)

(define canna-input-rule-roma 0)
(define canna-input-rule-kana 1)
(define canna-input-rule-azik 2)

(define canna-candidate-type-katakana -2)
(define canna-candidate-type-hiragana -3)
(define canna-candidate-type-hankana -4)
(define canna-candidate-type-latin -5)
(define canna-candidate-type-wide-latin -6)

;; I don't think the key needs to be customizable.
(define-key canna-space-key? '(" "))

(define canna-prepare-activation
  (lambda (cc)
    (if (canna-context-state cc)
        (let ((cc-id (canna-context-cc-id cc)))
          (canna-lib-reset-conversion cc-id)))
    (canna-flush cc)
    (canna-update-preedit cc)))

(register-action 'action_canna_hiragana
		 (lambda (cc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (cc) ;; activity predicate
		   (and (canna-context-on cc)
		        (not (canna-context-ascii-with-preedit cc))
			(= (canna-context-kana-mode cc)
			   canna-type-hiragana)))

		 (lambda (cc) ;; action handler
		   (if (not (canna-context-on cc))
		       (canna-prepare-activation cc))
		   (canna-context-set-on! cc #t)
		   (canna-context-set-ascii-with-preedit! cc #f)
		   (canna-context-change-kana-mode! cc
						 canna-type-hiragana)))

(register-action 'action_canna_katakana
		 (lambda (cc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
		        (not (canna-context-ascii-with-preedit cc))
			(= (canna-context-kana-mode cc)
			   canna-type-katakana)))
		 (lambda (cc)
		   (if (not (canna-context-on cc))
		       (canna-prepare-activation cc))
		   (canna-context-set-on! cc #t)
		   (canna-context-set-ascii-with-preedit! cc #f)
		   (canna-context-change-kana-mode! cc canna-type-katakana)))

(register-action 'action_canna_hankana
		 (lambda (cc)
		   '(ja_halfwidth_katakana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
			(not (canna-context-ascii-with-preedit cc))
			(= (canna-context-kana-mode cc) canna-type-hankana)))
		 (lambda (cc)
		   (if (not (canna-context-on cc))
		       (canna-prepare-activation cc))
		   (canna-context-set-on! cc #t)
		   (canna-context-set-ascii-with-preedit! cc #f)
		   (canna-context-change-kana-mode! cc canna-type-hankana)))

(register-action 'action_canna_ascii_with_preedit
		 (lambda (cc) ;; indication handler
		   '(ja_ascii_with_preedit
		     "aA"
		     "英数変換"
		     "英数変換モード"))
		 (lambda (cc) ;; activity predicate
		   (and (canna-context-on cc)
			(canna-context-ascii-with-preedit cc)))
		 (lambda (cc) ;; action handler
		   (if (not (canna-context-on cc))
		       (begin
		         (canna-prepare-activation cc)
		         (canna-context-set-on! cc #t)))
		   (canna-context-set-ascii-with-preedit! cc #t)))

(register-action 'action_canna_direct
		 (lambda (cc)
		   '(ja_direct
		     "a"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (cc)
		   (and (not (canna-context-on cc))
			(not (canna-context-wide-latin cc))))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (canna-context-set-on! cc #f)
		   (canna-context-set-wide-latin! cc #f)))

(register-action 'action_canna_zenkaku
		 (lambda (cc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (cc)
		   (and (not (canna-context-on cc))
			(canna-context-wide-latin cc)))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (canna-context-set-on! cc #f)
		   (canna-context-set-wide-latin! cc #t)))

(register-action 'action_canna_roma
		 (lambda (cc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (cc)
		   (= (canna-context-input-rule cc)
		      canna-input-rule-roma))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (rk-context-set-rule! (canna-context-rkc cc)
					 ja-rk-rule)
		   (canna-context-set-input-rule! cc canna-input-rule-roma)))

(register-action 'action_canna_kana
		 (lambda (cc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (cc)
		   (= (canna-context-input-rule cc)
		      canna-input-rule-kana))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (canna-context-set-input-rule! cc canna-input-rule-kana)
                   (canna-context-change-kana-mode!
                     cc (canna-context-kana-mode cc))))

(register-action 'action_canna_azik
		 (lambda (cc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (cc)
		   (= (canna-context-input-rule cc)
		      canna-input-rule-azik))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (rk-context-set-rule! (canna-context-rkc cc)
					 ja-azik-rule)
		   (canna-context-set-input-rule! cc canna-input-rule-azik)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define canna-configure-widgets
  (lambda ()
    (register-widget 'widget_canna_input_mode
		     (activity-indicator-new canna-input-mode-actions)
		     (actions-new canna-input-mode-actions))

    (register-widget 'widget_canna_kana_input_method
		     (activity-indicator-new canna-kana-input-method-actions)
		     (actions-new canna-kana-input-method-actions))
    (context-list-replace-widgets! 'canna canna-widgets)))

(define canna-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              ())
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'cc-id              ()) ;; canna-context-id
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   ())
    (list 'candidate-op-count ())
    (list 'wide-latin         #f)
    (list 'kana-mode          canna-type-hiragana)
    (list 'ascii-with-preedit #f)
    (list 'commit-raw         #t)
    (list 'input-rule         canna-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'canna-context canna-context-rec-spec)
(define canna-context-new-internal canna-context-new)

(define (canna-context-new id im)
  (let ((cc (canna-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (canna-context-set-cc-id! cc (if canna-init-lib-ok?
;				      (canna-lib-alloc-context) ()))
    (canna-context-set-cc-id! cc (canna-lib-alloc-context))
    (canna-context-set-widgets! cc canna-widgets)
    (canna-context-set-rkc! cc rkc)
    (canna-context-set-preconv-ustr! cc (ustr-new))
    (canna-context-set-raw-ustr! cc (ustr-new))
    (canna-context-set-segments! cc (ustr-new))
    (if using-kana-table?
        (canna-context-set-input-rule! cc canna-input-rule-kana)
        (canna-context-set-input-rule! cc canna-input-rule-roma))
    cc))

(define (canna-commit-raw cc)
  (im-commit-raw cc)
  (canna-context-set-commit-raw! cc #t))

(define (canna-context-kana-toggle cc)
  (let* ((kana (canna-context-kana-mode cc))
	 (opposite-kana (ja-opposite-kana kana)))
    (canna-context-change-kana-mode! cc opposite-kana)))

(define (canna-toggle-ascii-with-preedit? cc key key-state)
  (let ((state (canna-context-ascii-with-preedit cc)))
    (cond
     ((and
       state
       (canna-ascii-mode-off-key? key key-state))
      (canna-context-set-ascii-with-preedit! cc #f)
      #t)
     ((and
       (not state)
       (canna-ascii-mode-on-key? key key-state))
      (canna-context-set-ascii-with-preedit! cc #t)
      #t)
     (else
      #f))))

(define canna-context-change-kana-mode!
  (lambda (cc kana-mode)
    (if (= (canna-context-input-rule cc)
           canna-input-rule-kana)
        (rk-context-set-rule!
          (canna-context-rkc cc)
          (cond
            ((= kana-mode canna-type-hiragana) ja-kana-hiragana-rule)
            ((= kana-mode canna-type-katakana) ja-kana-katakana-rule)
            ((= kana-mode canna-type-hankana)  ja-kana-hankana-rule))))
    (canna-context-set-kana-mode! cc kana-mode)))

(define canna-make-whole-string
  (lambda (cc convert-pending-into-kana? kana)
    (let* ((rkc (canna-context-rkc cc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (canna-context-input-rule cc))
           (preconv-str (canna-context-preconv-ustr cc))
           (extract-kana
            (if (= rule canna-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule canna-input-rule-kana)
	  (ja-make-kana-str
	   (ja-make-kana-str-list
	    (string-to-list 
	     (string-append
	      (string-append-map-ustr-former extract-kana preconv-str)
	      (if convert-pending-into-kana?
		  (if residual-kana
		      (extract-kana residual-kana)
                      pending)
		  pending)
              (string-append-map-ustr-latter extract-kana preconv-str))))
	   kana)
          (string-append
	   (string-append-map-ustr-former extract-kana preconv-str)
           (if convert-pending-into-kana?
               (if residual-kana
                   (extract-kana residual-kana)
                   "")
               pending)
           (string-append-map-ustr-latter extract-kana preconv-str))))))

(define canna-make-raw-string
  (lambda (raw-str-list wide?)
    (if (not (null? raw-str-list))
	(if wide?
	    (string-append
	     (ja-string-list-to-wide-alphabet
	      (string-to-list (car raw-str-list)))
	     (canna-make-raw-string (cdr raw-str-list) wide?))
	    (string-append
	     (car raw-str-list)
	     (canna-make-raw-string (cdr raw-str-list) wide?)))
	"")))

(define canna-make-whole-raw-string
  (lambda (cc wide?)
    (canna-make-raw-string (canna-get-raw-str-seq cc) wide?)))

(define (canna-init-handler id im arg)
  (if (not canna-init-lib-ok?)
      (begin
	(canna-lib-init canna-server-name)
	(set! canna-init-lib-ok? #t)))
  (canna-context-new id im))

(define (canna-release-handler cc)
  (let ((cc-id (canna-context-cc-id cc)))
    (if (number? cc-id)
        (canna-lib-release-context cc-id))))

(define (canna-flush cc)
  (rk-flush (canna-context-rkc cc))
  (ustr-clear! (canna-context-preconv-ustr cc))
  (ustr-clear! (canna-context-raw-ustr cc))
  (ustr-clear! (canna-context-segments cc))
  (canna-context-set-state! cc #f)
  (canna-context-set-transposing! cc #f)
  (canna-context-set-ascii-with-preedit! cc #f)
  (if (canna-context-candidate-window cc)
        (im-deactivate-candidate-selector cc))
  (canna-context-set-candidate-window! cc #f)
  (canna-context-set-candidate-op-count! cc 0))

(define (canna-begin-input cc)
  (canna-context-set-on! cc #t)
  (rk-flush (canna-context-rkc cc))
  (canna-context-set-state! cc #f))

(define (canna-update-preedit cc)
  (if (not (canna-context-commit-raw cc))
      (let ((segments (if (canna-context-on cc)
			  (if (canna-context-transposing cc)
			      (canna-context-transposing-state-preedit cc)
			      (if (canna-context-state cc)
				  (canna-compose-state-preedit cc)
				  (canna-input-state-preedit cc)))
			  ())))
	(context-update-preedit cc segments))
      (canna-context-set-commit-raw! cc #f)))

(define (canna-begin-conv cc)
  (let ((cc-id (canna-context-cc-id cc))
	(preconv-str (canna-make-whole-string cc #t canna-type-hiragana)))
    (if (and (number? cc-id)
             (> (string-length preconv-str) 0))
	(let ((num (canna-lib-begin-conversion cc-id preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (canna-context-segments cc)
		 (make-list num 0))
		(canna-context-set-state! cc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; canna-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define canna-cancel-conv
  (lambda (cc)
    (let ((cc-id (canna-context-cc-id cc)))
      (canna-reset-candidate-window cc)
      (canna-context-set-state! cc #f)
      (ustr-clear! (canna-context-segments cc))
      (canna-lib-reset-conversion cc-id))))

(define (canna-proc-input-state-no-preedit cc key key-state)
  (let
      ((rkc (canna-context-rkc cc))
       (direct (ja-direct (charcode->string key)))
       (rule (canna-context-input-rule cc)))
    (cond
     ((and canna-use-with-vi?
           (canna-vi-escape-key? key key-state))
      (begin
        (canna-flush cc)
        (canna-context-set-on! cc #f)
        (canna-context-set-wide-latin! cc #f)
        (canna-commit-raw cc)))

     ((canna-wide-latin-key? key key-state)
      (begin
	(canna-flush cc)
	(canna-context-set-on! cc #f)
	(canna-context-set-wide-latin! cc #t)))
     
     ((canna-latin-key? key key-state)
      (begin
	(canna-flush cc)
	(canna-context-set-on! cc #f)
	(canna-context-set-wide-latin! cc #f)))
     
     ((canna-backspace-key? key key-state)
      (canna-commit-raw cc))
     
     ((canna-delete-key? key key-state)
      (canna-commit-raw cc))
     
     ((canna-hankaku-kana-key? key key-state)
      (canna-context-change-kana-mode! cc canna-type-hankana))
     
     ((canna-kana-toggle-key? key key-state)
      (canna-context-kana-toggle cc))
     
     ((canna-toggle-ascii-with-preedit? cc key key-state))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (canna-commit-raw cc))
     
     ;; direct key => commit
     (direct
      (im-commit cc direct))

     ;; space key
     ((canna-space-key? key key-state)
      (im-commit cc (list-ref ja-space (canna-context-kana-mode cc))))

     ((symbol? key)
      (canna-commit-raw cc))

     (else
      (if (canna-context-ascii-with-preedit cc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (canna-context-preconv-ustr cc)
			       (list key-str key-str key-str))
	    (ustr-insert-elem! (canna-context-raw-ustr cc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule canna-input-rule-kana)
			       key
			       (to-lower-char key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
	          (ustr-insert-elem! (canna-context-preconv-ustr cc) res)
	          (ustr-insert-elem! (canna-context-raw-ustr cc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (canna-commit-raw cc)))))))))

(define (canna-has-preedit? cc)
  (or (not (ustr-empty? (canna-context-preconv-ustr cc)))
      (> (string-length (rk-pending (canna-context-rkc cc))) 0)))

(define canna-proc-transposing-state
  (lambda (cc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (canna-transpose-as-wide-latin-key? key key-state)
	  (set! rotate-list (cons canna-type-wide-latin rotate-list)))
      (if (canna-transpose-as-latin-key? key key-state)
	  (set! rotate-list (cons canna-type-latin rotate-list)))
      (if (canna-transpose-as-hankana-key? key key-state)
	  (set! rotate-list (cons canna-type-hankana rotate-list)))
      (if (canna-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons canna-type-katakana rotate-list)))
      (if (canna-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons canna-type-hiragana rotate-list)))

      (if (canna-context-transposing cc)
	  (let ((lst (member (canna-context-transposing-type cc) rotate-list)))
	    (if (and (not (null? lst))
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(set! state (car rotate-list))))
	  (begin
	    (canna-context-set-transposing! cc #t)
	    (set! state (car rotate-list))))

      (cond
       ((= state canna-type-hiragana)
	(canna-context-set-transposing-type! cc canna-type-hiragana))
       ((= state canna-type-katakana)
	(canna-context-set-transposing-type! cc canna-type-katakana))
       ((= state canna-type-hankana)
	(canna-context-set-transposing-type! cc canna-type-hankana))
       ((= state canna-type-latin)
	(if (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	    (canna-context-set-transposing-type! cc canna-type-latin)))
       ((= state canna-type-wide-latin)
	(if (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	    (canna-context-set-transposing-type! cc canna-type-wide-latin)))
       (else
	(and
	 ; begin-conv
	 (if (canna-begin-conv-key? key key-state)
	     (begin
	       (canna-context-set-transposing! cc #f)
	       (canna-begin-conv cc)
	       #f)
	     #t)
	 ; cancel
	 (if (canna-cancel-key? key key-state)
	     (begin
	       (canna-context-set-transposing! cc #f)
	       #f)
	     #t)
	; commit
	(begin
	  (im-commit cc (canna-transposing-text cc))
	  (canna-flush cc)
	  (if (not (canna-commit-key? key key-state))
	      (begin
	        (canna-context-set-transposing! cc #f)
		(canna-proc-input-state cc key key-state)
		(canna-context-set-commit-raw! cc #f))))))))))

(define (canna-proc-input-state-with-preedit cc key key-state)
  (let ((preconv-str (canna-context-preconv-ustr cc))
	(raw-str (canna-context-raw-ustr cc))
	(rkc (canna-context-rkc cc))
	(rule (canna-context-input-rule cc))
	(kana (canna-context-kana-mode cc)))
    (cond
     ;; begin conversion
     ((or 
       (and (canna-begin-conv-key? key key-state)
	    (not (canna-context-ascii-with-preedit cc)))
       (and (canna-begin-conv-with-ascii-mode-key? key key-state)
	    (canna-context-ascii-with-preedit cc)))
      (canna-begin-conv cc))

     ;; backspace
     ((canna-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (canna-context-input-rule cc) canna-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (char-printable?
		       (string->char
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((canna-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((canna-kill-key? key key-state)
      (ustr-clear-latter! preconv-str))
     
     ;; kill-backward
     ((canna-kill-backward-key? key key-state)
      (begin
        (rk-flush rkc)
        (ustr-clear-former! preconv-str)))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((canna-commit-as-opposite-kana-key? key key-state)
      (begin
	(im-commit
	 cc
	 (canna-make-whole-string cc #t (ja-opposite-kana kana)))
	(canna-flush cc)))

       ;; Transposing状態へ移行
     ((or (canna-transpose-as-hiragana-key?   key key-state)
	  (canna-transpose-as-katakana-key?   key key-state)
	  (canna-transpose-as-hankana-key?    key key-state)
	  (and
	   (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	   (or
	    (canna-transpose-as-latin-key?	key key-state)
	    (canna-transpose-as-wide-latin-key?	 key key-state))))
      (canna-proc-transposing-state cc key key-state))

     ;; 現在のかなを確定後、ひらがな/カタカナモードを切り換える
     ((canna-kana-toggle-key? key key-state)
      (begin
	(im-commit
	 cc
	 (canna-make-whole-string cc #t kana))
	(canna-flush cc)
	(canna-context-kana-toggle cc)))

     ((canna-toggle-ascii-with-preedit? cc key key-state))

     ;; cancel
     ((canna-cancel-key? key key-state)
      (canna-flush cc))

     ;; commit
     ((canna-commit-key? key key-state)
      (begin
	(im-commit
	 cc
	 (canna-make-whole-string cc #t kana))
	(canna-flush cc)))

     ;; left
     ((canna-go-left-key? key key-state)
      (canna-context-confirm-kana! cc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((canna-go-right-key? key key-state)
      (canna-context-confirm-kana! cc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((canna-beginning-of-preedit-key? key key-state)
      (canna-context-confirm-kana! cc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((canna-end-of-preedit-key? key key-state)
      (canna-context-confirm-kana! cc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     (else
      ;; handle "n1" sequence as "ん1"
      (if (and (not (canna-context-ascii-with-preedit cc))
	       (not (alphabet-char? key))
	       (not (string-find
		     (rk-expect rkc)
		     (charcode->string
		      (if (= rule canna-input-rule-kana)
			  key
			  (to-lower-char key))))))
	  (let ((pend (rk-pending rkc))
		(residual-kana (rk-push-key-last! rkc)))
	    (if residual-kana
		(begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))))

      (if (canna-context-ascii-with-preedit cc)
          (let ((key-str (charcode->string key))
	        (pend (rk-pending rkc))
		(residual-kana (rk-peek-terminal-match rkc)))
	    (rk-flush rkc) ;; OK to reset rkc here.
	    (if residual-kana
	        (begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))
	    (ustr-insert-elem! preconv-str (list key-str key-str key-str))
	    (ustr-insert-elem! raw-str key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule canna-input-rule-kana)
			       key
			       (to-lower-char key))))
	         (pend (rk-pending rkc))
	         (res (rk-push-key! rkc key-str)))
	    (if (and res
		     (or (list? (car res))
		         (not (string=? (car res) ""))))
	        (let ((next-pend (rk-pending rkc)))
	          (if (list? (car res))
		      (ustr-insert-seq!  preconv-str res)
		      (ustr-insert-elem! preconv-str res))
	          (if (and next-pend
		           (not (string=? next-pend "")))
		      (ustr-insert-elem! raw-str pend)
		      (if (list? (car res))
		          (begin
			    (ustr-insert-elem! raw-str pend)
			    (ustr-insert-elem! raw-str key-str))
		          (ustr-insert-elem!
		           raw-str
		           (string-append pend key-str))))))))))))

(define canna-context-confirm-kana!
  (lambda (cc)
    (if (= (canna-context-input-rule cc)
	   canna-input-rule-kana)
	(let* ((preconv-str (canna-context-preconv-ustr cc))
	       (rkc (canna-context-rkc cc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
		(ustr-insert-elem! preconv-str residual-kana)
		(rk-flush rkc)))))))

(define (canna-proc-input-state cc key key-state)
  (if (canna-has-preedit? cc)
      (canna-proc-input-state-with-preedit cc key key-state)
      (canna-proc-input-state-no-preedit cc key key-state)))

(define canna-separator
  (lambda (cc)
    (let ((attr (bit-or preedit-separator preedit-underline)))
      (if canna-show-segment-separator?
	  (cons attr canna-segment-separator)
	  #f))))

(define canna-context-transposing-state-preedit
  (lambda (cc)
    (let ((transposing-text (canna-transposing-text cc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define canna-transposing-text
  (lambda (cc)
    (let ((transposing-type (canna-context-transposing-type cc)))
      (cond
       ((= transposing-type canna-type-hiragana)
	(canna-make-whole-string cc #t canna-type-hiragana))
       ((= transposing-type canna-type-katakana)
	(canna-make-whole-string cc #t canna-type-katakana))
       ((= transposing-type canna-type-hankana)
	(canna-make-whole-string cc #t canna-type-hankana))
       ((= transposing-type canna-type-latin)
	(canna-make-whole-raw-string cc #f))
       ((= transposing-type canna-type-wide-latin)
	(canna-make-whole-raw-string cc #t))))))

(define canna-get-raw-str-seq
  (lambda (cc)
    (let* ((rkc (canna-context-rkc cc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (canna-context-raw-ustr cc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if (not (null? residual-kana))
		 (list pending))
	      right-str))))

(define canna-get-raw-candidate
  (lambda (cc cc-id seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (canna-make-whole-string cc #t canna-type-hiragana))))
	   (unconv-candidate (canna-lib-get-unconv-candidate cc-id seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (canna-get-raw-str-seq cc))))
      (cond
       ((= cand-idx canna-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx canna-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) canna-type-katakana))
       ((= cand-idx canna-candidate-type-hankana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) canna-type-hankana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if start
		      (canna-make-raw-string
		       (reverse (sublist raw-str start (+ start (- len 1))))
		       (if (= cand-idx canna-candidate-type-latin) #f #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (canna-compose-state-preedit cc)
  (let* ((cc-id (canna-context-cc-id cc))
	 (segments (canna-context-segments cc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (canna-separator cc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bit-or preedit-reverse
				preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx canna-candidate-type-katakana)
			(canna-lib-get-nth-candidate cc-id seg-idx cand-idx)
			(canna-get-raw-candidate cc cc-id seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (canna-input-state-preedit cc)
  (let* ((preconv-str (canna-context-preconv-ustr cc))
	 (rkc (canna-context-rkc cc))
	 (pending (rk-pending rkc))
	 (kana (canna-context-kana-mode cc))
	 (rule (canna-context-input-rule cc))
	 (extract-kana
	  (if (= rule canna-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (canna-has-preedit? cc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (canna-get-commit-string cc)
  (let ((cc-id (canna-context-cc-id cc))
	(segments (canna-context-segments cc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx canna-candidate-type-katakana)
			     (canna-lib-get-nth-candidate
			      cc-id seg-idx cand-idx)
			     (canna-get-raw-candidate
			      cc cc-id seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (canna-commit-string cc)
  (let ((cc-id (canna-context-cc-id cc))
        (segments (canna-context-segments cc)))
    (for-each (lambda (seg-idx cand-idx)
		(if (> cand-idx canna-candidate-type-katakana)
		    (canna-lib-commit-segment cc-id seg-idx cand-idx)))
	      (iota (ustr-length segments))
	      (ustr-whole-seq segments))
    (if (every (lambda (x) (<= x canna-candidate-type-katakana))
	       (ustr-whole-seq segments))
	(canna-lib-reset-conversion cc-id))))

(define (canna-do-commit cc)
    (im-commit cc (canna-get-commit-string cc))
    (canna-commit-string cc)
    (canna-reset-candidate-window cc)
    (canna-flush cc))

(define canna-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (canna-move-segment cc dir)
  (canna-reset-candidate-window cc)
  (let ((segments (canna-context-segments cc)))
    (ustr-cursor-move! segments dir)
    (canna-correct-segment-cursor segments)))

(define (canna-resize-segment cc cnt)
  (let* ((cc-id (canna-context-cc-id cc))
	 (segments (canna-context-segments cc))
	 (cur-seg (ustr-cursor-pos segments)))
    (canna-reset-candidate-window cc)
    (canna-lib-resize-segment cc-id cur-seg cnt)
    (let* ((resized-nseg (canna-lib-get-nr-segments cc-id))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (canna-move-candidate cc offset)
  (let* ((cc-id (canna-context-cc-id cc))
	 (segments (canna-context-segments cc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (canna-lib-get-nr-candidates cc-id cur-seg))
	 (n (if (< (ustr-cursor-frontside segments) 0) ;; segment-transposing
		0
		(+ (ustr-cursor-frontside segments) offset)))
	 (compensated-n (cond
			 ((>= n max)
			  0)
			 ((< n 0)
			  (- max 1))
			 (else
			  n)))
	 (new-op-count (+ 1 (canna-context-candidate-op-count cc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (canna-context-set-candidate-op-count! cc new-op-count)
    (if (and
	 (= (canna-context-candidate-op-count cc)
	    canna-candidate-op-count)
	 canna-use-candidate-window?)
	(begin
	  (canna-context-set-candidate-window! cc #t)
	  (im-activate-candidate-selector cc max canna-nr-candidate-max)))
    (if (canna-context-candidate-window cc)
	(im-select-candidate cc compensated-n))))

(define canna-move-candidate-in-page
  (lambda (cc numeralc)
    (let* ((cc-id (canna-context-cc-id cc))
	   (segments (canna-context-segments cc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (canna-lib-get-nr-candidates cc-id cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= canna-nr-candidate-max 0)
			 0
			 (quotient n canna-nr-candidate-max)))
	   (pageidx (- (numeral-char->number numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page canna-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (canna-context-candidate-op-count cc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (canna-context-set-candidate-op-count! cc new-op-count)
      (im-select-candidate cc compensated-idx))))

(define (canna-reset-candidate-window cc)
  (if (canna-context-candidate-window cc)
      (begin
	(im-deactivate-candidate-selector cc)
	(canna-context-set-candidate-window! cc #f)))
  (canna-context-set-candidate-op-count! cc 0))

(define canna-set-segment-transposing
  (lambda (cc key key-state)
    (let ((segments (canna-context-segments cc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(canna-reset-candidate-window cc)
	(canna-context-set-candidate-op-count! cc 0)

	(if (canna-transpose-as-wide-latin-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-wide-latin
				    rotate-list)))
	(if (canna-transpose-as-latin-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-latin
				    rotate-list)))
	(if (canna-transpose-as-hankana-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-hankana
				    rotate-list)))
	(if (canna-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-katakana
				    rotate-list)))
	(if (canna-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx canna-candidate-type-hiragana)
	     (= idx canna-candidate-type-katakana)
	     (= idx canna-candidate-type-hankana)
	     (= idx canna-candidate-type-latin)
	     (= idx canna-candidate-type-wide-latin))
	    (let ((lst (member idx rotate-list)))
	      (if (and (not (null? lst))
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (car rotate-list))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (canna-proc-compose-state cc key key-state)
  (let ((cc-id (canna-context-cc-id cc)))
    (cond
     ((canna-prev-page-key? key key-state)
      (if (canna-context-candidate-window cc)
	  (im-shift-page-candidate cc #f)))

     ((canna-next-page-key? key key-state)
      (if (canna-context-candidate-window cc)
	  (im-shift-page-candidate cc #t)))

     ((canna-commit-key? key key-state)
      (canna-do-commit cc))

     ((canna-extend-segment-key? key key-state)
      (canna-resize-segment cc 1))

     ((canna-shrink-segment-key? key key-state)
      (canna-resize-segment cc -1))

     ((canna-next-segment-key? key key-state)
      (canna-move-segment cc 1))

     ((canna-prev-segment-key? key key-state)
      (canna-move-segment cc -1))

     ((canna-beginning-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-beginning! (canna-context-segments cc))
	(canna-reset-candidate-window cc)))

     ((canna-end-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-end! (canna-context-segments cc))
	(canna-correct-segment-cursor (canna-context-segments cc))
	(canna-reset-candidate-window cc)))

     ((canna-backspace-key? key key-state)
      (canna-cancel-conv cc))

     ((canna-next-candidate-key? key key-state)
      (canna-move-candidate cc 1))

     ((canna-prev-candidate-key? key key-state)
      (canna-move-candidate cc -1))

     ((or (canna-transpose-as-hiragana-key? key key-state)
	  (canna-transpose-as-katakana-key? key key-state)
	  (canna-transpose-as-hankana-key?  key key-state)
	  (and
	   (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	   (or
	    (canna-transpose-as-latin-key?  key key-state)
	    (canna-transpose-as-wide-latin-key?  key key-state))))
      (canna-set-segment-transposing cc key key-state))

     ((canna-cancel-key? key key-state)
      (canna-cancel-conv cc))

     ((and canna-select-candidate-by-numeral-key?
	   (numeral-char? key)
	   (canna-context-candidate-window cc))
      (canna-move-candidate-in-page cc key))

     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     ((canna-begin-conv-with-ascii-mode-key? key key-state)
      #f)

     (else
      (begin
	(canna-do-commit cc)
	(canna-proc-input-state cc key key-state))))))

(define (canna-proc-wide-latin cc key key-state)
  (let* ((char (charcode->string key))
	 (w (ja-wide char)))
    (cond
     ((and canna-use-with-vi?
           (canna-vi-escape-key? key key-state))
      (begin
        (canna-flush cc)
        (canna-context-set-wide-latin! cc #f)
        (canna-commit-raw cc)))

     ((canna-on-key? key key-state)
      (canna-flush cc)
      (canna-context-set-on! cc #t))
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (canna-commit-raw cc))
     (w
      (im-commit cc w))
     (else
      (im-commit-raw cc)))
    ()))

(define (canna-press-key-handler cc key key-state)
  (if (control-char? key)
      (im-commit-raw cc)
      (if (canna-context-on cc)
          (if (canna-context-transposing cc)
              (canna-proc-transposing-state cc key key-state)
              (if (canna-context-state cc)
                  (canna-proc-compose-state cc key key-state)
                  (canna-proc-input-state cc key key-state)))
	  (if (canna-context-wide-latin cc)
	      (canna-proc-wide-latin cc key key-state)
	      (canna-proc-raw-state cc key key-state))))
  (canna-update-preedit cc))

;;;
(define (canna-release-key-handler cc key key-state)
  (if (or (control-char? key)
	  (and
	   (not (canna-context-on cc))
	   (not (canna-context-wide-latin cc))))
      (canna-commit-raw cc)))
;;;
(define (canna-reset-handler cc)
  (if (canna-context-on cc)
      (begin
	(if (canna-context-state cc)
	  (let ((cc-id (canna-context-cc-id cc)))
	    (canna-lib-reset-conversion cc-id)))
	(canna-flush cc))))

;;;
(define (canna-get-candidate-handler cc idx accel-enum-hint)
  (let* ((cc-id (canna-context-cc-id cc))
	 (cur-seg (ustr-cursor-pos (canna-context-segments cc)))
	 (cand (canna-lib-get-nth-candidate
		cc-id cur-seg idx)))
    (list cand (digit->string (+ idx 1)) "")))

(define (canna-set-candidate-index-handler cc idx)
  (ustr-cursor-set-frontside! (canna-context-segments cc) idx)
  (canna-update-preedit cc))

(define (canna-proc-raw-state cc key key-state)
  (if (canna-on-key? key key-state)
      (canna-begin-input cc)
      (im-commit-raw cc)))

(canna-configure-widgets)
(register-im
 'canna
 "ja"
 "EUC-JP"
 canna-im-name-label
 canna-im-short-desc
 #f
 canna-init-handler
 canna-release-handler
 context-mode-handler
 canna-press-key-handler
 canna-release-key-handler
 canna-reset-handler
 canna-get-candidate-handler
 canna-set-candidate-index-handler
 context-prop-activate-handler)
