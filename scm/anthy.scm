;;; anthy.scm: Anthy for uim.
;;; charset: EUC-JP
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

(require "util.scm")
(require "ustr.scm")
(require "japanese.scm")
(require "japanese-kana.scm")
(require "japanese-azik.scm")
(require-custom "generic-key-custom.scm")
(require-custom "anthy-custom.scm")
(require-custom "anthy-key-custom.scm")


;;; implementations

(define anthy-lib-initialized? #f)

(define anthy-type-hiragana   0)
(define anthy-type-katakana   1)
(define anthy-type-hankana    2)
(define anthy-type-latin      3)
(define anthy-type-wide-latin 4)

(define anthy-input-rule-roma 0)
(define anthy-input-rule-kana 1)
(define anthy-input-rule-azik 2)

(define anthy-prepare-activation
  (lambda (ac)
    (anthy-flush ac)
    (anthy-update-preedit ac)))

(register-action 'action_anthy_hiragana
;;		 (indication-alist-indicator 'action_anthy_hiragana
;;					     anthy-input-mode-indication-alist)
		 (lambda (ac) ;; indication handler
		   '(figure_ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (ac) ;; activity predicate
		   (and (anthy-context-on ac)
			(= (anthy-context-kana-mode ac)
			   anthy-type-hiragana)))

		 (lambda (ac) ;; action handler
		   (anthy-prepare-activation ac)
		   (anthy-context-set-on! ac #t)
		   (anthy-context-set-kana-mode! ac anthy-type-hiragana)))

(register-action 'action_anthy_katakana
;;		 (indication-alist-indicator 'action_anthy_katakana
;;					     anthy-input-mode-indication-alist)
		 (lambda (ac)
		   '(figure_ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (ac)
		   (and (anthy-context-on ac)
			(= (anthy-context-kana-mode ac)
			   anthy-type-katakana)))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (anthy-context-set-on! ac #t)

		   (if (= anthy-input-rule-kana
			  (anthy-context-input-rule ac))
		       (rk-context-set-rule! (anthy-context-rkc ac)
					     ja-kana-katakana-rule))
		   (anthy-context-set-kana-mode! ac anthy-type-katakana)))

(register-action 'action_anthy_hankana
;;		 (indication-alist-indicator 'action_anthy_hankana
;;					     anthy-input-mode-indication-alist)
		 (lambda (ac)
		   '(figure_ja_hankana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (ac)
		   (and (anthy-context-on ac)
			(= (anthy-context-kana-mode ac)
			   anthy-type-hankana)))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (anthy-context-set-on! ac #t)
		   (anthy-context-set-kana-mode! ac anthy-type-hankana)))

(register-action 'action_anthy_direct
;;		 (indication-alist-indicator 'action_anthy_direct
;;					     anthy-input-mode-indication-alist)
		 (lambda (ac)
		   '(figure_ja_direct
		     "a"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (ac)
		   (and (not (anthy-context-on ac))
			(not (anthy-context-wide-latin ac))))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (anthy-context-set-on! ac #f)
		   (anthy-context-set-wide-latin! ac #f)))

(register-action 'action_anthy_zenkaku
;;		 (indication-alist-indicator 'action_anthy_zenkaku
;;					     anthy-input-mode-indication-alist)
		 (lambda (ac)
		   '(figure_ja_zenkaku
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (ac)
		   (and (not (anthy-context-on ac))
			(anthy-context-wide-latin ac)))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (anthy-context-set-on! ac #f)
		   (anthy-context-set-wide-latin! ac #t)))

(register-action 'action_anthy_roma
;;		 (indication-alist-indicator 'action_anthy_roma
;;					     anthy-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(figure_ja_roma
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-context-input-rule ac)
		      anthy-input-rule-roma))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (rk-context-set-rule! (anthy-context-rkc ac)
					 ja-rk-rule)
		   (anthy-context-set-input-rule! ac anthy-input-rule-roma)))

(register-action 'action_anthy_kana
;;		 (indication-alist-indicator 'action_anthy_kana
;;					     anthy-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(figure_ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (ac)
		   (= (anthy-context-input-rule ac)
		      anthy-input-rule-kana))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (rk-context-set-rule! (anthy-context-rkc ac)
					 (if (= (anthy-context-kana-mode ac)
						anthy-type-katakana)
						ja-kana-katakana-rule
						ja-kana-hiragana-rule))

		   (anthy-context-set-input-rule! ac anthy-input-rule-kana)
		   ;;(define-key anthy-kana-toggle-key? "")
		   ;;(define-key anthy-latin-key? generic-on-key?)
		   ;;(define-key anthy-wide-latin-key? "")
		   ))

(register-action 'action_anthy_azik
;;		 (indication-alist-indicator 'action_anthy_azik
;;					     anthy-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(figure_ja_azik
		     "Ａ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-context-input-rule ac)
		      anthy-input-rule-azik))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (rk-context-set-rule! (anthy-context-rkc ac)
					 ja-azik-rule)
		   (anthy-context-set-input-rule! ac anthy-input-rule-azik)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define anthy-configure-widgets
  (lambda ()
    (register-widget 'widget_anthy_input_mode
		     (activity-indicator-new anthy-input-mode-actions)
		     (actions-new anthy-input-mode-actions))

    (register-widget 'widget_anthy_kana_input_method
		     (activity-indicator-new anthy-kana-input-method-actions)
		     (actions-new anthy-kana-input-method-actions))
    (context-list-replace-widgets! 'anthy anthy-widgets)))

(define anthy-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'converting         #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'ac-id              #f) ;; anthy-context-id
    (list 'preconv-ustr       #f) ;; preedit strings
    (list 'rkc                #f)
    (list 'segments           #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'wide-latin         #f)
    (list 'kana-mode          anthy-type-hiragana)
    (list 'commit-raw         #t)
    (list 'input-rule         anthy-input-rule-roma)
    (list 'raw-ustr           #f))))
(define-record 'anthy-context anthy-context-rec-spec)
(define anthy-context-new-internal anthy-context-new)

(define anthy-context-new
 (lambda (id im)
   (let ((ac (anthy-context-new-internal id im))
	 (rkc (rk-context-new ja-rk-rule #t #f)))
     (if (symbol-bound? 'anthy-lib-init)
	 (set! anthy-lib-initialized? (anthy-lib-init)))
     (if anthy-lib-initialized?
	 (anthy-context-set-ac-id! ac (anthy-lib-alloc-context)))
     (anthy-context-set-widgets! ac anthy-widgets)
     (anthy-context-set-rkc! ac rkc)
     (anthy-context-set-preconv-ustr! ac (ustr-new))
     (anthy-context-set-raw-ustr! ac (ustr-new))
     (anthy-context-set-segments! ac (ustr-new))

     ;; 2004-08-26 Takuro Ashie <ashie@homa.ne.jp>
     ;;   * I think load-kana-table should be marked as depracated.
     ;;     Because it is a little violent (it overwrites ja-rk-rule table).
     ;;     We should prepare a custom entry like "uim-default-input-rule"
     ;;     instead of using-kana-table.
     (if using-kana-table?
	 (anthy-context-set-input-rule! ac anthy-input-rule-kana)
	 (anthy-context-set-input-rule! ac anthy-input-rule-roma))
     ac)))

(define anthy-commit-raw
  (lambda (ac)
    (im-commit-raw ac)
    (anthy-context-set-commit-raw! ac #t)))

(define anthy-context-kana-toggle
  (lambda (ac)
    (let* ((kana (anthy-context-kana-mode ac))
	   (opposite-kana (multi-segment-opposite-kana kana)))
      (anthy-context-set-kana-mode! ac opposite-kana))))

;; TODO: generarize as multi-segment procedure
;; side effect: none. rkc will not be altered
(define anthy-make-whole-string
  (lambda (ac convert-pending-into-kana? kana)
    (let* ((rkc (anthy-context-rkc ac))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (rule (anthy-context-input-rule ac))
	   (preconv-str (anthy-context-preconv-ustr ac))
	   (extract-kana
	    (if (= rule anthy-input-rule-kana)
		(lambda (entry) (car entry))
		(lambda (entry) (list-ref entry kana)))))

      (string-append
       (string-append-map-ustr-former extract-kana preconv-str)
       (if convert-pending-into-kana?
	   (if residual-kana
	       (extract-kana residual-kana)
	       (if (= rule anthy-input-rule-kana)
		   pending
		   ""))
	   pending)
       (string-append-map-ustr-latter extract-kana preconv-str)))))

(define anthy-make-raw-string
  (lambda (raw-str-list wide?)
    (if (not (null? raw-str-list))
        (if wide?
            (string-append
             (ja-string-list-to-wide-alphabet (string-to-list (car raw-str-list)))
             (anthy-make-raw-string (cdr raw-str-list) wide?))
            (string-append
             (car raw-str-list)
             (anthy-make-raw-string (cdr raw-str-list) wide?)))
        "")))

(define anthy-make-whole-raw-string
  (lambda (ac wide?)
    (let* ((rkc (anthy-context-rkc ac))
	   (pending (rk-pending rkc))
           (residual-kana (rk-push-key-last! rkc))
	   (raw-str (anthy-context-raw-ustr ac))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
      (anthy-make-raw-string
       (ja-raw-string-list-to-valid-roma
        (append left-str
                (if (null? residual-kana)
		    (begin
		      (if (null? right-str)
			  (list pending)
			  (append right-str (list pending))))
                    (begin
                      (rk-flush rkc)
                      (if (null? right-str)
                          (list pending)
			  (append right-str (list pending)))))))
       wide?))))

(define anthy-init-handler
  (lambda (id im arg)
    (anthy-context-new id im)))

(define anthy-release-handler
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac)))
      (anthy-lib-free-context ac-id))))

(define anthy-flush
  (lambda (ac)
    (rk-flush (anthy-context-rkc ac))
    (ustr-clear! (anthy-context-preconv-ustr ac))
    (ustr-clear! (anthy-context-raw-ustr ac))
    (ustr-clear! (anthy-context-segments ac))
    (anthy-context-set-transposing! ac #f)
    (anthy-context-set-converting! ac #f)
    (if (anthy-context-candidate-window ac)
	  (im-deactivate-candidate-selector ac))
    (anthy-context-set-candidate-window! ac #f)
    (anthy-context-set-candidate-op-count! ac 0)))

(define anthy-begin-input
  (lambda (ac)
    (anthy-context-set-on! ac #t)
    (rk-flush (anthy-context-rkc ac))
    (anthy-context-set-converting! ac #f)))

(define anthy-update-preedit
  (lambda (ac)
    (if (not (anthy-context-commit-raw ac))
	(let ((segments (if (anthy-context-on ac)
			    (if (anthy-context-transposing ac)
				(anthy-context-transposing-state-preedit ac)
				(if (anthy-context-converting ac)
				    (anthy-converting-state-preedit ac)
				    (anthy-input-state-preedit ac)))
			    ())))
	  (context-update-preedit ac segments))
	(anthy-context-set-commit-raw! ac #f))))
  
(define anthy-proc-raw-state
  (lambda (ac key key-state)
    (if (anthy-on-key? key key-state)
	(anthy-begin-input ac)
	(anthy-commit-raw ac))))

(define anthy-begin-conv
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (kana (anthy-context-kana-mode ac))
	   (preconv-str (anthy-make-whole-string ac #t anthy-type-hiragana)))
      (if (and (number? (anthy-context-ac-id ac))
	       (> (string-length preconv-str)
		  0))
	  (begin
	    (anthy-lib-set-string ac-id preconv-str)
	    (let ((nr-segments (anthy-lib-get-nr-segments ac-id)))
	      (ustr-set-latter-seq! (anthy-context-segments ac)
				    (make-list nr-segments 0))
	      (anthy-context-set-converting! ac #t)
	      ;; Don't perform rk-flush here. The rkc must be restored when
	      ;; anthy-cancel-conv invoked -- YamaKen 2004-10-25
	      ))))))

(define anthy-cancel-conv
  (lambda (ac)
    (anthy-reset-candidate-window ac)
    (anthy-context-set-converting! ac #f)
    (ustr-clear! (anthy-context-segments ac))))

(define anthy-proc-input-state-no-preedit
  (lambda (ac key key-state)
    (let ((rkc (anthy-context-rkc ac))
	  (direct (ja-direct (charcode->string key)))
	  (rule (anthy-context-input-rule ac)))
      (cond
       ((anthy-wide-latin-key? key key-state)
	(begin
	  (anthy-flush ac)
	  (anthy-context-set-on! ac #f)
	  (anthy-context-set-wide-latin! ac #t)))
	  
       ((anthy-latin-key? key key-state)
	   (begin
	     (anthy-flush ac)
	     (anthy-context-set-on! ac #f)
	     (anthy-context-set-wide-latin! ac #f)))

       ((anthy-backspace-key? key key-state)
	(anthy-commit-raw ac))

       ((anthy-delete-key? key key-state)
	(anthy-commit-raw ac))
       
       ((anthy-hankaku-kana-key? key key-state)
	(anthy-context-set-kana-mode! ac anthy-type-hankana))

       ((anthy-kana-toggle-key? key key-state)
	(anthy-context-kana-toggle ac))

       ;; modifiers (except shift) => ignore
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	(anthy-commit-raw ac))
       
       ;; direct key => commit
       (direct
	(im-commit ac direct))

       ((symbol? key)
	(anthy-commit-raw ac))

       (else
	(let* ((key-str (charcode->string
			 (if (= rule anthy-input-rule-kana)
			     key
			     (to-lower-char key))))
	       (res (rk-push-key! rkc key-str)))
	  (if res
              (begin
		(ustr-insert-elem! (anthy-context-preconv-ustr ac)
				   res)
		(ustr-insert-elem! (anthy-context-raw-ustr ac)
				   key-str))
	      (if (not (rk-pending rkc)
                  (anthy-commit-raw ac))))))))))

(define anthy-has-preedit?
  (lambda (ac)
    (or (not (ustr-empty? (anthy-context-preconv-ustr ac)))
	(> (length (rk-pending (anthy-context-rkc ac))) 0))))

(define anthy-proc-transposing-state
  (lambda (ac key key-state)
    (cond
     ((anthy-transpose-as-katakana-key? key key-state)
      (anthy-context-set-transposing-type! ac anthy-type-katakana))

     ((anthy-transpose-as-hankana-key? key key-state)
      (anthy-context-set-transposing-type! ac anthy-type-hankana))

     ((anthy-transpose-as-latin-key? key key-state)
      (anthy-context-set-transposing-type! ac anthy-type-latin))

     ((anthy-transpose-as-wide-latin-key? key key-state)
      (anthy-context-set-transposing-type! ac anthy-type-wide-latin))

     (else
      (begin
	; commit
	(im-commit ac (anthy-transposing-text ac))
	(anthy-flush ac)
	(if (not (anthy-commit-key? key key-state))
	    (begin 
	      (anthy-context-set-transposing! ac #f)
	      (anthy-proc-input-state ac key key-state))))))))

(define anthy-proc-input-state-with-preedit
  (lambda (ac key key-state)
    (let ((preconv-str (anthy-context-preconv-ustr ac))
	  (raw-str (anthy-context-raw-ustr ac))
	  (rkc (anthy-context-rkc ac))
	  (kana (anthy-context-kana-mode ac))
	  (rule (anthy-context-input-rule ac)))
      (cond

       ;; begin conversion
       ((anthy-begin-conv-key? key key-state)
	(anthy-begin-conv ac))
       
       ;; backspace
       ((anthy-backspace-key? key key-state)
	(if (not (rk-backspace rkc))
            (begin
	      (ustr-cursor-delete-backside! preconv-str)
	      (ustr-cursor-delete-backside! raw-str))))

       ;; delete
       ((anthy-delete-key? key key-state)
	(if (not (rk-delete rkc))
            (begin
	      (ustr-cursor-delete-frontside! preconv-str)
	      (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
       ((anthy-kill-key? key key-state)
	(ustr-clear-latter! preconv-str))
       
       ;; kill-backward
       ((anthy-kill-backward-key? key key-state)
	(begin
	  (rk-flush rkc)
	  (ustr-clear-former! preconv-str)))

       ;; 現在とは逆のかなモードでかなを確定する
       ((anthy-commit-as-opposite-kana-key? key key-state)
	(begin
	  (im-commit
	   ac
	   (anthy-make-whole-string ac #t (multi-segment-opposite-kana kana)))
	  (anthy-flush ac)))

       ;; Transposing状態へ移行
       ((or (anthy-transpose-as-katakana-key?   key key-state)
	    (anthy-transpose-as-hankana-key?    key key-state)
	    (anthy-transpose-as-latin-key?      key key-state)
	    (anthy-transpose-as-wide-latin-key? key key-state))
	(begin
	  (anthy-context-set-transposing! ac #t)
	  (anthy-proc-transposing-state ac key key-state)))

       ;; Commit current preedit string, then toggle hiragana/katakana mode.
       ((anthy-kana-toggle-key? key key-state)
	(begin
	  (im-commit
	   ac
	   (anthy-make-whole-string ac #t kana))
	  (anthy-flush ac)
	  (anthy-context-kana-toggle ac)))

       ;; cancel
       ((anthy-cancel-key? key key-state)
	(anthy-flush ac))

       ;; commit
       ((anthy-commit-key? key key-state)
	(begin
	  (im-commit
	   ac
	   (anthy-make-whole-string ac #t kana))
	  (anthy-flush ac)))

       ;; left
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-go-left-key? key key-state)
	(anthy-context-confirm-kana! ac)
	(ustr-cursor-move-backward! preconv-str)
	(ustr-cursor-move-backward! raw-str))

       ;; right
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-go-right-key? key key-state)
	(anthy-context-confirm-kana! ac)
	(ustr-cursor-move-forward! preconv-str)
	(ustr-cursor-move-forward! raw-str))

       ;; beginning-of-preedit
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-beginning-of-preedit-key? key key-state)
	(anthy-context-confirm-kana! ac)
	(ustr-cursor-move-beginning! preconv-str))

       ;; end-of-preedit
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-end-of-preedit-key? key key-state)
	(anthy-context-confirm-kana! ac)
	(ustr-cursor-move-end! preconv-str))

       ;; modifiers (except shift) => ignore
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	(anthy-commit-raw ac))

       (else	
	(let* ((key-str (charcode->string 
			 (if (= rule anthy-input-rule-kana)
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
		    (ustr-insert-elem! raw-str (string-append pend key-str))))
	      )))))))

(define anthy-context-confirm-kana!
  (lambda (ac)
    (if (= (anthy-context-input-rule ac)
	   anthy-input-rule-kana)
	(let* ((preconv-str (anthy-context-preconv-ustr ac))
	       (rkc (anthy-context-rkc ac))
	       (residual-kana (rk-peek-terminal-match rkc)))
	    (if residual-kana
		(begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (rk-flush rkc)))))))

(define anthy-proc-input-state
  (lambda (ac key key-state)
    (if (anthy-has-preedit? ac)
	(anthy-proc-input-state-with-preedit ac key key-state)
	(anthy-proc-input-state-no-preedit ac key key-state))))

(define anthy-separator
  (lambda (ac)
    (let ((attr (bit-or preedit-separator
			preedit-underline)))
      (if anthy-show-segment-separator?
	  (cons attr anthy-segment-separator)
	  #f))))

(define anthy-context-transposing-state-preedit
  (lambda (ac)
    (let* ((transposing-text (anthy-transposing-text ac)))
      (list (cons preedit-underline transposing-text)
	    (cons preedit-cursor "")))))

(define anthy-transposing-text
  (lambda (ac)
    (let* ((transposing-type (anthy-context-transposing-type ac)))
      (cond
       ((= transposing-type anthy-type-katakana)
	(anthy-make-whole-string ac #t multi-segment-type-katakana))

       ((= transposing-type anthy-type-hankana)
	(anthy-make-whole-string ac #t multi-segment-type-hankana))

       ((= transposing-type anthy-type-latin)
	(anthy-make-whole-raw-string ac #f))

       ((= transposing-type anthy-type-wide-latin)
	(anthy-make-whole-raw-string ac #t))
       ))))

(define anthy-converting-state-preedit
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (separator (anthy-separator ac)))
      (append-map
       (lambda (seg-idx cand-idx)
	 (let* ((attr (if (= seg-idx cur-seg)
			  (bit-or preedit-reverse
				  preedit-cursor)
			  preedit-underline))
		(cand (anthy-lib-get-nth-candidate ac-id seg-idx cand-idx))
		(seg (list (cons attr cand))))
	   (if (and separator
		    (< 0 seg-idx))
	       (cons separator seg)
	       seg)))
       (iota (ustr-length segments))
       (ustr-whole-seq segments)))))

(define anthy-input-state-preedit
  (lambda (ac)
    (let* ((preconv-str (anthy-context-preconv-ustr ac))
	   (rkc (anthy-context-rkc ac))
	   (pending (rk-pending rkc))
	   (kana (anthy-context-kana-mode ac))
	   (rule (anthy-context-input-rule ac))
	   (extract-kana
	    (if (= rule anthy-input-rule-kana)
		(lambda (entry) (car entry))
		(lambda (entry) (list-ref entry kana)))))

      (list
       (and (not (ustr-cursor-at-beginning? preconv-str))
	    (cons preedit-underline
		  (string-append-map-ustr-former extract-kana preconv-str)))
       (and (> (length pending) 0)
	    (cons preedit-underline pending))
       (and (anthy-has-preedit? ac)
	    (cons preedit-cursor ""))
       (and (not (ustr-cursor-at-end? preconv-str))
	    (cons preedit-underline
		  (string-append-map-ustr-latter extract-kana preconv-str)))))))

(define anthy-get-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (string-append-map (lambda (seg-idx cand-idx)
			   (anthy-lib-get-nth-candidate ac-id seg-idx cand-idx))
			 (iota (ustr-length segments))
			 (ustr-whole-seq segments)))))

(define anthy-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (for-each (lambda (seg-idx cand-idx)
		  (anthy-lib-commit-segment ac-id seg-idx cand-idx))
		(iota (ustr-length segments))
		(ustr-whole-seq segments)))))

(define anthy-do-commit
  (lambda (ac)
    (im-commit ac (anthy-get-commit-string ac))
    (anthy-commit-string ac)
    (anthy-reset-candidate-window ac)
    (anthy-flush ac)))

(define anthy-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define anthy-move-segment
  (lambda (ac offset)
    (anthy-reset-candidate-window ac)
    (let ((segments (anthy-context-segments ac)))
      (ustr-cursor-move! segments offset)
      (anthy-correct-segment-cursor segments))))

(define anthy-resize-segment
  (lambda (ac cnt)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments)))
      (anthy-reset-candidate-window ac)
      (anthy-lib-resize-segment ac-id cur-seg cnt)
      (let* ((resized-nseg (anthy-lib-get-nr-segments ac-id))
	     (latter-nseg (- resized-nseg cur-seg)))
	(ustr-set-latter-seq! segments (make-list latter-nseg 0))))))

(define anthy-move-candidate
  (lambda (ac offset)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (n (+ (ustr-cursor-frontside segments)
		 offset))
	   (compensated-n (cond
			   ((>= n max)
			    0)
			   ((< n 0)
			    (- max 1))
			   (else
			    n)))
	   (new-op-count (+ 1 (anthy-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-n)
      (anthy-context-set-candidate-op-count! ac new-op-count)
      (if (and anthy-use-candidate-window?
	       (= (anthy-context-candidate-op-count ac)
		  anthy-candidate-op-count))
	  (begin
	    (anthy-context-set-candidate-window! ac #t)
	    (im-activate-candidate-selector ac max anthy-nr-candidate-max)))
      (if (anthy-context-candidate-window ac)
	  (im-select-candidate ac compensated-n)))))

(define anthy-move-candidate-in-page
  (lambda (ac numeralc)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= anthy-nr-candidate-max 0)
	   		 0
			 (quotient n anthy-nr-candidate-max)))
	   (pageidx (- (numeral-char->number numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page anthy-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (anthy-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (anthy-context-set-candidate-op-count! ac new-op-count)
      (im-select-candidate ac compensated-idx))))

(define anthy-reset-candidate-window
  (lambda (ac)
    (if (anthy-context-candidate-window ac)
	(begin
	  (im-deactivate-candidate-selector ac)
	  (anthy-context-set-candidate-window! ac #f)))
    (anthy-context-set-candidate-op-count! ac 0)))

(define anthy-proc-converting-state
  (lambda (ac key key-state)
    (cond
     ((anthy-prev-page-key? key key-state)
      (if (anthy-context-candidate-window ac)
	  (im-shift-page-candidate ac #f)))

     ((anthy-next-page-key? key key-state)
      (if (anthy-context-candidate-window ac)
	  (im-shift-page-candidate ac #t)))

     ((anthy-commit-key? key key-state)
      (anthy-do-commit ac))
     
     ((anthy-extend-segment-key? key key-state)
      (anthy-resize-segment ac 1))
     
     ((anthy-shrink-segment-key? key key-state)
      (anthy-resize-segment ac -1))
     
     ((anthy-next-segment-key? key key-state)
      (anthy-move-segment ac 1))
     
     ((anthy-prev-segment-key? key key-state)
      (anthy-move-segment ac -1))

     ((anthy-beginning-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-beginning! (anthy-context-segments ac))
	(anthy-reset-candidate-window ac)))

     ((anthy-end-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-end! (anthy-context-segments ac))
	(anthy-correct-segment-cursor (anthy-context-segments ac))
	(anthy-reset-candidate-window ac)))

     ((anthy-backspace-key? key key-state)
      (anthy-cancel-conv ac))

     ((anthy-next-candidate-key? key key-state)
      (anthy-move-candidate ac 1))

     ((anthy-prev-candidate-key? key key-state)
      (anthy-move-candidate ac -1))

     ((anthy-cancel-key? key key-state)
      (anthy-cancel-conv ac))

     ((and anthy-select-candidate-by-numeral-key?
	   (numeral-char? key)
	   (anthy-context-candidate-window ac))
      (anthy-move-candidate-in-page ac key))

     ;; don't discard shift-modified keys. Some of them ("?", "~",
     ;; etc) are used to implicit commit. Reported by [Anthy-dev 745]
     ;; -- YamaKen 2004-04-08
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      #f)  ;; use #f rather than () to conform to R5RS

     ((symbol? key)
      #f)

     (else
      (begin
	(anthy-do-commit ac)
	(anthy-proc-input-state ac key key-state))))))

(define anthy-proc-wide-latin
  (lambda (ac key key-state)
    (let* ((char (charcode->string key))
	   (w (or (ja-direct char)
		  (ja-wide char))))
      (cond
       ((anthy-on-key? key key-state)
	(anthy-flush ac)
	(anthy-context-set-on! ac #t))
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	(anthy-commit-raw ac))
       (w
	(im-commit ac w))
       (else
	(anthy-commit-raw ac)))
      ())))

(define anthy-press-key-handler
  (lambda (ac key key-state)
    (if (control-char? key)
	(im-commit-raw ac)
	(if (anthy-context-on ac)
	    (if (anthy-context-transposing ac)
		(anthy-proc-transposing-state ac key key-state)
		(if (anthy-context-converting ac)
		    (anthy-proc-converting-state ac key key-state)
		    (anthy-proc-input-state ac key key-state)))
	    (if (anthy-context-wide-latin ac)
		(anthy-proc-wide-latin ac key key-state)
		(anthy-proc-raw-state ac key key-state))))
    ;; preedit
    (anthy-update-preedit ac)
))


(define anthy-release-key-handler
  (lambda (ac key key-state)
    (if (or (control-char? key)
	    (and (not (anthy-context-on ac))
		 (not (anthy-context-wide-latin ac))))
	;; don't discard key release event for apps
	(anthy-commit-raw ac))))

(define anthy-reset-handler
  (lambda (ac)
    (if (anthy-context-on ac)
	(anthy-flush ac))
    ;; code to commit pending string must not be added to here.
    ;; -- YamaKen 2004-10-21
    ))

(define anthy-get-candidate-handler
  (lambda (ac idx accel-enum-hint)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (cur-seg (ustr-cursor-pos (anthy-context-segments ac)))
	   (cand (anthy-lib-get-nth-candidate ac-id cur-seg idx)))
      (list cand (digit->string (+ idx 1)) ""))))

(define anthy-set-candidate-index-handler
  (lambda (ac idx)
    (ustr-cursor-set-frontside! (anthy-context-segments ac) idx)
;    (anthy-move-segment ac 1)
    (anthy-update-preedit ac)))

(anthy-configure-widgets)

(register-im
 'anthy
 "ja"
 "EUC-JP"
 anthy-im-name-label
 anthy-im-short-desc
 #f
 anthy-init-handler
 anthy-release-handler
 context-mode-handler
 anthy-press-key-handler
 anthy-release-key-handler
 anthy-reset-handler
 anthy-get-candidate-handler
 anthy-set-candidate-index-handler
 context-prop-activate-handler
)
