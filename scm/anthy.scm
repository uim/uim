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

(require "util.scm")
(require "ustr.scm")
(require "event.scm")
(require "evmap.scm")
(require "legacy-api-bridge.scm")
(require "ng-japanese.scm")
(require-custom "generic-key-custom.scm")
(require-custom "anthy-custom.scm")
(require-custom "anthy-key-custom.scm")


;;; implementations

(define anthy-lib-initialized? #f)

(define anthy-type-hiragana 0)
(define anthy-type-katakana 1)
(define anthy-type-hankana 2)
(define anthy-type-halfwidth 3)
(define anthy-type-fullwidth 4)
(define anthy-type-direct 5)

(define anthy-input-rule-roma 0)
(define anthy-input-rule-kana 1)
(define anthy-input-rule-azik 2)
(define anthy-input-rule-nicola 3)

(define anthy-direct-convert-opposite-kana -1)
(define anthy-direct-convert-hiragana -2)
(define anthy-direct-convert-katakana -3)
(define anthy-direct-convert-hankana -4)
(define anthy-direct-convert-latin -5)
(define anthy-direct-convert-wide-latin -6)

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
		   (anthy-switch-kana-mode! ac anthy-type-hiragana)))

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
		   (anthy-switch-kana-mode! ac anthy-type-katakana)))

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
		   (anthy-switch-kana-mode! ac anthy-type-hankana)))

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
		   (anthy-context-set-wide-latin! ac #f)
		   (anthy-switch-kana-mode! ac anthy-type-direct)))

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
		   (anthy-context-set-wide-latin! ac #t)
		   (anthy-switch-kana-mode! ac anthy-type-fullwidth)))

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
		   (anthy-switch-ruletree! ac
					   anthy-input-rule-roma
					   (anthy-context-kana-mode ac))))

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
		   (anthy-switch-ruletree! ac
					   anthy-input-rule-kana
					   (anthy-context-kana-mode ac))
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
		   (anthy-switch-ruletree! ac
					   anthy-input-rule-azik
					   (anthy-context-kana-mode ac))))

(register-action 'action_anthy_nicola
;;		 (indication-alist-indicator 'action_anthy_nicola
;;					     anthy-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(figure_ja_nicola
		     "親"
		     "NICOLA"
		     "NICOLA入力モード"))
		 (lambda (ac)
		   (= (anthy-context-input-rule ac)
		      anthy-input-rule-nicola))
		 (lambda (ac)
		   (anthy-prepare-activation ac)
		   (anthy-switch-ruletree! ac
					   anthy-input-rule-nicola
					   (anthy-context-kana-mode ac))))

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

(define evmap-context-list-preedit-string
  (lambda (emc-list)
    (apply string-append
	   (apply append
		  (filter-map evmap-context-preedit-string
			      emc-list)))))

(define evmap-ustr-preedit-string
  (lambda (ustr)
    (evmap-context-list-preedit-string (ustr-whole-seq ustr))))

(define evmap-ustr-input-with-new-emc!
  (lambda (ustr ruletree ev)
    (let* ((emc (evmap-context-new ruletree))
	   (closer-tree (evmap-context-input! emc ev)))
      (if closer-tree
	  (begin
	    (ustr-insert-elem! ustr emc)
	    closer-tree)))))

(define evmap-ustr-input!
  (lambda (ustr ruletree ev)
    (let* ((former-emc (and (not (ustr-cursor-at-beginning? ustr))
			    (ustr-cursor-backside ustr)))
	   (closer-tree (or (and former-emc
				 (not (evmap-context-complete? former-emc))
				 (evmap-context-input! former-emc ev))
			    (evmap-ustr-input-with-new-emc! ustr ruletree ev))))
      (if (event-loopback ev)
	  (begin
	    (event-set-loopback! ev #f)
	    (evmap-ustr-input! ustr ruletree ev))
	  closer-tree))))

;; TODO: Support following alternative behavior
;; "ちゃ" -> backspace -> "ち"
(define evmap-ustr-backspace!
  (lambda (ustr)
    (let ((former-emc (and (not (ustr-cursor-at-beginning? ustr))
			   (ustr-cursor-backside ustr))))
      (if former-emc
	  (if (evmap-context-complete? former-emc)
	      (ustr-cursor-delete-backside! ustr)
	      (evmap-context-undo! former-emc))))))

(define evmap-ustr-transpose
  (lambda (ustr new-ruletree)
    (fold (lambda (ev new-ustr)
	    (evmap-ustr-input! new-ustr new-ruletree ev)
	    new-ustr)
	  (ustr-new)
	  (append-map-ustr-whole evmap-context-event-seq ustr))))

(define anthy-ruletree
  (let ((alphanumeric-alist
	 (list
	  (cons anthy-type-fullwidth ja-fullwidth-alphanumeric-ruletree)
	  (cons anthy-type-halfwidth ja-halfwidth-alphanumeric-ruletree)
	  (cons anthy-type-direct    ja-direct-ruletree))))
    (lambda (input-rule kana-mode)
      (safe-cdr
       (assoc kana-mode
	      (cond
	       ((= input-rule anthy-input-rule-roma)
		(require "ng-japanese-romaji.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-romaji-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-romaji-katakana-ruletree)
		  (cons anthy-type-hankana   ja-romaji-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-kana)
		(require "ng-japanese-kana.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-kana-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-kana-katakana-ruletree)
		  (cons anthy-type-hankana   ja-kana-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-azik)
		(require "ng-japanese-azik.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-azik-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-azik-katakana-ruletree)
		  (cons anthy-type-hankana   ja-azik-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-nicola)
		(require "japanese-nicola.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-nicola-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-nicola-katakana-ruletree)
		  (cons anthy-type-hankana   ja-nicola-halfkana-ruletree))
		 alphanumeric-alist))))))))

(define anthy-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'converting         #f)
    (list 'ac-id              #f) ;; anthy-context-id
    (list 'preconv-ustr       #f) ;; preedit strings
    (list 'segments           #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'wide-latin         #f)
    (list 'kana-mode          anthy-type-hiragana)
    (list 'input-rule         anthy-input-rule-roma)
    (list 'ruletree           #f)
    (list 'keytrans-emc      #f))))  ;; evmap-context for key-event translator
(define-record 'anthy-context anthy-context-rec-spec)
(define anthy-context-new-internal anthy-context-new)

(define anthy-context-new
 (lambda (id im)
   (let ((ac (anthy-context-new-internal id im)))
     (if (symbol-bound? 'anthy-lib-init)
	 (set! anthy-lib-initialized? (anthy-lib-init)))
     (if anthy-lib-initialized?
	 (anthy-context-set-ac-id! ac (anthy-lib-alloc-context)))
     (anthy-context-set-widgets! ac anthy-widgets)
     (anthy-context-set-preconv-ustr! ac (ustr-new))
     (anthy-context-set-segments! ac (ustr-new))
     (anthy-context-set-keytrans-emc! ac (key-event-translator-new))

     ;; 2004-08-26 Takuro Ashie <ashie@homa.ne.jp>
     ;;   * I think load-kana-table should be marked as depracated.
     ;;     Because it is a little violent (it overwrites ja-rk-rule table).
     ;;     We should prepare a custom entry like "uim-default-input-rule"
     ;;     instead of using-kana-table.
     (if (and (symbol-bound? 'using-kana-table?)
	      using-kana-table?)
	 (anthy-context-set-input-rule! ac anthy-input-rule-kana))
     (anthy-switch-ruletree! ac
			     (anthy-context-input-rule ac)
			     (anthy-context-kana-mode ac))
     ac)))

(define anthy-switch-ruletree!
  (lambda (ac input-rule kana-mode)
    (let ((ruletree (anthy-ruletree input-rule kana-mode)))
      (anthy-context-set-input-rule! ac input-rule)
      (anthy-context-set-kana-mode! ac kana-mode)
      (anthy-context-set-ruletree! ac ruletree))))

(define anthy-switch-kana-mode!
  (lambda (ac kana-mode)
    (let ((rule (anthy-context-input-rule ac)))
      (anthy-switch-ruletree! ac rule kana-mode))))

(define anthy-toggle-kana-mode!
  (lambda (ac)
    (let* ((kana (anthy-context-kana-mode ac))
	   (opposite-kana (multi-segment-opposite-kana kana)))
      (anthy-switch-kana-mode! ac opposite-kana))))

(define anthy-transpose-preconv!
  (lambda (ac kana-mode)
    (let* ((preconv-ustr (anthy-context-preconv-ustr ac))
	   (rule (anthy-context-input-rule ac))
	   (ruletree (anthy-ruletree rule kana-mode))
	   (transposed (evmap-ustr-transpose preconv-ustr ruletree)))
      (anthy-context-set-preconv-ustr! ac transposed))))

(define anthy-commit-transposed-preconv!
  (lambda (ac kana-mode)
    (anthy-transpose-preconv! ac kana-mode)
    (anthy-commit! ac)))

(define anthy-commit!
  (lambda (ac)
    (im-commit ac (evmap-ustr-preedit-string (anthy-context-preconv-ustr ac)))
    (anthy-flush ac)))

(define anthy-input!
  (lambda (ac ev)
    (let ((ruletree (anthy-context-ruletree ac))
	  (preconv-ustr (anthy-context-preconv-ustr ac)))
      (if (evmap-ustr-input! preconv-ustr ruletree ev)
	  (anthy-update-preedit ac)))))

(define anthy-init-handler
  (lambda (id im arg)
    (anthy-context-new id im)))

(define anthy-release-handler
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac)))
      (anthy-lib-free-context ac-id))))

(define anthy-flush
  (lambda (ac)
    (ustr-clear! (anthy-context-preconv-ustr ac))
    (ustr-clear! (anthy-context-segments ac))
    (anthy-context-set-converting! ac #f)
    (if (anthy-context-candidate-window ac)
	(im-deactivate-candidate-selector ac))
    (anthy-context-set-candidate-window! ac #f)
    (anthy-context-set-candidate-op-count! ac 0)
    (anthy-update-preedit ac)  ;; TODO: remove this
    ))

(define anthy-begin-input
  (lambda (ac)
    (anthy-context-set-on! ac #t)
    (anthy-context-set-converting! ac #f)))

(define anthy-update-preedit
  (lambda (ac)
    (let ((segments (if (anthy-context-on ac)
			(if (anthy-context-converting ac)
			    (anthy-converting-state-preedit ac)
			    (anthy-input-state-preedit ac))
			())))
      (context-update-preedit ac segments))))
  
(define anthy-proc-raw-state
  (lambda (ac ev key key-state)
    (if (anthy-on-key? key key-state)
	(begin
	  (anthy-begin-input ac)
	  (event-set-consumed! ev #t)))))

(define anthy-begin-conv
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (kana (anthy-context-kana-mode ac))
	   (preconv-ustr (anthy-context-preconv-ustr ac))
	   (preconv-str (evmap-ustr-preedit-string preconv-ustr)))
      (if (and (number? (anthy-context-ac-id ac))
	       (> (string-length preconv-str)
		  0))
	  (begin
	    (anthy-lib-set-string ac-id preconv-str)
	    (let ((nr-segments (anthy-lib-get-nr-segments ac-id)))
	      (ustr-set-latter-seq! (anthy-context-segments ac)
				    (make-list nr-segments 0))
	      (anthy-context-set-converting! ac #t)))))))

(define anthy-cancel-conv
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
           (preconv-ustr (anthy-context-preconv-ustr ac))
	   (segments (anthy-context-segments ac))
           (cur-seg (ustr-cursor-pos segments))
           (cur-seg-pos (anthy-get-segment-pos ac cur-seg))
           (cur-seg-len (anthy-lib-get-segment-length ac-id cur-seg))
	   (new-pos (+ cur-seg-pos cur-seg-len)))
      (anthy-reset-candidate-window ac)
      (anthy-context-set-converting! ac #f)
      (ustr-set-cursor-pos! preconv-ustr new-pos)
      (ustr-clear! segments)
      (anthy-update-preedit ac)  ;; TODO: remove this
      )))

(define anthy-proc-input-state-no-preedit
  (lambda (ac ev key key-state)
    (cond
     ((anthy-wide-latin-key? key key-state)
      (anthy-flush ac)
      (anthy-context-set-on! ac #f)
      (anthy-context-set-wide-latin! ac #t))
	  
     ((anthy-latin-key? key key-state)
      (anthy-flush ac)
      (anthy-context-set-on! ac #f)
      (anthy-context-set-wide-latin! ac #f))

     ((anthy-hankaku-kana-key? key key-state)
      (anthy-switch-kana-mode! ac anthy-type-hankana))

     ((anthy-kana-toggle-key? key key-state)
      (anthy-toggle-kana-mode! ac))

     (else
      (anthy-input! ac ev)))))

(define anthy-has-preedit?
  (lambda (ac)
    (not (ustr-empty? (anthy-context-preconv-ustr ac)))))

(define anthy-proc-input-state-with-preedit
  (lambda (ac ev key key-state)
    (let ((preconv-ustr (anthy-context-preconv-ustr ac))
	  (kana (anthy-context-kana-mode ac))
	  (transpose (if #t
			 anthy-commit-transposed-preconv!
			 anthy-transpose-preconv!))) ;; does not commit
      (cond
       ;; begin conversion
       ((anthy-begin-conv-key? key key-state)
	(anthy-begin-conv ac))
       
       ;; backspace
       ((anthy-backspace-key? key key-state)
	(evmap-ustr-backspace! preconv-ustr))

       ;; delete
       ((anthy-delete-key? key key-state)
	(ustr-cursor-delete-frontside! preconv-ustr))

       ;; kill
       ((anthy-kill-key? key key-state)
	(ustr-clear-latter! preconv-ustr))
       
       ;; kill-backward
       ((anthy-kill-backward-key? key key-state)
	(ustr-clear-former! preconv-ustr))

       ;; commit as opposite kana
       ((anthy-commit-as-opposite-kana-key? key key-state)
	(transpose ac (multi-segment-opposite-kana kana)))

       ;; commit as hiragana
       ((anthy-commit-as-hiragana-key? key key-state)
	(transpose ac anthy-type-hiragana))

       ;; commit as katakana
       ((anthy-commit-as-katakana-key? key key-state)
	(transpose ac anthy-type-katakana))

       ;; commit as halfwidth katakana
       ((anthy-commit-as-hankana-key? key key-state)
	(transpose ac anthy-type-hankana))

       ;; commit as halfwidth alphanumeric
       ((anthy-commit-as-latin-key? key key-state)
	(transpose ac anthy-type-halfwidth))

       ;; commit as fullwidth alphanumeric
       ((anthy-commit-as-wide-latin-key? key key-state)
	(transpose ac anthy-type-fullwidth))

       ;; commit current preedit string, then toggle hiragana/katakana mode.
       ((anthy-kana-toggle-key? key key-state)
	(anthy-commit! ac)
	(anthy-toggle-kana-mode! ac))

       ;; cancel
       ((anthy-cancel-key? key key-state)
	(anthy-flush ac))

       ;; commit
       ((anthy-commit-key? key key-state)
	(anthy-commit! ac))

       ((anthy-go-left-key? key key-state)
	(ustr-cursor-move-backward! preconv-ustr))

       ;; right
       ((anthy-go-right-key? key key-state)
	(ustr-cursor-move-forward! preconv-ustr))

       ;; beginning-of-preedit
       ((anthy-beginning-of-preedit-key? key key-state)
	(ustr-cursor-move-beginning! preconv-ustr))

       ;; end-of-preedit
       ((anthy-end-of-preedit-key? key key-state)
	(ustr-cursor-move-end! preconv-ustr))

       (else
	(anthy-input! ac ev))))))

(define anthy-proc-input-state
  (lambda (ac ev key key-state)
    (if (anthy-has-preedit? ac)
	(anthy-proc-input-state-with-preedit ac ev key key-state)
	(anthy-proc-input-state-no-preedit ac ev key key-state))))

(define anthy-separator
  (lambda (ac)
    (let ((attr (bitwise-or preedit-separator
			    preedit-underline)))
      (and anthy-show-segment-separator?
	   (cons attr anthy-segment-separator)))))

(define anthy-get-segment-pos
  (lambda (ac seg-idx)
    (let ((ac-id (anthy-context-ac-id ac)))
      (apply + (map (lambda (idx)
		      (anthy-lib-get-segment-length ac-id idx))
		    (iota seg-idx))))))

(define anthy-cand-idx->type
  (let ((cand-idx->type-alist
	 (list
	  (cons anthy-direct-convert-hiragana      anthy-type-hiragana)
	  (cons anthy-direct-convert-katakana      anthy-type-katakana) 
	  (cons anthy-direct-convert-hankana       anthy-type-hankana)	 
	  (cons anthy-direct-convert-latin         anthy-type-halfwidth)
	  (cons anthy-direct-convert-wide-latin    anthy-type-fullwidth))))
    (lambda (ac idx)
      (if (= idx anthy-direct-convert-opposite-kana)
	  (multi-segment-opposite-kana (anthy-context-kana-mode ac))
	  (safe-cdr (assv idx cand-idx->type-alist))))))

(define anthy-get-nth-candidate
  (lambda (ac seg-idx cand-idx)
    (let ((ac-id (anthy-context-ac-id ac)))
      (if (>= cand-idx 0)
	  (anthy-lib-get-nth-candidate ac-id seg-idx cand-idx)
	  (let* (;; TODO: acquire correct positions. FIXME!
		 ;;(seg-pos (anthy-get-segment-pos ac seg-idx))
		 ;;(seg-len (anthy-lib-get-segment-length ac-id seg-idx))
		 (seg-pos 0)
		 (seg-len 2)
		 (preconv-ustr (anthy-context-preconv-ustr ac))
		 (seg-ustr (ustr-new
			    (list-head (list-tail (ustr-whole-seq preconv-ustr)
						  seg-pos)
				       seg-len)))
		 (cand-kana (anthy-cand-idx->type ac cand-idx))
		 (cand-rule (anthy-context-input-rule ac))
		 (cand-ruletree (anthy-ruletree cand-rule cand-kana))
		 (cand-ustr (evmap-ustr-transpose seg-ustr cand-ruletree)))
	    (evmap-ustr-preedit-string cand-ustr))))))

(define anthy-converting-state-preedit
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (separator (anthy-separator ac))
	   (mapped-segs (map (lambda (seg-idx cand-idx)
			       (let* ((attr (if (= seg-idx cur-seg)
						(bitwise-or preedit-reverse
							    preedit-cursor)
						preedit-underline))
				      (cand (anthy-get-nth-candidate
					     ac seg-idx cand-idx)))
				 (cons attr cand)))
			     (iota (ustr-length segments))
			     (ustr-whole-seq segments))))
      (if separator
	  (join separator mapped-segs)
	  mapped-segs))))

(define anthy-input-state-preedit
  (lambda (ac)
    (let* ((preconv-ustr (anthy-context-preconv-ustr ac))
	   (former (ustr-former-seq preconv-ustr))
	   (latter (ustr-latter-seq preconv-ustr)))
      (remove not
	      (list
	       (and (not (ustr-cursor-at-beginning? preconv-ustr))
		    (cons preedit-underline
			  (evmap-context-list-preedit-string former)))
	       (and (anthy-has-preedit? ac)
		    (cons preedit-cursor ""))
	       (and (not (ustr-cursor-at-end? preconv-ustr))
		    (cons preedit-underline
			  (evmap-context-list-preedit-string latter))))))))

(define anthy-get-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (string-append-map (lambda (seg-idx cand-idx)
                           (anthy-get-nth-candidate ac seg-idx cand-idx))
 			 (iota (ustr-length segments))
			 (ustr-whole-seq segments)))))

(define anthy-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (for-each (lambda (seg-idx cand-idx)
                  (if (>= cand-idx 0)
                      (anthy-lib-commit-segment ac-id seg-idx cand-idx)))
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

(define anthy-set-candidate
  (lambda (ac idx)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (compensated-idx (cond
			     ((>= idx max)
			      0)
			     ((< idx anthy-direct-convert-wide-latin)
			      (- max 1))
			     (else
			      idx))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (if (anthy-context-candidate-window ac)
	  ;;(im-select-candidate ac compensated-idx)
	  (begin
	    (im-deactivate-candidate-selector ac)
	    (anthy-update-preedit ac))))))

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
	  (begin
	    (im-select-candidate ac compensated-n)
	    (anthy-update-preedit ac))))))

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
      (im-select-candidate ac compensated-idx)
      (anthy-update-preedit ac))))

(define anthy-reset-candidate-window
  (lambda (ac)
    (if (anthy-context-candidate-window ac)
	(begin
	  (im-deactivate-candidate-selector ac)
	  (anthy-context-set-candidate-window! ac #f)))
    (anthy-context-set-candidate-op-count! ac 0)))

(define anthy-proc-converting-state
  (lambda (ac ev key key-state)
    (let ((preconv-ustr (anthy-context-preconv-ustr ac))
	  (segments (anthy-context-segments ac)))
      (cond
       ;; transpose current segment to opposite kana
       ((anthy-commit-as-opposite-kana-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-opposite-kana))

       ;; transpose current segment to hiragana
       ((anthy-commit-as-hiragana-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-hiragana))

       ;; transpose current segment to katakana
       ((anthy-commit-as-katakana-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-katakana))

       ;; transpose current segment to halfwidth katakana
       ((anthy-commit-as-hankana-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-hankana))

       ;; transpose current segment to halfwidth alphanumeric
       ((anthy-commit-as-latin-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-latin))

       ;; transpose current segment to fullwidth alphanumeric
       ((anthy-commit-as-wide-latin-key? key key-state)
	(anthy-set-candidate ac anthy-direct-convert-wide-latin))

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
	(ustr-cursor-move-beginning! segments)
	(anthy-reset-candidate-window ac))

       ((anthy-end-of-preedit-key? key key-state)
	(ustr-cursor-move-end! segments)
	(anthy-correct-segment-cursor segments)
	(anthy-reset-candidate-window ac))

       ((anthy-backspace-key? key key-state)
	(anthy-cancel-conv ac)
	(ustr-cursor-delete-backside! preconv-ustr))

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

       (else
	(anthy-cancel-conv ac)
	(anthy-proc-input-state-with-preedit ac ev key key-state))))))

(define anthy-proc-wide-latin
  (lambda (ac ev key key-state)
    (cond
     ((anthy-on-key? key key-state)
      (anthy-flush ac)
      (anthy-context-set-on! ac #t))
     (else
      (anthy-input! ac ev)))))

(define anthy-key-handler
  (lambda (ac key key-state press?)
    (let ((ev (legacy-key->key-event key key-state press?))
	  (keytrans-emc (anthy-context-keytrans-emc ac)))
      (key-event-print-inspected "key-event:  " ev)
      (key-event-translator-translate! keytrans-emc ev)
      (key-event-print-inspected "translated: " ev)
      (if (anthy-context-on ac)
	  (if (anthy-context-converting ac)
	      (anthy-proc-converting-state ac ev key key-state)
	      (anthy-proc-input-state ac ev key key-state))
	  (if (anthy-context-wide-latin ac)
	      (anthy-proc-wide-latin ac ev key key-state)
	      (anthy-proc-raw-state ac ev key key-state)))
      (if (not (event-consumed ev))
	  (im-commit-raw ac)))))

(define anthy-press-key-handler
  (lambda (ac key key-state)
    (anthy-key-handler ac key key-state #t)))

(define anthy-release-key-handler
  (lambda (ac key key-state)
    (anthy-key-handler ac key key-state #f)))

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
	   (cand (anthy-get-nth-candidate ac cur-seg idx)))
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
