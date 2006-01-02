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

(require "japanese.scm")
(require "japanese-kana.scm")
(require "japanese-azik.scm")
(require-custom "generic-key-custom.scm")
(require-custom "canna-custom.scm")
(require-custom "canna-key-custom.scm")


;;; implementations

(define canna-init-lib-ok? #f)

(define canna-input-rule-roma 0)
(define canna-input-rule-kana 1)
(define canna-input-rule-azik 2)

(define canna-prepare-activation
  (lambda (cc)
    (if (canna-context-state cc)
        (let ((cc-id (canna-context-cc-id cc)))
          (canna-lib-reset-conversion cc-id)))
    (canna-flush cc)
    (canna-update-preedit cc)))

(register-action 'action_canna_hiragana
		 (lambda (cc) ;; indication handler
		   '(figure_ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (cc) ;; activity predicate
		   (and (canna-context-on cc)
			(= (canna-context-kana-mode cc)
			   multi-segment-type-hiragana)))

		 (lambda (cc) ;; action handler
		   (canna-prepare-activation cc)
		   (canna-context-set-on! cc #t)
		   (canna-context-change-kana-mode! cc
						 multi-segment-type-hiragana)))

(register-action 'action_canna_katakana
		 (lambda (cc)
		   '(figure_ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
			(= (canna-context-kana-mode cc)
			   multi-segment-type-katakana)))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (canna-context-set-on! cc #t)
		   (canna-context-change-kana-mode! cc
						 multi-segment-type-katakana)))

(register-action 'action_canna_hankana
		 (lambda (cc)
		   '(figure_ja_hankana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
			(= (canna-context-kana-mode cc)
			   multi-segment-type-hankana)))
		 (lambda (cc)
		   (canna-prepare-activation cc)
		   (canna-context-set-on! cc #t)
		   (canna-context-change-kana-mode! cc
						 multi-segment-type-hankana)))

(register-action 'action_canna_direct
		 (lambda (cc)
		   '(figure_ja_direct
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
		   '(figure_ja_zenkaku
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
		   '(figure_ja_roma
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
		   '(figure_ja_kana
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
		   '(figure_ja_azik
		     "Ａ"
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
    (list 'left-string        ()) ;; preedit strings in the left of cursor
    (list 'right-string       ())
    (list 'rkc                ())
    (list 'index-list         ())
    (list 'cur-seg            ())
    (list 'candidate-window   ())
    (list 'candidate-op-count ())
    (list 'wide-latin         #f)
    (list 'kana-mode          multi-segment-type-hiragana)
    (list 'commit-raw         #t)
    (list 'input-rule         canna-input-rule-roma))))
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
    (if using-kana-table?
        (canna-context-set-input-rule! cc canna-input-rule-kana)
        (canna-context-set-input-rule! cc canna-input-rule-roma))
    cc))

(define (canna-commit-raw cc)
  (im-commit-raw cc)
  (canna-context-set-commit-raw! cc #t))

(define canna-opposite-kana
  (lambda (kana)
    (cond
     ((= kana multi-segment-type-hiragana)
      multi-segment-type-katakana)
     ((= kana multi-segment-type-katakana)
      multi-segment-type-hiragana)
     ((= kana multi-segment-type-hankana)
      multi-segment-type-hiragana))))

(define (canna-context-kana-toggle cc)
  (let* ((kana (canna-context-kana-mode cc))
	 (opposite-kana (canna-opposite-kana kana)))
    (canna-context-change-kana-mode! cc opposite-kana)))

(define canna-context-change-kana-mode!
  (lambda (cc kana-mode)
    (if (= (canna-context-input-rule cc)
           canna-input-rule-kana)
        (rk-context-set-rule!
          (canna-context-rkc cc)
          (cond
            ((= kana-mode multi-segment-type-hiragana) ja-kana-hiragana-rule)
            ((= kana-mode multi-segment-type-katakana) ja-kana-katakana-rule)
            ((= kana-mode multi-segment-type-hankana)  ja-kana-hankana-rule))))
    (canna-context-set-kana-mode! cc kana-mode)))

(define (canna-flush cc)
  (rk-flush (canna-context-rkc cc))
  (canna-context-set-left-string! cc '())
  (canna-context-set-right-string! cc '())
  (canna-context-set-state! cc #f)
  (canna-context-set-index-list! cc ())
  (canna-context-set-transposing! cc #f)
  (if (canna-context-candidate-window cc)
        (im-deactivate-candidate-selector cc))
  (canna-context-set-candidate-window! cc #f)
  (canna-context-set-candidate-op-count! cc 0))

(define (canna-begin-input cc)
  (canna-context-set-on! cc #t)
  (rk-flush (canna-context-rkc cc))
  (canna-context-set-state! cc #f))

(define (canna-update-preedit cc)
  (if (canna-context-on cc)
      (if (canna-context-transposing cc)
          (canna-context-transposing-state-preedit cc)
          (if (canna-context-state cc)
              (canna-compose-state-preedit cc)
              (canna-input-state-preedit cc)))
      (begin
	(im-clear-preedit cc)
	(im-update-preedit cc))))

(define (canna-append-string cc str)
  (and str
       (if (not (string? (car str)))
	   (begin
	     (canna-append-string cc (car str))
	     (canna-append-string cc (cdr str))
	     #f)
	   #t)
       (canna-context-set-left-string!
	cc (cons str (canna-context-left-string cc)))))

(define (canna-begin-conv cc)
  (let* ((cc-id (canna-context-cc-id cc))
	 (rkc (canna-context-rkc cc))
	 (kana (canna-context-kana-mode cc))
	 (last "")
	 (res))
    (set! res (rk-push-key-last! rkc))
    (if res
	(canna-append-string cc res))

    (let ((preconv-str (string-append
                         (multi-segment-make-left-string (canna-context-left-string cc)
                                                         multi-segment-type-hiragana)
                         (multi-segment-make-right-string (canna-context-right-string cc)
                                                          multi-segment-type-hiragana))))
      (if (and (number? cc-id)
               (> (string-length preconv-str) 0))
          (begin
            (canna-context-set-index-list!
              cc
              (multi-segment-make-index-list
                (canna-lib-begin-conversion cc-id preconv-str) #f))
            (canna-context-set-state! cc #t)
            (canna-context-set-cur-seg! cc 0)
            (rk-flush (canna-context-rkc cc)))))))

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
      (canna-context-change-kana-mode! cc multi-segment-type-hankana))
     
     ((canna-kana-toggle-key? key key-state)
      (canna-context-kana-toggle cc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (canna-commit-raw cc))
     
     ;; direct key => commit
     (direct
      (im-commit cc direct))

     ((symbol? key)
      (canna-commit-raw cc))

     (else
      (let* ((key-str (charcode->string
		       (if (= rule canna-input-rule-kana)
			   key
			   (to-lower-char key))))
	     (res (rk-push-key! rkc key-str)))
	(if res
	    (canna-append-string cc res)
	    (if (not (rk-pending rkc)
		       (canna-commit-raw cc)))))))))

(define (canna-has-preedit? cc)
  (or
   (> (length (canna-context-left-string cc)) 0)
   (> (length (canna-context-right-string cc)) 0)
   (> (length (rk-pending (canna-context-rkc cc))) 0)))

(define canna-proc-transposing-state
  (lambda (cc key key-state)
    (cond
     ((canna-transpose-as-hiragana-key? key key-state)
      (canna-context-set-transposing-type! cc multi-segment-type-hiragana))

     ((canna-transpose-as-katakana-key? key key-state)
      (canna-context-set-transposing-type! cc multi-segment-type-katakana))

     ((canna-transpose-as-hankana-key? key key-state)
      (canna-context-set-transposing-type! cc multi-segment-type-hankana))

     (else
      (begin
	; commit
	(im-commit cc (canna-transposing-text cc))
	(canna-flush cc)
	(if (not (canna-commit-key? key key-state))
	      (canna-proc-input-state cc key key-state)))))))

(define (canna-proc-input-state-with-preedit cc key key-state)
  (let* ((rkc (canna-context-rkc cc))
         (rule (canna-context-input-rule cc))
         (kana (if (= rule canna-input-rule-kana)
                   multi-segment-type-hiragana
                   (canna-context-kana-mode cc))))

    (cond
     ;; begin conversion
     ((canna-begin-conv-key? key key-state)
      (canna-begin-conv cc))
;     ((and (canna-begin-conv-key? key key-state)
;	   canna-init-lib-ok?)
;      (canna-begin-conv cc))
     ;; backspace
     ((canna-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
          (if (canna-context-left-string cc)
              (canna-context-set-left-string!
                cc
                (cdr (canna-context-left-string cc))))))
     ;; delete
     ((canna-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (if (canna-context-right-string cc)
	      (canna-context-set-right-string!
	       cc
	       (cdr (canna-context-right-string cc))))))

       ;; kill
     ((canna-kill-key? key key-state)
      (canna-context-set-right-string! cc ()))
     
     ;; kill-backward
     ((canna-kill-backward-key? key key-state)
      (begin
        (rk-flush rkc)
        (canna-context-set-left-string! cc ())))
       
     ;; ひらがなモードでカタカナを確定する
     ((canna-commit-as-opposite-kana-key? key key-state)
      (let ((opposite-kana (if (= rule canna-input-rule-kana)
                               multi-segment-type-hiragana
                               (canna-opposite-kana kana))))
	(im-commit
	 cc
	 (string-append
	  (multi-segment-make-left-string (canna-context-left-string cc)
					  opposite-kana)
	  (rk-pending rkc)
	  (multi-segment-make-right-string (canna-context-right-string cc)
					   opposite-kana)))
	(canna-flush cc)))

       ;; Transposing状態へ移行
     ((or (canna-transpose-as-hiragana-key?   key key-state)
          (canna-transpose-as-katakana-key?   key key-state)
          (canna-transpose-as-hankana-key?    key key-state))
      (begin
        (canna-context-confirm-kana! cc)
        (canna-context-set-transposing! cc #t)
        (canna-proc-transposing-state cc key key-state)))

     ;; 現在のかなを確定後、ひらがな/カタカナモードを切り換える
     ((canna-kana-toggle-key? key key-state)
      (begin
	(im-commit
	 cc
	 (string-append
	  (multi-segment-make-left-string (canna-context-left-string cc)
					  kana)
	  (rk-pending rkc)
	  (multi-segment-make-right-string (canna-context-right-string cc)
					   kana)))
	(canna-flush cc)
	(canna-context-kana-toggle cc)))
     ;; cancel
     ((canna-cancel-key? key key-state)
      (canna-flush cc))
     ;; commit
     ((canna-commit-key? key key-state)
      (begin
	(im-commit
	 cc
	 (string-append
	  (multi-segment-make-left-string (canna-context-left-string cc)
					  kana)
	  (rk-pending rkc)
	  (multi-segment-make-right-string (canna-context-right-string cc)
					   kana)))
	(canna-flush cc)))
     ;; left
     ((canna-go-left-key? key key-state)
      (begin
        (canna-context-confirm-kana! cc)
	(if (canna-context-left-string cc)
	    (let
		((c (car (canna-context-left-string cc))))
	      (canna-context-set-left-string!
	       cc (cdr (canna-context-left-string cc)))
	      (canna-context-set-right-string!
	       cc
	       (cons c (canna-context-right-string cc)))))))
     ;; right
     ((canna-go-right-key? key key-state)
      (begin
        (canna-context-confirm-kana! cc)
	(if (canna-context-right-string cc)
	    (let
		((c (car (canna-context-right-string cc))))
	      (canna-context-set-right-string!
	       cc (cdr (canna-context-right-string cc)))
	      (canna-append-string cc c)))))

     ;; beginning-of-preedit
     ((canna-beginning-of-preedit-key? key key-state)
      (begin
        (canna-context-confirm-kana! cc)
        (if (canna-context-left-string cc)
            (begin
              (canna-context-set-right-string!
              cc
              (append
                (reverse (canna-context-left-string cc))
                (canna-context-right-string cc)))
              (canna-context-set-left-string! cc ())))))

     ;; end-of-preedit
     ((canna-end-of-preedit-key? key key-state)
      (begin
        (canna-context-confirm-kana! cc)
        (if (canna-context-right-string cc)
            (begin
              (canna-context-set-left-string!
              cc
              (append
                (reverse (canna-context-right-string cc))
                (canna-context-left-string cc)))
                (canna-context-set-right-string! cc ())))))
;		   (rk-flush rkc)))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      (canna-commit-raw cc))
     (else
      (let* ((key-str (charcode->string key))
	     (pend (rk-pending rkc))
	     (res (rk-push-key! rkc key-str)))
	(if (and res (or
		      (list? (car res))
		      (not (string=? (car res) ""))))
	    (canna-append-string cc res)
	    (if (= rule canna-input-rule-kana)
		(begin
		  (canna-append-string cc (list pend "" ""))
;     (set! key (to-lower-char key))
;     (let ((res)
;	   (key-str (charcode->string key)))
;       (set! res (rk-push-key! rkc key-str))
;       (if res
;	   (canna-append-string cc res))))))
))))))))

(define canna-context-confirm-kana!
  (lambda (cc)
    (if (= (canna-context-input-rule cc)
	   canna-input-rule-kana)
	(let* ((rkc (canna-context-rkc cc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	    (if residual-kana
		(begin
		  (canna-append-string cc residual-kana)
		  (rk-flush rkc)))))))

(define (canna-proc-input-state cc key key-state)
  (if (canna-has-preedit? cc)
      (canna-proc-input-state-with-preedit cc key key-state)
      (canna-proc-input-state-no-preedit cc key key-state)))

(define canna-context-transposing-state-preedit
  (lambda (cc)
    (let ((transposing-text (canna-transposing-text cc)))
      (im-clear-preedit cc)
      (im-pushback-preedit
        cc
        preedit-underline
        transposing-text)
      (im-pushback-preedit
        cc
        preedit-cursor
        "")
      (im-update-preedit cc))))

(define canna-transposing-text
  (lambda (cc)
    (let ((transposing-type (if (= (canna-context-input-rule cc) canna-input-rule-kana)
                                multi-segment-type-hiragana
                                (canna-context-transposing-type cc))))
      (string-append
        (multi-segment-make-left-string (canna-context-left-string cc) transposing-type)
        (multi-segment-make-right-string (canna-context-right-string cc) transposing-type)))))

(define (canna-pushback-preedit-segment-rec cc idx nseg)
  (let ((cc-id (canna-context-cc-id cc)))
    (if (< idx nseg)
	(begin
	  (if (and
	       canna-show-segment-separator?
	       (< 0 idx))
	      (im-pushback-preedit
	       cc
	       (bit-or preedit-separator
		       preedit-underline)
	       canna-segment-separator))
	  (im-pushback-preedit
	   cc
	   (if (= idx (canna-context-cur-seg cc))
	       (+ preedit-reverse preedit-cursor)
	       preedit-underline)
	   (canna-lib-get-nth-candidate
	      cc-id idx
	      (nth idx (canna-context-index-list cc))))
	    (canna-pushback-preedit-segment-rec cc (+ idx 1) nseg)))))

(define (canna-compose-state-preedit cc)
  (im-clear-preedit cc)
  (canna-pushback-preedit-segment-rec
   cc
   0 (length (canna-context-index-list cc)))
  (im-update-preedit cc))

(define (canna-input-state-preedit cc)
  (let ((rkc (canna-context-rkc cc))
        (kana (if (= (canna-context-input-rule cc)
                     canna-input-rule-kana)
                   multi-segment-type-hiragana
                   (canna-context-kana-mode cc))))
    (im-clear-preedit cc)
    (im-pushback-preedit
     cc preedit-underline
     (multi-segment-make-left-string (canna-context-left-string cc) kana))
    (im-pushback-preedit cc preedit-underline
			 (rk-pending rkc))
    (if (canna-has-preedit? cc)
	(im-pushback-preedit cc preedit-cursor ""))
    (im-pushback-preedit
     cc preedit-underline
     (multi-segment-make-right-string (canna-context-right-string cc) kana))
    (im-update-preedit cc)))

(define (canna-get-commit-string cc idx nseg)
  (let ((cc-id (canna-context-cc-id cc)))
    (if (< idx nseg)
	(string-append
	 (canna-lib-get-nth-candidate
	  cc-id idx
	  (nth idx (canna-context-index-list cc)))
	 (canna-get-commit-string cc (+ idx 1) nseg))
	"")))

(define (canna-commit-string cc idx nseg)
  (let ((cc-id (canna-context-cc-id cc)))
    (if (< idx nseg)
	(begin
	  (canna-lib-commit-segment
	   cc-id idx (nth idx (canna-context-index-list cc)))
	  (canna-commit-string cc
			       (+ idx 1) nseg))
	#f)))

(define (canna-do-commit cc)
    (canna-reset-candidate-window cc)
    (im-commit cc
	       (canna-get-commit-string
		cc 0
		(length (canna-context-index-list cc))))
    (canna-commit-string
     cc 0
     (length (canna-context-index-list cc)))
    (canna-flush cc))

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

(define (canna-move-segment cc dir)
  (let ((pos (+ (canna-context-cur-seg cc) dir))
	(nseg (length (canna-context-index-list cc))))
      (if (and
	   (> pos -1)
	   (< pos nseg))
	  (canna-context-set-cur-seg! cc pos))))

(define (canna-move-candidate cc off)
  (let* ((seg (canna-context-cur-seg cc))
	 (n (nth seg (canna-context-index-list cc)))
	 (cc-id (canna-context-cc-id cc))
	 (max (canna-lib-get-nr-candidates cc-id seg)))
    (set! n (+ n off))
    (if (>= n max)
	(set! n 0))
    (if (< n 0)
	(set! n (- max 1)))
    (set-car! (nthcdr seg (canna-context-index-list cc)) n)
    (canna-context-set-candidate-op-count!
     cc
     (+ 1 (canna-context-candidate-op-count cc)))
    (if (and
	 (= (canna-context-candidate-op-count cc)
	    canna-candidate-op-count)
	 canna-use-candidate-window?)
	(begin
	  (canna-context-set-candidate-window! cc #t)
	  (im-activate-candidate-selector
	   cc
	   max canna-nr-candidate-max)))
    (if (canna-context-candidate-window cc)
	(im-select-candidate cc n))))

(define canna-move-candidate-in-page
  (lambda (cc numeralc)
    (let* ((cc-id (canna-context-cc-id cc))
	   (cur-seg (canna-context-cur-seg cc))
	   (max (canna-lib-get-nr-candidates cc-id cur-seg))
           (n (nth cur-seg (canna-context-index-list cc)))
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
      (set-car! (nthcdr cur-seg (canna-context-index-list cc)) compensated-idx)
      (canna-context-set-candidate-op-count! cc new-op-count)
      (im-select-candidate cc compensated-idx))))

(define (canna-reset-candidate-window cc)
  (if (canna-context-candidate-window cc)
      (begin
	(im-deactivate-candidate-selector cc)
	(canna-context-set-candidate-window! cc #f)))
  (canna-context-set-candidate-op-count! cc 0))

(define (canna-resize-segment cc cnt)
  (let
      ((cc-id (canna-context-cc-id cc)))
      (canna-reset-candidate-window cc)
      (canna-lib-resize-segment
       cc-id (canna-context-cur-seg cc) cnt)
      (canna-context-set-index-list!
       cc
       (multi-segment-make-index-list
	(canna-lib-get-nr-segments cc-id)
	(truncate-list
	 (canna-context-index-list cc)
	 (canna-context-cur-seg cc)))
      )))

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
      (begin
	(canna-move-segment cc 1)
	(canna-reset-candidate-window cc)))

     ((canna-prev-segment-key? key key-state)
      (begin
	(canna-move-segment cc -1)
	(canna-reset-candidate-window cc)))

     ((canna-backspace-key? key key-state)
      (begin
	(canna-context-set-state! cc #f)
	(canna-reset-candidate-window cc)
	(canna-lib-reset-conversion cc-id)))

     ((canna-next-candidate-key? key key-state)
      (canna-move-candidate cc 1))

     ((canna-prev-candidate-key? key key-state)
      (canna-move-candidate cc -1))

     ((canna-cancel-key? key key-state)
      (begin
	(canna-context-set-state! cc #f)
	(canna-reset-candidate-window cc)
	(canna-lib-reset-conversion cc-id)))

     ((and canna-select-candidate-by-numeral-key?
	   (numeral-char? key)
	   (canna-context-candidate-window cc))
      (canna-move-candidate-in-page cc key))

     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (begin
	(canna-do-commit cc)
	(canna-proc-input-state cc key key-state))))))

(define (canna-proc-wide-latin cc key key-state)
  (let* ((char (charcode->string key))
	 (w (or (ja-direct char)
		(ja-wide char))))
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
        (canna-flush cc)
        (if (canna-context-state cc)
          (let ((cc-id (canna-context-cc-id cc)))
            (canna-lib-reset-conversion cc-id))))))

;;;
(define (canna-get-candidate-handler cc idx accel-enum-hint)
  (let* ((cc-id (canna-context-cc-id cc))
	 (cand (canna-lib-get-nth-candidate
		cc-id (canna-context-cur-seg cc) idx)))
    (list cand (digit->string (+ idx 1)) "")))

(define (canna-set-candidate-index-handler cc idx)
  (let* ((seg (canna-context-cur-seg cc))
	 (cc-id (canna-context-cc-id cc)))
    (set-car! (nthcdr seg (canna-context-index-list cc)) idx)
    (canna-update-preedit cc)))

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
