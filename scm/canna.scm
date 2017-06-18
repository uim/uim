;;; canna.scm: Canna for uim.
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

(require "ustr.scm")
(require "japanese.scm")
(require "generic-predict.scm")
(require "cannav3-socket.scm")
(require-custom "generic-key-custom.scm")
(require-custom "canna-custom.scm")
(require-custom "canna-key-custom.scm")

;;; implementations

(define canna-init-lib-ok? #f)

(define canna-type-direct	   ja-type-direct)
(define canna-type-hiragana	   ja-type-hiragana)
(define canna-type-katakana	   ja-type-katakana)
(define canna-type-halfkana	   ja-type-halfkana)
(define canna-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define canna-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define canna-input-rule-roma 0)
(define canna-input-rule-kana 1)
(define canna-input-rule-azik 2)
(define canna-input-rule-act 3)
(define canna-input-rule-kzik 4)

(define canna-candidate-type-katakana -2)
(define canna-candidate-type-hiragana -3)
(define canna-candidate-type-halfkana -4)
(define canna-candidate-type-halfwidth-alnum -5)
(define canna-candidate-type-fullwidth-alnum -6)
(define canna-candidate-type-upper-halfwidth-alnum -7)
(define canna-candidate-type-upper-fullwidth-alnum -8)

;; I don't think the key needs to be customizable.
(define-key canna-space-key? '(" "))

(define canna-prepare-input-rule-activation
  (lambda (cc)
    (cond
     ((canna-context-state cc)
      (canna-do-commit cc))
     ((canna-context-transposing cc)
      (im-commit cc (canna-transposing-text cc)))
     ((and
       (canna-context-on cc)
       (canna-has-preedit? cc))
      (im-commit
       cc (canna-make-whole-string cc #t (canna-context-kana-mode cc)))))
    (canna-flush cc)
    (canna-update-preedit cc)))

(define canna-prepare-input-mode-activation
  (lambda (cc new-mode)
    (let ((old-kana (canna-context-kana-mode cc)))
      (cond
       ((canna-context-state cc)
	(canna-do-commit cc))
       ((canna-context-transposing cc)
	(im-commit cc (canna-transposing-text cc))
	(canna-flush cc))
       ((and
	 (canna-context-on cc)
	 (canna-has-preedit? cc)
	 (not (= old-kana new-mode)))
	(im-commit
	 cc (canna-make-whole-string cc #t (canna-context-kana-mode cc)))
	(canna-flush cc)))
      (canna-update-preedit cc))))

(register-action 'action_canna_hiragana
		 (lambda (cc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (cc) ;; activity predicate
		   (and (canna-context-on cc)
		        (not (canna-context-alnum cc))
			(= (canna-context-kana-mode cc)
			   canna-type-hiragana)))

		 (lambda (cc) ;; action handler
		   (canna-prepare-input-mode-activation cc canna-type-hiragana)
		   (canna-context-set-on! cc #t)
		   (canna-context-set-alnum! cc #f)
		   (canna-context-change-kana-mode! cc canna-type-hiragana)))

(register-action 'action_canna_katakana
		 (lambda (cc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
		        (not (canna-context-alnum cc))
			(= (canna-context-kana-mode cc)
			   canna-type-katakana)))
		 (lambda (cc)
		   (canna-prepare-input-mode-activation cc canna-type-katakana)
		   (canna-context-set-on! cc #t)
		   (canna-context-set-alnum! cc #f)
		   (canna-context-change-kana-mode! cc canna-type-katakana)))

(register-action 'action_canna_halfkana
		 (lambda (cc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
			(not (canna-context-alnum cc))
			(= (canna-context-kana-mode cc) canna-type-halfkana)))
		 (lambda (cc)
		   (canna-prepare-input-mode-activation cc canna-type-halfkana)
		   (canna-context-set-on! cc #t)
		   (canna-context-set-alnum! cc #f)
		   (canna-context-change-kana-mode! cc canna-type-halfkana)))

(register-action 'action_canna_halfwidth_alnum
		 (lambda (cc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (cc) ;; activity predicate
		   (and (canna-context-on cc)
			(canna-context-alnum cc)
			(= (canna-context-alnum-type cc)
			   canna-type-halfwidth-alnum)))
		 (lambda (cc) ;; action handler
		   (canna-prepare-input-mode-activation
		    cc (canna-context-kana-mode cc))
		   (canna-context-set-on! cc #t)
		   (canna-context-set-alnum! cc #t)
		   (canna-context-set-alnum-type!
		    cc canna-type-halfwidth-alnum)))

(register-action 'action_canna_direct
		 (lambda (cc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (cc)
		   (not (canna-context-on cc)))
		 (lambda (cc)
		   (canna-prepare-input-mode-activation cc canna-type-direct)
		   (canna-context-set-on! cc #f)))

(register-action 'action_canna_fullwidth_alnum
		 (lambda (cc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (cc)
		   (and (canna-context-on cc)
			(canna-context-alnum cc)
			(= (canna-context-alnum-type cc)
			   canna-type-fullwidth-alnum)))
		 (lambda (cc)
		   (canna-prepare-input-mode-activation
		    cc (canna-context-kana-mode cc))
		   (canna-context-set-on! cc #t)
		   (canna-context-set-alnum! cc #t)
		   (canna-context-set-alnum-type!
		    cc canna-type-fullwidth-alnum)))

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
		   (canna-prepare-input-rule-activation cc)
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
		   (canna-prepare-input-rule-activation cc)
                   (require "japanese-kana.scm")
		   (canna-context-set-input-rule! cc canna-input-rule-kana)
                   (canna-context-change-kana-mode!
                     cc (canna-context-kana-mode cc))
		   (canna-context-set-alnum! cc #f)))

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
		   (canna-prepare-input-rule-activation cc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (canna-context-rkc cc)
					 ja-azik-rule)
		   (canna-context-set-input-rule! cc canna-input-rule-azik)))

(register-action 'action_canna_kzik
		 (lambda (cc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (cc)
		   (= (canna-context-input-rule cc)
		      canna-input-rule-kzik))
		 (lambda (cc)
		   (canna-prepare-input-rule-activation cc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (canna-context-rkc cc)
					 ja-kzik-rule)
		   (canna-context-set-input-rule! cc canna-input-rule-kzik)))

(register-action 'action_canna_act
		 (lambda (cc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (cc)
		   (= (canna-context-input-rule cc)
		      canna-input-rule-act))
		 (lambda (cc)
		   (canna-prepare-input-rule-activation cc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (canna-context-rkc cc)
					 ja-act-rule)
		   (canna-context-set-input-rule! cc canna-input-rule-act)))

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
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'cc-id              ()) ;; canna-context-id
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'kana-mode          canna-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      canna-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         canna-input-rule-roma)
    (list 'raw-ustr	      #f)
    (list 'prediction-ctx     '())
    (list 'prediction-word    '())
    (list 'prediction-candidates '())
    (list 'prediction-appendix '())
    (list 'prediction-nr      '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '()))))
(define-record 'canna-context canna-context-rec-spec)
(define canna-context-new-internal canna-context-new)

(define (canna-predict cc str)
  (predict-meta-search
   (canna-context-prediction-ctx cc)
   str))
(define (canna-lib-set-prediction-src-string cc str)
  (let* ((ret      (canna-predict cc str))
         (word     (predict-meta-word? ret))
         (cands    (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret)))
    (canna-context-set-prediction-word! cc word)
    (canna-context-set-prediction-candidates! cc cands)
    (canna-context-set-prediction-appendix! cc cands)
    (canna-context-set-prediction-nr! cc (length cands)))
  #f)
(define (canna-lib-get-nr-predictions cc)
  (canna-context-prediction-nr cc))
(define (canna-lib-get-nth-word cc nth)
  (let ((word (canna-context-prediction-word cc)))
    (list-ref word nth)))
(define (canna-lib-get-nth-prediction cc nth)
  (let ((cands (canna-context-prediction-candidates cc)))
    (list-ref cands nth)))
(define (canna-lib-get-nth-appendix cc nth)
  (let ((appendix (canna-context-prediction-candidates cc)))
    (list-ref appendix nth)))
(define (canna-lib-commit-nth-prediction cc nth)
  (predict-meta-commit
   (canna-context-prediction-ctx cc)
   (canna-lib-get-nth-word cc nth)
   (canna-lib-get-nth-prediction cc nth)
   (canna-lib-get-nth-appendix cc nth)))

(define (canna-context-new id im)
  (let ((cc (canna-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (canna-context-set-cc-id! cc (if canna-init-lib-ok?
;				      (canna-lib-alloc-context) ()))
    (canna-context-set-cc-id! cc (canna-lib-alloc-context))
    (canna-context-set-widgets! cc canna-widgets)
    (canna-context-set-rkc! cc rkc)
    (canna-context-set-preconv-ustr! cc (ustr-new '()))
    (canna-context-set-raw-ustr! cc (ustr-new '()))
    (canna-context-set-segments! cc (ustr-new '()))
    (if canna-use-prediction?
        (begin
          (canna-context-set-prediction-ctx! cc (predict-make-meta-search))
          (predict-meta-open (canna-context-prediction-ctx cc) "canna")
          (predict-meta-set-external-charset! (canna-context-prediction-ctx cc) "EUC-JP")))
    cc))

(define (canna-commit-raw cc)
  (im-commit-raw cc)
  (canna-context-set-commit-raw! cc #t))

(define (canna-context-kana-toggle cc)
  (let* ((kana (canna-context-kana-mode cc))
	 (opposite-kana (ja-opposite-kana kana)))
    (canna-context-change-kana-mode! cc opposite-kana)))

(define canna-context-alkana-toggle
  (lambda (cc)
    (let ((alnum-state (canna-context-alnum cc)))
      (canna-context-set-alnum! cc (not alnum-state)))))

(define canna-context-change-kana-mode!
  (lambda (cc kana-mode)
    (if (= (canna-context-input-rule cc)
           canna-input-rule-kana)
        (rk-context-set-rule!
	 (canna-context-rkc cc)
	 (cond
	  ((= kana-mode canna-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode canna-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode canna-type-halfkana) ja-kana-halfkana-rule))))
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
                    (if (list? (car residual-kana))
                      (string-append-map extract-kana residual-kana)
		      (extract-kana residual-kana))
                    pending)
		  pending)
              (string-append-map-ustr-latter extract-kana preconv-str))))
	   kana)
          (string-append
	   (string-append-map-ustr-former extract-kana preconv-str)
           (if convert-pending-into-kana?
               (if residual-kana
                 (if (list? (car residual-kana))
                   (string-append-map extract-kana residual-kana)
                   (extract-kana residual-kana))
                 "")
               pending)
           (string-append-map-ustr-latter extract-kana preconv-str))))))

(define canna-make-raw-string
  (lambda (raw-str-list wide? upper?)
    (if (not (null? raw-str-list))
	(if wide?
	    (string-append
	     (ja-string-list-to-wide-alphabet
	      (if upper?
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list)))))
		  (string-to-list (car raw-str-list))))
	     (canna-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (canna-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define canna-make-whole-raw-string
  (lambda (cc wide? upper?)
    (canna-make-raw-string (canna-get-raw-str-seq cc) wide? upper?)))

(define (canna-init-handler id im arg)
  (if (not canna-init-lib-ok?)
      (begin
	(canna-lib-init canna-server-name)
	(set! canna-init-lib-ok? #t)))
  (canna-context-new id im))

(define (canna-release-handler cc)
  (let ((cc-id (canna-context-cc-id cc)))
    (if cc-id
        (canna-lib-release-context cc-id))))

(define (canna-flush cc)
  (rk-flush (canna-context-rkc cc))
  (ustr-clear! (canna-context-preconv-ustr cc))
  (ustr-clear! (canna-context-raw-ustr cc))
  (ustr-clear! (canna-context-segments cc))
  (canna-context-set-transposing! cc #f)
  (canna-context-set-state! cc #f)
  (if (or (canna-context-candidate-window cc)
          (canna-context-prediction-window cc))
      (im-deactivate-candidate-selector cc))
  (canna-context-set-candidate-window! cc #f)
  (canna-context-set-prediction-window! cc #f)
  (canna-context-set-candidate-op-count! cc 0))

(define (canna-begin-input cc key key-state)
  (if (cond
       ((canna-on-key? key key-state)
	#t)
       ((and
	 canna-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((canna-hiragana-key? key key-state)
	   (canna-context-set-kana-mode! cc canna-type-hiragana)
	   (canna-context-set-alnum! cc #f)
	   #t)
	  ((canna-katakana-key? key key-state)
	   (canna-context-set-kana-mode! cc canna-type-katakana)
	   (canna-context-set-alnum! cc #f)
	   #t)
	  ((canna-halfkana-key? key key-state)
	   (canna-context-set-kana-mode! cc canna-type-halfkana)
	   (canna-context-set-alnum! cc #f)
	   #t)
	  ((canna-halfwidth-alnum-key? key key-state)
	   (canna-context-set-alnum-type! cc canna-type-halfwidth-alnum)
	   (canna-context-set-alnum! cc #t)
	   #t)
	  ((canna-halfwidth-alnum-key? key key-state)
	   (canna-context-set-alnum-type! cc canna-type-fullwidth-alnum)
	   (canna-context-set-alnum! cc #t)
	   #t)
	  ((canna-kana-toggle-key? key key-state)
	   (canna-context-kana-toggle cc)
	   (canna-context-set-alnum! cc #f)
	   #t)
	  ((canna-alkana-toggle-key? key key-state)
	   (canna-context-alkana-toggle cc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(canna-context-set-on! cc #t)
	(rk-flush (canna-context-rkc cc))
	(canna-context-set-state! cc #f)
	#t)
      #f))

(define (canna-update-preedit cc)
  (if (not (canna-context-commit-raw cc))
      (let ((segments (if (canna-context-on cc)
			  (if (canna-context-transposing cc)
			      (canna-context-transposing-state-preedit cc)
			      (if (canna-context-state cc)
				  (canna-compose-state-preedit cc)
                                  (if (canna-context-predicting cc)
                                      (canna-predicting-state-preedit cc)
                                      (canna-input-state-preedit cc))))
			  ())))
	(context-update-preedit cc segments))
      (canna-context-set-commit-raw! cc #f)))

(define (canna-begin-conv cc)
  (let ((cc-id (canna-context-cc-id cc))
	(preconv-str (canna-make-whole-string cc #t canna-type-hiragana)))
    (if (and cc-id
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
      (canna-flush cc)
      (canna-context-set-on! cc #f)
      (canna-commit-raw cc))

     ((canna-off-key? key key-state)
      (canna-flush cc)
      (canna-context-set-on! cc #f))

     ((canna-backspace-key? key key-state)
      (canna-commit-raw cc))
     
     ((canna-delete-key? key key-state)
      (canna-commit-raw cc))

     ((and
       (canna-hiragana-key? key key-state)
       (not
        (and
	 (= (canna-context-kana-mode cc) canna-type-hiragana)
	 (not (canna-context-alnum cc)))))
      (canna-context-change-kana-mode! cc canna-type-hiragana)
      (canna-context-set-alnum! cc #f))

     ((and
       (canna-katakana-key? key key-state)
       (not
        (and
	 (= (canna-context-kana-mode cc) canna-type-katakana)
	 (not (canna-context-alnum cc)))))
      (canna-context-change-kana-mode! cc canna-type-katakana)
      (canna-context-set-alnum! cc #f))
     
     ((and
       (canna-halfkana-key? key key-state)
       (not
        (and
	 (= (canna-context-kana-mode cc) canna-type-halfkana)
	 (not (canna-context-alnum cc)))))
      (canna-context-change-kana-mode! cc canna-type-halfkana)
      (canna-context-set-alnum! cc #f))
     
     ((and
       (canna-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (canna-context-alnum-type cc) canna-type-halfwidth-alnum)
	 (canna-context-alnum cc))))
      (canna-context-set-alnum-type! cc canna-type-halfwidth-alnum)
      (canna-context-set-alnum! cc #t))
     
     ((and
       (canna-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (canna-context-alnum-type cc) canna-type-fullwidth-alnum)
	 (canna-context-alnum cc))))
      (canna-context-set-alnum-type! cc canna-type-fullwidth-alnum)
      (canna-context-set-alnum! cc #t))
     
     ((and
       (not (canna-context-alnum cc))
       (canna-kana-toggle-key? key key-state))
      (canna-context-kana-toggle cc))

     ((canna-alkana-toggle-key? key key-state)
      (canna-context-alkana-toggle cc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (canna-commit-raw cc))
     
     ;; direct key => commit
     (direct
      (im-commit cc direct))

     ;; space key
     ((canna-space-key? key key-state)
      (if (canna-context-alnum cc)
	  (im-commit cc (list-ref
			 ja-alnum-space
			 (- (canna-context-alnum-type cc)
			    canna-type-halfwidth-alnum)))
	  (im-commit cc (list-ref ja-space (canna-context-kana-mode cc)))))

     ((symbol? key)
      (canna-commit-raw cc))

     (else
      (if (canna-context-alnum cc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (canna-context-preconv-ustr cc)
			       (if (= (canna-context-alnum-type cc)
				      canna-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (canna-context-raw-ustr cc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule canna-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (canna-context-preconv-ustr cc) res)
                    (ustr-insert-elem! (canna-context-preconv-ustr cc) res))
	          (ustr-insert-elem! (canna-context-raw-ustr cc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (canna-commit-raw cc)))))))))

(define (canna-has-preedit? cc)
  (or (not (ustr-empty? (canna-context-preconv-ustr cc)))
      (> (string-length (rk-pending (canna-context-rkc cc))) 0)))

(define canna-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type canna-type-halfwidth-alnum)
       (= state canna-type-halfwidth-alnum))
      canna-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type canna-type-fullwidth-alnum)
       (= state canna-type-fullwidth-alnum))
      canna-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define canna-proc-transposing-state
  (lambda (cc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (canna-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons canna-type-fullwidth-alnum rotate-list)))
      (if (canna-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons canna-type-halfwidth-alnum rotate-list)))
      (if (canna-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons canna-type-halfkana rotate-list)))
      (if (canna-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons canna-type-katakana rotate-list)))
      (if (canna-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons canna-type-hiragana rotate-list)))

      (if (canna-context-transposing cc)
	  (let ((lst (member (canna-context-transposing-type cc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (canna-rotate-transposing-alnum-type
				 (canna-context-transposing-type cc)
				 (car rotate-list))))))
	  (begin
	    (canna-context-set-transposing! cc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state canna-type-hiragana)
	      (= state canna-type-katakana)
	      (= state canna-type-halfkana)))
	(canna-context-set-transposing-type! cc state))
       ((and state
	     (or
	      (= state canna-type-halfwidth-alnum)
	      (= state canna-candidate-type-upper-halfwidth-alnum)
	      (= state canna-type-fullwidth-alnum)
	      (= state canna-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	    (canna-context-set-transposing-type! cc state)))
       (else
	(and
	 ; commit
	 (if (canna-commit-key? key key-state)
	     (begin
	       (im-commit cc (canna-transposing-text cc))
	       (canna-flush cc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (canna-begin-conv-key? key key-state)
	     (begin
	       (canna-context-set-transposing! cc #f)
	       (canna-begin-conv cc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (canna-cancel-key? key key-state)
	      (canna-backspace-key? key key-state))
	     (begin
	       (canna-context-set-transposing! cc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (canna-prev-page-key? key key-state)
	      (canna-next-page-key? key key-state)
	      (canna-extend-segment-key? key key-state)
	      (canna-shrink-segment-key? key key-state)
	      (canna-next-segment-key? key key-state)
	      (canna-beginning-of-preedit-key? key key-state)
	      (canna-end-of-preedit-key? key key-state)
	      (canna-next-candidate-key? key key-state)
	      (canna-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit cc (canna-transposing-text cc))
	   (canna-flush cc)
	   (canna-proc-input-state cc key key-state))))))))

(define (canna-move-prediction cc offset)
  (let* ((nr (canna-lib-get-nr-predictions cc))
         (idx (canna-context-prediction-index cc))
         (n (if (not idx)
                0
                (+ idx offset)))
         (compensated-n (cond
                         ((>= n nr)
                          0)
                        ((< n 0)
                         (- nr 1))
                        (else
                         n))))
    (im-select-candidate cc compensated-n)
    (canna-context-set-prediction-index! cc compensated-n)))

(define (canna-move-prediction-in-page sc numeralc)
  (let* ((nr (canna-lib-get-nr-predictions sc))
         (p-idx (canna-context-prediction-index sc))
         (n (if (not p-idx)
                0
                p-idx))
         (cur-page (if (= canna-nr-candidate-max 0)
                       0
                       (quotient n canna-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page canna-nr-candidate-max) compensated-pageidx))
         (compensated-idx (cond
                           ((>= idx nr)
                            #f)
                           (else
                            idx)))
         (selected-pageidx (if (not p-idx)
                               #f
                               (if (= canna-nr-candidate-max 0)
                                   p-idx
                                   (remainder p-idx
                                              canna-nr-candidate-max)))))
    (if (and
         compensated-idx
         (not (eqv? compensated-pageidx selected-pageidx)))
        (begin
          (canna-context-set-prediction-index! sc compensated-idx)
          (im-select-candidate sc compensated-idx)
          #t)
       #f)))

(define (canna-prediction-select-non-existing-index? cc numeralc)
  (let* ((nr (canna-lib-get-nr-predictions cc))
         (p-idx (canna-context-prediction-index cc))
         (cur-page (if (= canna-nr-candidate-max 0)
                       0
                       (quotient p-idx canna-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page canna-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (canna-prediction-keys-handled? cc key key-state)
  (cond
   ((canna-next-prediction-key? key key-state)
    (canna-move-prediction cc 1)
    #t)
   ((canna-prev-prediction-key? key key-state)
    (canna-move-prediction cc -1)
    #t)
   ((and
     canna-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (canna-move-prediction-in-page cc key))
   ((and
     (canna-context-prediction-index cc)
     (canna-prev-page-key? key key-state))
    (im-shift-page-candidate cc #f)
    #t)
   ((and
     (canna-context-prediction-index cc)
     (canna-next-page-key? key key-state))
    (im-shift-page-candidate cc #t)
    #t)
   (else
    #f)))

(define (canna-proc-prediction-state cc key key-state)
  (cond
   ;; prediction index change
   ((canna-prediction-keys-handled? cc key key-state))

   ;; cancel
   ((canna-cancel-key? key key-state)
    (if (canna-context-prediction-index cc)
        (canna-reset-prediction-window cc)
        (begin
          (canna-reset-prediction-window cc)
          (canna-proc-input-state cc key key-state))))

   ;; commit
   ((and
     (canna-context-prediction-index cc)
     (canna-commit-key? key key-state))
    (canna-do-commit-prediction cc))
   (else
    (if (and
         canna-use-implicit-commit-prediction?
         (canna-context-prediction-index cc))
        (cond
         ((or
           ;; check keys used in canna-proc-input-state-with-preedit
           (canna-begin-conv-key? key key-state)
           (canna-backspace-key? key key-state)
           (canna-delete-key? key key-state)
           (canna-kill-key? key key-state)
           (canna-kill-backward-key? key key-state)
           (and
            (not (canna-context-alnum cc))
            (canna-commit-as-opposite-kana-key? key key-state))
           (canna-transpose-as-hiragana-key? key key-state)
           (canna-transpose-as-katakana-key? key key-state)
           (canna-transpose-as-halfkana-key? key key-state)
           (and
            (not (= (canna-context-input-rule cc) canna-input-rule-kana))
            (or
             (canna-transpose-as-halfwidth-alnum-key? key key-state)
             (canna-transpose-as-fullwidth-alnum-key? key key-state)))
           (canna-hiragana-key? key key-state)
           (canna-katakana-key? key key-state)
           (canna-halfkana-key? key key-state)
           (canna-halfwidth-alnum-key? key key-state)
           (canna-fullwidth-alnum-key? key key-state)
           (and
            (not (canna-context-alnum cc))
            (canna-kana-toggle-key? key key-state))
           (canna-alkana-toggle-key? key key-state)
           (canna-go-left-key? key key-state)
           (canna-go-right-key? key key-state)
           (canna-beginning-of-preedit-key? key key-state)
           (canna-end-of-preedit-key? key key-state)
           (and
            (modifier-key-mask key-state)
            (not (shift-key-mask key-state))))
          ;; go back to unselected prediction
          (canna-reset-prediction-window cc)
          (canna-check-prediction cc #f))
         ((and
           (ichar-numeric? key)
           canna-select-prediction-by-numeral-key?
           (not (canna-prediction-select-non-existing-index? cc key)))
          (canna-context-set-predicting! cc #f)
          (canna-context-set-prediction-index! cc #f)
          (canna-proc-input-state cc key key-state))
         (else
          ;; implicit commit
          (canna-do-commit-prediction cc)
          (canna-proc-input-state cc key key-state)))
        (begin
          (canna-context-set-predicting! cc #f)
          (canna-context-set-prediction-index! cc #f)
          (canna-proc-input-state cc key key-state))))))

(define (canna-proc-input-state-with-preedit cc key key-state)
  (define (check-auto-conv str)
    (and
      str
      canna-auto-start-henkan?
      (string-find japanese-auto-start-henkan-keyword-list str)
      (begin
	(canna-reset-prediction-window cc)
	(canna-begin-conv cc))))
  (let ((preconv-str (canna-context-preconv-ustr cc))
	(raw-str (canna-context-raw-ustr cc))
	(rkc (canna-context-rkc cc))
	(rule (canna-context-input-rule cc))
	(kana (canna-context-kana-mode cc)))
    (cond
     ;; begin conversion
     ((canna-begin-conv-key? key key-state)
      (canna-reset-prediction-window cc)
      (canna-begin-conv cc))

     ;; prediction
     ((canna-next-prediction-key? key key-state)
      (canna-check-prediction cc #t))

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
		 (not (ichar-printable?
		       (string->ichar
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
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((canna-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (canna-context-alnum cc))
       (canna-commit-as-opposite-kana-key? key key-state))
      (im-commit cc (canna-make-whole-string cc #t (ja-opposite-kana kana)))
      (canna-flush cc))

     ;; Transposing状態へ移行
     ((or (canna-transpose-as-hiragana-key? key key-state)
	  (canna-transpose-as-katakana-key? key key-state)
	  (canna-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	   (or
	    (canna-transpose-as-halfwidth-alnum-key? key key-state)
	    (canna-transpose-as-fullwidth-alnum-key? key key-state))))
      (canna-reset-prediction-window cc)
      (canna-proc-transposing-state cc key key-state))

     ((canna-hiragana-key? key key-state)
      (if (not (= kana canna-type-hiragana))
	  (begin
	    (im-commit cc (canna-make-whole-string cc #t kana))
	    (canna-flush cc)))
      (canna-context-set-kana-mode! cc canna-type-hiragana)
      (canna-context-set-alnum! cc #f))

     ((canna-katakana-key? key key-state)
      (if (not (= kana canna-type-katakana))
	  (begin
	    (im-commit cc (canna-make-whole-string cc #t kana))
	    (canna-flush cc)))
      (canna-context-set-kana-mode! cc canna-type-katakana)
      (canna-context-set-alnum! cc #f))

     ((canna-halfkana-key? key key-state)
      (if (not (= kana canna-type-halfkana))
	  (begin
	    (im-commit cc (canna-make-whole-string cc #t kana))
	    (canna-flush cc)))
      (canna-context-set-kana-mode! cc canna-type-halfkana)
      (canna-context-set-alnum! cc #f))

     ((and
       (canna-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (canna-context-alnum-type cc) canna-type-halfwidth-alnum)
	 (canna-context-alnum cc))))
      (canna-context-set-alnum-type! cc canna-type-halfwidth-alnum)
      (canna-context-set-alnum! cc #t))

     ((and
       (canna-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (canna-context-alnum-type cc) canna-type-fullwidth-alnum)
	 (canna-context-alnum cc))))
      (canna-context-set-alnum-type! cc canna-type-fullwidth-alnum)
      (canna-context-set-alnum! cc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (canna-context-alnum cc))
       (canna-kana-toggle-key? key key-state))
      (im-commit cc (canna-make-whole-string cc #t kana))
      (canna-flush cc)
      (canna-context-kana-toggle cc))

     ((canna-alkana-toggle-key? key key-state)
      (canna-context-alkana-toggle cc))

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

     ((symbol? key)
      #f)

     (else
      (if (canna-context-alnum cc)
          (let ((key-str (charcode->string key))
	        (pend (rk-pending rkc))
		(residual-kana (rk-peek-terminal-match rkc)))
	    (rk-flush rkc) ;; OK to reset rkc here.
	    (if residual-kana
	        (begin
                  (if (list? (car residual-kana))
                    (begin
                      (ustr-insert-seq! preconv-str residual-kana)
                      (ustr-insert-elem! raw-str (reverse
                                                   (string-to-list pend))))
                    (begin
                      (ustr-insert-elem! preconv-str residual-kana)
                      (ustr-insert-elem! raw-str pend)))))
	    (ustr-insert-elem! preconv-str
			       (if (= (canna-context-alnum-type cc)
				      canna-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str)
	    (check-auto-conv key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule canna-input-rule-kana)
			       key
			       (ichar-downcase key))))
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
		      (ustr-insert-seq! raw-str
                                        (reverse (string-to-list pend)))
		      (if (list? (car res))
		          (begin
                            (if (member pend
                                        (map car
                                             ja-consonant-syllable-table))
                              ;; treat consonant having more than one
                              ;; charactear as one raw-str in this case
                              (ustr-insert-elem! raw-str pend)
                              (ustr-insert-elem! raw-str (reverse
                                                           (string-to-list
                                                             pend))))
                            ;; assume key-str as a vowel
			    (ustr-insert-elem! raw-str key-str))
		          (ustr-insert-elem!
		           raw-str
		           (string-append pend key-str))))))
	    (check-auto-conv (if res (car res) #f))))))))

(define canna-context-confirm-kana!
  (lambda (cc)
    (if (= (canna-context-input-rule cc)
	   canna-input-rule-kana)
	(let* ((preconv-str (canna-context-preconv-ustr cc))
	       (rkc (canna-context-rkc cc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (canna-reset-prediction-window cc)
  (if (canna-context-prediction-window cc)
      (im-deactivate-candidate-selector cc))
  (canna-context-set-predicting! cc #f)
  (canna-context-set-prediction-window! cc #f)
  (canna-context-set-prediction-index! cc #f))

(define (canna-check-prediction cc force-check?)
  (if (and
       (not (canna-context-state cc))
       (not (canna-context-transposing cc))
       (not (canna-context-predicting cc)))
      (let* ((use-pending-rk-for-prediction? #t)
	     (preconv-str
	      (canna-make-whole-string
	       cc
	       (not use-pending-rk-for-prediction?)
	       (canna-context-kana-mode cc)))
	     (preedit-len (+
			   (ustr-length (canna-context-preconv-ustr cc))
			   (if (not use-pending-rk-for-prediction?)
			       0
			       (string-length (rk-pending
					       (canna-context-rkc
						cc)))))))
	(if (or
	     (>= preedit-len canna-prediction-start-char-count)
	     force-check?)
	    (begin
	      (canna-lib-set-prediction-src-string cc preconv-str)
	      (let ((nr (canna-lib-get-nr-predictions cc)))
		(if (and
		     nr
		     (> nr 0))
		    (begin
		      (im-activate-candidate-selector
		       cc nr canna-nr-candidate-max)
		      (canna-context-set-prediction-window! cc #t)
		      (canna-context-set-predicting! cc #t))
		    (canna-reset-prediction-window cc))))
	    (canna-reset-prediction-window cc)))))

(define (canna-proc-input-state cc key key-state)
  (if (canna-has-preedit? cc)
      (canna-proc-input-state-with-preedit cc key key-state)
      (canna-proc-input-state-no-preedit cc key key-state))
  (if canna-use-prediction?
      (canna-check-prediction cc #f)))

(define canna-separator
  (lambda (cc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
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
       ((or
	 (= transposing-type canna-type-hiragana)
	 (= transposing-type canna-type-katakana)
	 (= transposing-type canna-type-halfkana))
	(canna-make-whole-string cc #t transposing-type))
       ((= transposing-type canna-type-halfwidth-alnum)
	(canna-make-whole-raw-string cc #f #f))
       ((= transposing-type canna-candidate-type-upper-halfwidth-alnum)
	(canna-make-whole-raw-string cc #f #t))
       ((= transposing-type canna-type-fullwidth-alnum)
	(canna-make-whole-raw-string cc #t #f))
       ((= transposing-type canna-candidate-type-upper-fullwidth-alnum)
	(canna-make-whole-raw-string cc #t #t))))))

(define canna-get-raw-str-seq
  (lambda (cc)
    (let* ((rkc (canna-context-rkc cc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (canna-context-raw-ustr cc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
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
       ((= cand-idx canna-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) canna-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (canna-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx canna-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       canna-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx canna-candidate-type-halfwidth-alnum)
			    (= cand-idx canna-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (canna-predicting-state-preedit cc)
  (if (or
       (not canna-use-implicit-commit-prediction?)
       (not (canna-context-prediction-index cc)))
      (canna-input-state-preedit cc)
      (let ((cand (canna-get-prediction-string cc)))
        (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (canna-compose-state-preedit cc)
  (let* ((cc-id (canna-context-cc-id cc))
	 (segments (canna-context-segments cc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (canna-separator cc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
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
    (if cc-id
	(begin
	  (for-each (lambda (seg-idx cand-idx)
		      (if (> cand-idx canna-candidate-type-katakana)
			  (canna-lib-commit-segment cc-id seg-idx cand-idx)))
		    (iota (ustr-length segments))
		    (ustr-whole-seq segments))
	  (if (every (lambda (x) (<= x canna-candidate-type-katakana))
		     (ustr-whole-seq segments))
	      (canna-lib-reset-conversion cc-id))))))

(define (canna-do-commit cc)
    (im-commit cc (canna-get-commit-string cc))
    (canna-commit-string cc)
    (canna-reset-candidate-window cc)
    (canna-flush cc))

(define (canna-get-prediction-string cc)
  (canna-lib-get-nth-prediction
   cc
   (canna-context-prediction-index cc)))

(define (canna-learn-prediction-string cc)
  (canna-lib-commit-nth-prediction
   cc
   (canna-context-prediction-index cc)))

(define (canna-do-commit-prediction cc)
  (im-commit cc (canna-get-prediction-string cc))
  (canna-learn-prediction-string cc)
  (canna-reset-prediction-window cc)
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
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
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

(define canna-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx canna-candidate-type-halfwidth-alnum)
       (= state canna-candidate-type-halfwidth-alnum))
      canna-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx canna-candidate-type-fullwidth-alnum)
       (= state canna-candidate-type-fullwidth-alnum))
      canna-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define canna-set-segment-transposing
  (lambda (cc key key-state)
    (let ((segments (canna-context-segments cc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(canna-reset-candidate-window cc)
	(canna-context-set-candidate-op-count! cc 0)

	(if (canna-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (canna-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (canna-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons canna-candidate-type-halfkana
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
	     (= idx canna-candidate-type-halfkana)
	     (= idx canna-candidate-type-halfwidth-alnum)
	     (= idx canna-candidate-type-fullwidth-alnum)
	     (= idx canna-candidate-type-upper-halfwidth-alnum)
	     (= idx canna-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (canna-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
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
	  (canna-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (canna-context-input-rule cc) canna-input-rule-kana))
	   (or
	    (canna-transpose-as-halfwidth-alnum-key? key key-state)
	    (canna-transpose-as-fullwidth-alnum-key? key key-state))))
      (canna-set-segment-transposing cc key key-state))

     ((canna-cancel-key? key key-state)
      (canna-cancel-conv cc))

     ((and canna-select-candidate-by-numeral-key?
	   (ichar-numeric? key)
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

(define (canna-press-key-handler cc key key-state)
  (if (ichar-control? key)
      (im-commit-raw cc)
      (if (canna-context-on cc)
          (if (canna-context-transposing cc)
              (canna-proc-transposing-state cc key key-state)
              (if (canna-context-state cc)
                  (canna-proc-compose-state cc key key-state)
                  (if (canna-context-predicting cc)
                      (canna-proc-prediction-state cc key key-state)
                      (canna-proc-input-state cc key key-state))))
	  (canna-proc-raw-state cc key key-state)))
  (canna-update-preedit cc))

;;;
(define (canna-release-key-handler cc key key-state)
  (if (or (ichar-control? key)
	  (not (canna-context-on cc)))
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
         (cand (if (canna-context-state cc)
                   (canna-lib-get-nth-candidate cc-id cur-seg idx)
                   (canna-lib-get-nth-prediction cc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (canna-set-candidate-index-handler cc idx)
  (cond
   ((canna-context-state cc)
    (ustr-cursor-set-frontside! (canna-context-segments cc) idx)
    (canna-update-preedit cc))
   ((canna-context-predicting cc)
    (canna-context-set-prediction-index! cc idx)
    (canna-update-preedit cc))))

(define (canna-proc-raw-state cc key key-state)
  (if (not (canna-begin-input cc key key-state))
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
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
