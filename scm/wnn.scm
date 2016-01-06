;;; wnn.scm: Wnn for uim.
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
(require-custom "generic-key-custom.scm")
(require-custom "wnn-custom.scm")
(require-custom "wnn-key-custom.scm")


;;; implementations

;;
;; canna emulating functions
;;
(define (wnn-move wc-ctx nth)
  (wnn-lib-top wc-ctx)
  (if (< 0 nth)
      (for-each (lambda (x) (wnn-lib-move wc-ctx #t 'forward)) (iota nth))))
(define (wnn-lib-init server)
  (if wnn-use-remote-server?
      (wnn-lib-open server "uim" wnn-rcfile 0)
      (wnn-lib-open "" "uim" wnn-rcfile 0)))
(define (wnn-lib-alloc-context wc)
  (wnn-lib-create-buffer (wnn-context-wnn-buf wc) 0 0))
(define (wnn-lib-get-nth-candidate wc seg nth)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-move wc-ctx seg)
    (wnn-lib-candidate-info wc-ctx #t)
    (wnn-lib-get-candidate wc-ctx nth)))
(define (wnn-lib-release-context wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-lib-destroy-buffer wc-ctx #t)
    (wnn-lib-close (wnn-context-wnn-buf wc))
    (wnn-context-set-wc-ctx! wc #f)))
(define (wnn-lib-get-unconv-candidate wc seg-idx)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-move wc-ctx seg-idx)
    (cdr (assoc 'kanap (cdr (assoc 'clause-info (wnn-lib-get-jconvbuf wc-ctx)))))))
(define (wnn-lib-get-nr-segments wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (cdr (assoc 'cur-n-clause (wnn-lib-get-jconvbuf wc-ctx)))))
(define (wnn-lib-get-nr-candidates wc seg)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-move wc-ctx seg)
    (cdr (assoc 'ncand (wnn-lib-candidate-info wc-ctx #t)))))
(define (wnn-lib-resize-segment wc seg cnt)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-move wc-ctx seg)
    (cond ((< cnt 0)
           (for-each (lambda (x) (wnn-lib-shrink wc-ctx #t #t))
                     (iota (* -1 cnt))))
          ((< 0 cnt)
           (for-each (lambda (x) (wnn-lib-expand wc-ctx #t #t))
                     (iota cnt))))))
(define (wnn-lib-begin-conversion wc str)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (for-each (lambda (c) (wnn-lib-insert-char wc-ctx c))
              (reverse (string-to-list str)))
    (wnn-lib-convert wc-ctx #f #f #t)
    (wnn-lib-get-nr-segments wc)))
(define (wnn-lib-commit-segment wc seg delta)
  (let* ((wc-ctx (wnn-context-wc-ctx wc)))
    (predict-meta-commit
     (wnn-context-prediction-ctx wc)
     (cdr (assoc 'kanap (cdr (assoc 'clause-info (wnn-lib-get-jconvbuf wc-ctx)))))
     (wnn-lib-get-candidate wc-ctx delta)
     "")
    (wnn-lib-fix wc-ctx)
    (wnn-lib-save-dic wc-ctx)
    (wnn-lib-clear wc-ctx)
    #t))
(define (wnn-lib-reset-conversion wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (wnn-lib-clear wc-ctx)))


(define wnn-type-direct	   ja-type-direct)
(define wnn-type-hiragana	   ja-type-hiragana)
(define wnn-type-katakana	   ja-type-katakana)
(define wnn-type-halfkana	   ja-type-halfkana)
(define wnn-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define wnn-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define wnn-input-rule-roma 0)
(define wnn-input-rule-kana 1)
(define wnn-input-rule-azik 2)
(define wnn-input-rule-act 3)
(define wnn-input-rule-kzik 4)

(define wnn-candidate-type-katakana -2)
(define wnn-candidate-type-hiragana -3)
(define wnn-candidate-type-halfkana -4)
(define wnn-candidate-type-halfwidth-alnum -5)
(define wnn-candidate-type-fullwidth-alnum -6)
(define wnn-candidate-type-upper-halfwidth-alnum -7)
(define wnn-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key wnn-space-key? '(" "))

(define wnn-prepare-input-rule-activation
  (lambda (wc)
    (cond
     ((wnn-context-state wc)
      (wnn-do-commit wc))
     ((wnn-context-transposing wc)
      (im-commit wc (wnn-transposing-text wc)))
     ((and
       (wnn-context-on wc)
       (wnn-has-preedit? wc))
      (im-commit
       wc (wnn-make-whole-string wc #t (wnn-context-kana-mode wc)))))
    (wnn-flush wc)
    (wnn-update-preedit wc)))

(define wnn-prepare-input-mode-activation
  (lambda (wc new-mode)
    (let ((old-kana (wnn-context-kana-mode wc)))
      (cond
       ((wnn-context-state wc)
	(wnn-do-commit wc))
       ((wnn-context-transposing wc)
	(im-commit wc (wnn-transposing-text wc))
	(wnn-flush wc))
       ((and
	 (wnn-context-on wc)
	 (wnn-has-preedit? wc)
	 (not (= old-kana new-mode)))
	(im-commit
	 wc (wnn-make-whole-string wc #t (wnn-context-kana-mode wc)))
	(wnn-flush wc)))
      (wnn-update-preedit wc))))

(register-action 'action_wnn_hiragana
		 (lambda (wc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (wc) ;; activity predicate
		   (and (wnn-context-on wc)
		        (not (wnn-context-alnum wc))
			(= (wnn-context-kana-mode wc)
			   wnn-type-hiragana)))

		 (lambda (wc) ;; action handler
		   (wnn-prepare-input-mode-activation wc wnn-type-hiragana)
		   (wnn-context-set-on! wc #t)
		   (wnn-context-set-alnum! wc #f)
		   (wnn-context-change-kana-mode! wc wnn-type-hiragana)))

(register-action 'action_wnn_katakana
		 (lambda (wc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (wc)
		   (and (wnn-context-on wc)
		        (not (wnn-context-alnum wc))
			(= (wnn-context-kana-mode wc)
			   wnn-type-katakana)))
		 (lambda (wc)
		   (wnn-prepare-input-mode-activation wc wnn-type-katakana)
		   (wnn-context-set-on! wc #t)
		   (wnn-context-set-alnum! wc #f)
		   (wnn-context-change-kana-mode! wc wnn-type-katakana)))

(register-action 'action_wnn_halfkana
		 (lambda (wc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (wc)
		   (and (wnn-context-on wc)
			(not (wnn-context-alnum wc))
			(= (wnn-context-kana-mode wc) wnn-type-halfkana)))
		 (lambda (wc)
		   (wnn-prepare-input-mode-activation wc wnn-type-halfkana)
		   (wnn-context-set-on! wc #t)
		   (wnn-context-set-alnum! wc #f)
		   (wnn-context-change-kana-mode! wc wnn-type-halfkana)))

(register-action 'action_wnn_halfwidth_alnum
		 (lambda (wc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (wc) ;; activity predicate
		   (and (wnn-context-on wc)
			(wnn-context-alnum wc)
			(= (wnn-context-alnum-type wc)
			   wnn-type-halfwidth-alnum)))
		 (lambda (wc) ;; action handler
		   (wnn-prepare-input-mode-activation
		    wc (wnn-context-kana-mode wc))
		   (wnn-context-set-on! wc #t)
		   (wnn-context-set-alnum! wc #t)
		   (wnn-context-set-alnum-type!
		    wc wnn-type-halfwidth-alnum)))

(register-action 'action_wnn_direct
		 (lambda (wc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (wc)
		   (not (wnn-context-on wc)))
		 (lambda (wc)
		   (wnn-prepare-input-mode-activation wc wnn-type-direct)
		   (wnn-context-set-on! wc #f)))

(register-action 'action_wnn_fullwidth_alnum
		 (lambda (wc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (wc)
		   (and (wnn-context-on wc)
			(wnn-context-alnum wc)
			(= (wnn-context-alnum-type wc)
			   wnn-type-fullwidth-alnum)))
		 (lambda (wc)
		   (wnn-prepare-input-mode-activation
		    wc (wnn-context-kana-mode wc))
		   (wnn-context-set-on! wc #t)
		   (wnn-context-set-alnum! wc #t)
		   (wnn-context-set-alnum-type!
		    wc wnn-type-fullwidth-alnum)))

(register-action 'action_wnn_roma
		 (lambda (wc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (wc)
		   (= (wnn-context-input-rule wc)
		      wnn-input-rule-roma))
		 (lambda (wc)
		   (wnn-prepare-input-rule-activation wc)
		   (rk-context-set-rule! (wnn-context-rkc wc)
					 ja-rk-rule)
		   (wnn-context-set-input-rule! wc wnn-input-rule-roma)))

(register-action 'action_wnn_kana
		 (lambda (wc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (wc)
		   (= (wnn-context-input-rule wc)
		      wnn-input-rule-kana))
		 (lambda (wc)
		   (wnn-prepare-input-rule-activation wc)
                   (require "japanese-kana.scm")
		   (wnn-context-set-input-rule! wc wnn-input-rule-kana)
                   (wnn-context-change-kana-mode!
                     wc (wnn-context-kana-mode wc))
		   (wnn-context-set-alnum! wc #f)))

(register-action 'action_wnn_azik
		 (lambda (wc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (wc)
		   (= (wnn-context-input-rule wc)
		      wnn-input-rule-azik))
		 (lambda (wc)
		   (wnn-prepare-input-rule-activation wc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (wnn-context-rkc wc)
					 ja-azik-rule)
		   (wnn-context-set-input-rule! wc wnn-input-rule-azik)))

(register-action 'action_wnn_kzik
		 (lambda (wc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (wc)
		   (= (wnn-context-input-rule wc)
		      wnn-input-rule-kzik))
		 (lambda (wc)
		   (wnn-prepare-input-rule-activation wc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (wnn-context-rkc wc)
					 ja-kzik-rule)
		   (wnn-context-set-input-rule! wc wnn-input-rule-kzik)))

(register-action 'action_wnn_act
		 (lambda (wc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (wc)
		   (= (wnn-context-input-rule wc)
		      wnn-input-rule-act))
		 (lambda (wc)
		   (wnn-prepare-input-rule-activation wc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (wnn-context-rkc wc)
					 ja-act-rule)
		   (wnn-context-set-input-rule! wc wnn-input-rule-act)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define wnn-configure-widgets
  (lambda ()
    (register-widget 'widget_wnn_input_mode
		     (activity-indicator-new wnn-input-mode-actions)
		     (actions-new wnn-input-mode-actions))

    (register-widget 'widget_wnn_kana_input_method
		     (activity-indicator-new wnn-kana-input-method-actions)
		     (actions-new wnn-kana-input-method-actions))
    (context-list-replace-widgets! 'wnn wnn-widgets)))

(define wnn-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'wnn-buf            #f)
    (list 'wc-ctx             #f) ;; wnn-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'kana-mode          wnn-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      wnn-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         wnn-input-rule-roma)
    (list 'raw-ustr	      #f)
    (list 'prediction-ctx     '())
    (list 'prediction-word    '())
    (list 'prediction-candidates '())
    (list 'prediction-appendix '())
    (list 'prediction-nr      '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '()))))

(define-record 'wnn-context wnn-context-rec-spec)
(define wnn-context-new-internal wnn-context-new)

(define (wnn-predict wc str)
  (predict-meta-search
   (wnn-context-prediction-ctx wc)
   str))
(define (wnn-lib-set-prediction-src-string wc str)
  (let* ((ret      (wnn-predict wc str))
         (word     (predict-meta-word? ret))
         (cands    (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret)))
    (wnn-context-set-prediction-word! wc word)
    (wnn-context-set-prediction-candidates! wc cands)
    (wnn-context-set-prediction-appendix! wc appendix)
    (wnn-context-set-prediction-nr! wc (length cands)))
  #f)
(define (wnn-lib-get-nr-predictions wc)
  (wnn-context-prediction-nr wc))
(define (wnn-lib-get-nth-word wc nth)
  (let ((word (wnn-context-prediction-word wc)))
    (list-ref word nth)))
(define (wnn-lib-get-nth-prediction wc nth)
  (let ((cands (wnn-context-prediction-candidates wc)))
    (list-ref cands nth)))
(define (wnn-lib-get-nth-appendix wc nth)
  (let ((appendix (wnn-context-prediction-appendix wc)))
    (list-ref appendix nth)))
(define (wnn-lib-commit-nth-prediction wc nth)
  (predict-meta-commit
   (wnn-context-prediction-ctx wc)
   (wnn-lib-get-nth-word wc nth)
   (wnn-lib-get-nth-prediction wc nth)
   (wnn-lib-get-nth-appendix wc nth)))


(define (wnn-context-new id im)
  (let ((wc (wnn-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
    (if (not (wnn-context-wnn-buf wc))
        (wnn-context-set-wnn-buf! wc (wnn-lib-init wnn-server-name)))
    (wnn-context-set-wc-ctx! wc (wnn-lib-alloc-context wc))
    (wnn-context-set-widgets! wc wnn-widgets)
    (wnn-context-set-rkc! wc rkc)
    (wnn-context-set-preconv-ustr! wc (ustr-new '()))
    (wnn-context-set-raw-ustr! wc (ustr-new '()))
    (wnn-context-set-segments! wc (ustr-new '()))
    (if wnn-use-prediction?
        (begin
          (wnn-context-set-prediction-ctx! wc (predict-make-meta-search))
          (predict-meta-open (wnn-context-prediction-ctx wc) "wnn")
          (predict-meta-set-external-charset! (wnn-context-prediction-ctx wc) "EUC-JP")))
    wc))

(define (wnn-commit-raw wc)
  (im-commit-raw wc)
  (wnn-context-set-commit-raw! wc #t))

(define (wnn-context-kana-toggle wc)
  (let* ((kana (wnn-context-kana-mode wc))
	 (opposite-kana (ja-opposite-kana kana)))
    (wnn-context-change-kana-mode! wc opposite-kana)))

(define wnn-context-alkana-toggle
  (lambda (wc)
    (let ((alnum-state (wnn-context-alnum wc)))
      (wnn-context-set-alnum! wc (not alnum-state)))))

(define wnn-context-change-kana-mode!
  (lambda (wc kana-mode)
    (if (= (wnn-context-input-rule wc)
           wnn-input-rule-kana)
        (rk-context-set-rule!
	 (wnn-context-rkc wc)
	 (cond
	  ((= kana-mode wnn-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode wnn-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode wnn-type-halfkana) ja-kana-halfkana-rule))))
    (wnn-context-set-kana-mode! wc kana-mode)))

(define wnn-make-whole-string
  (lambda (wc convert-pending-into-kana? kana)
    (let* ((rkc (wnn-context-rkc wc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (wnn-context-input-rule wc))
           (preconv-str (wnn-context-preconv-ustr wc))
           (extract-kana
            (if (= rule wnn-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule wnn-input-rule-kana)
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

(define wnn-make-raw-string
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
	     (wnn-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (wnn-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define wnn-make-whole-raw-string
  (lambda (wc wide? upper?)
    (wnn-make-raw-string (wnn-get-raw-str-seq wc) wide? upper?)))

(define (wnn-init-handler id im arg)
  (wnn-context-new id im))

(define (wnn-release-handler wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc)))
    (if wc-ctx
        (wnn-lib-release-context wc))))

(define (wnn-flush wc)
  (rk-flush (wnn-context-rkc wc))
  (ustr-clear! (wnn-context-preconv-ustr wc))
  (ustr-clear! (wnn-context-raw-ustr wc))
  (ustr-clear! (wnn-context-segments wc))
  (wnn-context-set-transposing! wc #f)
  (wnn-context-set-state! wc #f)
  (if (or (wnn-context-candidate-window wc)
          (wnn-context-prediction-window wc))
      (im-deactivate-candidate-selector wc))
  (wnn-context-set-candidate-window! wc #f)
  (wnn-context-set-prediction-window! wc #f)
  (wnn-context-set-candidate-op-count! wc 0))

(define (wnn-begin-input wc key key-state)
  (if (cond
       ((wnn-on-key? key key-state)
	#t)
       ((and
	 wnn-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((wnn-hiragana-key? key key-state)
	   (wnn-context-set-kana-mode! wc wnn-type-hiragana)
	   (wnn-context-set-alnum! wc #f)
	   #t)
	  ((wnn-katakana-key? key key-state)
	   (wnn-context-set-kana-mode! wc wnn-type-katakana)
	   (wnn-context-set-alnum! wc #f)
	   #t)
	  ((wnn-halfkana-key? key key-state)
	   (wnn-context-set-kana-mode! wc wnn-type-halfkana)
	   (wnn-context-set-alnum! wc #f)
	   #t)
	  ((wnn-halfwidth-alnum-key? key key-state)
	   (wnn-context-set-alnum-type! wc wnn-type-halfwidth-alnum)
	   (wnn-context-set-alnum! wc #t)
	   #t)
	  ((wnn-halfwidth-alnum-key? key key-state)
	   (wnn-context-set-alnum-type! wc wnn-type-fullwidth-alnum)
	   (wnn-context-set-alnum! wc #t)
	   #t)
	  ((wnn-kana-toggle-key? key key-state)
	   (wnn-context-kana-toggle wc)
	   (wnn-context-set-alnum! wc #f)
	   #t)
	  ((wnn-alkana-toggle-key? key key-state)
	   (wnn-context-alkana-toggle wc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(wnn-context-set-on! wc #t)
	(rk-flush (wnn-context-rkc wc))
	(wnn-context-set-state! wc #f)
	#t)
      #f))

(define (wnn-update-preedit wc)
  (if (not (wnn-context-commit-raw wc))
      (let ((segments (if (wnn-context-on wc)
			  (if (wnn-context-transposing wc)
			      (wnn-context-transposing-state-preedit wc)
			      (if (wnn-context-state wc)
				  (wnn-compose-state-preedit wc)
                                  (if (wnn-context-predicting wc)
                                      (wnn-predicting-state-preedit wc)
                                      (wnn-input-state-preedit wc))))
                          ())))
	(context-update-preedit wc segments))
      (wnn-context-set-commit-raw! wc #f)))

(define (wnn-begin-conv wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc))
        (preconv-str (wnn-make-whole-string wc #t wnn-type-hiragana)))
    (if (and wc-ctx
             (> (string-length preconv-str) 0))
	(let ((num (wnn-lib-begin-conversion wc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (wnn-context-segments wc)
		 (make-list num 0))
		(wnn-context-set-state! wc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; wnn-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define wnn-cancel-conv
  (lambda (wc)
    (wnn-reset-candidate-window wc)
    (wnn-context-set-state! wc #f)
    (ustr-clear! (wnn-context-segments wc))
    (wnn-lib-reset-conversion wc)))

(define (wnn-proc-input-state-no-preedit wc key key-state)
  (let
      ((rkc (wnn-context-rkc wc))
       (direct (ja-direct (charcode->string key)))
       (rule (wnn-context-input-rule wc)))
    (cond
     ((and wnn-use-with-vi?
           (wnn-vi-escape-key? key key-state))
      (wnn-flush wc)
      (wnn-context-set-on! wc #f)
      (wnn-commit-raw wc))

     ((wnn-off-key? key key-state)
      (wnn-flush wc)
      (wnn-context-set-on! wc #f))

     ((wnn-backspace-key? key key-state)
      (wnn-commit-raw wc))
     
     ((wnn-delete-key? key key-state)
      (wnn-commit-raw wc))

     ((and
       (wnn-hiragana-key? key key-state)
       (not
        (and
	 (= (wnn-context-kana-mode wc) wnn-type-hiragana)
	 (not (wnn-context-alnum wc)))))
      (wnn-context-change-kana-mode! wc wnn-type-hiragana)
      (wnn-context-set-alnum! wc #f))

     ((and
       (wnn-katakana-key? key key-state)
       (not
        (and
	 (= (wnn-context-kana-mode wc) wnn-type-katakana)
	 (not (wnn-context-alnum wc)))))
      (wnn-context-change-kana-mode! wc wnn-type-katakana)
      (wnn-context-set-alnum! wc #f))
     
     ((and
       (wnn-halfkana-key? key key-state)
       (not
        (and
	 (= (wnn-context-kana-mode wc) wnn-type-halfkana)
	 (not (wnn-context-alnum wc)))))
      (wnn-context-change-kana-mode! wc wnn-type-halfkana)
      (wnn-context-set-alnum! wc #f))
     
     ((and
       (wnn-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (wnn-context-alnum-type wc) wnn-type-halfwidth-alnum)
	 (wnn-context-alnum wc))))
      (wnn-context-set-alnum-type! wc wnn-type-halfwidth-alnum)
      (wnn-context-set-alnum! wc #t))
     
     ((and
       (wnn-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (wnn-context-alnum-type wc) wnn-type-fullwidth-alnum)
	 (wnn-context-alnum wc))))
      (wnn-context-set-alnum-type! wc wnn-type-fullwidth-alnum)
      (wnn-context-set-alnum! wc #t))
     
     ((and
       (not (wnn-context-alnum wc))
       (wnn-kana-toggle-key? key key-state))
      (wnn-context-kana-toggle wc))

     ((wnn-alkana-toggle-key? key key-state)
      (wnn-context-alkana-toggle wc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (wnn-commit-raw wc))
     
     ;; direct key => commit
     (direct
      (im-commit wc direct))

     ;; space key
     ((wnn-space-key? key key-state)
      (if (wnn-context-alnum wc)
	  (im-commit wc (list-ref
			 ja-alnum-space
			 (- (wnn-context-alnum-type wc)
			    wnn-type-halfwidth-alnum)))
	  (im-commit wc (list-ref ja-space (wnn-context-kana-mode wc)))))

     ((symbol? key)
      (wnn-commit-raw wc))

     (else
      (if (wnn-context-alnum wc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (wnn-context-preconv-ustr wc)
			       (if (= (wnn-context-alnum-type wc)
				      wnn-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (wnn-context-raw-ustr wc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule wnn-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (wnn-context-preconv-ustr wc) res)
                    (ustr-insert-elem! (wnn-context-preconv-ustr wc) res))
	          (ustr-insert-elem! (wnn-context-raw-ustr wc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (wnn-commit-raw wc)))))))))

(define (wnn-has-preedit? wc)
  (or (not (ustr-empty? (wnn-context-preconv-ustr wc)))
      (> (string-length (rk-pending (wnn-context-rkc wc))) 0)))

(define wnn-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type wnn-type-halfwidth-alnum)
       (= state wnn-type-halfwidth-alnum))
      wnn-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type wnn-type-fullwidth-alnum)
       (= state wnn-type-fullwidth-alnum))
      wnn-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define wnn-proc-transposing-state
  (lambda (wc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (wnn-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons wnn-type-fullwidth-alnum rotate-list)))
      (if (wnn-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons wnn-type-halfwidth-alnum rotate-list)))
      (if (wnn-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons wnn-type-halfkana rotate-list)))
      (if (wnn-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons wnn-type-katakana rotate-list)))
      (if (wnn-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons wnn-type-hiragana rotate-list)))

      (if (wnn-context-transposing wc)
	  (let ((lst (member (wnn-context-transposing-type wc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (wnn-rotate-transposing-alnum-type
				 (wnn-context-transposing-type wc)
				 (car rotate-list))))))
	  (begin
	    (wnn-context-set-transposing! wc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state wnn-type-hiragana)
	      (= state wnn-type-katakana)
	      (= state wnn-type-halfkana)))
	(wnn-context-set-transposing-type! wc state))
       ((and state
	     (or
	      (= state wnn-type-halfwidth-alnum)
	      (= state wnn-candidate-type-upper-halfwidth-alnum)
	      (= state wnn-type-fullwidth-alnum)
	      (= state wnn-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (wnn-context-input-rule wc) wnn-input-rule-kana))
	    (wnn-context-set-transposing-type! wc state)))
       (else
	(and
	 ; commit
	 (if (wnn-commit-key? key key-state)
	     (begin
	       (im-commit wc (wnn-transposing-text wc))
	       (wnn-flush wc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (wnn-begin-conv-key? key key-state)
	     (begin
	       (wnn-context-set-transposing! wc #f)
	       (wnn-begin-conv wc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (wnn-cancel-key? key key-state)
	      (wnn-backspace-key? key key-state))
	     (begin
	       (wnn-context-set-transposing! wc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (wnn-prev-page-key? key key-state)
	      (wnn-next-page-key? key key-state)
	      (wnn-extend-segment-key? key key-state)
	      (wnn-shrink-segment-key? key key-state)
	      (wnn-next-segment-key? key key-state)
	      (wnn-beginning-of-preedit-key? key key-state)
	      (wnn-end-of-preedit-key? key key-state)
	      (wnn-next-candidate-key? key key-state)
	      (wnn-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit wc (wnn-transposing-text wc))
	   (wnn-flush wc)
	   (wnn-proc-input-state wc key key-state))))))))

(define (wnn-move-prediction wc offset)
  (let* ((nr (wnn-lib-get-nr-predictions wc))
         (idx (wnn-context-prediction-index wc))
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
    (im-select-candidate wc compensated-n)
    (wnn-context-set-prediction-index! wc compensated-n)))

(define (wnn-move-prediction-in-page wc numeralc)
  (let* ((nr (wnn-lib-get-nr-predictions wc))
         (p-idx (wnn-context-prediction-index wc))
         (n (if (not p-idx)
                0
                p-idx))
         (cur-page (if (= wnn-nr-candidate-max 0)
                       0
                       (quotient n wnn-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page wnn-nr-candidate-max) compensated-pageidx))
         (compensated-idx (cond
                           ((>= idx nr)
                            #f)
                           (else
                            idx)))
         (selected-pageidx (if (not p-idx)
                               #f
                               (if (= wnn-nr-candidate-max 0)
                                   p-idx
                                   (remainder p-idx
                                              wnn-nr-candidate-max)))))
    (if (and
         compensated-idx
         (not (eqv? compensated-pageidx selected-pageidx)))
        (begin
          (wnn-context-set-prediction-index! wc compensated-idx)
          (im-select-candidate wc compensated-idx)
          #t)
       #f)))

(define (wnn-prediction-select-non-existing-index? wc numeralc)
  (let* ((nr (wnn-lib-get-nr-predictions wc))
         (p-idx (wnn-context-prediction-index wc))
         (cur-page (if (= wnn-nr-candidate-max 0)
                       0
                       (quotient p-idx wnn-nr-candidate-max)))
         (pageidx (- (numeric-ichar->integer numeralc) 1))
         (compensated-pageidx (cond
                               ((< pageidx 0) ; pressing key_0
                                (+ pageidx 10))
                               (else
                                pageidx)))
         (idx (+ (* cur-page wnn-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (wnn-prediction-keys-handled? wc key key-state)
  (cond
   ((wnn-next-prediction-key? key key-state)
    (wnn-move-prediction wc 1)
    #t)
   ((wnn-prev-prediction-key? key key-state)
    (wnn-move-prediction wc -1)
    #t)
   ((and
     wnn-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (wnn-move-prediction-in-page wc key))
   ((and
     (wnn-context-prediction-index wc)
     (wnn-prev-page-key? key key-state))
    (im-shift-page-candidate wc #f)
    #t)
   ((and
     (wnn-context-prediction-index wc)
     (wnn-next-page-key? key key-state))
    (im-shift-page-candidate wc #t)
    #t)
   (else
    #f)))

(define (wnn-proc-prediction-state wc key key-state)
  (cond
   ;; prediction index change
   ((wnn-prediction-keys-handled? wc key key-state))

   ;; cancel
   ((wnn-cancel-key? key key-state)
    (if (wnn-context-prediction-index wc)
        (wnn-reset-prediction-window wc)
        (begin
          (wnn-reset-prediction-window wc)
          (wnn-proc-input-state wc key key-state))))

   ;; commit
   ((and
     (wnn-context-prediction-index wc)
     (wnn-commit-key? key key-state))
    (wnn-do-commit-prediction wc))
   (else
    (if (and
         wnn-use-implicit-commit-prediction?
         (wnn-context-prediction-index wc))
        (cond
         ((or
           ;; check keys used in wnn-proc-input-state-with-preedit
           (wnn-begin-conv-key? key key-state)
           (wnn-backspace-key? key key-state)
           (wnn-delete-key? key key-state)
           (wnn-kill-key? key key-state)
           (wnn-kill-backward-key? key key-state)
           (and
            (not (wnn-context-alnum wc))
            (wnn-commit-as-opposite-kana-key? key key-state))
           (wnn-transpose-as-hiragana-key? key key-state)
           (wnn-transpose-as-katakana-key? key key-state)
           (wnn-transpose-as-halfkana-key? key key-state)
           (and
            (not (= (wnn-context-input-rule wc) wnn-input-rule-kana))
            (or
             (wnn-transpose-as-halfwidth-alnum-key? key key-state)
             (wnn-transpose-as-fullwidth-alnum-key? key key-state)))
           (wnn-hiragana-key? key key-state)
           (wnn-katakana-key? key key-state)
           (wnn-halfkana-key? key key-state)
           (wnn-halfwidth-alnum-key? key key-state)
           (wnn-fullwidth-alnum-key? key key-state)
           (and
            (not (wnn-context-alnum wc))
            (wnn-kana-toggle-key? key key-state))
           (wnn-alkana-toggle-key? key key-state)
           (wnn-go-left-key? key key-state)
           (wnn-go-right-key? key key-state)
           (wnn-beginning-of-preedit-key? key key-state)
           (wnn-end-of-preedit-key? key key-state)
           (and
            (modifier-key-mask key-state)
            (not (shift-key-mask key-state))))
          ;; go back to unselected prediction
          (wnn-reset-prediction-window wc)
          (wnn-check-prediction wc #f))
         ((and
           (ichar-numeric? key)
           wnn-select-prediction-by-numeral-key?
           (not (wnn-prediction-select-non-existing-index? wc key)))
          (wnn-context-set-predicting! wc #f)
          (wnn-context-set-prediction-index! wc #f)
          (wnn-proc-input-state wc key key-state))
         (else
          ;; implicit commit
          (wnn-do-commit-prediction wc)
          (wnn-proc-input-state wc key key-state)))
        (begin
          (wnn-context-set-predicting! wc #f)
          (wnn-context-set-prediction-index! wc #f)
          (wnn-proc-input-state wc key key-state))))))

(define (wnn-proc-input-state-with-preedit wc key key-state)
  (let ((preconv-str (wnn-context-preconv-ustr wc))
	(raw-str (wnn-context-raw-ustr wc))
	(rkc (wnn-context-rkc wc))
	(rule (wnn-context-input-rule wc))
	(kana (wnn-context-kana-mode wc)))
    (cond
     ;; begin conversion
     ((wnn-begin-conv-key? key key-state)
      (wnn-reset-prediction-window wc)
      (wnn-begin-conv wc))

     ;; prediction
     ((wnn-next-prediction-key? key key-state)
      (wnn-check-prediction wc #t))

     ;; backspace
     ((wnn-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (wnn-context-input-rule wc) wnn-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((wnn-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((wnn-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((wnn-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (wnn-context-alnum wc))
       (wnn-commit-as-opposite-kana-key? key key-state))
      (im-commit wc (wnn-make-whole-string wc #t (ja-opposite-kana kana)))
      (wnn-flush wc))

     ;; Transposing状態へ移行
     ((or (wnn-transpose-as-hiragana-key? key key-state)
	  (wnn-transpose-as-katakana-key? key key-state)
	  (wnn-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (wnn-context-input-rule wc) wnn-input-rule-kana))
	   (or
	    (wnn-transpose-as-halfwidth-alnum-key? key key-state)
	    (wnn-transpose-as-fullwidth-alnum-key? key key-state))))
      (wnn-reset-prediction-window wc)
      (wnn-proc-transposing-state wc key key-state))

     ((wnn-hiragana-key? key key-state)
      (if (not (= kana wnn-type-hiragana))
	  (begin
	    (im-commit wc (wnn-make-whole-string wc #t kana))
	    (wnn-flush wc)))
      (wnn-context-set-kana-mode! wc wnn-type-hiragana)
      (wnn-context-set-alnum! wc #f))

     ((wnn-katakana-key? key key-state)
      (if (not (= kana wnn-type-katakana))
	  (begin
	    (im-commit wc (wnn-make-whole-string wc #t kana))
	    (wnn-flush wc)))
      (wnn-context-set-kana-mode! wc wnn-type-katakana)
      (wnn-context-set-alnum! wc #f))

     ((wnn-halfkana-key? key key-state)
      (if (not (= kana wnn-type-halfkana))
	  (begin
	    (im-commit wc (wnn-make-whole-string wc #t kana))
	    (wnn-flush wc)))
      (wnn-context-set-kana-mode! wc wnn-type-halfkana)
      (wnn-context-set-alnum! wc #f))

     ((and
       (wnn-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (wnn-context-alnum-type wc) wnn-type-halfwidth-alnum)
	 (wnn-context-alnum wc))))
      (wnn-context-set-alnum-type! wc wnn-type-halfwidth-alnum)
      (wnn-context-set-alnum! wc #t))

     ((and
       (wnn-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (wnn-context-alnum-type wc) wnn-type-fullwidth-alnum)
	 (wnn-context-alnum wc))))
      (wnn-context-set-alnum-type! wc wnn-type-fullwidth-alnum)
      (wnn-context-set-alnum! wc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (wnn-context-alnum wc))
       (wnn-kana-toggle-key? key key-state))
      (im-commit wc (wnn-make-whole-string wc #t kana))
      (wnn-flush wc)
      (wnn-context-kana-toggle wc))

     ((wnn-alkana-toggle-key? key key-state)
      (wnn-context-alkana-toggle wc))

     ;; cancel
     ((wnn-cancel-key? key key-state)
      (wnn-flush wc))

     ;; commit
     ((wnn-commit-key? key key-state)
      (begin
	(im-commit
	 wc
	 (wnn-make-whole-string wc #t kana))
	(wnn-flush wc)))

     ;; left
     ((wnn-go-left-key? key key-state)
      (wnn-context-confirm-kana! wc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((wnn-go-right-key? key key-state)
      (wnn-context-confirm-kana! wc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((wnn-beginning-of-preedit-key? key key-state)
      (wnn-context-confirm-kana! wc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((wnn-end-of-preedit-key? key key-state)
      (wnn-context-confirm-kana! wc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (if (wnn-context-alnum wc)
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
			       (if (= (wnn-context-alnum-type wc)
				      wnn-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule wnn-input-rule-kana)
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
		           (string-append pend key-str))))))))))))

(define wnn-context-confirm-kana!
  (lambda (wc)
    (if (= (wnn-context-input-rule wc)
	   wnn-input-rule-kana)
	(let* ((preconv-str (wnn-context-preconv-ustr wc))
	       (rkc (wnn-context-rkc wc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (wnn-reset-prediction-window wc)
  (if (wnn-context-prediction-window wc)
      (im-deactivate-candidate-selector wc))
  (wnn-context-set-predicting! wc #f)
  (wnn-context-set-prediction-window! wc #f)
  (wnn-context-set-prediction-index! wc #f))

(define (wnn-check-prediction wc force-check?)
  (if (and
       (not (wnn-context-state wc))
       (not (wnn-context-transposing wc))
       (not (wnn-context-predicting wc)))
      (let* ((use-pending-rk-for-prediction? #t)
	     (preconv-str
	      (wnn-make-whole-string
	       wc
	       (not use-pending-rk-for-prediction?)
	       (wnn-context-kana-mode wc)))
	     (preedit-len (+
			   (ustr-length (wnn-context-preconv-ustr wc))
			   (if (not use-pending-rk-for-prediction?)
			       0
			       (string-length (rk-pending
					       (wnn-context-rkc
						wc)))))))
	(if (or
	     (>= preedit-len wnn-prediction-start-char-count)
	     force-check?)
	    (begin
	      (wnn-lib-set-prediction-src-string wc preconv-str)
	      (let ((nr (wnn-lib-get-nr-predictions wc)))
		(if (and
		     nr
		     (> nr 0))
		    (begin
		      (im-activate-candidate-selector
		       wc nr wnn-nr-candidate-max)
		      (wnn-context-set-prediction-window! wc #t)
		      (wnn-context-set-predicting! wc #t))
		    (wnn-reset-prediction-window wc))))
	    (wnn-reset-prediction-window wc)))))

(define (wnn-proc-input-state wc key key-state)
  (if (wnn-has-preedit? wc)
      (wnn-proc-input-state-with-preedit wc key key-state)
      (wnn-proc-input-state-no-preedit wc key key-state))
  (if wnn-use-prediction?
      (wnn-check-prediction wc #f)))

(define wnn-separator
  (lambda (wc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if wnn-show-segment-separator?
	  (cons attr wnn-segment-separator)
	  #f))))

(define wnn-context-transposing-state-preedit
  (lambda (wc)
    (let ((transposing-text (wnn-transposing-text wc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define wnn-transposing-text
  (lambda (wc)
    (let ((transposing-type (wnn-context-transposing-type wc)))
      (cond
       ((or
	 (= transposing-type wnn-type-hiragana)
	 (= transposing-type wnn-type-katakana)
	 (= transposing-type wnn-type-halfkana))
	(wnn-make-whole-string wc #t transposing-type))
       ((= transposing-type wnn-type-halfwidth-alnum)
	(wnn-make-whole-raw-string wc #f #f))
       ((= transposing-type wnn-candidate-type-upper-halfwidth-alnum)
	(wnn-make-whole-raw-string wc #f #t))
       ((= transposing-type wnn-type-fullwidth-alnum)
	(wnn-make-whole-raw-string wc #t #f))
       ((= transposing-type wnn-candidate-type-upper-fullwidth-alnum)
	(wnn-make-whole-raw-string wc #t #t))))))

(define wnn-get-raw-str-seq
  (lambda (wc)
    (let* ((rkc (wnn-context-rkc wc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (wnn-context-raw-ustr wc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
	      right-str))))

(define wnn-get-raw-candidate
  (lambda (wc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (wnn-make-whole-string wc #t wnn-type-hiragana))))
	   (unconv-candidate (wnn-lib-get-unconv-candidate wc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (wnn-get-raw-str-seq wc))))
      (cond
       ((= cand-idx wnn-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx wnn-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) wnn-type-katakana))
       ((= cand-idx wnn-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) wnn-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (wnn-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx wnn-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       wnn-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx wnn-candidate-type-halfwidth-alnum)
			    (= cand-idx wnn-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (wnn-predicting-state-preedit wc)
  (if (or
       (not wnn-use-implicit-commit-prediction?)
       (not (wnn-context-prediction-index wc)))
      (wnn-input-state-preedit wc)
      (let ((cand (wnn-get-prediction-string wc)))
        (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (wnn-compose-state-preedit wc)
  (let* ((segments (wnn-context-segments wc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (wnn-separator wc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx wnn-candidate-type-katakana)
			(wnn-lib-get-nth-candidate wc seg-idx cand-idx)
			(wnn-get-raw-candidate wc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (wnn-input-state-preedit wc)
  (let* ((preconv-str (wnn-context-preconv-ustr wc))
	 (rkc (wnn-context-rkc wc))
	 (pending (rk-pending rkc))
	 (kana (wnn-context-kana-mode wc))
	 (rule (wnn-context-input-rule wc))
	 (extract-kana
	  (if (= rule wnn-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (wnn-has-preedit? wc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (wnn-get-commit-string wc)
  (let ((segments (wnn-context-segments wc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx wnn-candidate-type-katakana)
			     (wnn-lib-get-nth-candidate
			      wc seg-idx cand-idx)
			     (wnn-get-raw-candidate
			      wc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (wnn-commit-string wc)
  (let ((wc-ctx (wnn-context-wc-ctx wc))
        (segments (wnn-context-segments wc)))
    (if wc-ctx
	(begin
	  (for-each (lambda (seg-idx cand-idx)
		      (if (> cand-idx wnn-candidate-type-katakana)
			  (wnn-lib-commit-segment wc seg-idx cand-idx)))
		    (iota (ustr-length segments))
		    (ustr-whole-seq segments))
	  (if (every (lambda (x) (<= x wnn-candidate-type-katakana))
		     (ustr-whole-seq segments))
	      (wnn-lib-reset-conversion wc))))))

(define (wnn-do-commit wc)
    (im-commit wc (wnn-get-commit-string wc))
    (wnn-commit-string wc)
    (wnn-reset-candidate-window wc)
    (wnn-flush wc))

(define (wnn-get-prediction-string wc)
  (wnn-lib-get-nth-prediction
   wc
   (wnn-context-prediction-index wc)))

(define (wnn-learn-prediction-string wc)
  (wnn-lib-commit-nth-prediction
   wc
   (wnn-context-prediction-index wc)))

(define (wnn-do-commit-prediction wc)
  (im-commit wc (wnn-get-prediction-string wc))
  (wnn-learn-prediction-string wc)
  (wnn-reset-prediction-window wc)
  (wnn-flush wc))

(define wnn-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (wnn-move-segment wc dir)
  (wnn-reset-candidate-window wc)
  (let ((segments (wnn-context-segments wc)))
    (ustr-cursor-move! segments dir)
    (wnn-correct-segment-cursor segments)))

(define (wnn-resize-segment wc cnt)
  (let* ((segments (wnn-context-segments wc))
	 (cur-seg (ustr-cursor-pos segments)))
    (wnn-reset-candidate-window wc)
    (wnn-lib-resize-segment wc cur-seg cnt)
    (let* ((resized-nseg (wnn-lib-get-nr-segments wc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (wnn-move-candidate wc offset)
  (let* ((segments (wnn-context-segments wc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (wnn-lib-get-nr-candidates wc cur-seg))
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
	 (new-op-count (+ 1 (wnn-context-candidate-op-count wc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (wnn-context-set-candidate-op-count! wc new-op-count)
    (if (and
	 (= (wnn-context-candidate-op-count wc)
	    wnn-candidate-op-count)
	 wnn-use-candidate-window?)
	(begin
	  (wnn-context-set-candidate-window! wc #t)
	  (im-activate-candidate-selector wc max wnn-nr-candidate-max)))
    (if (wnn-context-candidate-window wc)
	(im-select-candidate wc compensated-n))))

(define wnn-move-candidate-in-page
  (lambda (wc numeralc)
    (let* ((segments (wnn-context-segments wc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (wnn-lib-get-nr-candidates wc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= wnn-nr-candidate-max 0)
			 0
			 (quotient n wnn-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page wnn-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (wnn-context-candidate-op-count wc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (wnn-context-set-candidate-op-count! wc new-op-count)
      (im-select-candidate wc compensated-idx))))

(define (wnn-reset-candidate-window wc)
  (if (wnn-context-candidate-window wc)
      (begin
	(im-deactivate-candidate-selector wc)
	(wnn-context-set-candidate-window! wc #f)))
  (wnn-context-set-candidate-op-count! wc 0))

(define wnn-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx wnn-candidate-type-halfwidth-alnum)
       (= state wnn-candidate-type-halfwidth-alnum))
      wnn-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx wnn-candidate-type-fullwidth-alnum)
       (= state wnn-candidate-type-fullwidth-alnum))
      wnn-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define wnn-set-segment-transposing
  (lambda (wc key key-state)
    (let ((segments (wnn-context-segments wc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(wnn-reset-candidate-window wc)
	(wnn-context-set-candidate-op-count! wc 0)

	(if (wnn-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons wnn-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (wnn-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons wnn-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (wnn-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons wnn-candidate-type-halfkana
				    rotate-list)))
	(if (wnn-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons wnn-candidate-type-katakana
				    rotate-list)))
	(if (wnn-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons wnn-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx wnn-candidate-type-hiragana)
	     (= idx wnn-candidate-type-katakana)
	     (= idx wnn-candidate-type-halfkana)
	     (= idx wnn-candidate-type-halfwidth-alnum)
	     (= idx wnn-candidate-type-fullwidth-alnum)
	     (= idx wnn-candidate-type-upper-halfwidth-alnum)
	     (= idx wnn-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (wnn-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (wnn-proc-compose-state wc key key-state)
  (cond
   ((wnn-prev-page-key? key key-state)
    (if (wnn-context-candidate-window wc)
        (im-shift-page-candidate wc #f)))

   ((wnn-next-page-key? key key-state)
    (if (wnn-context-candidate-window wc)
        (im-shift-page-candidate wc #t)))

   ((wnn-commit-key? key key-state)
    (wnn-do-commit wc))

   ((wnn-extend-segment-key? key key-state)
    (wnn-resize-segment wc 1))

   ((wnn-shrink-segment-key? key key-state)
    (wnn-resize-segment wc -1))

   ((wnn-next-segment-key? key key-state)
    (wnn-move-segment wc 1))

   ((wnn-prev-segment-key? key key-state)
    (wnn-move-segment wc -1))

   ((wnn-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (wnn-context-segments wc))
      (wnn-reset-candidate-window wc)))

   ((wnn-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (wnn-context-segments wc))
      (wnn-correct-segment-cursor (wnn-context-segments wc))
      (wnn-reset-candidate-window wc)))

   ((wnn-backspace-key? key key-state)
    (wnn-cancel-conv wc))

   ((wnn-next-candidate-key? key key-state)
    (wnn-move-candidate wc 1))

   ((wnn-prev-candidate-key? key key-state)
    (wnn-move-candidate wc -1))

   ((or (wnn-transpose-as-hiragana-key? key key-state)
        (wnn-transpose-as-katakana-key? key key-state)
        (wnn-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (wnn-context-input-rule wc) wnn-input-rule-kana))
         (or
          (wnn-transpose-as-halfwidth-alnum-key? key key-state)
          (wnn-transpose-as-fullwidth-alnum-key? key key-state))))
    (wnn-set-segment-transposing wc key key-state))

   ((wnn-cancel-key? key key-state)
    (wnn-cancel-conv wc))

   ((and wnn-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (wnn-context-candidate-window wc))
    (wnn-move-candidate-in-page wc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (wnn-do-commit wc)
      (wnn-proc-input-state wc key key-state)))))

(define (wnn-press-key-handler wc key key-state)
  (if (ichar-control? key)
      (im-commit-raw wc)
      (if (wnn-context-on wc)
          (if (wnn-context-transposing wc)
              (wnn-proc-transposing-state wc key key-state)
              (if (wnn-context-state wc)
                  (wnn-proc-compose-state wc key key-state)
                  (if (wnn-context-predicting wc)
                      (wnn-proc-prediction-state wc key key-state)
                      (wnn-proc-input-state wc key key-state))))
	  (wnn-proc-raw-state wc key key-state)))
  (wnn-update-preedit wc))

;;;
(define (wnn-release-key-handler wc key key-state)
  (if (or (ichar-control? key)
	  (not (wnn-context-on wc)))
      (wnn-commit-raw wc)))
;;;
(define (wnn-reset-handler wc)
  (if (wnn-context-on wc)
      (begin
	(if (wnn-context-state wc)
	    (wnn-lib-reset-conversion wc))
	(wnn-flush wc))))

;;;
(define (wnn-get-candidate-handler wc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (wnn-context-segments wc)))
         (cand (if (wnn-context-state wc)
                   (wnn-lib-get-nth-candidate wc cur-seg idx)
                   (wnn-lib-get-nth-prediction wc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (wnn-set-candidate-index-handler wc idx)
  (cond
   ((wnn-context-state wc)
    (ustr-cursor-set-frontside! (wnn-context-segments wc) idx)
    (wnn-update-preedit wc))
   ((wnn-context-predicting wc)
    (wnn-context-set-prediction-index! wc idx)
    (wnn-update-preedit wc))))

(define (wnn-proc-raw-state wc key key-state)
  (if (not (wnn-begin-input wc key key-state))
      (im-commit-raw wc)))

(wnn-configure-widgets)
(register-im
 'wnn
 "ja"
 "EUC-JP"
 wnn-im-name-label
 wnn-im-short-desc
 #f
 wnn-init-handler
 wnn-release-handler
 context-mode-handler
 wnn-press-key-handler
 wnn-release-key-handler
 wnn-reset-handler
 wnn-get-candidate-handler
 wnn-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
