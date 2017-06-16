;;; google-cgiapi-jp.scm: google-cgiapi-jp for uim.
;;;
;;; Copyright (c) 2011-2013 uim Project https://github.com/uim/uim
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

(require-extension (srfi 1 2 6 23 34 48))

(require "ustr.scm")
(require "japanese.scm")
(require "http-client.scm")
(require "json.scm")
(require "generic-predict.scm")
(require "util.scm")
(require-custom "generic-key-custom.scm")
(require-custom "google-cgiapi-jp-custom.scm")
(require-custom "google-cgiapi-jp-key-custom.scm")

;;; implementations

;;
;; canna emulating functions
;;

(define google-cgiapi-jp-internal-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'yomi-seg    '())
    (list 'candidates  '())
    (list 'seg-cnts '())
    (list 'prediction-word '())
    (list 'prediction-candidates '())
    (list 'prediction-appendix '())
    (list 'prediction-nr '()))))
(define-record 'google-cgiapi-jp-internal-context google-cgiapi-jp-internal-context-rec-spec)
(define google-cgiapi-jp-internal-context-new-internal google-cgiapi-jp-internal-context-new)

(define (google-cgiapi-jp-conversion str opts)
  (define (fromconv str)
    (iconv-convert "UTF-8" "EUC-JP" str))
  (define (toconv str)
    (iconv-convert "EUC-JP" "UTF-8" str))
  (define (make-query)
    (format "/transliterate?langpair=ja-Hira|ja&text=~a~a"
            (http:encode-uri-string (fromconv str)) opts))
  (define (parse str)
    (receive (cars cdrs)
        (unzip2 (call-with-input-string
                 str
                 (lambda (port)
                   (json-read port))))
      (cons (map toconv cars)
            (map (lambda (x) (map toconv x)) cdrs))))
  (let* ((proxy (make-http-proxy-from-custom))
         (ssl (and google-cgiapi-jp-use-ssl?
                   (make-http-ssl (SSLv3-client-method) 443)))
         (ret (http:get google-cgiapi-jp-server (make-query) 80 proxy ssl)))
    (parse ret)))

(define (google-cgiapi-jp-predict ggc str)
  (predict-meta-search
   (google-cgiapi-context-prediction-ctx ggc)
   str))

(define (google-cgiapi-jp-conversion-make-resize-query yomi-seg)
  (let ((len (length yomi-seg)))
    (apply string-append (map (lambda (idx)
                                (if (= (+ idx 1) len)
                                    (list-ref yomi-seg idx)
                                    (string-append (list-ref yomi-seg idx) ",")))
                              (iota len)))))
(define (google-cgiapi-jp-conversion-resize yomi-seg)
  (google-cgiapi-jp-conversion
   (google-cgiapi-jp-conversion-make-resize-query yomi-seg) ""))

(define (google-cgiapi-jp-lib-init)
  #t)
(define (google-cgiapi-jp-lib-alloc-context)
  (google-cgiapi-jp-internal-context-new-internal))
(define (google-cgiapi-jp-lib-get-nth-candidate ggc seg nth)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cand (google-cgiapi-jp-internal-context-candidates ggx-ctx)))
    (list-ref (list-ref cand seg) nth)))
(define (google-cgiapi-jp-lib-release-context ggc)
  #t)
(define (google-cgiapi-jp-lib-get-unconv-candidate ggc seg-idx)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cand (google-cgiapi-jp-internal-context-candidates ggx-ctx)))
    ;; XXX
    (car (take-right (list-ref cand seg-idx) 1))))
(define (google-cgiapi-jp-lib-get-nr-segments ggc)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cand (google-cgiapi-jp-internal-context-candidates ggx-ctx)))
    (length cand)))
(define (google-cgiapi-jp-lib-get-nr-candidates ggc seg)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cand (google-cgiapi-jp-internal-context-candidates ggx-ctx)))
    (length (list-ref cand seg))))
(define (google-cgiapi-jp-next-yomi-seg yomi-seg seg cnt)
  (let* ((kana-str (list-ref yomi-seg seg))
         (kana-list (reverse (string-to-list kana-str))))
    (cond ((and (< cnt 0) ;; shrink segment
                (< 1 (length kana-list)))
           (let* ((not-edited-head (if (< 0 seg)
                                       (take yomi-seg seg)
                                       '()))
                  (edited-head (list (apply string-append (drop-right kana-list (* -1 cnt)))))
                  (edited-tail (if (= (+ 1 seg) (length yomi-seg)) ;; end of segments
                                   (take-right kana-list (* -1 cnt))
                                   (let* ((next-char (car (take-right kana-list (* -1 cnt))))
                                          (kana-next-str (list-ref yomi-seg (+ 1 seg))))
                                     (list (string-append next-char kana-next-str)))))
                  (not-edited-tail (if (= (+ 1 seg) (length yomi-seg))
                                       '()
                                       (drop yomi-seg (+ seg 2)))))
             (append not-edited-head edited-head edited-tail not-edited-tail)))
          ((and (< 0 cnt) ;; stretch segment
                (< (+ seg 1) (length yomi-seg))
                (< 0 (length (string-to-list (list-ref yomi-seg (+ seg 1))))))
           (let* ((next-str (list-ref yomi-seg (+ seg 1)))
                  (next-kana-list (reverse (string-to-list next-str)))
                  (not-edited-head (if (< 0 seg)
                                       (take yomi-seg seg)
                                       '()))
                  (edited-head (list (apply string-append
                                            (append kana-list
                                                    (take next-kana-list cnt)))))
                  (edited-tail (if (= 1 (length next-kana-list))
                                   '()
                                   (list (apply string-append (drop next-kana-list cnt)))))
                  (not-edited-tail (if (< (length yomi-seg) 2)
                                       '()
                                       (drop yomi-seg (+ 2 seg)))))
             (append not-edited-head edited-head edited-tail not-edited-tail)))
          (else
           yomi-seg))))
(define (google-cgiapi-jp-lib-resize-segment ggc seg cnt)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cand (google-cgiapi-jp-internal-context-candidates ggx-ctx))
         (yomi-seg (google-cgiapi-jp-internal-context-yomi-seg ggx-ctx))
         (next-yomi-seg (google-cgiapi-jp-next-yomi-seg yomi-seg seg cnt))
         (replace-yomi-seg-and-next-cand (google-cgiapi-jp-conversion-resize next-yomi-seg))
         (replace-yomi-seg (car replace-yomi-seg-and-next-cand))
         (next-cand        (cdr replace-yomi-seg-and-next-cand)))
    (if (and next-cand
             (not (equal? next-cand cand)))
        (begin
          (google-cgiapi-jp-internal-context-set-candidates! ggx-ctx next-cand)
          (google-cgiapi-jp-internal-context-set-yomi-seg! ggx-ctx replace-yomi-seg)))
    #t))
(define (google-cgiapi-jp-lib-begin-conversion ggc str)
  (let* ((yomi-seg-and-cand (google-cgiapi-jp-conversion str ""))
         (yomi-seg (car yomi-seg-and-cand))
         (cand (cdr yomi-seg-and-cand))
         (ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc)))
    (google-cgiapi-jp-internal-context-set-yomi-seg! ggx-ctx yomi-seg)
    (google-cgiapi-jp-internal-context-set-candidates! ggx-ctx cand)
    (length cand)))
(define (google-cgiapi-jp-lib-commit-segments ggc delta)
  #t)
(define (google-cgiapi-jp-lib-reset-conversion ggc)
  #f)
(define (google-cgiapi-jp-lib-set-prediction-src-string ggc str)
  (let* ((ret (predict-meta-search
               (google-cgiapi-jp-context-prediction-ctx ggc)
               str))
         (ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (word     (predict-meta-word? ret))
         (cands    (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret)))
    (google-cgiapi-jp-internal-context-set-prediction-word! ggx-ctx word)
    (google-cgiapi-jp-internal-context-set-prediction-candidates! ggx-ctx cands)
    (google-cgiapi-jp-internal-context-set-prediction-appendix! ggx-ctx appendix)
    (google-cgiapi-jp-internal-context-set-prediction-nr! ggx-ctx (length cands))
    #f))
(define (google-cgiapi-jp-lib-get-nr-predictions ggc)
  (let ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc)))
    (google-cgiapi-jp-internal-context-prediction-nr ggx-ctx)))
(define (google-cgiapi-jp-lib-get-nth-word ggc nth)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (word (google-cgiapi-jp-internal-context-prediction-word ggx-ctx)))
    (list-ref word nth)))
(define (google-cgiapi-jp-lib-get-nth-prediction ggc nth)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (cands (google-cgiapi-jp-internal-context-prediction-candidates ggx-ctx)))
    (list-ref cands nth)))
(define (google-cgiapi-jp-lib-get-nth-appendix ggc nth)
  (let* ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
         (appendix (google-cgiapi-jp-internal-context-prediction-appendix ggx-ctx)))
    (list-ref appendix nth)))
(define (google-cgiapi-jp-lib-commit-nth-prediction ggc nth)
  (let ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc)))
    (predict-meta-commit
     (google-cgiapi-jp-context-prediction-ctx ggc)
     (google-cgiapi-jp-lib-get-nth-word ggc nth)
     (google-cgiapi-jp-lib-get-nth-prediction ggc nth)
     (google-cgiapi-jp-lib-get-nth-appendix ggc nth))
    #f))

(define google-cgiapi-jp-init-lib-ok? #f)

(define google-cgiapi-jp-type-direct	   ja-type-direct)
(define google-cgiapi-jp-type-hiragana	   ja-type-hiragana)
(define google-cgiapi-jp-type-katakana	   ja-type-katakana)
(define google-cgiapi-jp-type-halfkana	   ja-type-halfkana)
(define google-cgiapi-jp-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define google-cgiapi-jp-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define google-cgiapi-jp-input-rule-roma 0)
(define google-cgiapi-jp-input-rule-kana 1)
(define google-cgiapi-jp-input-rule-azik 2)
(define google-cgiapi-jp-input-rule-act 3)
(define google-cgiapi-jp-input-rule-kzik 4)

(define google-cgiapi-jp-candidate-type-katakana -2)
(define google-cgiapi-jp-candidate-type-hiragana -3)
(define google-cgiapi-jp-candidate-type-halfkana -4)
(define google-cgiapi-jp-candidate-type-halfwidth-alnum -5)
(define google-cgiapi-jp-candidate-type-fullwidth-alnum -6)
(define google-cgiapi-jp-candidate-type-upper-halfwidth-alnum -7)
(define google-cgiapi-jp-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key google-cgiapi-jp-space-key? '(" "))

(define google-cgiapi-jp-prepare-input-rule-activation
  (lambda (ggc)
    (cond
     ((google-cgiapi-jp-context-state ggc)
      (google-cgiapi-jp-do-commit ggc))
     ((google-cgiapi-jp-context-transposing ggc)
      (im-commit ggc (google-cgiapi-jp-transposing-text ggc)))
     ((and
       (google-cgiapi-jp-context-on ggc)
       (google-cgiapi-jp-has-preedit? ggc))
      (im-commit
       ggc (google-cgiapi-jp-make-whole-string ggc #t (google-cgiapi-jp-context-kana-mode ggc)))))
    (google-cgiapi-jp-flush ggc)
    (google-cgiapi-jp-update-preedit ggc)))

(define google-cgiapi-jp-prepare-input-mode-activation
  (lambda (ggc new-mode)
    (let ((old-kana (google-cgiapi-jp-context-kana-mode ggc)))
      (cond
       ((google-cgiapi-jp-context-state ggc)
	(google-cgiapi-jp-do-commit ggc))
       ((google-cgiapi-jp-context-transposing ggc)
	(im-commit ggc (google-cgiapi-jp-transposing-text ggc))
	(google-cgiapi-jp-flush ggc))
       ((and
	 (google-cgiapi-jp-context-on ggc)
	 (google-cgiapi-jp-has-preedit? ggc)
	 (not (= old-kana new-mode)))
	(im-commit
	 ggc (google-cgiapi-jp-make-whole-string ggc #t (google-cgiapi-jp-context-kana-mode ggc)))
	(google-cgiapi-jp-flush ggc)))
      (google-cgiapi-jp-update-preedit ggc))))

(register-action 'action_google-cgiapi-jp_hiragana
		 (lambda (ggc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (ggc) ;; activity predicate
		   (and (google-cgiapi-jp-context-on ggc)
		        (not (google-cgiapi-jp-context-alnum ggc))
			(= (google-cgiapi-jp-context-kana-mode ggc)
			   google-cgiapi-jp-type-hiragana)))

		 (lambda (ggc) ;; action handler
		   (google-cgiapi-jp-prepare-input-mode-activation ggc google-cgiapi-jp-type-hiragana)
		   (google-cgiapi-jp-context-set-on! ggc #t)
		   (google-cgiapi-jp-context-set-alnum! ggc #f)
		   (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-hiragana)))

(register-action 'action_google-cgiapi-jp_katakana
		 (lambda (ggc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (ggc)
		   (and (google-cgiapi-jp-context-on ggc)
		        (not (google-cgiapi-jp-context-alnum ggc))
			(= (google-cgiapi-jp-context-kana-mode ggc)
			   google-cgiapi-jp-type-katakana)))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-mode-activation ggc google-cgiapi-jp-type-katakana)
		   (google-cgiapi-jp-context-set-on! ggc #t)
		   (google-cgiapi-jp-context-set-alnum! ggc #f)
		   (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-katakana)))

(register-action 'action_google-cgiapi-jp_halfkana
		 (lambda (ggc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (ggc)
		   (and (google-cgiapi-jp-context-on ggc)
			(not (google-cgiapi-jp-context-alnum ggc))
			(= (google-cgiapi-jp-context-kana-mode ggc) google-cgiapi-jp-type-halfkana)))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-mode-activation ggc google-cgiapi-jp-type-halfkana)
		   (google-cgiapi-jp-context-set-on! ggc #t)
		   (google-cgiapi-jp-context-set-alnum! ggc #f)
		   (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-halfkana)))

(register-action 'action_google-cgiapi-jp_halfwidth_alnum
		 (lambda (ggc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (ggc) ;; activity predicate
		   (and (google-cgiapi-jp-context-on ggc)
			(google-cgiapi-jp-context-alnum ggc)
			(= (google-cgiapi-jp-context-alnum-type ggc)
			   google-cgiapi-jp-type-halfwidth-alnum)))
		 (lambda (ggc) ;; action handler
		   (google-cgiapi-jp-prepare-input-mode-activation
		    ggc (google-cgiapi-jp-context-kana-mode ggc))
		   (google-cgiapi-jp-context-set-on! ggc #t)
		   (google-cgiapi-jp-context-set-alnum! ggc #t)
		   (google-cgiapi-jp-context-set-alnum-type!
		    ggc google-cgiapi-jp-type-halfwidth-alnum)))

(register-action 'action_google-cgiapi-jp_direct
		 (lambda (ggc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (ggc)
		   (not (google-cgiapi-jp-context-on ggc)))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-mode-activation ggc google-cgiapi-jp-type-direct)
		   (google-cgiapi-jp-context-set-on! ggc #f)))

(register-action 'action_google-cgiapi-jp_fullwidth_alnum
		 (lambda (ggc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (ggc)
		   (and (google-cgiapi-jp-context-on ggc)
			(google-cgiapi-jp-context-alnum ggc)
			(= (google-cgiapi-jp-context-alnum-type ggc)
			   google-cgiapi-jp-type-fullwidth-alnum)))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-mode-activation
		    ggc (google-cgiapi-jp-context-kana-mode ggc))
		   (google-cgiapi-jp-context-set-on! ggc #t)
		   (google-cgiapi-jp-context-set-alnum! ggc #t)
		   (google-cgiapi-jp-context-set-alnum-type!
		    ggc google-cgiapi-jp-type-fullwidth-alnum)))

(register-action 'action_google-cgiapi-jp_roma
		 (lambda (ggc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (ggc)
		   (= (google-cgiapi-jp-context-input-rule ggc)
		      google-cgiapi-jp-input-rule-roma))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-rule-activation ggc)
		   (rk-context-set-rule! (google-cgiapi-jp-context-rkc ggc)
					 ja-rk-rule)
		   (google-cgiapi-jp-context-set-input-rule! ggc google-cgiapi-jp-input-rule-roma)))

(register-action 'action_google-cgiapi-jp_kana
		 (lambda (ggc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (ggc)
		   (= (google-cgiapi-jp-context-input-rule ggc)
		      google-cgiapi-jp-input-rule-kana))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-rule-activation ggc)
                   (require "japanese-kana.scm")
		   (google-cgiapi-jp-context-set-input-rule! ggc google-cgiapi-jp-input-rule-kana)
                   (google-cgiapi-jp-context-change-kana-mode!
                     ggc (google-cgiapi-jp-context-kana-mode ggc))
		   (google-cgiapi-jp-context-set-alnum! ggc #f)))

(register-action 'action_google-cgiapi-jp_azik
		 (lambda (ggc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (ggc)
		   (= (google-cgiapi-jp-context-input-rule ggc)
		      google-cgiapi-jp-input-rule-azik))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-rule-activation ggc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (google-cgiapi-jp-context-rkc ggc)
					 ja-azik-rule)
		   (google-cgiapi-jp-context-set-input-rule! ggc google-cgiapi-jp-input-rule-azik)))

(register-action 'action_google-cgiapi-jp_kzik
		 (lambda (ggc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (ggc)
		   (= (google-cgiapi-jp-context-input-rule ggc)
		      google-cgiapi-jp-input-rule-kzik))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-rule-activation ggc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (google-cgiapi-jp-context-rkc ggc)
					 ja-kzik-rule)
		   (google-cgiapi-jp-context-set-input-rule! ggc google-cgiapi-jp-input-rule-kzik)))

(register-action 'action_google-cgiapi-jp_act
		 (lambda (ggc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (ggc)
		   (= (google-cgiapi-jp-context-input-rule ggc)
		      google-cgiapi-jp-input-rule-act))
		 (lambda (ggc)
		   (google-cgiapi-jp-prepare-input-rule-activation ggc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (google-cgiapi-jp-context-rkc ggc)
					 ja-act-rule)
		   (google-cgiapi-jp-context-set-input-rule! ggc google-cgiapi-jp-input-rule-act)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define google-cgiapi-jp-configure-widgets
  (lambda ()
    (register-widget 'widget_google-cgiapi-jp_input_mode
		     (activity-indicator-new google-cgiapi-jp-input-mode-actions)
		     (actions-new google-cgiapi-jp-input-mode-actions))

    (register-widget 'widget_google-cgiapi-jp_kana_input_method
		     (activity-indicator-new google-cgiapi-jp-kana-input-method-actions)
		     (actions-new google-cgiapi-jp-kana-input-method-actions))
    (context-list-replace-widgets! 'google-cgiapi-jp google-cgiapi-jp-widgets)))

(define google-cgiapi-jp-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'ggx-ctx             ()) ;; google-cgiapi-jp-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'prediction-ctx     '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '())
    (list 'kana-mode          google-cgiapi-jp-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      google-cgiapi-jp-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         google-cgiapi-jp-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'google-cgiapi-jp-context google-cgiapi-jp-context-rec-spec)
(define google-cgiapi-jp-context-new-internal google-cgiapi-jp-context-new)

(define (google-cgiapi-jp-context-new id im)
  (let ((ggc (google-cgiapi-jp-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (google-cgiapi-jp-context-set-ggx-ctx! ggc (if google-cgiapi-jp-init-lib-ok?
;				      (google-cgiapi-jp-lib-alloc-context) ()))
    (google-cgiapi-jp-context-set-ggx-ctx! ggc (google-cgiapi-jp-lib-alloc-context))
    (google-cgiapi-jp-context-set-widgets! ggc google-cgiapi-jp-widgets)
    (google-cgiapi-jp-context-set-rkc! ggc rkc)
    (google-cgiapi-jp-context-set-preconv-ustr! ggc (ustr-new '()))
    (google-cgiapi-jp-context-set-raw-ustr! ggc (ustr-new '()))
    (google-cgiapi-jp-context-set-segments! ggc (ustr-new '()))
    (if google-cgiapi-jp-use-prediction?
        (begin
          (google-cgiapi-jp-context-set-prediction-ctx! ggc (predict-make-meta-search))
          (predict-meta-open (google-cgiapi-jp-context-prediction-ctx ggc) "google-cgiapi-jp")
          (predict-meta-set-external-charset! (google-cgiapi-jp-context-prediction-ctx ggc) "UTF-8")))
    ggc))

(define (google-cgiapi-jp-commit-raw ggc)
  (im-commit-raw ggc)
  (google-cgiapi-jp-context-set-commit-raw! ggc #t))

(define (google-cgiapi-jp-context-kana-toggle ggc)
  (let* ((kana (google-cgiapi-jp-context-kana-mode ggc))
	 (opposite-kana (ja-opposite-kana kana)))
    (google-cgiapi-jp-context-change-kana-mode! ggc opposite-kana)))

(define google-cgiapi-jp-context-alkana-toggle
  (lambda (ggc)
    (let ((alnum-state (google-cgiapi-jp-context-alnum ggc)))
      (google-cgiapi-jp-context-set-alnum! ggc (not alnum-state)))))

(define google-cgiapi-jp-context-change-kana-mode!
  (lambda (ggc kana-mode)
    (if (= (google-cgiapi-jp-context-input-rule ggc)
           google-cgiapi-jp-input-rule-kana)
        (rk-context-set-rule!
	 (google-cgiapi-jp-context-rkc ggc)
	 (cond
	  ((= kana-mode google-cgiapi-jp-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode google-cgiapi-jp-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode google-cgiapi-jp-type-halfkana) ja-kana-halfkana-rule))))
    (google-cgiapi-jp-context-set-kana-mode! ggc kana-mode)))

(define google-cgiapi-jp-make-whole-string
  (lambda (ggc convert-pending-into-kana? kana)
    (let* ((rkc (google-cgiapi-jp-context-rkc ggc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (google-cgiapi-jp-context-input-rule ggc))
           (preconv-str (google-cgiapi-jp-context-preconv-ustr ggc))
           (extract-kana
            (if (= rule google-cgiapi-jp-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule google-cgiapi-jp-input-rule-kana)
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

(define google-cgiapi-jp-make-raw-string
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
	     (google-cgiapi-jp-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (google-cgiapi-jp-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define google-cgiapi-jp-make-whole-raw-string
  (lambda (ggc wide? upper?)
    (google-cgiapi-jp-make-raw-string (google-cgiapi-jp-get-raw-str-seq ggc) wide? upper?)))

(define (google-cgiapi-jp-init-handler id im arg)
  (if (not google-cgiapi-jp-init-lib-ok?)
      (begin
	(google-cgiapi-jp-lib-init)
	(set! google-cgiapi-jp-init-lib-ok? #t)))
  (google-cgiapi-jp-context-new id im))

(define (google-cgiapi-jp-release-handler ggc)
  (if ggc
      (google-cgiapi-jp-lib-release-context ggc)))

(define (google-cgiapi-jp-flush ggc)
  (rk-flush (google-cgiapi-jp-context-rkc ggc))
  (ustr-clear! (google-cgiapi-jp-context-preconv-ustr ggc))
  (ustr-clear! (google-cgiapi-jp-context-raw-ustr ggc))
  (ustr-clear! (google-cgiapi-jp-context-segments ggc))
  (google-cgiapi-jp-context-set-transposing! ggc #f)
  (google-cgiapi-jp-context-set-state! ggc #f)
  (if (or
       (google-cgiapi-jp-context-candidate-window ggc)
       (google-cgiapi-jp-context-prediction-window ggc))
      (im-deactivate-candidate-selector ggc))
  (google-cgiapi-jp-context-set-candidate-window! ggc #f)
  (google-cgiapi-jp-context-set-prediction-window! ggc #f)
  (google-cgiapi-jp-context-set-candidate-op-count! ggc 0))

(define (google-cgiapi-jp-begin-input ggc key key-state)
  (if (cond
       ((google-cgiapi-jp-on-key? key key-state)
	#t)
       ((and
	 google-cgiapi-jp-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((google-cgiapi-jp-hiragana-key? key key-state)
	   (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-hiragana)
	   (google-cgiapi-jp-context-set-alnum! ggc #f)
	   #t)
	  ((google-cgiapi-jp-katakana-key? key key-state)
	   (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-katakana)
	   (google-cgiapi-jp-context-set-alnum! ggc #f)
	   #t)
	  ((google-cgiapi-jp-halfkana-key? key key-state)
	   (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-halfkana)
	   (google-cgiapi-jp-context-set-alnum! ggc #f)
	   #t)
	  ((google-cgiapi-jp-halfwidth-alnum-key? key key-state)
	   (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-halfwidth-alnum)
	   (google-cgiapi-jp-context-set-alnum! ggc #t)
	   #t)
	  ((google-cgiapi-jp-halfwidth-alnum-key? key key-state)
	   (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-fullwidth-alnum)
	   (google-cgiapi-jp-context-set-alnum! ggc #t)
	   #t)
	  ((google-cgiapi-jp-kana-toggle-key? key key-state)
	   (google-cgiapi-jp-context-kana-toggle ggc)
	   (google-cgiapi-jp-context-set-alnum! ggc #f)
	   #t)
	  ((google-cgiapi-jp-alkana-toggle-key? key key-state)
	   (google-cgiapi-jp-context-alkana-toggle ggc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(google-cgiapi-jp-context-set-on! ggc #t)
	(rk-flush (google-cgiapi-jp-context-rkc ggc))
	(google-cgiapi-jp-context-set-state! ggc #f)
	#t)
      #f))

(define (google-cgiapi-jp-update-preedit ggc)
  (if (not (google-cgiapi-jp-context-commit-raw ggc))
      (let ((segments (if (google-cgiapi-jp-context-on ggc)
			  (if (google-cgiapi-jp-context-transposing ggc)
			      (google-cgiapi-jp-context-transposing-state-preedit ggc)
			      (if (google-cgiapi-jp-context-state ggc)
				  (google-cgiapi-jp-compose-state-preedit ggc)
                                  (if (google-cgiapi-jp-context-predicting ggc)
                                      (google-cgiapi-jp-predicting-state-preedit ggc)
                                      (google-cgiapi-jp-input-state-preedit ggc))))
			  ())))
	(context-update-preedit ggc segments))
      (google-cgiapi-jp-context-set-commit-raw! ggc #f)))

(define (google-cgiapi-jp-begin-conv ggc)
  (let ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
	(preconv-str (google-cgiapi-jp-make-whole-string ggc #t google-cgiapi-jp-type-hiragana)))
    (if (and ggx-ctx
             (> (string-length preconv-str) 0))
	(let ((num (google-cgiapi-jp-lib-begin-conversion ggc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (google-cgiapi-jp-context-segments ggc)
		 (make-list num 0))
		(google-cgiapi-jp-context-set-state! ggc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; google-cgiapi-jp-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define google-cgiapi-jp-cancel-conv
  (lambda (ggc)
    (google-cgiapi-jp-reset-candidate-window ggc)
    (google-cgiapi-jp-context-set-state! ggc #f)
    (ustr-clear! (google-cgiapi-jp-context-segments ggc))
    (google-cgiapi-jp-lib-reset-conversion ggc)))

(define (google-cgiapi-jp-proc-input-state-no-preedit ggc key key-state)
  (let
      ((rkc (google-cgiapi-jp-context-rkc ggc))
       (direct (ja-direct (charcode->string key)))
       (rule (google-cgiapi-jp-context-input-rule ggc)))
    (cond
     ((and google-cgiapi-jp-use-with-vi?
           (google-cgiapi-jp-vi-escape-key? key key-state))
      (google-cgiapi-jp-flush ggc)
      (google-cgiapi-jp-context-set-on! ggc #f)
      (google-cgiapi-jp-commit-raw ggc))

     ((google-cgiapi-jp-off-key? key key-state)
      (google-cgiapi-jp-flush ggc)
      (google-cgiapi-jp-context-set-on! ggc #f))

     ((google-cgiapi-jp-backspace-key? key key-state)
      (google-cgiapi-jp-commit-raw ggc))
     
     ((google-cgiapi-jp-delete-key? key key-state)
      (google-cgiapi-jp-commit-raw ggc))

     ((and
       (google-cgiapi-jp-hiragana-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-kana-mode ggc) google-cgiapi-jp-type-hiragana)
	 (not (google-cgiapi-jp-context-alnum ggc)))))
      (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-hiragana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))

     ((and
       (google-cgiapi-jp-katakana-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-kana-mode ggc) google-cgiapi-jp-type-katakana)
	 (not (google-cgiapi-jp-context-alnum ggc)))))
      (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-katakana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))
     
     ((and
       (google-cgiapi-jp-halfkana-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-kana-mode ggc) google-cgiapi-jp-type-halfkana)
	 (not (google-cgiapi-jp-context-alnum ggc)))))
      (google-cgiapi-jp-context-change-kana-mode! ggc google-cgiapi-jp-type-halfkana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))
     
     ((and
       (google-cgiapi-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-alnum-type ggc) google-cgiapi-jp-type-halfwidth-alnum)
	 (google-cgiapi-jp-context-alnum ggc))))
      (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-halfwidth-alnum)
      (google-cgiapi-jp-context-set-alnum! ggc #t))
     
     ((and
       (google-cgiapi-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-alnum-type ggc) google-cgiapi-jp-type-fullwidth-alnum)
	 (google-cgiapi-jp-context-alnum ggc))))
      (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-fullwidth-alnum)
      (google-cgiapi-jp-context-set-alnum! ggc #t))
     
     ((and
       (not (google-cgiapi-jp-context-alnum ggc))
       (google-cgiapi-jp-kana-toggle-key? key key-state))
      (google-cgiapi-jp-context-kana-toggle ggc))

     ((google-cgiapi-jp-alkana-toggle-key? key key-state)
      (google-cgiapi-jp-context-alkana-toggle ggc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (google-cgiapi-jp-commit-raw ggc))
     
     ;; direct key => commit
     (direct
      (im-commit ggc direct))

     ;; space key
     ((google-cgiapi-jp-space-key? key key-state)
      (if (google-cgiapi-jp-context-alnum ggc)
	  (im-commit ggc (list-ref
			 ja-alnum-space
			 (- (google-cgiapi-jp-context-alnum-type ggc)
			    google-cgiapi-jp-type-halfwidth-alnum)))
	  (im-commit ggc (list-ref ja-space (google-cgiapi-jp-context-kana-mode ggc)))))

     ((symbol? key)
      (google-cgiapi-jp-commit-raw ggc))

     (else
      (if (google-cgiapi-jp-context-alnum ggc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (google-cgiapi-jp-context-preconv-ustr ggc)
			       (if (= (google-cgiapi-jp-context-alnum-type ggc)
				      google-cgiapi-jp-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (google-cgiapi-jp-context-raw-ustr ggc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule google-cgiapi-jp-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (google-cgiapi-jp-context-preconv-ustr ggc) res)
                    (ustr-insert-elem! (google-cgiapi-jp-context-preconv-ustr ggc) res))
	          (ustr-insert-elem! (google-cgiapi-jp-context-raw-ustr ggc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (google-cgiapi-jp-commit-raw ggc)))))))))

(define (google-cgiapi-jp-has-preedit? ggc)
  (or (not (ustr-empty? (google-cgiapi-jp-context-preconv-ustr ggc)))
      (> (string-length (rk-pending (google-cgiapi-jp-context-rkc ggc))) 0)))

(define google-cgiapi-jp-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type google-cgiapi-jp-type-halfwidth-alnum)
       (= state google-cgiapi-jp-type-halfwidth-alnum))
      google-cgiapi-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type google-cgiapi-jp-type-fullwidth-alnum)
       (= state google-cgiapi-jp-type-fullwidth-alnum))
      google-cgiapi-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define google-cgiapi-jp-proc-transposing-state
  (lambda (ggc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (google-cgiapi-jp-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons google-cgiapi-jp-type-fullwidth-alnum rotate-list)))
      (if (google-cgiapi-jp-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons google-cgiapi-jp-type-halfwidth-alnum rotate-list)))
      (if (google-cgiapi-jp-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons google-cgiapi-jp-type-halfkana rotate-list)))
      (if (google-cgiapi-jp-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons google-cgiapi-jp-type-katakana rotate-list)))
      (if (google-cgiapi-jp-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons google-cgiapi-jp-type-hiragana rotate-list)))

      (if (google-cgiapi-jp-context-transposing ggc)
	  (let ((lst (member (google-cgiapi-jp-context-transposing-type ggc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (google-cgiapi-jp-rotate-transposing-alnum-type
				 (google-cgiapi-jp-context-transposing-type ggc)
				 (car rotate-list))))))
	  (begin
	    (google-cgiapi-jp-context-set-transposing! ggc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state google-cgiapi-jp-type-hiragana)
	      (= state google-cgiapi-jp-type-katakana)
	      (= state google-cgiapi-jp-type-halfkana)))
	(google-cgiapi-jp-context-set-transposing-type! ggc state))
       ((and state
	     (or
	      (= state google-cgiapi-jp-type-halfwidth-alnum)
	      (= state google-cgiapi-jp-candidate-type-upper-halfwidth-alnum)
	      (= state google-cgiapi-jp-type-fullwidth-alnum)
	      (= state google-cgiapi-jp-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (google-cgiapi-jp-context-input-rule ggc) google-cgiapi-jp-input-rule-kana))
	    (google-cgiapi-jp-context-set-transposing-type! ggc state)))
       (else
	(and
	 ; commit
	 (if (google-cgiapi-jp-commit-key? key key-state)
	     (begin
	       (im-commit ggc (google-cgiapi-jp-transposing-text ggc))
	       (google-cgiapi-jp-flush ggc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (google-cgiapi-jp-begin-conv-key? key key-state)
	     (begin
	       (google-cgiapi-jp-context-set-transposing! ggc #f)
	       (google-cgiapi-jp-begin-conv ggc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (google-cgiapi-jp-cancel-key? key key-state)
	      (google-cgiapi-jp-backspace-key? key key-state))
	     (begin
	       (google-cgiapi-jp-context-set-transposing! ggc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (google-cgiapi-jp-prev-page-key? key key-state)
	      (google-cgiapi-jp-next-page-key? key key-state)
	      (google-cgiapi-jp-extend-segment-key? key key-state)
	      (google-cgiapi-jp-shrink-segment-key? key key-state)
	      (google-cgiapi-jp-next-segment-key? key key-state)
	      (google-cgiapi-jp-beginning-of-preedit-key? key key-state)
	      (google-cgiapi-jp-end-of-preedit-key? key key-state)
	      (google-cgiapi-jp-next-candidate-key? key key-state)
	      (google-cgiapi-jp-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit ggc (google-cgiapi-jp-transposing-text ggc))
	   (google-cgiapi-jp-flush ggc)
	   (google-cgiapi-jp-proc-input-state ggc key key-state))))))))

(define (google-cgiapi-jp-move-prediction ggc offset)
  (let* ((nr (google-cgiapi-jp-lib-get-nr-predictions ggc))
         (idx (google-cgiapi-jp-context-prediction-index ggc))
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
    (im-select-candidate ggc compensated-n)
    (google-cgiapi-jp-context-set-prediction-index! ggc compensated-n)))

(define (google-cgiapi-jp-move-prediction-in-page ggc numeralc)
  (let* ((nr (google-cgiapi-jp-lib-get-nr-predictions ggc))
	 (p-idx (google-cgiapi-jp-context-prediction-index ggc))
	 (n (if (not p-idx)
		0
		p-idx))
	 (cur-page (if (= google-cgiapi-jp-nr-candidate-max 0)
		       0
		       (quotient n google-cgiapi-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page google-cgiapi-jp-nr-candidate-max) compensated-pageidx))
	 (compensated-idx (cond
			   ((>= idx nr)
			    #f)
			   (else
			    idx)))
	 (selected-pageidx (if (not p-idx)
			       #f
			       (if (= google-cgiapi-jp-nr-candidate-max 0)
				   p-idx
				   (remainder p-idx
					      google-cgiapi-jp-nr-candidate-max)))))
    (if (and
	 compensated-idx
	 (not (eqv? compensated-pageidx selected-pageidx)))
	(begin
	  (google-cgiapi-jp-context-set-prediction-index! ggc compensated-idx)
	  (im-select-candidate ggc compensated-idx)
	  #t)
	#f)))

(define (google-cgiapi-jp-prediction-select-non-existing-index? ggc numeralc)
  (let* ((nr (google-cgiapi-jp-lib-get-nr-predictions ggc))
	 (p-idx (google-cgiapi-jp-context-prediction-index ggc))
	 (cur-page (if (= google-cgiapi-jp-nr-candidate-max 0)
		       0
		       (quotient p-idx google-cgiapi-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page google-cgiapi-jp-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (google-cgiapi-jp-prediction-keys-handled? ggc key key-state)
  (cond
   ((google-cgiapi-jp-next-prediction-key? key key-state)
    (google-cgiapi-jp-move-prediction ggc 1)
    #t)
   ((google-cgiapi-jp-prev-prediction-key? key key-state)
    (google-cgiapi-jp-move-prediction ggc -1)
    #t)
   ((and
     google-cgiapi-jp-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (google-cgiapi-jp-move-prediction-in-page ggc key))
   ((and
     (google-cgiapi-jp-context-prediction-index ggc)
     (google-cgiapi-jp-prev-page-key? key key-state))
    (im-shift-page-candidate ggc #f)
    #t)
   ((and
     (google-cgiapi-jp-context-prediction-index ggc)
     (google-cgiapi-jp-next-page-key? key key-state))
    (im-shift-page-candidate ggc #t)
    #t)
   (else
    #f)))

(define (google-cgiapi-jp-proc-prediction-state ggc key key-state)
  (cond
   ;; prediction index change
   ((google-cgiapi-jp-prediction-keys-handled? ggc key key-state))

   ;; cancel
   ((google-cgiapi-jp-cancel-key? key key-state)
    (if (google-cgiapi-jp-context-prediction-index ggc)
	(google-cgiapi-jp-reset-prediction-window ggc)
	(begin
	  (google-cgiapi-jp-reset-prediction-window ggc)
	  (google-cgiapi-jp-proc-input-state ggc key key-state))))

   ;; commit
   ((and
     (google-cgiapi-jp-context-prediction-index ggc)
     (google-cgiapi-jp-commit-key? key key-state))
    (google-cgiapi-jp-do-commit-prediction ggc))
   (else
    (if (and
	 google-cgiapi-jp-use-implicit-commit-prediction?
	 (google-cgiapi-jp-context-prediction-index ggc))
	(cond
	 ((or
	   ;; check keys used in google-cgiapi-jp-proc-input-state-with-preedit
	   (google-cgiapi-jp-begin-conv-key? key key-state)
	   (google-cgiapi-jp-backspace-key? key key-state)
	   (google-cgiapi-jp-delete-key? key key-state)
	   (google-cgiapi-jp-kill-key? key key-state)
	   (google-cgiapi-jp-kill-backward-key? key key-state)
	   (and
	    (not (google-cgiapi-jp-context-alnum ggc))
	    (google-cgiapi-jp-commit-as-opposite-kana-key? key key-state))
	   (google-cgiapi-jp-transpose-as-hiragana-key? key key-state)
	   (google-cgiapi-jp-transpose-as-katakana-key? key key-state)
	   (google-cgiapi-jp-transpose-as-halfkana-key? key key-state)
	   (and
	    (not (= (google-cgiapi-jp-context-input-rule ggc) google-cgiapi-jp-input-rule-kana))
	    (or
	     (google-cgiapi-jp-transpose-as-halfwidth-alnum-key? key key-state)
	     (google-cgiapi-jp-transpose-as-fullwidth-alnum-key? key key-state)))
	   (google-cgiapi-jp-hiragana-key? key key-state)
	   (google-cgiapi-jp-katakana-key? key key-state)
	   (google-cgiapi-jp-halfkana-key? key key-state)
	   (google-cgiapi-jp-halfwidth-alnum-key? key key-state)
	   (google-cgiapi-jp-fullwidth-alnum-key? key key-state)
	   (and
	    (not (google-cgiapi-jp-context-alnum ggc))
	    (google-cgiapi-jp-kana-toggle-key? key key-state))
	   (google-cgiapi-jp-alkana-toggle-key? key key-state)
	   (google-cgiapi-jp-go-left-key? key key-state)
	   (google-cgiapi-jp-go-right-key? key key-state)
	   (google-cgiapi-jp-beginning-of-preedit-key? key key-state)
	   (google-cgiapi-jp-end-of-preedit-key? key key-state)
	   (and
	    (modifier-key-mask key-state)
	    (not (shift-key-mask key-state))))
	  ;; go back to unselected prediction
	  (google-cgiapi-jp-reset-prediction-window ggc)
	  (google-cgiapi-jp-check-prediction ggc #f))
	 ((and
	   (ichar-numeric? key)
	   google-cgiapi-jp-select-prediction-by-numeral-key?
	   (not (google-cgiapi-jp-prediction-select-non-existing-index? ggc key)))
	  (google-cgiapi-jp-context-set-predicting! ggc #f)
	  (google-cgiapi-jp-context-set-prediction-index! ggc #f)
	  (google-cgiapi-jp-proc-input-state ggc key key-state))
	 (else
	  ;; implicit commit
	  (google-cgiapi-jp-do-commit-prediction ggc)
	  (google-cgiapi-jp-proc-input-state ggc key key-state)))
	(begin
	  (google-cgiapi-jp-context-set-predicting! ggc #f)
	  (google-cgiapi-jp-context-set-prediction-index! ggc #f)
	  (if (not google-cgiapi-jp-use-prediction?)
	      (google-cgiapi-jp-reset-prediction-window ggc))
	  (google-cgiapi-jp-proc-input-state ggc key key-state))))))

(define (google-cgiapi-jp-proc-input-state-with-preedit ggc key key-state)
  (define (check-auto-conv str)
    (and
      str
      google-cgiapi-jp-auto-start-henkan?
      (string-find japanese-auto-start-henkan-keyword-list str)
      (begin
	(google-cgiapi-jp-reset-prediction-window ggc)
	(google-cgiapi-jp-begin-conv ggc))))
  (let ((preconv-str (google-cgiapi-jp-context-preconv-ustr ggc))
	(raw-str (google-cgiapi-jp-context-raw-ustr ggc))
	(rkc (google-cgiapi-jp-context-rkc ggc))
	(rule (google-cgiapi-jp-context-input-rule ggc))
	(kana (google-cgiapi-jp-context-kana-mode ggc)))
    (cond
     ;; begin conversion
     ((google-cgiapi-jp-begin-conv-key? key key-state)
      (google-cgiapi-jp-begin-conv ggc))

     ;; prediction
     ((google-cgiapi-jp-next-prediction-key? key key-state)
      (google-cgiapi-jp-check-prediction ggc #t))

     ;; backspace
     ((google-cgiapi-jp-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (google-cgiapi-jp-context-input-rule ggc) google-cgiapi-jp-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((google-cgiapi-jp-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((google-cgiapi-jp-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((google-cgiapi-jp-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (google-cgiapi-jp-context-alnum ggc))
       (google-cgiapi-jp-commit-as-opposite-kana-key? key key-state))
      (im-commit ggc (google-cgiapi-jp-make-whole-string ggc #t (ja-opposite-kana kana)))
      (google-cgiapi-jp-flush ggc))

     ;; Transposing状態へ移行
     ((or (google-cgiapi-jp-transpose-as-hiragana-key? key key-state)
	  (google-cgiapi-jp-transpose-as-katakana-key? key key-state)
	  (google-cgiapi-jp-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (google-cgiapi-jp-context-input-rule ggc) google-cgiapi-jp-input-rule-kana))
	   (or
	    (google-cgiapi-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (google-cgiapi-jp-transpose-as-fullwidth-alnum-key? key key-state))))
      (google-cgiapi-jp-reset-prediction-window ggc)
      (google-cgiapi-jp-proc-transposing-state ggc key key-state))

     ((google-cgiapi-jp-hiragana-key? key key-state)
      (if (not (= kana google-cgiapi-jp-type-hiragana))
	  (begin
	    (im-commit ggc (google-cgiapi-jp-make-whole-string ggc #t kana))
	    (google-cgiapi-jp-flush ggc)))
      (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-hiragana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))

     ((google-cgiapi-jp-katakana-key? key key-state)
      (if (not (= kana google-cgiapi-jp-type-katakana))
	  (begin
	    (im-commit ggc (google-cgiapi-jp-make-whole-string ggc #t kana))
	    (google-cgiapi-jp-flush ggc)))
      (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-katakana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))

     ((google-cgiapi-jp-halfkana-key? key key-state)
      (if (not (= kana google-cgiapi-jp-type-halfkana))
	  (begin
	    (im-commit ggc (google-cgiapi-jp-make-whole-string ggc #t kana))
	    (google-cgiapi-jp-flush ggc)))
      (google-cgiapi-jp-context-set-kana-mode! ggc google-cgiapi-jp-type-halfkana)
      (google-cgiapi-jp-context-set-alnum! ggc #f))

     ((and
       (google-cgiapi-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-alnum-type ggc) google-cgiapi-jp-type-halfwidth-alnum)
	 (google-cgiapi-jp-context-alnum ggc))))
      (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-halfwidth-alnum)
      (google-cgiapi-jp-context-set-alnum! ggc #t))

     ((and
       (google-cgiapi-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (google-cgiapi-jp-context-alnum-type ggc) google-cgiapi-jp-type-fullwidth-alnum)
	 (google-cgiapi-jp-context-alnum ggc))))
      (google-cgiapi-jp-context-set-alnum-type! ggc google-cgiapi-jp-type-fullwidth-alnum)
      (google-cgiapi-jp-context-set-alnum! ggc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (google-cgiapi-jp-context-alnum ggc))
       (google-cgiapi-jp-kana-toggle-key? key key-state))
      (im-commit ggc (google-cgiapi-jp-make-whole-string ggc #t kana))
      (google-cgiapi-jp-flush ggc)
      (google-cgiapi-jp-context-kana-toggle ggc))

     ((google-cgiapi-jp-alkana-toggle-key? key key-state)
      (google-cgiapi-jp-context-alkana-toggle ggc))

     ;; cancel
     ((google-cgiapi-jp-cancel-key? key key-state)
      (google-cgiapi-jp-flush ggc))

     ;; commit
     ((google-cgiapi-jp-commit-key? key key-state)
      (begin
	(im-commit
	 ggc
	 (google-cgiapi-jp-make-whole-string ggc #t kana))
	(google-cgiapi-jp-flush ggc)))

     ;; left
     ((google-cgiapi-jp-go-left-key? key key-state)
      (google-cgiapi-jp-context-confirm-kana! ggc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((google-cgiapi-jp-go-right-key? key key-state)
      (google-cgiapi-jp-context-confirm-kana! ggc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((google-cgiapi-jp-beginning-of-preedit-key? key key-state)
      (google-cgiapi-jp-context-confirm-kana! ggc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((google-cgiapi-jp-end-of-preedit-key? key key-state)
      (google-cgiapi-jp-context-confirm-kana! ggc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (if (google-cgiapi-jp-context-alnum ggc)
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
			       (if (= (google-cgiapi-jp-context-alnum-type ggc)
				      google-cgiapi-jp-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str)
	    (check-auto-conv key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule google-cgiapi-jp-input-rule-kana)
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

(define google-cgiapi-jp-context-confirm-kana!
  (lambda (ggc)
    (if (= (google-cgiapi-jp-context-input-rule ggc)
	   google-cgiapi-jp-input-rule-kana)
	(let* ((preconv-str (google-cgiapi-jp-context-preconv-ustr ggc))
	       (rkc (google-cgiapi-jp-context-rkc ggc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (google-cgiapi-jp-reset-prediction-window ggc)
  (if (google-cgiapi-jp-context-prediction-window ggc)
      (im-deactivate-candidate-selector ggc))
  (google-cgiapi-jp-context-set-predicting! ggc #f)
  (google-cgiapi-jp-context-set-prediction-window! ggc #f)
  (google-cgiapi-jp-context-set-prediction-index! ggc #f))

(define (google-cgiapi-jp-check-prediction ggc force-check?)
  (if (and
       (not (google-cgiapi-jp-context-state ggc))
       (not (google-cgiapi-jp-context-transposing ggc))
       (not (google-cgiapi-jp-context-predicting ggc)))
      (let* ((use-pending-rk-for-prediction? #t)
             (preconv-str
              (google-cgiapi-jp-make-whole-string
               ggc
               (not use-pending-rk-for-prediction?)
               (google-cgiapi-jp-context-kana-mode ggc)))
             (preedit-len (+
                           (ustr-length (google-cgiapi-jp-context-preconv-ustr ggc))
                           (if (not use-pending-rk-for-prediction?)
                               0
                               (string-length (rk-pending
                                               (google-cgiapi-jp-context-rkc
                                                ggc)))))))
        (if (or
             (>= preedit-len google-cgiapi-jp-prediction-start-char-count)
             force-check?)
            (begin
              (google-cgiapi-jp-lib-set-prediction-src-string ggc preconv-str)
              (let ((nr (google-cgiapi-jp-lib-get-nr-predictions ggc)))
                (if (and
                     nr
                     (> nr 0))
                    (begin
                     (im-activate-candidate-selector
                      ggc nr google-cgiapi-jp-nr-candidate-max)
                     (google-cgiapi-jp-context-set-prediction-window! ggc #t)
                     (google-cgiapi-jp-context-set-predicting! ggc #t))
                    (google-cgiapi-jp-reset-prediction-window ggc))))
            (google-cgiapi-jp-reset-prediction-window ggc)))))

(define (google-cgiapi-jp-proc-input-state ggc key key-state)
  (if (google-cgiapi-jp-has-preedit? ggc)
      (google-cgiapi-jp-proc-input-state-with-preedit ggc key key-state)
      (google-cgiapi-jp-proc-input-state-no-preedit ggc key key-state))
  (if google-cgiapi-jp-use-prediction?
      (google-cgiapi-jp-check-prediction ggc #f)))

(define google-cgiapi-jp-separator
  (lambda (ggc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if google-cgiapi-jp-show-segment-separator?
	  (cons attr google-cgiapi-jp-segment-separator)
	  #f))))

(define google-cgiapi-jp-context-transposing-state-preedit
  (lambda (ggc)
    (let ((transposing-text (google-cgiapi-jp-transposing-text ggc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define google-cgiapi-jp-transposing-text
  (lambda (ggc)
    (let ((transposing-type (google-cgiapi-jp-context-transposing-type ggc)))
      (cond
       ((or
	 (= transposing-type google-cgiapi-jp-type-hiragana)
	 (= transposing-type google-cgiapi-jp-type-katakana)
	 (= transposing-type google-cgiapi-jp-type-halfkana))
	(google-cgiapi-jp-make-whole-string ggc #t transposing-type))
       ((= transposing-type google-cgiapi-jp-type-halfwidth-alnum)
	(google-cgiapi-jp-make-whole-raw-string ggc #f #f))
       ((= transposing-type google-cgiapi-jp-candidate-type-upper-halfwidth-alnum)
	(google-cgiapi-jp-make-whole-raw-string ggc #f #t))
       ((= transposing-type google-cgiapi-jp-type-fullwidth-alnum)
	(google-cgiapi-jp-make-whole-raw-string ggc #t #f))
       ((= transposing-type google-cgiapi-jp-candidate-type-upper-fullwidth-alnum)
	(google-cgiapi-jp-make-whole-raw-string ggc #t #t))))))

(define google-cgiapi-jp-get-raw-str-seq
  (lambda (ggc)
    (let* ((rkc (google-cgiapi-jp-context-rkc ggc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (google-cgiapi-jp-context-raw-ustr ggc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
	      right-str))))

(define google-cgiapi-jp-get-raw-candidate
  (lambda (ggc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (google-cgiapi-jp-make-whole-string ggc #t google-cgiapi-jp-type-hiragana))))
	   (unconv-candidate (google-cgiapi-jp-lib-get-unconv-candidate ggc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (google-cgiapi-jp-get-raw-str-seq ggc))))
      (cond
       ((= cand-idx google-cgiapi-jp-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx google-cgiapi-jp-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) google-cgiapi-jp-type-katakana))
       ((= cand-idx google-cgiapi-jp-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) google-cgiapi-jp-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (google-cgiapi-jp-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx google-cgiapi-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       google-cgiapi-jp-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx google-cgiapi-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx google-cgiapi-jp-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (google-cgiapi-jp-predicting-state-preedit ggc)
  (if (or
       (not google-cgiapi-jp-use-implicit-commit-prediction?)
       (not (google-cgiapi-jp-context-prediction-index ggc)))
      (google-cgiapi-jp-input-state-preedit ggc)
      (let ((cand (google-cgiapi-jp-get-prediction-string ggc)))
       (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (google-cgiapi-jp-compose-state-preedit ggc)
  (let* ((segments (google-cgiapi-jp-context-segments ggc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (google-cgiapi-jp-separator ggc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx google-cgiapi-jp-candidate-type-katakana)
			(google-cgiapi-jp-lib-get-nth-candidate ggc seg-idx cand-idx)
			(google-cgiapi-jp-get-raw-candidate ggc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (google-cgiapi-jp-input-state-preedit ggc)
  (let* ((preconv-str (google-cgiapi-jp-context-preconv-ustr ggc))
	 (rkc (google-cgiapi-jp-context-rkc ggc))
	 (pending (rk-pending rkc))
	 (kana (google-cgiapi-jp-context-kana-mode ggc))
	 (rule (google-cgiapi-jp-context-input-rule ggc))
	 (extract-kana
	  (if (= rule google-cgiapi-jp-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (google-cgiapi-jp-has-preedit? ggc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (google-cgiapi-jp-get-commit-string ggc)
  (let ((segments (google-cgiapi-jp-context-segments ggc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx google-cgiapi-jp-candidate-type-katakana)
			     (google-cgiapi-jp-lib-get-nth-candidate
			      ggc seg-idx cand-idx)
			     (google-cgiapi-jp-get-raw-candidate
			      ggc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (google-cgiapi-jp-commit-string ggc)
    (let ((ggx-ctx (google-cgiapi-jp-context-ggx-ctx ggc))
          (segments (google-cgiapi-jp-context-segments ggc)))
      (if ggx-ctx
          (begin
            (google-cgiapi-jp-lib-commit-segments ggc (ustr-whole-seq segments))
            (if (every (lambda (x) (<= x google-cgiapi-jp-candidate-type-katakana))
                       (ustr-whole-seq segments))
                (google-cgiapi-jp-lib-reset-conversion ggc))))))

(define (google-cgiapi-jp-do-commit ggc)
    (im-commit ggc (google-cgiapi-jp-get-commit-string ggc))
    (google-cgiapi-jp-commit-string ggc)
    (google-cgiapi-jp-reset-candidate-window ggc)
    (google-cgiapi-jp-flush ggc))

(define (google-cgiapi-jp-get-prediction-string ggc)
  (google-cgiapi-jp-lib-get-nth-prediction
   ggc
   (google-cgiapi-jp-context-prediction-index ggc)))

(define (google-cgiapi-jp-learn-prediction-string ggc)
  (google-cgiapi-jp-lib-commit-nth-prediction
   ggc
   (google-cgiapi-jp-context-prediction-index ggc)))

(define (google-cgiapi-jp-do-commit-prediction ggc)
  (im-commit ggc (google-cgiapi-jp-get-prediction-string ggc))
  (google-cgiapi-jp-learn-prediction-string ggc)
  (google-cgiapi-jp-reset-prediction-window ggc)
  (google-cgiapi-jp-flush ggc))

(define google-cgiapi-jp-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (google-cgiapi-jp-move-segment ggc dir)
  (google-cgiapi-jp-reset-candidate-window ggc)
  (let ((segments (google-cgiapi-jp-context-segments ggc)))
    (ustr-cursor-move! segments dir)
    (google-cgiapi-jp-correct-segment-cursor segments)))

(define (google-cgiapi-jp-resize-segment ggc cnt)
  (let* ((segments (google-cgiapi-jp-context-segments ggc))
	 (cur-seg (ustr-cursor-pos segments)))
    (google-cgiapi-jp-reset-candidate-window ggc)
    (google-cgiapi-jp-lib-resize-segment ggc cur-seg cnt)
    (let* ((resized-nseg (google-cgiapi-jp-lib-get-nr-segments ggc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (google-cgiapi-jp-move-candidate ggc offset)
  (let* ((segments (google-cgiapi-jp-context-segments ggc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (google-cgiapi-jp-lib-get-nr-candidates ggc cur-seg))
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
	 (new-op-count (+ 1 (google-cgiapi-jp-context-candidate-op-count ggc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (google-cgiapi-jp-context-set-candidate-op-count! ggc new-op-count)
    (if (and
	 (= (google-cgiapi-jp-context-candidate-op-count ggc)
	    google-cgiapi-jp-candidate-op-count)
	 google-cgiapi-jp-use-candidate-window?)
	(begin
	  (google-cgiapi-jp-context-set-candidate-window! ggc #t)
	  (im-activate-candidate-selector ggc max google-cgiapi-jp-nr-candidate-max)))
    (if (google-cgiapi-jp-context-candidate-window ggc)
	(im-select-candidate ggc compensated-n))))

(define google-cgiapi-jp-move-candidate-in-page
  (lambda (ggc numeralc)
    (let* ((segments (google-cgiapi-jp-context-segments ggc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (google-cgiapi-jp-lib-get-nr-candidates ggc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= google-cgiapi-jp-nr-candidate-max 0)
			 0
			 (quotient n google-cgiapi-jp-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page google-cgiapi-jp-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (google-cgiapi-jp-context-candidate-op-count ggc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (google-cgiapi-jp-context-set-candidate-op-count! ggc new-op-count)
      (im-select-candidate ggc compensated-idx))))

(define (google-cgiapi-jp-reset-candidate-window ggc)
  (if (google-cgiapi-jp-context-candidate-window ggc)
      (begin
	(im-deactivate-candidate-selector ggc)
	(google-cgiapi-jp-context-set-candidate-window! ggc #f)))
  (google-cgiapi-jp-context-set-candidate-op-count! ggc 0))

(define google-cgiapi-jp-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx google-cgiapi-jp-candidate-type-halfwidth-alnum)
       (= state google-cgiapi-jp-candidate-type-halfwidth-alnum))
      google-cgiapi-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx google-cgiapi-jp-candidate-type-fullwidth-alnum)
       (= state google-cgiapi-jp-candidate-type-fullwidth-alnum))
      google-cgiapi-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define google-cgiapi-jp-set-segment-transposing
  (lambda (ggc key key-state)
    (let ((segments (google-cgiapi-jp-context-segments ggc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(google-cgiapi-jp-reset-candidate-window ggc)
	(google-cgiapi-jp-context-set-candidate-op-count! ggc 0)

	(if (google-cgiapi-jp-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons google-cgiapi-jp-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (google-cgiapi-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons google-cgiapi-jp-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (google-cgiapi-jp-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons google-cgiapi-jp-candidate-type-halfkana
				    rotate-list)))
	(if (google-cgiapi-jp-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons google-cgiapi-jp-candidate-type-katakana
				    rotate-list)))
	(if (google-cgiapi-jp-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons google-cgiapi-jp-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx google-cgiapi-jp-candidate-type-hiragana)
	     (= idx google-cgiapi-jp-candidate-type-katakana)
	     (= idx google-cgiapi-jp-candidate-type-halfkana)
	     (= idx google-cgiapi-jp-candidate-type-halfwidth-alnum)
	     (= idx google-cgiapi-jp-candidate-type-fullwidth-alnum)
	     (= idx google-cgiapi-jp-candidate-type-upper-halfwidth-alnum)
	     (= idx google-cgiapi-jp-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (google-cgiapi-jp-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (google-cgiapi-jp-proc-compose-state ggc key key-state)
  (cond
   ((google-cgiapi-jp-prev-page-key? key key-state)
    (if (google-cgiapi-jp-context-candidate-window ggc)
        (im-shift-page-candidate ggc #f)))

   ((google-cgiapi-jp-next-page-key? key key-state)
    (if (google-cgiapi-jp-context-candidate-window ggc)
        (im-shift-page-candidate ggc #t)))

   ((google-cgiapi-jp-commit-key? key key-state)
    (google-cgiapi-jp-do-commit ggc))

   ((google-cgiapi-jp-extend-segment-key? key key-state)
    (google-cgiapi-jp-resize-segment ggc 1))

   ((google-cgiapi-jp-shrink-segment-key? key key-state)
    (google-cgiapi-jp-resize-segment ggc -1))

   ((google-cgiapi-jp-next-segment-key? key key-state)
    (google-cgiapi-jp-move-segment ggc 1))

   ((google-cgiapi-jp-prev-segment-key? key key-state)
    (google-cgiapi-jp-move-segment ggc -1))

   ((google-cgiapi-jp-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (google-cgiapi-jp-context-segments ggc))
      (google-cgiapi-jp-reset-candidate-window ggc)))

   ((google-cgiapi-jp-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (google-cgiapi-jp-context-segments ggc))
      (google-cgiapi-jp-correct-segment-cursor (google-cgiapi-jp-context-segments ggc))
      (google-cgiapi-jp-reset-candidate-window ggc)))

   ((google-cgiapi-jp-backspace-key? key key-state)
    (google-cgiapi-jp-cancel-conv ggc))

   ((google-cgiapi-jp-next-candidate-key? key key-state)
    (google-cgiapi-jp-move-candidate ggc 1))

   ((google-cgiapi-jp-prev-candidate-key? key key-state)
    (google-cgiapi-jp-move-candidate ggc -1))

   ((or (google-cgiapi-jp-transpose-as-hiragana-key? key key-state)
        (google-cgiapi-jp-transpose-as-katakana-key? key key-state)
        (google-cgiapi-jp-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (google-cgiapi-jp-context-input-rule ggc) google-cgiapi-jp-input-rule-kana))
         (or
          (google-cgiapi-jp-transpose-as-halfwidth-alnum-key? key key-state)
          (google-cgiapi-jp-transpose-as-fullwidth-alnum-key? key key-state))))
    (google-cgiapi-jp-set-segment-transposing ggc key key-state))

   ((google-cgiapi-jp-cancel-key? key key-state)
    (google-cgiapi-jp-cancel-conv ggc))

   ((and google-cgiapi-jp-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (google-cgiapi-jp-context-candidate-window ggc))
    (google-cgiapi-jp-move-candidate-in-page ggc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (google-cgiapi-jp-do-commit ggc)
      (google-cgiapi-jp-proc-input-state ggc key key-state)))))

(define (google-cgiapi-jp-press-key-handler ggc key key-state)
  (if (ichar-control? key)
      (im-commit-raw ggc)
      (if (google-cgiapi-jp-context-on ggc)
          (if (google-cgiapi-jp-context-transposing ggc)
              (google-cgiapi-jp-proc-transposing-state ggc key key-state)
              (if (google-cgiapi-jp-context-state ggc)
                  (google-cgiapi-jp-proc-compose-state ggc key key-state)
                  (if (google-cgiapi-jp-context-predicting ggc)
                      (google-cgiapi-jp-proc-prediction-state ggc key key-state)
                      (google-cgiapi-jp-proc-input-state ggc key key-state))))
	  (google-cgiapi-jp-proc-raw-state ggc key key-state)))
  (google-cgiapi-jp-update-preedit ggc))

;;;
(define (google-cgiapi-jp-release-key-handler ggc key key-state)
  (if (or (ichar-control? key)
	  (not (google-cgiapi-jp-context-on ggc)))
      (google-cgiapi-jp-commit-raw ggc)))
;;;
(define (google-cgiapi-jp-reset-handler ggc)
  (if (google-cgiapi-jp-context-on ggc)
      (begin
	(if (google-cgiapi-jp-context-state ggc)
            (google-cgiapi-jp-lib-reset-conversion ggc))
	(google-cgiapi-jp-flush ggc))))

;;;
(define (google-cgiapi-jp-get-candidate-handler ggc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (google-cgiapi-jp-context-segments ggc)))
         (cand (if (google-cgiapi-jp-context-state ggc)
                   (google-cgiapi-jp-lib-get-nth-candidate ggc cur-seg idx)
                   (google-cgiapi-jp-lib-get-nth-prediction ggc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (google-cgiapi-jp-set-candidate-index-handler ggc idx)
    (cond
     ((google-cgiapi-jp-context-state ggc)
      (ustr-cursor-set-frontside! (google-cgiapi-jp-context-segments ggc) idx)
      (google-cgiapi-jp-update-preedit ggc))
     ((google-cgiapi-jp-context-predicting ggc)
      (google-cgiapi-jp-context-set-prediction-index! ggc idx)
      (google-cgiapi-jp-update-preedit ggc))))

(define (google-cgiapi-jp-proc-raw-state ggc key key-state)
  (if (not (google-cgiapi-jp-begin-input ggc key key-state))
      (im-commit-raw ggc)))

(google-cgiapi-jp-configure-widgets)
(register-im
 'google-cgiapi-jp
 "ja"
 "EUC-JP"
 google-cgiapi-jp-im-name-label
 google-cgiapi-jp-im-short-desc
 #f
 google-cgiapi-jp-init-handler
 google-cgiapi-jp-release-handler
 context-mode-handler
 google-cgiapi-jp-press-key-handler
 google-cgiapi-jp-release-key-handler
 google-cgiapi-jp-reset-handler
 google-cgiapi-jp-get-candidate-handler
 google-cgiapi-jp-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
