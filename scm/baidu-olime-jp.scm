;;; baidu-olime-jp.scm: baidu online ime for uim.
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
(require-custom "generic-key-custom.scm")
(require-custom "baidu-olime-jp-custom.scm")
(require-custom "baidu-olime-jp-key-custom.scm")

;;; implementations

;;
;; canna emulating functions
;;

(define baidu-olime-jp-internal-context-rec-spec
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
(define-record 'baidu-olime-jp-internal-context baidu-olime-jp-internal-context-rec-spec)
(define baidu-olime-jp-internal-context-new-internal baidu-olime-jp-internal-context-new)

(define (baidu-olime-jp-conversion str opts)
  (define (fromconv str)
    (iconv-convert "UTF-8" "EUC-JP" str))
  (define (toconv str)
    (iconv-convert "EUC-JP" "UTF-8" str))
  (define (make-query)
    (format "/py?ol=1&web=1&py=~a~a"
            (http:encode-uri-string (fromconv str)) opts))
  (define (parse str)
    (receive (cars cdrs)
        (unzip2 (call-with-input-string
                 str
                 (lambda (port)
                   (car (json-read port)))))
      (cons (map toconv cars)
            (map (lambda (x) (map toconv x)) cdrs))))
  (let* ((proxy (make-http-proxy-from-custom))
         (ssl (make-http-ssl (SSLv3-client-method) 443))
         (ret (http:get baidu-olime-jp-server (make-query) 80 proxy ssl)))
    (parse ret)))

(define (baidu-olime-jp-predict bdc str)
  (predict-meta-search
   (baidu-olime-context-prediction-ctx bdc)
   str))

(define (baidu-olime-jp-conversion-make-resize-query yomi-seg)
  (let ((len (length yomi-seg)))
    (apply string-append (map (lambda (idx)
                                (if (= (+ idx 1) len)
                                    (list-ref yomi-seg idx)
                                    (string-append (list-ref yomi-seg idx) ",")))
                              (iota len)))))
(define (baidu-olime-jp-conversion-resize yomi-seg)
  (baidu-olime-jp-conversion
   (baidu-olime-jp-conversion-make-resize-query yomi-seg) ""))

(define (baidu-olime-jp-lib-init)
  #t)
(define (baidu-olime-jp-lib-alloc-context)
  (baidu-olime-jp-internal-context-new-internal))
(define (baidu-olime-jp-lib-get-nth-candidate bdc seg nth)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cand (baidu-olime-jp-internal-context-candidates bdx-ctx)))
    (list-ref (list-ref cand seg) nth)))
(define (baidu-olime-jp-lib-release-context bdc)
  #t)
(define (baidu-olime-jp-lib-get-unconv-candidate bdc seg-idx)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cand (baidu-olime-jp-internal-context-candidates bdx-ctx)))
    ;; XXX
    (car (take-right (list-ref cand seg-idx) 1))))
(define (baidu-olime-jp-lib-get-nr-segments bdc)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cand (baidu-olime-jp-internal-context-candidates bdx-ctx)))
    (length cand)))
(define (baidu-olime-jp-lib-get-nr-candidates bdc seg)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cand (baidu-olime-jp-internal-context-candidates bdx-ctx)))
    (length (list-ref cand seg))))
(define (baidu-olime-jp-next-yomi-seg yomi-seg seg cnt)
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
(define (baidu-olime-jp-lib-resize-segment bdc seg cnt)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cand (baidu-olime-jp-internal-context-candidates bdx-ctx))
         (yomi-seg (baidu-olime-jp-internal-context-yomi-seg bdx-ctx))
         (next-yomi-seg (baidu-olime-jp-next-yomi-seg yomi-seg seg cnt))
         (replace-yomi-seg-and-next-cand (baidu-olime-jp-conversion-resize next-yomi-seg))
         (replace-yomi-seg (car replace-yomi-seg-and-next-cand))
         (next-cand        (cdr replace-yomi-seg-and-next-cand)))
    (if (and next-cand
             (not (equal? next-cand cand)))
        (begin
          (baidu-olime-jp-internal-context-set-candidates! bdx-ctx next-cand)
          (baidu-olime-jp-internal-context-set-yomi-seg! bdx-ctx replace-yomi-seg)))
    #t))
(define (baidu-olime-jp-lib-begin-conversion bdc str)
  (let* ((yomi-seg-and-cand (baidu-olime-jp-conversion str ""))
         (yomi-seg (car yomi-seg-and-cand))
         (cand (cdr yomi-seg-and-cand))
         (bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc)))
    (baidu-olime-jp-internal-context-set-yomi-seg! bdx-ctx yomi-seg)
    (baidu-olime-jp-internal-context-set-candidates! bdx-ctx cand)
    (length cand)))
(define (baidu-olime-jp-lib-commit-segments bdc delta)
  #t)
(define (baidu-olime-jp-lib-reset-conversion bdc)
  #f)
(define (baidu-olime-jp-lib-set-prediction-src-string bdc str)
  (let* ((ret (predict-meta-search
               (baidu-olime-jp-context-prediction-ctx bdc)
               str))
         (bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (word     (predict-meta-word? ret))
         (cands    (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret)))
    (baidu-olime-jp-internal-context-set-prediction-word! bdx-ctx word)
    (baidu-olime-jp-internal-context-set-prediction-candidates! bdx-ctx cands)
    (baidu-olime-jp-internal-context-set-prediction-appendix! bdx-ctx appendix)
    (baidu-olime-jp-internal-context-set-prediction-nr! bdx-ctx (length cands))
    #f))
(define (baidu-olime-jp-lib-get-nr-predictions bdc)
  (let ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc)))
    (baidu-olime-jp-internal-context-prediction-nr bdx-ctx)))
(define (baidu-olime-jp-lib-get-nth-word bdc nth)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (word (baidu-olime-jp-internal-context-prediction-word bdx-ctx)))
    (list-ref word nth)))
(define (baidu-olime-jp-lib-get-nth-prediction bdc nth)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (cands (baidu-olime-jp-internal-context-prediction-candidates bdx-ctx)))
    (list-ref cands nth)))
(define (baidu-olime-jp-lib-get-nth-appendix bdc nth)
  (let* ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
         (appendix (baidu-olime-jp-internal-context-prediction-appendix bdx-ctx)))
    (list-ref appendix nth)))
(define (baidu-olime-jp-lib-commit-nth-prediction bdc nth)
  (let ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc)))
    (predict-meta-commit
     (baidu-olime-jp-context-prediction-ctx bdc)
     (baidu-olime-jp-lib-get-nth-word bdc nth)
     (baidu-olime-jp-lib-get-nth-prediction bdc nth)
     (baidu-olime-jp-lib-get-nth-appendix bdc nth))
    #f))

(define baidu-olime-jp-init-lib-ok? #f)

(define baidu-olime-jp-type-direct	   ja-type-direct)
(define baidu-olime-jp-type-hiragana	   ja-type-hiragana)
(define baidu-olime-jp-type-katakana	   ja-type-katakana)
(define baidu-olime-jp-type-halfkana	   ja-type-halfkana)
(define baidu-olime-jp-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define baidu-olime-jp-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define baidu-olime-jp-input-rule-roma 0)
(define baidu-olime-jp-input-rule-kana 1)
(define baidu-olime-jp-input-rule-azik 2)
(define baidu-olime-jp-input-rule-act 3)
(define baidu-olime-jp-input-rule-kzik 4)

(define baidu-olime-jp-candidate-type-katakana -2)
(define baidu-olime-jp-candidate-type-hiragana -3)
(define baidu-olime-jp-candidate-type-halfkana -4)
(define baidu-olime-jp-candidate-type-halfwidth-alnum -5)
(define baidu-olime-jp-candidate-type-fullwidth-alnum -6)
(define baidu-olime-jp-candidate-type-upper-halfwidth-alnum -7)
(define baidu-olime-jp-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key baidu-olime-jp-space-key? '(" "))

(define baidu-olime-jp-prepare-input-rule-activation
  (lambda (bdc)
    (cond
     ((baidu-olime-jp-context-state bdc)
      (baidu-olime-jp-do-commit bdc))
     ((baidu-olime-jp-context-transposing bdc)
      (im-commit bdc (baidu-olime-jp-transposing-text bdc)))
     ((and
       (baidu-olime-jp-context-on bdc)
       (baidu-olime-jp-has-preedit? bdc))
      (im-commit
       bdc (baidu-olime-jp-make-whole-string bdc #t (baidu-olime-jp-context-kana-mode bdc)))))
    (baidu-olime-jp-flush bdc)
    (baidu-olime-jp-update-preedit bdc)))

(define baidu-olime-jp-prepare-input-mode-activation
  (lambda (bdc new-mode)
    (let ((old-kana (baidu-olime-jp-context-kana-mode bdc)))
      (cond
       ((baidu-olime-jp-context-state bdc)
	(baidu-olime-jp-do-commit bdc))
       ((baidu-olime-jp-context-transposing bdc)
	(im-commit bdc (baidu-olime-jp-transposing-text bdc))
	(baidu-olime-jp-flush bdc))
       ((and
	 (baidu-olime-jp-context-on bdc)
	 (baidu-olime-jp-has-preedit? bdc)
	 (not (= old-kana new-mode)))
	(im-commit
	 bdc (baidu-olime-jp-make-whole-string bdc #t (baidu-olime-jp-context-kana-mode bdc)))
	(baidu-olime-jp-flush bdc)))
      (baidu-olime-jp-update-preedit bdc))))

(register-action 'action_baidu-olime-jp_hiragana
		 (lambda (bdc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (bdc) ;; activity predicate
		   (and (baidu-olime-jp-context-on bdc)
		        (not (baidu-olime-jp-context-alnum bdc))
			(= (baidu-olime-jp-context-kana-mode bdc)
			   baidu-olime-jp-type-hiragana)))

		 (lambda (bdc) ;; action handler
		   (baidu-olime-jp-prepare-input-mode-activation bdc baidu-olime-jp-type-hiragana)
		   (baidu-olime-jp-context-set-on! bdc #t)
		   (baidu-olime-jp-context-set-alnum! bdc #f)
		   (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-hiragana)))

(register-action 'action_baidu-olime-jp_katakana
		 (lambda (bdc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (bdc)
		   (and (baidu-olime-jp-context-on bdc)
		        (not (baidu-olime-jp-context-alnum bdc))
			(= (baidu-olime-jp-context-kana-mode bdc)
			   baidu-olime-jp-type-katakana)))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-mode-activation bdc baidu-olime-jp-type-katakana)
		   (baidu-olime-jp-context-set-on! bdc #t)
		   (baidu-olime-jp-context-set-alnum! bdc #f)
		   (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-katakana)))

(register-action 'action_baidu-olime-jp_halfkana
		 (lambda (bdc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (bdc)
		   (and (baidu-olime-jp-context-on bdc)
			(not (baidu-olime-jp-context-alnum bdc))
			(= (baidu-olime-jp-context-kana-mode bdc) baidu-olime-jp-type-halfkana)))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-mode-activation bdc baidu-olime-jp-type-halfkana)
		   (baidu-olime-jp-context-set-on! bdc #t)
		   (baidu-olime-jp-context-set-alnum! bdc #f)
		   (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-halfkana)))

(register-action 'action_baidu-olime-jp_halfwidth_alnum
		 (lambda (bdc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (bdc) ;; activity predicate
		   (and (baidu-olime-jp-context-on bdc)
			(baidu-olime-jp-context-alnum bdc)
			(= (baidu-olime-jp-context-alnum-type bdc)
			   baidu-olime-jp-type-halfwidth-alnum)))
		 (lambda (bdc) ;; action handler
		   (baidu-olime-jp-prepare-input-mode-activation
		    bdc (baidu-olime-jp-context-kana-mode bdc))
		   (baidu-olime-jp-context-set-on! bdc #t)
		   (baidu-olime-jp-context-set-alnum! bdc #t)
		   (baidu-olime-jp-context-set-alnum-type!
		    bdc baidu-olime-jp-type-halfwidth-alnum)))

(register-action 'action_baidu-olime-jp_direct
		 (lambda (bdc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (bdc)
		   (not (baidu-olime-jp-context-on bdc)))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-mode-activation bdc baidu-olime-jp-type-direct)
		   (baidu-olime-jp-context-set-on! bdc #f)))

(register-action 'action_baidu-olime-jp_fullwidth_alnum
		 (lambda (bdc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (bdc)
		   (and (baidu-olime-jp-context-on bdc)
			(baidu-olime-jp-context-alnum bdc)
			(= (baidu-olime-jp-context-alnum-type bdc)
			   baidu-olime-jp-type-fullwidth-alnum)))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-mode-activation
		    bdc (baidu-olime-jp-context-kana-mode bdc))
		   (baidu-olime-jp-context-set-on! bdc #t)
		   (baidu-olime-jp-context-set-alnum! bdc #t)
		   (baidu-olime-jp-context-set-alnum-type!
		    bdc baidu-olime-jp-type-fullwidth-alnum)))

(register-action 'action_baidu-olime-jp_roma
		 (lambda (bdc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (bdc)
		   (= (baidu-olime-jp-context-input-rule bdc)
		      baidu-olime-jp-input-rule-roma))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-rule-activation bdc)
		   (rk-context-set-rule! (baidu-olime-jp-context-rkc bdc)
					 ja-rk-rule)
		   (baidu-olime-jp-context-set-input-rule! bdc baidu-olime-jp-input-rule-roma)))

(register-action 'action_baidu-olime-jp_kana
		 (lambda (bdc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (bdc)
		   (= (baidu-olime-jp-context-input-rule bdc)
		      baidu-olime-jp-input-rule-kana))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-rule-activation bdc)
                   (require "japanese-kana.scm")
		   (baidu-olime-jp-context-set-input-rule! bdc baidu-olime-jp-input-rule-kana)
                   (baidu-olime-jp-context-change-kana-mode!
                     bdc (baidu-olime-jp-context-kana-mode bdc))
		   (baidu-olime-jp-context-set-alnum! bdc #f)))

(register-action 'action_baidu-olime-jp_azik
		 (lambda (bdc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (bdc)
		   (= (baidu-olime-jp-context-input-rule bdc)
		      baidu-olime-jp-input-rule-azik))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-rule-activation bdc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (baidu-olime-jp-context-rkc bdc)
					 ja-azik-rule)
		   (baidu-olime-jp-context-set-input-rule! bdc baidu-olime-jp-input-rule-azik)))

(register-action 'action_baidu-olime-jp_kzik
		 (lambda (bdc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (bdc)
		   (= (baidu-olime-jp-context-input-rule bdc)
		      baidu-olime-jp-input-rule-kzik))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-rule-activation bdc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (baidu-olime-jp-context-rkc bdc)
					 ja-kzik-rule)
		   (baidu-olime-jp-context-set-input-rule! bdc baidu-olime-jp-input-rule-kzik)))

(register-action 'action_baidu-olime-jp_act
		 (lambda (bdc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (bdc)
		   (= (baidu-olime-jp-context-input-rule bdc)
		      baidu-olime-jp-input-rule-act))
		 (lambda (bdc)
		   (baidu-olime-jp-prepare-input-rule-activation bdc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (baidu-olime-jp-context-rkc bdc)
					 ja-act-rule)
		   (baidu-olime-jp-context-set-input-rule! bdc baidu-olime-jp-input-rule-act)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define baidu-olime-jp-configure-widgets
  (lambda ()
    (register-widget 'widget_baidu-olime-jp_input_mode
		     (activity-indicator-new baidu-olime-jp-input-mode-actions)
		     (actions-new baidu-olime-jp-input-mode-actions))

    (register-widget 'widget_baidu-olime-jp_kana_input_method
		     (activity-indicator-new baidu-olime-jp-kana-input-method-actions)
		     (actions-new baidu-olime-jp-kana-input-method-actions))
    (context-list-replace-widgets! 'baidu-olime-jp baidu-olime-jp-widgets)))

(define baidu-olime-jp-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'bdx-ctx             ()) ;; baidu-olime-jp-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'prediction-ctx     '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '())
    (list 'kana-mode          baidu-olime-jp-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      baidu-olime-jp-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         baidu-olime-jp-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'baidu-olime-jp-context baidu-olime-jp-context-rec-spec)
(define baidu-olime-jp-context-new-internal baidu-olime-jp-context-new)

(define (baidu-olime-jp-context-new id im)
  (let ((bdc (baidu-olime-jp-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (baidu-olime-jp-context-set-bdx-ctx! bdc (if baidu-olime-jp-init-lib-ok?
;				      (baidu-olime-jp-lib-alloc-context) ()))
    (baidu-olime-jp-context-set-bdx-ctx! bdc (baidu-olime-jp-lib-alloc-context))
    (baidu-olime-jp-context-set-widgets! bdc baidu-olime-jp-widgets)
    (baidu-olime-jp-context-set-rkc! bdc rkc)
    (baidu-olime-jp-context-set-preconv-ustr! bdc (ustr-new '()))
    (baidu-olime-jp-context-set-raw-ustr! bdc (ustr-new '()))
    (baidu-olime-jp-context-set-segments! bdc (ustr-new '()))
    (if baidu-olime-jp-use-prediction?
        (begin
          (baidu-olime-jp-context-set-prediction-ctx! bdc (predict-make-meta-search))
          (predict-meta-open (baidu-olime-jp-context-prediction-ctx bdc) "baidu-olime-jp")
          (predict-meta-set-external-charset! (baidu-olime-jp-context-prediction-ctx bdc) "UTF-8")))
    bdc))

(define (baidu-olime-jp-commit-raw bdc)
  (im-commit-raw bdc)
  (baidu-olime-jp-context-set-commit-raw! bdc #t))

(define (baidu-olime-jp-context-kana-toggle bdc)
  (let* ((kana (baidu-olime-jp-context-kana-mode bdc))
	 (opposite-kana (ja-opposite-kana kana)))
    (baidu-olime-jp-context-change-kana-mode! bdc opposite-kana)))

(define baidu-olime-jp-context-alkana-toggle
  (lambda (bdc)
    (let ((alnum-state (baidu-olime-jp-context-alnum bdc)))
      (baidu-olime-jp-context-set-alnum! bdc (not alnum-state)))))

(define baidu-olime-jp-context-change-kana-mode!
  (lambda (bdc kana-mode)
    (if (= (baidu-olime-jp-context-input-rule bdc)
           baidu-olime-jp-input-rule-kana)
        (rk-context-set-rule!
	 (baidu-olime-jp-context-rkc bdc)
	 (cond
	  ((= kana-mode baidu-olime-jp-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode baidu-olime-jp-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode baidu-olime-jp-type-halfkana) ja-kana-halfkana-rule))))
    (baidu-olime-jp-context-set-kana-mode! bdc kana-mode)))

(define baidu-olime-jp-make-whole-string
  (lambda (bdc convert-pending-into-kana? kana)
    (let* ((rkc (baidu-olime-jp-context-rkc bdc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (baidu-olime-jp-context-input-rule bdc))
           (preconv-str (baidu-olime-jp-context-preconv-ustr bdc))
           (extract-kana
            (if (= rule baidu-olime-jp-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule baidu-olime-jp-input-rule-kana)
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

(define baidu-olime-jp-make-raw-string
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
	     (baidu-olime-jp-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (baidu-olime-jp-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define baidu-olime-jp-make-whole-raw-string
  (lambda (bdc wide? upper?)
    (baidu-olime-jp-make-raw-string (baidu-olime-jp-get-raw-str-seq bdc) wide? upper?)))

(define (baidu-olime-jp-init-handler id im arg)
  (if (not baidu-olime-jp-init-lib-ok?)
      (begin
	(baidu-olime-jp-lib-init)
	(set! baidu-olime-jp-init-lib-ok? #t)))
  (baidu-olime-jp-context-new id im))

(define (baidu-olime-jp-release-handler bdc)
  (if bdc
      (baidu-olime-jp-lib-release-context bdc)))

(define (baidu-olime-jp-flush bdc)
  (rk-flush (baidu-olime-jp-context-rkc bdc))
  (ustr-clear! (baidu-olime-jp-context-preconv-ustr bdc))
  (ustr-clear! (baidu-olime-jp-context-raw-ustr bdc))
  (ustr-clear! (baidu-olime-jp-context-segments bdc))
  (baidu-olime-jp-context-set-transposing! bdc #f)
  (baidu-olime-jp-context-set-state! bdc #f)
  (if (or
       (baidu-olime-jp-context-candidate-window bdc)
       (baidu-olime-jp-context-prediction-window bdc))
      (im-deactivate-candidate-selector bdc))
  (baidu-olime-jp-context-set-candidate-window! bdc #f)
  (baidu-olime-jp-context-set-prediction-window! bdc #f)
  (baidu-olime-jp-context-set-candidate-op-count! bdc 0))

(define (baidu-olime-jp-begin-input bdc key key-state)
  (if (cond
       ((baidu-olime-jp-on-key? key key-state)
	#t)
       ((and
	 baidu-olime-jp-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((baidu-olime-jp-hiragana-key? key key-state)
	   (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-hiragana)
	   (baidu-olime-jp-context-set-alnum! bdc #f)
	   #t)
	  ((baidu-olime-jp-katakana-key? key key-state)
	   (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-katakana)
	   (baidu-olime-jp-context-set-alnum! bdc #f)
	   #t)
	  ((baidu-olime-jp-halfkana-key? key key-state)
	   (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-halfkana)
	   (baidu-olime-jp-context-set-alnum! bdc #f)
	   #t)
	  ((baidu-olime-jp-halfwidth-alnum-key? key key-state)
	   (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-halfwidth-alnum)
	   (baidu-olime-jp-context-set-alnum! bdc #t)
	   #t)
	  ((baidu-olime-jp-halfwidth-alnum-key? key key-state)
	   (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-fullwidth-alnum)
	   (baidu-olime-jp-context-set-alnum! bdc #t)
	   #t)
	  ((baidu-olime-jp-kana-toggle-key? key key-state)
	   (baidu-olime-jp-context-kana-toggle bdc)
	   (baidu-olime-jp-context-set-alnum! bdc #f)
	   #t)
	  ((baidu-olime-jp-alkana-toggle-key? key key-state)
	   (baidu-olime-jp-context-alkana-toggle bdc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(baidu-olime-jp-context-set-on! bdc #t)
	(rk-flush (baidu-olime-jp-context-rkc bdc))
	(baidu-olime-jp-context-set-state! bdc #f)
	#t)
      #f))

(define (baidu-olime-jp-update-preedit bdc)
  (if (not (baidu-olime-jp-context-commit-raw bdc))
      (let ((segments (if (baidu-olime-jp-context-on bdc)
			  (if (baidu-olime-jp-context-transposing bdc)
			      (baidu-olime-jp-context-transposing-state-preedit bdc)
			      (if (baidu-olime-jp-context-state bdc)
				  (baidu-olime-jp-compose-state-preedit bdc)
                                  (if (baidu-olime-jp-context-predicting bdc)
                                      (baidu-olime-jp-predicting-state-preedit bdc)
                                      (baidu-olime-jp-input-state-preedit bdc))))
			  ())))
	(context-update-preedit bdc segments))
      (baidu-olime-jp-context-set-commit-raw! bdc #f)))

(define (baidu-olime-jp-begin-conv bdc)
  (let ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
	(preconv-str (baidu-olime-jp-make-whole-string bdc #t baidu-olime-jp-type-hiragana)))
    (if (and bdx-ctx
             (> (string-length preconv-str) 0))
	(let ((num (baidu-olime-jp-lib-begin-conversion bdc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (baidu-olime-jp-context-segments bdc)
		 (make-list num 0))
		(baidu-olime-jp-context-set-state! bdc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; baidu-olime-jp-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define baidu-olime-jp-cancel-conv
  (lambda (bdc)
    (baidu-olime-jp-reset-candidate-window bdc)
    (baidu-olime-jp-context-set-state! bdc #f)
    (ustr-clear! (baidu-olime-jp-context-segments bdc))
    (baidu-olime-jp-lib-reset-conversion bdc)))

(define (baidu-olime-jp-proc-input-state-no-preedit bdc key key-state)
  (let
      ((rkc (baidu-olime-jp-context-rkc bdc))
       (direct (ja-direct (charcode->string key)))
       (rule (baidu-olime-jp-context-input-rule bdc)))
    (cond
     ((and baidu-olime-jp-use-with-vi?
           (baidu-olime-jp-vi-escape-key? key key-state))
      (baidu-olime-jp-flush bdc)
      (baidu-olime-jp-context-set-on! bdc #f)
      (baidu-olime-jp-commit-raw bdc))

     ((baidu-olime-jp-off-key? key key-state)
      (baidu-olime-jp-flush bdc)
      (baidu-olime-jp-context-set-on! bdc #f))

     ((baidu-olime-jp-backspace-key? key key-state)
      (baidu-olime-jp-commit-raw bdc))
     
     ((baidu-olime-jp-delete-key? key key-state)
      (baidu-olime-jp-commit-raw bdc))

     ((and
       (baidu-olime-jp-hiragana-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-kana-mode bdc) baidu-olime-jp-type-hiragana)
	 (not (baidu-olime-jp-context-alnum bdc)))))
      (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-hiragana)
      (baidu-olime-jp-context-set-alnum! bdc #f))

     ((and
       (baidu-olime-jp-katakana-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-kana-mode bdc) baidu-olime-jp-type-katakana)
	 (not (baidu-olime-jp-context-alnum bdc)))))
      (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-katakana)
      (baidu-olime-jp-context-set-alnum! bdc #f))
     
     ((and
       (baidu-olime-jp-halfkana-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-kana-mode bdc) baidu-olime-jp-type-halfkana)
	 (not (baidu-olime-jp-context-alnum bdc)))))
      (baidu-olime-jp-context-change-kana-mode! bdc baidu-olime-jp-type-halfkana)
      (baidu-olime-jp-context-set-alnum! bdc #f))
     
     ((and
       (baidu-olime-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-alnum-type bdc) baidu-olime-jp-type-halfwidth-alnum)
	 (baidu-olime-jp-context-alnum bdc))))
      (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-halfwidth-alnum)
      (baidu-olime-jp-context-set-alnum! bdc #t))
     
     ((and
       (baidu-olime-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-alnum-type bdc) baidu-olime-jp-type-fullwidth-alnum)
	 (baidu-olime-jp-context-alnum bdc))))
      (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-fullwidth-alnum)
      (baidu-olime-jp-context-set-alnum! bdc #t))
     
     ((and
       (not (baidu-olime-jp-context-alnum bdc))
       (baidu-olime-jp-kana-toggle-key? key key-state))
      (baidu-olime-jp-context-kana-toggle bdc))

     ((baidu-olime-jp-alkana-toggle-key? key key-state)
      (baidu-olime-jp-context-alkana-toggle bdc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (baidu-olime-jp-commit-raw bdc))
     
     ;; direct key => commit
     (direct
      (im-commit bdc direct))

     ;; space key
     ((baidu-olime-jp-space-key? key key-state)
      (if (baidu-olime-jp-context-alnum bdc)
	  (im-commit bdc (list-ref
			 ja-alnum-space
			 (- (baidu-olime-jp-context-alnum-type bdc)
			    baidu-olime-jp-type-halfwidth-alnum)))
	  (im-commit bdc (list-ref ja-space (baidu-olime-jp-context-kana-mode bdc)))))

     ((symbol? key)
      (baidu-olime-jp-commit-raw bdc))

     (else
      (if (baidu-olime-jp-context-alnum bdc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (baidu-olime-jp-context-preconv-ustr bdc)
			       (if (= (baidu-olime-jp-context-alnum-type bdc)
				      baidu-olime-jp-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (baidu-olime-jp-context-raw-ustr bdc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule baidu-olime-jp-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (baidu-olime-jp-context-preconv-ustr bdc) res)
                    (ustr-insert-elem! (baidu-olime-jp-context-preconv-ustr bdc) res))
	          (ustr-insert-elem! (baidu-olime-jp-context-raw-ustr bdc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (baidu-olime-jp-commit-raw bdc)))))))))

(define (baidu-olime-jp-has-preedit? bdc)
  (or (not (ustr-empty? (baidu-olime-jp-context-preconv-ustr bdc)))
      (> (string-length (rk-pending (baidu-olime-jp-context-rkc bdc))) 0)))

(define baidu-olime-jp-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type baidu-olime-jp-type-halfwidth-alnum)
       (= state baidu-olime-jp-type-halfwidth-alnum))
      baidu-olime-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type baidu-olime-jp-type-fullwidth-alnum)
       (= state baidu-olime-jp-type-fullwidth-alnum))
      baidu-olime-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define baidu-olime-jp-proc-transposing-state
  (lambda (bdc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (baidu-olime-jp-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons baidu-olime-jp-type-fullwidth-alnum rotate-list)))
      (if (baidu-olime-jp-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons baidu-olime-jp-type-halfwidth-alnum rotate-list)))
      (if (baidu-olime-jp-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons baidu-olime-jp-type-halfkana rotate-list)))
      (if (baidu-olime-jp-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons baidu-olime-jp-type-katakana rotate-list)))
      (if (baidu-olime-jp-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons baidu-olime-jp-type-hiragana rotate-list)))

      (if (baidu-olime-jp-context-transposing bdc)
	  (let ((lst (member (baidu-olime-jp-context-transposing-type bdc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (baidu-olime-jp-rotate-transposing-alnum-type
				 (baidu-olime-jp-context-transposing-type bdc)
				 (car rotate-list))))))
	  (begin
	    (baidu-olime-jp-context-set-transposing! bdc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state baidu-olime-jp-type-hiragana)
	      (= state baidu-olime-jp-type-katakana)
	      (= state baidu-olime-jp-type-halfkana)))
	(baidu-olime-jp-context-set-transposing-type! bdc state))
       ((and state
	     (or
	      (= state baidu-olime-jp-type-halfwidth-alnum)
	      (= state baidu-olime-jp-candidate-type-upper-halfwidth-alnum)
	      (= state baidu-olime-jp-type-fullwidth-alnum)
	      (= state baidu-olime-jp-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (baidu-olime-jp-context-input-rule bdc) baidu-olime-jp-input-rule-kana))
	    (baidu-olime-jp-context-set-transposing-type! bdc state)))
       (else
	(and
	 ; commit
	 (if (baidu-olime-jp-commit-key? key key-state)
	     (begin
	       (im-commit bdc (baidu-olime-jp-transposing-text bdc))
	       (baidu-olime-jp-flush bdc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (baidu-olime-jp-begin-conv-key? key key-state)
	     (begin
	       (baidu-olime-jp-context-set-transposing! bdc #f)
	       (baidu-olime-jp-begin-conv bdc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (baidu-olime-jp-cancel-key? key key-state)
	      (baidu-olime-jp-backspace-key? key key-state))
	     (begin
	       (baidu-olime-jp-context-set-transposing! bdc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (baidu-olime-jp-prev-page-key? key key-state)
	      (baidu-olime-jp-next-page-key? key key-state)
	      (baidu-olime-jp-extend-segment-key? key key-state)
	      (baidu-olime-jp-shrink-segment-key? key key-state)
	      (baidu-olime-jp-next-segment-key? key key-state)
	      (baidu-olime-jp-beginning-of-preedit-key? key key-state)
	      (baidu-olime-jp-end-of-preedit-key? key key-state)
	      (baidu-olime-jp-next-candidate-key? key key-state)
	      (baidu-olime-jp-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit bdc (baidu-olime-jp-transposing-text bdc))
	   (baidu-olime-jp-flush bdc)
	   (baidu-olime-jp-proc-input-state bdc key key-state))))))))

(define (baidu-olime-jp-move-prediction bdc offset)
  (let* ((nr (baidu-olime-jp-lib-get-nr-predictions bdc))
         (idx (baidu-olime-jp-context-prediction-index bdc))
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
    (im-select-candidate bdc compensated-n)
    (baidu-olime-jp-context-set-prediction-index! bdc compensated-n)))

(define (baidu-olime-jp-move-prediction-in-page bdc numeralc)
  (let* ((nr (baidu-olime-jp-lib-get-nr-predictions bdc))
	 (p-idx (baidu-olime-jp-context-prediction-index bdc))
	 (n (if (not p-idx)
		0
		p-idx))
	 (cur-page (if (= baidu-olime-jp-nr-candidate-max 0)
		       0
		       (quotient n baidu-olime-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page baidu-olime-jp-nr-candidate-max) compensated-pageidx))
	 (compensated-idx (cond
			   ((>= idx nr)
			    #f)
			   (else
			    idx)))
	 (selected-pageidx (if (not p-idx)
			       #f
			       (if (= baidu-olime-jp-nr-candidate-max 0)
				   p-idx
				   (remainder p-idx
					      baidu-olime-jp-nr-candidate-max)))))
    (if (and
	 compensated-idx
	 (not (eqv? compensated-pageidx selected-pageidx)))
	(begin
	  (baidu-olime-jp-context-set-prediction-index! bdc compensated-idx)
	  (im-select-candidate bdc compensated-idx)
	  #t)
	#f)))

(define (baidu-olime-jp-prediction-select-non-existing-index? bdc numeralc)
  (let* ((nr (baidu-olime-jp-lib-get-nr-predictions bdc))
	 (p-idx (baidu-olime-jp-context-prediction-index bdc))
	 (cur-page (if (= baidu-olime-jp-nr-candidate-max 0)
		       0
		       (quotient p-idx baidu-olime-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page baidu-olime-jp-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (baidu-olime-jp-prediction-keys-handled? bdc key key-state)
  (cond
   ((baidu-olime-jp-next-prediction-key? key key-state)
    (baidu-olime-jp-move-prediction bdc 1)
    #t)
   ((baidu-olime-jp-prev-prediction-key? key key-state)
    (baidu-olime-jp-move-prediction bdc -1)
    #t)
   ((and
     baidu-olime-jp-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (baidu-olime-jp-move-prediction-in-page bdc key))
   ((and
     (baidu-olime-jp-context-prediction-index bdc)
     (baidu-olime-jp-prev-page-key? key key-state))
    (im-shift-page-candidate bdc #f)
    #t)
   ((and
     (baidu-olime-jp-context-prediction-index bdc)
     (baidu-olime-jp-next-page-key? key key-state))
    (im-shift-page-candidate bdc #t)
    #t)
   (else
    #f)))

(define (baidu-olime-jp-proc-prediction-state bdc key key-state)
  (cond
   ;; prediction index change
   ((baidu-olime-jp-prediction-keys-handled? bdc key key-state))

   ;; cancel
   ((baidu-olime-jp-cancel-key? key key-state)
    (if (baidu-olime-jp-context-prediction-index bdc)
	(baidu-olime-jp-reset-prediction-window bdc)
	(begin
	  (baidu-olime-jp-reset-prediction-window bdc)
	  (baidu-olime-jp-proc-input-state bdc key key-state))))

   ;; commit
   ((and
     (baidu-olime-jp-context-prediction-index bdc)
     (baidu-olime-jp-commit-key? key key-state))
    (baidu-olime-jp-do-commit-prediction bdc))
   (else
    (if (and
	 baidu-olime-jp-use-implicit-commit-prediction?
	 (baidu-olime-jp-context-prediction-index bdc))
	(cond
	 ((or
	   ;; check keys used in baidu-olime-jp-proc-input-state-with-preedit
	   (baidu-olime-jp-begin-conv-key? key key-state)
	   (baidu-olime-jp-backspace-key? key key-state)
	   (baidu-olime-jp-delete-key? key key-state)
	   (baidu-olime-jp-kill-key? key key-state)
	   (baidu-olime-jp-kill-backward-key? key key-state)
	   (and
	    (not (baidu-olime-jp-context-alnum bdc))
	    (baidu-olime-jp-commit-as-opposite-kana-key? key key-state))
	   (baidu-olime-jp-transpose-as-hiragana-key? key key-state)
	   (baidu-olime-jp-transpose-as-katakana-key? key key-state)
	   (baidu-olime-jp-transpose-as-halfkana-key? key key-state)
	   (and
	    (not (= (baidu-olime-jp-context-input-rule bdc) baidu-olime-jp-input-rule-kana))
	    (or
	     (baidu-olime-jp-transpose-as-halfwidth-alnum-key? key key-state)
	     (baidu-olime-jp-transpose-as-fullwidth-alnum-key? key key-state)))
	   (baidu-olime-jp-hiragana-key? key key-state)
	   (baidu-olime-jp-katakana-key? key key-state)
	   (baidu-olime-jp-halfkana-key? key key-state)
	   (baidu-olime-jp-halfwidth-alnum-key? key key-state)
	   (baidu-olime-jp-fullwidth-alnum-key? key key-state)
	   (and
	    (not (baidu-olime-jp-context-alnum bdc))
	    (baidu-olime-jp-kana-toggle-key? key key-state))
	   (baidu-olime-jp-alkana-toggle-key? key key-state)
	   (baidu-olime-jp-go-left-key? key key-state)
	   (baidu-olime-jp-go-right-key? key key-state)
	   (baidu-olime-jp-beginning-of-preedit-key? key key-state)
	   (baidu-olime-jp-end-of-preedit-key? key key-state)
	   (and
	    (modifier-key-mask key-state)
	    (not (shift-key-mask key-state))))
	  ;; go back to unselected prediction
	  (baidu-olime-jp-reset-prediction-window bdc)
	  (baidu-olime-jp-check-prediction bdc #f))
	 ((and
	   (ichar-numeric? key)
	   baidu-olime-jp-select-prediction-by-numeral-key?
	   (not (baidu-olime-jp-prediction-select-non-existing-index? bdc key)))
	  (baidu-olime-jp-context-set-predicting! bdc #f)
	  (baidu-olime-jp-context-set-prediction-index! bdc #f)
	  (baidu-olime-jp-proc-input-state bdc key key-state))
	 (else
	  ;; implicit commit
	  (baidu-olime-jp-do-commit-prediction bdc)
	  (baidu-olime-jp-proc-input-state bdc key key-state)))
	(begin
	  (baidu-olime-jp-context-set-predicting! bdc #f)
	  (baidu-olime-jp-context-set-prediction-index! bdc #f)
	  (if (not baidu-olime-jp-use-prediction?)
	      (baidu-olime-jp-reset-prediction-window bdc))
	  (baidu-olime-jp-proc-input-state bdc key key-state))))))

(define (baidu-olime-jp-proc-input-state-with-preedit bdc key key-state)
  (define (check-auto-conv str)
    (and
      str
      baidu-olime-jp-auto-start-henkan?
      (string-find japanese-auto-start-henkan-keyword-list str)
      (begin
	(baidu-olime-jp-reset-prediction-window bdc)
	(baidu-olime-jp-begin-conv bdc))))
  (let ((preconv-str (baidu-olime-jp-context-preconv-ustr bdc))
	(raw-str (baidu-olime-jp-context-raw-ustr bdc))
	(rkc (baidu-olime-jp-context-rkc bdc))
	(rule (baidu-olime-jp-context-input-rule bdc))
	(kana (baidu-olime-jp-context-kana-mode bdc)))
    (cond
     ;; begin conversion
     ((baidu-olime-jp-begin-conv-key? key key-state)
      (baidu-olime-jp-begin-conv bdc))

     ;; prediction
     ((baidu-olime-jp-next-prediction-key? key key-state)
      (baidu-olime-jp-check-prediction bdc #t))

     ;; backspace
     ((baidu-olime-jp-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (baidu-olime-jp-context-input-rule bdc) baidu-olime-jp-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((baidu-olime-jp-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((baidu-olime-jp-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((baidu-olime-jp-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (baidu-olime-jp-context-alnum bdc))
       (baidu-olime-jp-commit-as-opposite-kana-key? key key-state))
      (im-commit bdc (baidu-olime-jp-make-whole-string bdc #t (ja-opposite-kana kana)))
      (baidu-olime-jp-flush bdc))

     ;; Transposing状態へ移行
     ((or (baidu-olime-jp-transpose-as-hiragana-key? key key-state)
	  (baidu-olime-jp-transpose-as-katakana-key? key key-state)
	  (baidu-olime-jp-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (baidu-olime-jp-context-input-rule bdc) baidu-olime-jp-input-rule-kana))
	   (or
	    (baidu-olime-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (baidu-olime-jp-transpose-as-fullwidth-alnum-key? key key-state))))
      (baidu-olime-jp-reset-prediction-window bdc)
      (baidu-olime-jp-proc-transposing-state bdc key key-state))

     ((baidu-olime-jp-hiragana-key? key key-state)
      (if (not (= kana baidu-olime-jp-type-hiragana))
	  (begin
	    (im-commit bdc (baidu-olime-jp-make-whole-string bdc #t kana))
	    (baidu-olime-jp-flush bdc)))
      (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-hiragana)
      (baidu-olime-jp-context-set-alnum! bdc #f))

     ((baidu-olime-jp-katakana-key? key key-state)
      (if (not (= kana baidu-olime-jp-type-katakana))
	  (begin
	    (im-commit bdc (baidu-olime-jp-make-whole-string bdc #t kana))
	    (baidu-olime-jp-flush bdc)))
      (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-katakana)
      (baidu-olime-jp-context-set-alnum! bdc #f))

     ((baidu-olime-jp-halfkana-key? key key-state)
      (if (not (= kana baidu-olime-jp-type-halfkana))
	  (begin
	    (im-commit bdc (baidu-olime-jp-make-whole-string bdc #t kana))
	    (baidu-olime-jp-flush bdc)))
      (baidu-olime-jp-context-set-kana-mode! bdc baidu-olime-jp-type-halfkana)
      (baidu-olime-jp-context-set-alnum! bdc #f))

     ((and
       (baidu-olime-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-alnum-type bdc) baidu-olime-jp-type-halfwidth-alnum)
	 (baidu-olime-jp-context-alnum bdc))))
      (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-halfwidth-alnum)
      (baidu-olime-jp-context-set-alnum! bdc #t))

     ((and
       (baidu-olime-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (baidu-olime-jp-context-alnum-type bdc) baidu-olime-jp-type-fullwidth-alnum)
	 (baidu-olime-jp-context-alnum bdc))))
      (baidu-olime-jp-context-set-alnum-type! bdc baidu-olime-jp-type-fullwidth-alnum)
      (baidu-olime-jp-context-set-alnum! bdc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (baidu-olime-jp-context-alnum bdc))
       (baidu-olime-jp-kana-toggle-key? key key-state))
      (im-commit bdc (baidu-olime-jp-make-whole-string bdc #t kana))
      (baidu-olime-jp-flush bdc)
      (baidu-olime-jp-context-kana-toggle bdc))

     ((baidu-olime-jp-alkana-toggle-key? key key-state)
      (baidu-olime-jp-context-alkana-toggle bdc))

     ;; cancel
     ((baidu-olime-jp-cancel-key? key key-state)
      (baidu-olime-jp-flush bdc))

     ;; commit
     ((baidu-olime-jp-commit-key? key key-state)
      (begin
	(im-commit
	 bdc
	 (baidu-olime-jp-make-whole-string bdc #t kana))
	(baidu-olime-jp-flush bdc)))

     ;; left
     ((baidu-olime-jp-go-left-key? key key-state)
      (baidu-olime-jp-context-confirm-kana! bdc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((baidu-olime-jp-go-right-key? key key-state)
      (baidu-olime-jp-context-confirm-kana! bdc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((baidu-olime-jp-beginning-of-preedit-key? key key-state)
      (baidu-olime-jp-context-confirm-kana! bdc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((baidu-olime-jp-end-of-preedit-key? key key-state)
      (baidu-olime-jp-context-confirm-kana! bdc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (if (baidu-olime-jp-context-alnum bdc)
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
			       (if (= (baidu-olime-jp-context-alnum-type bdc)
				      baidu-olime-jp-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str)
	    (check-auto-conv key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule baidu-olime-jp-input-rule-kana)
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

(define baidu-olime-jp-context-confirm-kana!
  (lambda (bdc)
    (if (= (baidu-olime-jp-context-input-rule bdc)
	   baidu-olime-jp-input-rule-kana)
	(let* ((preconv-str (baidu-olime-jp-context-preconv-ustr bdc))
	       (rkc (baidu-olime-jp-context-rkc bdc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (baidu-olime-jp-reset-prediction-window bdc)
  (if (baidu-olime-jp-context-prediction-window bdc)
      (im-deactivate-candidate-selector bdc))
  (baidu-olime-jp-context-set-predicting! bdc #f)
  (baidu-olime-jp-context-set-prediction-window! bdc #f)
  (baidu-olime-jp-context-set-prediction-index! bdc #f))

(define (baidu-olime-jp-check-prediction bdc force-check?)
  (if (and
       (not (baidu-olime-jp-context-state bdc))
       (not (baidu-olime-jp-context-transposing bdc))
       (not (baidu-olime-jp-context-predicting bdc)))
      (let* ((use-pending-rk-for-prediction? #t)
             (preconv-str
              (baidu-olime-jp-make-whole-string
               bdc
               (not use-pending-rk-for-prediction?)
               (baidu-olime-jp-context-kana-mode bdc)))
             (preedit-len (+
                           (ustr-length (baidu-olime-jp-context-preconv-ustr bdc))
                           (if (not use-pending-rk-for-prediction?)
                               0
                               (string-length (rk-pending
                                               (baidu-olime-jp-context-rkc
                                                bdc)))))))
        (if (or
             (>= preedit-len baidu-olime-jp-prediction-start-char-count)
             force-check?)
            (begin
              (baidu-olime-jp-lib-set-prediction-src-string bdc preconv-str)
              (let ((nr (baidu-olime-jp-lib-get-nr-predictions bdc)))
                (if (and
                     nr
                     (> nr 0))
                    (begin
                     (im-activate-candidate-selector
                      bdc nr baidu-olime-jp-nr-candidate-max)
                     (baidu-olime-jp-context-set-prediction-window! bdc #t)
                     (baidu-olime-jp-context-set-predicting! bdc #t))
                    (baidu-olime-jp-reset-prediction-window bdc))))
            (baidu-olime-jp-reset-prediction-window bdc)))))

(define (baidu-olime-jp-proc-input-state bdc key key-state)
  (if (baidu-olime-jp-has-preedit? bdc)
      (baidu-olime-jp-proc-input-state-with-preedit bdc key key-state)
      (baidu-olime-jp-proc-input-state-no-preedit bdc key key-state))
  (if baidu-olime-jp-use-prediction?
      (baidu-olime-jp-check-prediction bdc #f)))

(define baidu-olime-jp-separator
  (lambda (bdc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if baidu-olime-jp-show-segment-separator?
	  (cons attr baidu-olime-jp-segment-separator)
	  #f))))

(define baidu-olime-jp-context-transposing-state-preedit
  (lambda (bdc)
    (let ((transposing-text (baidu-olime-jp-transposing-text bdc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define baidu-olime-jp-transposing-text
  (lambda (bdc)
    (let ((transposing-type (baidu-olime-jp-context-transposing-type bdc)))
      (cond
       ((or
	 (= transposing-type baidu-olime-jp-type-hiragana)
	 (= transposing-type baidu-olime-jp-type-katakana)
	 (= transposing-type baidu-olime-jp-type-halfkana))
	(baidu-olime-jp-make-whole-string bdc #t transposing-type))
       ((= transposing-type baidu-olime-jp-type-halfwidth-alnum)
	(baidu-olime-jp-make-whole-raw-string bdc #f #f))
       ((= transposing-type baidu-olime-jp-candidate-type-upper-halfwidth-alnum)
	(baidu-olime-jp-make-whole-raw-string bdc #f #t))
       ((= transposing-type baidu-olime-jp-type-fullwidth-alnum)
	(baidu-olime-jp-make-whole-raw-string bdc #t #f))
       ((= transposing-type baidu-olime-jp-candidate-type-upper-fullwidth-alnum)
	(baidu-olime-jp-make-whole-raw-string bdc #t #t))))))

(define baidu-olime-jp-get-raw-str-seq
  (lambda (bdc)
    (let* ((rkc (baidu-olime-jp-context-rkc bdc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (baidu-olime-jp-context-raw-ustr bdc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
	      right-str))))

(define baidu-olime-jp-get-raw-candidate
  (lambda (bdc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (baidu-olime-jp-make-whole-string bdc #t baidu-olime-jp-type-hiragana))))
	   (unconv-candidate (baidu-olime-jp-lib-get-unconv-candidate bdc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (baidu-olime-jp-get-raw-str-seq bdc))))
      (cond
       ((= cand-idx baidu-olime-jp-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx baidu-olime-jp-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) baidu-olime-jp-type-katakana))
       ((= cand-idx baidu-olime-jp-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) baidu-olime-jp-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (baidu-olime-jp-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx baidu-olime-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       baidu-olime-jp-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx baidu-olime-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx baidu-olime-jp-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (baidu-olime-jp-predicting-state-preedit bdc)
  (if (or
       (not baidu-olime-jp-use-implicit-commit-prediction?)
       (not (baidu-olime-jp-context-prediction-index bdc)))
      (baidu-olime-jp-input-state-preedit bdc)
      (let ((cand (baidu-olime-jp-get-prediction-string bdc)))
       (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (baidu-olime-jp-compose-state-preedit bdc)
  (let* ((segments (baidu-olime-jp-context-segments bdc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (baidu-olime-jp-separator bdc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx baidu-olime-jp-candidate-type-katakana)
			(baidu-olime-jp-lib-get-nth-candidate bdc seg-idx cand-idx)
			(baidu-olime-jp-get-raw-candidate bdc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (baidu-olime-jp-input-state-preedit bdc)
  (let* ((preconv-str (baidu-olime-jp-context-preconv-ustr bdc))
	 (rkc (baidu-olime-jp-context-rkc bdc))
	 (pending (rk-pending rkc))
	 (kana (baidu-olime-jp-context-kana-mode bdc))
	 (rule (baidu-olime-jp-context-input-rule bdc))
	 (extract-kana
	  (if (= rule baidu-olime-jp-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (baidu-olime-jp-has-preedit? bdc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (baidu-olime-jp-get-commit-string bdc)
  (let ((segments (baidu-olime-jp-context-segments bdc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx baidu-olime-jp-candidate-type-katakana)
			     (baidu-olime-jp-lib-get-nth-candidate
			      bdc seg-idx cand-idx)
			     (baidu-olime-jp-get-raw-candidate
			      bdc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (baidu-olime-jp-commit-string bdc)
    (let ((bdx-ctx (baidu-olime-jp-context-bdx-ctx bdc))
          (segments (baidu-olime-jp-context-segments bdc)))
      (if bdx-ctx
          (begin
            (baidu-olime-jp-lib-commit-segments bdc (ustr-whole-seq segments))
            (if (every (lambda (x) (<= x baidu-olime-jp-candidate-type-katakana))
                       (ustr-whole-seq segments))
                (baidu-olime-jp-lib-reset-conversion bdc))))))

(define (baidu-olime-jp-do-commit bdc)
    (im-commit bdc (baidu-olime-jp-get-commit-string bdc))
    (baidu-olime-jp-commit-string bdc)
    (baidu-olime-jp-reset-candidate-window bdc)
    (baidu-olime-jp-flush bdc))

(define (baidu-olime-jp-get-prediction-string bdc)
  (baidu-olime-jp-lib-get-nth-prediction
   bdc
   (baidu-olime-jp-context-prediction-index bdc)))

(define (baidu-olime-jp-learn-prediction-string bdc)
  (baidu-olime-jp-lib-commit-nth-prediction
   bdc
   (baidu-olime-jp-context-prediction-index bdc)))

(define (baidu-olime-jp-do-commit-prediction bdc)
  (im-commit bdc (baidu-olime-jp-get-prediction-string bdc))
  (baidu-olime-jp-learn-prediction-string bdc)
  (baidu-olime-jp-reset-prediction-window bdc)
  (baidu-olime-jp-flush bdc))

(define baidu-olime-jp-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (baidu-olime-jp-move-segment bdc dir)
  (baidu-olime-jp-reset-candidate-window bdc)
  (let ((segments (baidu-olime-jp-context-segments bdc)))
    (ustr-cursor-move! segments dir)
    (baidu-olime-jp-correct-segment-cursor segments)))

(define (baidu-olime-jp-resize-segment bdc cnt)
  (let* ((segments (baidu-olime-jp-context-segments bdc))
	 (cur-seg (ustr-cursor-pos segments)))
    (baidu-olime-jp-reset-candidate-window bdc)
    (baidu-olime-jp-lib-resize-segment bdc cur-seg cnt)
    (let* ((resized-nseg (baidu-olime-jp-lib-get-nr-segments bdc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (baidu-olime-jp-move-candidate bdc offset)
  (let* ((segments (baidu-olime-jp-context-segments bdc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (baidu-olime-jp-lib-get-nr-candidates bdc cur-seg))
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
	 (new-op-count (+ 1 (baidu-olime-jp-context-candidate-op-count bdc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (baidu-olime-jp-context-set-candidate-op-count! bdc new-op-count)
    (if (and
	 (= (baidu-olime-jp-context-candidate-op-count bdc)
	    baidu-olime-jp-candidate-op-count)
	 baidu-olime-jp-use-candidate-window?)
	(begin
	  (baidu-olime-jp-context-set-candidate-window! bdc #t)
	  (im-activate-candidate-selector bdc max baidu-olime-jp-nr-candidate-max)))
    (if (baidu-olime-jp-context-candidate-window bdc)
	(im-select-candidate bdc compensated-n))))

(define baidu-olime-jp-move-candidate-in-page
  (lambda (bdc numeralc)
    (let* ((segments (baidu-olime-jp-context-segments bdc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (baidu-olime-jp-lib-get-nr-candidates bdc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= baidu-olime-jp-nr-candidate-max 0)
			 0
			 (quotient n baidu-olime-jp-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page baidu-olime-jp-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (baidu-olime-jp-context-candidate-op-count bdc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (baidu-olime-jp-context-set-candidate-op-count! bdc new-op-count)
      (im-select-candidate bdc compensated-idx))))

(define (baidu-olime-jp-reset-candidate-window bdc)
  (if (baidu-olime-jp-context-candidate-window bdc)
      (begin
	(im-deactivate-candidate-selector bdc)
	(baidu-olime-jp-context-set-candidate-window! bdc #f)))
  (baidu-olime-jp-context-set-candidate-op-count! bdc 0))

(define baidu-olime-jp-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx baidu-olime-jp-candidate-type-halfwidth-alnum)
       (= state baidu-olime-jp-candidate-type-halfwidth-alnum))
      baidu-olime-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx baidu-olime-jp-candidate-type-fullwidth-alnum)
       (= state baidu-olime-jp-candidate-type-fullwidth-alnum))
      baidu-olime-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define baidu-olime-jp-set-segment-transposing
  (lambda (bdc key key-state)
    (let ((segments (baidu-olime-jp-context-segments bdc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(baidu-olime-jp-reset-candidate-window bdc)
	(baidu-olime-jp-context-set-candidate-op-count! bdc 0)

	(if (baidu-olime-jp-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons baidu-olime-jp-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (baidu-olime-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons baidu-olime-jp-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (baidu-olime-jp-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons baidu-olime-jp-candidate-type-halfkana
				    rotate-list)))
	(if (baidu-olime-jp-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons baidu-olime-jp-candidate-type-katakana
				    rotate-list)))
	(if (baidu-olime-jp-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons baidu-olime-jp-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx baidu-olime-jp-candidate-type-hiragana)
	     (= idx baidu-olime-jp-candidate-type-katakana)
	     (= idx baidu-olime-jp-candidate-type-halfkana)
	     (= idx baidu-olime-jp-candidate-type-halfwidth-alnum)
	     (= idx baidu-olime-jp-candidate-type-fullwidth-alnum)
	     (= idx baidu-olime-jp-candidate-type-upper-halfwidth-alnum)
	     (= idx baidu-olime-jp-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (baidu-olime-jp-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (baidu-olime-jp-proc-compose-state bdc key key-state)
  (cond
   ((baidu-olime-jp-prev-page-key? key key-state)
    (if (baidu-olime-jp-context-candidate-window bdc)
        (im-shift-page-candidate bdc #f)))

   ((baidu-olime-jp-next-page-key? key key-state)
    (if (baidu-olime-jp-context-candidate-window bdc)
        (im-shift-page-candidate bdc #t)))

   ((baidu-olime-jp-commit-key? key key-state)
    (baidu-olime-jp-do-commit bdc))

   ((baidu-olime-jp-extend-segment-key? key key-state)
    (baidu-olime-jp-resize-segment bdc 1))

   ((baidu-olime-jp-shrink-segment-key? key key-state)
    (baidu-olime-jp-resize-segment bdc -1))

   ((baidu-olime-jp-next-segment-key? key key-state)
    (baidu-olime-jp-move-segment bdc 1))

   ((baidu-olime-jp-prev-segment-key? key key-state)
    (baidu-olime-jp-move-segment bdc -1))

   ((baidu-olime-jp-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (baidu-olime-jp-context-segments bdc))
      (baidu-olime-jp-reset-candidate-window bdc)))

   ((baidu-olime-jp-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (baidu-olime-jp-context-segments bdc))
      (baidu-olime-jp-correct-segment-cursor (baidu-olime-jp-context-segments bdc))
      (baidu-olime-jp-reset-candidate-window bdc)))

   ((baidu-olime-jp-backspace-key? key key-state)
    (baidu-olime-jp-cancel-conv bdc))

   ((baidu-olime-jp-next-candidate-key? key key-state)
    (baidu-olime-jp-move-candidate bdc 1))

   ((baidu-olime-jp-prev-candidate-key? key key-state)
    (baidu-olime-jp-move-candidate bdc -1))

   ((or (baidu-olime-jp-transpose-as-hiragana-key? key key-state)
        (baidu-olime-jp-transpose-as-katakana-key? key key-state)
        (baidu-olime-jp-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (baidu-olime-jp-context-input-rule bdc) baidu-olime-jp-input-rule-kana))
         (or
          (baidu-olime-jp-transpose-as-halfwidth-alnum-key? key key-state)
          (baidu-olime-jp-transpose-as-fullwidth-alnum-key? key key-state))))
    (baidu-olime-jp-set-segment-transposing bdc key key-state))

   ((baidu-olime-jp-cancel-key? key key-state)
    (baidu-olime-jp-cancel-conv bdc))

   ((and baidu-olime-jp-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (baidu-olime-jp-context-candidate-window bdc))
    (baidu-olime-jp-move-candidate-in-page bdc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (baidu-olime-jp-do-commit bdc)
      (baidu-olime-jp-proc-input-state bdc key key-state)))))

(define (baidu-olime-jp-press-key-handler bdc key key-state)
  (if (ichar-control? key)
      (im-commit-raw bdc)
      (if (baidu-olime-jp-context-on bdc)
          (if (baidu-olime-jp-context-transposing bdc)
              (baidu-olime-jp-proc-transposing-state bdc key key-state)
              (if (baidu-olime-jp-context-state bdc)
                  (baidu-olime-jp-proc-compose-state bdc key key-state)
                  (if (baidu-olime-jp-context-predicting bdc)
                      (baidu-olime-jp-proc-prediction-state bdc key key-state)
                      (baidu-olime-jp-proc-input-state bdc key key-state))))
	  (baidu-olime-jp-proc-raw-state bdc key key-state)))
  (baidu-olime-jp-update-preedit bdc))

;;;
(define (baidu-olime-jp-release-key-handler bdc key key-state)
  (if (or (ichar-control? key)
	  (not (baidu-olime-jp-context-on bdc)))
      (baidu-olime-jp-commit-raw bdc)))
;;;
(define (baidu-olime-jp-reset-handler bdc)
  (if (baidu-olime-jp-context-on bdc)
      (begin
	(if (baidu-olime-jp-context-state bdc)
            (baidu-olime-jp-lib-reset-conversion bdc))
	(baidu-olime-jp-flush bdc))))

;;;
(define (baidu-olime-jp-get-candidate-handler bdc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (baidu-olime-jp-context-segments bdc)))
         (cand (if (baidu-olime-jp-context-state bdc)
                   (baidu-olime-jp-lib-get-nth-candidate bdc cur-seg idx)
                   (baidu-olime-jp-lib-get-nth-prediction bdc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (baidu-olime-jp-set-candidate-index-handler bdc idx)
    (cond
     ((baidu-olime-jp-context-state bdc)
      (ustr-cursor-set-frontside! (baidu-olime-jp-context-segments bdc) idx)
      (baidu-olime-jp-update-preedit bdc))
     ((baidu-olime-jp-context-predicting bdc)
      (baidu-olime-jp-context-set-prediction-index! bdc idx)
      (baidu-olime-jp-update-preedit bdc))))

(define (baidu-olime-jp-proc-raw-state bdc key key-state)
  (if (not (baidu-olime-jp-begin-input bdc key key-state))
      (im-commit-raw bdc)))

(baidu-olime-jp-configure-widgets)
(register-im
 'baidu-olime-jp
 "ja"
 "EUC-JP"
 baidu-olime-jp-im-name-label
 baidu-olime-jp-im-short-desc
 #f
 baidu-olime-jp-init-handler
 baidu-olime-jp-release-handler
 context-mode-handler
 baidu-olime-jp-press-key-handler
 baidu-olime-jp-release-key-handler
 baidu-olime-jp-reset-handler
 baidu-olime-jp-get-candidate-handler
 baidu-olime-jp-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
