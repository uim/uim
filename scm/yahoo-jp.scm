;;; yahoo-jp.scm: yahoo-jp for uim.
;;;
;;; Copyright (c) 2008-2013 uim Project https://github.com/uim/uim
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
(require "generic-predict.scm")
(require "util.scm")
(require-custom "generic-key-custom.scm")
(require-custom "yahoo-jp-custom.scm")
(require-custom "yahoo-jp-key-custom.scm")

(require-dynlib "expat")

;;; implementations

;;
;; canna emulating functions
;;

(define yahoo-jp-internal-context-rec-spec
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
(define-record 'yahoo-jp-internal-context yahoo-jp-internal-context-rec-spec)
(define yahoo-jp-internal-context-new-internal yahoo-jp-internal-context-new)

(define (yahoo-jp-conversion str opts)
  (define (fromconv str)
    (iconv-convert "UTF-8" "EUC-JP" str))
  (define (toconv str)
    (iconv-convert "EUC-JP" "UTF-8" str))
  (define (make-query appid)
    (format "~aconversion?appid=~a&sentence=~a~a"
            yahoo-jp-path
            appid
            (http:encode-uri-string (fromconv str))
            opts))
  (define (parse str)
    (let ((parser (xml-parser-create "UTF-8"))
          (path '())
          (seg '())
          (seg-txt "")
          (candidate '())
          (cand-queue '()))
      (define (elem-start name atts)
        (set! path (append path (list name))))
      (define (elem-end name)
        (cond ((equal? '("ResultSet" "Result" "SegmentList" "Segment" "CandidateList")
                       path)
               (set! candidate (append candidate (list cand-queue)))
               (set! cand-queue '()))
              ((equal? '("ResultSet" "Result" "SegmentList" "Segment")
                       path)
               (set! seg (append seg (list seg-txt)))
               (set! seg-txt "")))
        (set! path (drop-right path 1)))
      (define (chardata str)
        (cond ((equal? '("ResultSet" "Result" "SegmentList" "Segment" "CandidateList" "Candidate")
                       path)
               (set! cand-queue (append cand-queue (list (toconv str)))))
              ((equal? '("ResultSet" "Result" "SegmentList" "Segment" "SegmentText")
                       path)
               (set! seg-txt (toconv str)))))
      (xml-element-handler-set! parser elem-start elem-end)
      (xml-characterdata-handler-set! parser chardata)
      (xml-parse parser str 1)
      (xml-parser-free parser)
      (cons seg candidate)))

  (let* ((appid (if (string=? yahoo-jp-appid "")
                    (begin (uim-notify-fatal (N_ "Please regist Api key from <a href='http://developer.yahoo.co.jp/'>developer network</a> and set value on advanced menu."))
                           #f)
                    yahoo-jp-appid))
         (proxy (make-http-proxy-from-custom))
         (ssl (and yahoo-jp-use-ssl?
                   (make-http-ssl (SSLv3-client-method) 443)))
         (ret (and appid
                   (http:get yahoo-jp-server (make-query appid) 80 proxy ssl))))
    (if (string? ret)
        (parse ret)
        (cons '() (list (list str))))))

(define (yahoo-jp-predict-memoize! yc str cand)
  (let ((cache (yahoo-jp-context-prediction-cache yc)))
    (yahoo-jp-context-set-prediction-cache!
     yc
     (append (if (<= yahoo-jp-prediction-cache-words
                     (length cache))
                 (cdr cache)
                 cache)
             (list (cons str cand))))))
(define (yahoo-jp-predict yc str opts)
  (let ((ret (assoc str (yahoo-jp-context-prediction-cache yc))))
    (if ret
        (cdr ret)
        (let ((cand (yahoo-jp-predict-from-server str opts)))
          (if (not (null? (car cand)))
              (yahoo-jp-predict-memoize! yc str cand))
          cand))))
(define (yahoo-jp-predict-from-server str opts)
  (cadr (yahoo-jp-conversion str (string-append "&mode=predictive" opts))))

(define (yahoo-jp-conversion-make-resize-query yomi-seg)
  (let ((len (length yomi-seg)))
    (apply string-append (map (lambda (idx)
                                (if (= (+ idx 1) len)
                                    (list-ref yomi-seg idx)
                                    (string-append (list-ref yomi-seg idx) " ")))
                              (iota len)))))
(define (yahoo-jp-conversion-resize yomi-seg)
  (yahoo-jp-conversion
   (yahoo-jp-conversion-make-resize-query yomi-seg) ""))

(define (yahoo-jp-lib-init)
  #t)
(define (yahoo-jp-lib-alloc-context)
  (yahoo-jp-internal-context-new-internal))
(define (yahoo-jp-lib-get-nth-candidate yc seg nth)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cand (yahoo-jp-internal-context-candidates yx-ctx)))
    (list-ref (list-ref cand seg) nth)))
(define (yahoo-jp-lib-release-context yc)
  #t)
(define (yahoo-jp-lib-get-unconv-candidate yc seg-idx)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cand (yahoo-jp-internal-context-candidates yx-ctx)))
    ;; XXX
    (car (take-right (list-ref cand seg-idx) 1))))
(define (yahoo-jp-lib-get-nr-segments yc)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cand (yahoo-jp-internal-context-candidates yx-ctx)))
    (length cand)))
(define (yahoo-jp-lib-get-nr-candidates yc seg)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cand (yahoo-jp-internal-context-candidates yx-ctx)))
    (length (list-ref cand seg))))
(define (yahoo-jp-next-yomi-seg yomi-seg seg cnt)
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
(define (yahoo-jp-lib-resize-segment yc seg cnt)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cand (yahoo-jp-internal-context-candidates yx-ctx))
         (yomi-seg (yahoo-jp-internal-context-yomi-seg yx-ctx))
         (next-yomi-seg (yahoo-jp-next-yomi-seg yomi-seg seg cnt))
         (replace-yomi-seg-and-next-cand (yahoo-jp-conversion-resize next-yomi-seg))
         (replace-yomi-seg (car replace-yomi-seg-and-next-cand))
         (next-cand        (cdr replace-yomi-seg-and-next-cand)))
    (if (and next-cand
             (not (equal? next-cand cand)))
        (begin
          (yahoo-jp-internal-context-set-candidates! yx-ctx next-cand)
          (yahoo-jp-internal-context-set-yomi-seg! yx-ctx replace-yomi-seg)))
    #t))
(define (yahoo-jp-lib-begin-conversion yc str)
  (let* ((yomi-seg-and-cand (yahoo-jp-conversion str ""))
         (yomi-seg (car yomi-seg-and-cand))
         (cand (cdr yomi-seg-and-cand))
         (yx-ctx (yahoo-jp-context-yx-ctx yc)))
    (yahoo-jp-internal-context-set-yomi-seg! yx-ctx yomi-seg)
    (yahoo-jp-internal-context-set-candidates! yx-ctx cand)
    (length cand)))
(define (yahoo-jp-lib-commit-segments yc delta)
  #t)
(define (yahoo-jp-lib-reset-conversion yc)
  #f)
(define (yahoo-jp-lib-set-prediction-src-string yc str)
  (cond ((eq? yahoo-jp-prediction-type 'www)
         (let ((yx-ctx (yahoo-jp-context-yx-ctx yc))
               (cands (yahoo-jp-predict yc str "")))
           (yahoo-jp-internal-context-set-prediction-candidates! yx-ctx cands)
           (yahoo-jp-internal-context-set-prediction-nr! yx-ctx (length cands))))
        ((eq? yahoo-jp-prediction-type 'uim)
         (let* ((ret (predict-meta-search
                      (yahoo-jp-context-prediction-ctx yc)
                      str))
                (yx-ctx (yahoo-jp-context-yx-ctx yc))
                (word     (predict-meta-word? ret))
                (cands    (predict-meta-candidates? ret))
                (appendix (predict-meta-appendix? ret)))
           (yahoo-jp-internal-context-set-prediction-word! yx-ctx word)
           (yahoo-jp-internal-context-set-prediction-candidates! yx-ctx cands)
           (yahoo-jp-internal-context-set-prediction-appendix! yx-ctx appendix)
           (yahoo-jp-internal-context-set-prediction-nr! yx-ctx (length cands)))))
  #f)
(define (yahoo-jp-lib-get-nr-predictions yc)
  (let ((yx-ctx (yahoo-jp-context-yx-ctx yc)))
    (yahoo-jp-internal-context-prediction-nr yx-ctx)))
(define (yahoo-jp-lib-get-nth-word yc nth)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (word (yahoo-jp-internal-context-prediction-word yx-ctx)))
    (list-ref word nth)))
(define (yahoo-jp-lib-get-nth-prediction yc nth)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (cands (yahoo-jp-internal-context-prediction-candidates yx-ctx)))
    (list-ref cands nth)))
(define (yahoo-jp-lib-get-nth-appendix yc nth)
  (let* ((yx-ctx (yahoo-jp-context-yx-ctx yc))
         (appendix (yahoo-jp-internal-context-prediction-appendix yx-ctx)))
    (list-ref appendix nth)))
(define (yahoo-jp-lib-commit-nth-prediction yc nth)
  (if (eq? yahoo-jp-prediction-type 'uim)
      (let ((yx-ctx (yahoo-jp-context-yx-ctx yc)))
        (predict-meta-commit
         (yahoo-jp-context-prediction-ctx yc)
         (yahoo-jp-lib-get-nth-word yc nth)
         (yahoo-jp-lib-get-nth-prediction yc nth)
         (yahoo-jp-lib-get-nth-appendix yc nth))))
  #f)

(define yahoo-jp-init-lib-ok? #f)

(define yahoo-jp-type-direct	   ja-type-direct)
(define yahoo-jp-type-hiragana	   ja-type-hiragana)
(define yahoo-jp-type-katakana	   ja-type-katakana)
(define yahoo-jp-type-halfkana	   ja-type-halfkana)
(define yahoo-jp-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define yahoo-jp-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define yahoo-jp-input-rule-roma 0)
(define yahoo-jp-input-rule-kana 1)
(define yahoo-jp-input-rule-azik 2)
(define yahoo-jp-input-rule-act 3)
(define yahoo-jp-input-rule-kzik 4)

(define yahoo-jp-candidate-type-katakana -2)
(define yahoo-jp-candidate-type-hiragana -3)
(define yahoo-jp-candidate-type-halfkana -4)
(define yahoo-jp-candidate-type-halfwidth-alnum -5)
(define yahoo-jp-candidate-type-fullwidth-alnum -6)
(define yahoo-jp-candidate-type-upper-halfwidth-alnum -7)
(define yahoo-jp-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key yahoo-jp-space-key? '(" "))

(define yahoo-jp-prepare-input-rule-activation
  (lambda (yc)
    (cond
     ((yahoo-jp-context-state yc)
      (yahoo-jp-do-commit yc))
     ((yahoo-jp-context-transposing yc)
      (im-commit yc (yahoo-jp-transposing-text yc)))
     ((and
       (yahoo-jp-context-on yc)
       (yahoo-jp-has-preedit? yc))
      (im-commit
       yc (yahoo-jp-make-whole-string yc #t (yahoo-jp-context-kana-mode yc)))))
    (yahoo-jp-flush yc)
    (yahoo-jp-update-preedit yc)))

(define yahoo-jp-prepare-input-mode-activation
  (lambda (yc new-mode)
    (let ((old-kana (yahoo-jp-context-kana-mode yc)))
      (cond
       ((yahoo-jp-context-state yc)
	(yahoo-jp-do-commit yc))
       ((yahoo-jp-context-transposing yc)
	(im-commit yc (yahoo-jp-transposing-text yc))
	(yahoo-jp-flush yc))
       ((and
	 (yahoo-jp-context-on yc)
	 (yahoo-jp-has-preedit? yc)
	 (not (= old-kana new-mode)))
	(im-commit
	 yc (yahoo-jp-make-whole-string yc #t (yahoo-jp-context-kana-mode yc)))
	(yahoo-jp-flush yc)))
      (yahoo-jp-update-preedit yc))))

(register-action 'action_yahoo-jp_hiragana
		 (lambda (yc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (yc) ;; activity predicate
		   (and (yahoo-jp-context-on yc)
		        (not (yahoo-jp-context-alnum yc))
			(= (yahoo-jp-context-kana-mode yc)
			   yahoo-jp-type-hiragana)))

		 (lambda (yc) ;; action handler
		   (yahoo-jp-prepare-input-mode-activation yc yahoo-jp-type-hiragana)
		   (yahoo-jp-context-set-on! yc #t)
		   (yahoo-jp-context-set-alnum! yc #f)
		   (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-hiragana)))

(register-action 'action_yahoo-jp_katakana
		 (lambda (yc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (yc)
		   (and (yahoo-jp-context-on yc)
		        (not (yahoo-jp-context-alnum yc))
			(= (yahoo-jp-context-kana-mode yc)
			   yahoo-jp-type-katakana)))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-mode-activation yc yahoo-jp-type-katakana)
		   (yahoo-jp-context-set-on! yc #t)
		   (yahoo-jp-context-set-alnum! yc #f)
		   (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-katakana)))

(register-action 'action_yahoo-jp_halfkana
		 (lambda (yc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (yc)
		   (and (yahoo-jp-context-on yc)
			(not (yahoo-jp-context-alnum yc))
			(= (yahoo-jp-context-kana-mode yc) yahoo-jp-type-halfkana)))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-mode-activation yc yahoo-jp-type-halfkana)
		   (yahoo-jp-context-set-on! yc #t)
		   (yahoo-jp-context-set-alnum! yc #f)
		   (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-halfkana)))

(register-action 'action_yahoo-jp_halfwidth_alnum
		 (lambda (yc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (yc) ;; activity predicate
		   (and (yahoo-jp-context-on yc)
			(yahoo-jp-context-alnum yc)
			(= (yahoo-jp-context-alnum-type yc)
			   yahoo-jp-type-halfwidth-alnum)))
		 (lambda (yc) ;; action handler
		   (yahoo-jp-prepare-input-mode-activation
		    yc (yahoo-jp-context-kana-mode yc))
		   (yahoo-jp-context-set-on! yc #t)
		   (yahoo-jp-context-set-alnum! yc #t)
		   (yahoo-jp-context-set-alnum-type!
		    yc yahoo-jp-type-halfwidth-alnum)))

(register-action 'action_yahoo-jp_direct
		 (lambda (yc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (yc)
		   (not (yahoo-jp-context-on yc)))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-mode-activation yc yahoo-jp-type-direct)
		   (yahoo-jp-context-set-on! yc #f)))

(register-action 'action_yahoo-jp_fullwidth_alnum
		 (lambda (yc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (yc)
		   (and (yahoo-jp-context-on yc)
			(yahoo-jp-context-alnum yc)
			(= (yahoo-jp-context-alnum-type yc)
			   yahoo-jp-type-fullwidth-alnum)))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-mode-activation
		    yc (yahoo-jp-context-kana-mode yc))
		   (yahoo-jp-context-set-on! yc #t)
		   (yahoo-jp-context-set-alnum! yc #t)
		   (yahoo-jp-context-set-alnum-type!
		    yc yahoo-jp-type-fullwidth-alnum)))

(register-action 'action_yahoo-jp_roma
		 (lambda (yc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (yc)
		   (= (yahoo-jp-context-input-rule yc)
		      yahoo-jp-input-rule-roma))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-rule-activation yc)
		   (rk-context-set-rule! (yahoo-jp-context-rkc yc)
					 ja-rk-rule)
		   (yahoo-jp-context-set-input-rule! yc yahoo-jp-input-rule-roma)))

(register-action 'action_yahoo-jp_kana
		 (lambda (yc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (yc)
		   (= (yahoo-jp-context-input-rule yc)
		      yahoo-jp-input-rule-kana))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-rule-activation yc)
                   (require "japanese-kana.scm")
		   (yahoo-jp-context-set-input-rule! yc yahoo-jp-input-rule-kana)
                   (yahoo-jp-context-change-kana-mode!
                     yc (yahoo-jp-context-kana-mode yc))
		   (yahoo-jp-context-set-alnum! yc #f)))

(register-action 'action_yahoo-jp_azik
		 (lambda (yc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (yc)
		   (= (yahoo-jp-context-input-rule yc)
		      yahoo-jp-input-rule-azik))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-rule-activation yc)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (yahoo-jp-context-rkc yc)
					 ja-azik-rule)
		   (yahoo-jp-context-set-input-rule! yc yahoo-jp-input-rule-azik)))

(register-action 'action_yahoo-jp_kzik
		 (lambda (yc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (yc)
		   (= (yahoo-jp-context-input-rule yc)
		      yahoo-jp-input-rule-kzik))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-rule-activation yc)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (yahoo-jp-context-rkc yc)
					 ja-kzik-rule)
		   (yahoo-jp-context-set-input-rule! yc yahoo-jp-input-rule-kzik)))

(register-action 'action_yahoo-jp_act
		 (lambda (yc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (yc)
		   (= (yahoo-jp-context-input-rule yc)
		      yahoo-jp-input-rule-act))
		 (lambda (yc)
		   (yahoo-jp-prepare-input-rule-activation yc)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (yahoo-jp-context-rkc yc)
					 ja-act-rule)
		   (yahoo-jp-context-set-input-rule! yc yahoo-jp-input-rule-act)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define yahoo-jp-configure-widgets
  (lambda ()
    (register-widget 'widget_yahoo-jp_input_mode
		     (activity-indicator-new yahoo-jp-input-mode-actions)
		     (actions-new yahoo-jp-input-mode-actions))

    (register-widget 'widget_yahoo-jp_kana_input_method
		     (activity-indicator-new yahoo-jp-kana-input-method-actions)
		     (actions-new yahoo-jp-kana-input-method-actions))
    (context-list-replace-widgets! 'yahoo-jp yahoo-jp-widgets)))

(define yahoo-jp-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'predicting         #f)
    (list 'yx-ctx             ()) ;; yahoo-jp-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'prediction-ctx     '())
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'prediction-cache   '())
    (list 'kana-mode          yahoo-jp-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      yahoo-jp-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         yahoo-jp-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'yahoo-jp-context yahoo-jp-context-rec-spec)
(define yahoo-jp-context-new-internal yahoo-jp-context-new)

(define (yahoo-jp-context-new id im)
  (let ((yc (yahoo-jp-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (yahoo-jp-context-set-yx-ctx! yc (if yahoo-jp-init-lib-ok?
;				      (yahoo-jp-lib-alloc-context) ()))
    (yahoo-jp-context-set-yx-ctx! yc (yahoo-jp-lib-alloc-context))
    (yahoo-jp-context-set-widgets! yc yahoo-jp-widgets)
    (yahoo-jp-context-set-rkc! yc rkc)
    (yahoo-jp-context-set-preconv-ustr! yc (ustr-new '()))
    (yahoo-jp-context-set-raw-ustr! yc (ustr-new '()))
    (yahoo-jp-context-set-segments! yc (ustr-new '()))
    (if (and yahoo-jp-use-prediction?
             (eq? yahoo-jp-prediction-type 'uim))
        (begin
          (yahoo-jp-context-set-prediction-ctx! yc (predict-make-meta-search))
          (predict-meta-open (yahoo-jp-context-prediction-ctx yc) "yahoo-jp")
          (predict-meta-set-external-charset! (yahoo-jp-context-prediction-ctx yc) "EUC-JP")))
    yc))

(define (yahoo-jp-commit-raw yc)
  (im-commit-raw yc)
  (yahoo-jp-context-set-commit-raw! yc #t))

(define (yahoo-jp-context-kana-toggle yc)
  (let* ((kana (yahoo-jp-context-kana-mode yc))
	 (opposite-kana (ja-opposite-kana kana)))
    (yahoo-jp-context-change-kana-mode! yc opposite-kana)))

(define yahoo-jp-context-alkana-toggle
  (lambda (yc)
    (let ((alnum-state (yahoo-jp-context-alnum yc)))
      (yahoo-jp-context-set-alnum! yc (not alnum-state)))))

(define yahoo-jp-context-change-kana-mode!
  (lambda (yc kana-mode)
    (if (= (yahoo-jp-context-input-rule yc)
           yahoo-jp-input-rule-kana)
        (rk-context-set-rule!
	 (yahoo-jp-context-rkc yc)
	 (cond
	  ((= kana-mode yahoo-jp-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode yahoo-jp-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode yahoo-jp-type-halfkana) ja-kana-halfkana-rule))))
    (yahoo-jp-context-set-kana-mode! yc kana-mode)))

(define yahoo-jp-make-whole-string
  (lambda (yc convert-pending-into-kana? kana)
    (let* ((rkc (yahoo-jp-context-rkc yc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (yahoo-jp-context-input-rule yc))
           (preconv-str (yahoo-jp-context-preconv-ustr yc))
           (extract-kana
            (if (= rule yahoo-jp-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule yahoo-jp-input-rule-kana)
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

(define yahoo-jp-make-raw-string
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
	     (yahoo-jp-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (yahoo-jp-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define yahoo-jp-make-whole-raw-string
  (lambda (yc wide? upper?)
    (yahoo-jp-make-raw-string (yahoo-jp-get-raw-str-seq yc) wide? upper?)))

(define (yahoo-jp-init-handler id im arg)
  (if (not yahoo-jp-init-lib-ok?)
      (begin
	(yahoo-jp-lib-init)
	(set! yahoo-jp-init-lib-ok? #t)))
  (yahoo-jp-context-new id im))

(define (yahoo-jp-release-handler yc)
  (if yc
      (yahoo-jp-lib-release-context yc)))

(define (yahoo-jp-flush yc)
  (rk-flush (yahoo-jp-context-rkc yc))
  (ustr-clear! (yahoo-jp-context-preconv-ustr yc))
  (ustr-clear! (yahoo-jp-context-raw-ustr yc))
  (ustr-clear! (yahoo-jp-context-segments yc))
  (yahoo-jp-context-set-transposing! yc #f)
  (yahoo-jp-context-set-state! yc #f)
  (if (or
       (yahoo-jp-context-candidate-window yc)
       (yahoo-jp-context-prediction-window yc))
      (im-deactivate-candidate-selector yc))
  (yahoo-jp-context-set-candidate-window! yc #f)
  (yahoo-jp-context-set-prediction-window! yc #f)
  (yahoo-jp-context-set-candidate-op-count! yc 0))

(define (yahoo-jp-begin-input yc key key-state)
  (if (cond
       ((yahoo-jp-on-key? key key-state)
	#t)
       ((and
	 yahoo-jp-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((yahoo-jp-hiragana-key? key key-state)
	   (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-hiragana)
	   (yahoo-jp-context-set-alnum! yc #f)
	   #t)
	  ((yahoo-jp-katakana-key? key key-state)
	   (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-katakana)
	   (yahoo-jp-context-set-alnum! yc #f)
	   #t)
	  ((yahoo-jp-halfkana-key? key key-state)
	   (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-halfkana)
	   (yahoo-jp-context-set-alnum! yc #f)
	   #t)
	  ((yahoo-jp-halfwidth-alnum-key? key key-state)
	   (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-halfwidth-alnum)
	   (yahoo-jp-context-set-alnum! yc #t)
	   #t)
	  ((yahoo-jp-halfwidth-alnum-key? key key-state)
	   (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-fullwidth-alnum)
	   (yahoo-jp-context-set-alnum! yc #t)
	   #t)
	  ((yahoo-jp-kana-toggle-key? key key-state)
	   (yahoo-jp-context-kana-toggle yc)
	   (yahoo-jp-context-set-alnum! yc #f)
	   #t)
	  ((yahoo-jp-alkana-toggle-key? key key-state)
	   (yahoo-jp-context-alkana-toggle yc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(yahoo-jp-context-set-on! yc #t)
	(rk-flush (yahoo-jp-context-rkc yc))
	(yahoo-jp-context-set-state! yc #f)
	#t)
      #f))

(define (yahoo-jp-update-preedit yc)
  (if (not (yahoo-jp-context-commit-raw yc))
      (let ((segments (if (yahoo-jp-context-on yc)
			  (if (yahoo-jp-context-transposing yc)
			      (yahoo-jp-context-transposing-state-preedit yc)
			      (if (yahoo-jp-context-state yc)
				  (yahoo-jp-compose-state-preedit yc)
                                  (if (yahoo-jp-context-predicting yc)
                                      (yahoo-jp-predicting-state-preedit yc)
                                      (yahoo-jp-input-state-preedit yc))))
			  ())))
	(context-update-preedit yc segments))
      (yahoo-jp-context-set-commit-raw! yc #f)))

(define (yahoo-jp-begin-conv yc)
  (let ((yx-ctx (yahoo-jp-context-yx-ctx yc))
	(preconv-str (yahoo-jp-make-whole-string yc #t yahoo-jp-type-hiragana)))
    (if (and yx-ctx
             (> (string-length preconv-str) 0))
	(let ((num (yahoo-jp-lib-begin-conversion yc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (yahoo-jp-context-segments yc)
		 (make-list num 0))
		(yahoo-jp-context-set-state! yc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; yahoo-jp-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define yahoo-jp-cancel-conv
  (lambda (yc)
    (yahoo-jp-reset-candidate-window yc)
    (yahoo-jp-context-set-state! yc #f)
    (ustr-clear! (yahoo-jp-context-segments yc))
    (yahoo-jp-lib-reset-conversion yc)))

(define (yahoo-jp-proc-input-state-no-preedit yc key key-state)
  (let
      ((rkc (yahoo-jp-context-rkc yc))
       (direct (ja-direct (charcode->string key)))
       (rule (yahoo-jp-context-input-rule yc)))
    (cond
     ((and yahoo-jp-use-with-vi?
           (yahoo-jp-vi-escape-key? key key-state))
      (yahoo-jp-flush yc)
      (yahoo-jp-context-set-on! yc #f)
      (yahoo-jp-commit-raw yc))

     ((yahoo-jp-off-key? key key-state)
      (yahoo-jp-flush yc)
      (yahoo-jp-context-set-on! yc #f))

     ((yahoo-jp-backspace-key? key key-state)
      (yahoo-jp-commit-raw yc))
     
     ((yahoo-jp-delete-key? key key-state)
      (yahoo-jp-commit-raw yc))

     ((and
       (yahoo-jp-hiragana-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-kana-mode yc) yahoo-jp-type-hiragana)
	 (not (yahoo-jp-context-alnum yc)))))
      (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-hiragana)
      (yahoo-jp-context-set-alnum! yc #f))

     ((and
       (yahoo-jp-katakana-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-kana-mode yc) yahoo-jp-type-katakana)
	 (not (yahoo-jp-context-alnum yc)))))
      (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-katakana)
      (yahoo-jp-context-set-alnum! yc #f))
     
     ((and
       (yahoo-jp-halfkana-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-kana-mode yc) yahoo-jp-type-halfkana)
	 (not (yahoo-jp-context-alnum yc)))))
      (yahoo-jp-context-change-kana-mode! yc yahoo-jp-type-halfkana)
      (yahoo-jp-context-set-alnum! yc #f))
     
     ((and
       (yahoo-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-alnum-type yc) yahoo-jp-type-halfwidth-alnum)
	 (yahoo-jp-context-alnum yc))))
      (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-halfwidth-alnum)
      (yahoo-jp-context-set-alnum! yc #t))
     
     ((and
       (yahoo-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-alnum-type yc) yahoo-jp-type-fullwidth-alnum)
	 (yahoo-jp-context-alnum yc))))
      (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-fullwidth-alnum)
      (yahoo-jp-context-set-alnum! yc #t))
     
     ((and
       (not (yahoo-jp-context-alnum yc))
       (yahoo-jp-kana-toggle-key? key key-state))
      (yahoo-jp-context-kana-toggle yc))

     ((yahoo-jp-alkana-toggle-key? key key-state)
      (yahoo-jp-context-alkana-toggle yc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (yahoo-jp-commit-raw yc))
     
     ;; direct key => commit
     (direct
      (im-commit yc direct))

     ;; space key
     ((yahoo-jp-space-key? key key-state)
      (if (yahoo-jp-context-alnum yc)
	  (im-commit yc (list-ref
			 ja-alnum-space
			 (- (yahoo-jp-context-alnum-type yc)
			    yahoo-jp-type-halfwidth-alnum)))
	  (im-commit yc (list-ref ja-space (yahoo-jp-context-kana-mode yc)))))

     ((symbol? key)
      (yahoo-jp-commit-raw yc))

     (else
      (if (yahoo-jp-context-alnum yc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (yahoo-jp-context-preconv-ustr yc)
			       (if (= (yahoo-jp-context-alnum-type yc)
				      yahoo-jp-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (yahoo-jp-context-raw-ustr yc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule yahoo-jp-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
                  (if (list? (car res))
                    (ustr-insert-seq! (yahoo-jp-context-preconv-ustr yc) res)
                    (ustr-insert-elem! (yahoo-jp-context-preconv-ustr yc) res))
	          (ustr-insert-elem! (yahoo-jp-context-raw-ustr yc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (yahoo-jp-commit-raw yc)))))))))

(define (yahoo-jp-has-preedit? yc)
  (or (not (ustr-empty? (yahoo-jp-context-preconv-ustr yc)))
      (> (string-length (rk-pending (yahoo-jp-context-rkc yc))) 0)))

(define yahoo-jp-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type yahoo-jp-type-halfwidth-alnum)
       (= state yahoo-jp-type-halfwidth-alnum))
      yahoo-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type yahoo-jp-type-fullwidth-alnum)
       (= state yahoo-jp-type-fullwidth-alnum))
      yahoo-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define yahoo-jp-proc-transposing-state
  (lambda (yc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (yahoo-jp-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons yahoo-jp-type-fullwidth-alnum rotate-list)))
      (if (yahoo-jp-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons yahoo-jp-type-halfwidth-alnum rotate-list)))
      (if (yahoo-jp-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons yahoo-jp-type-halfkana rotate-list)))
      (if (yahoo-jp-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons yahoo-jp-type-katakana rotate-list)))
      (if (yahoo-jp-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons yahoo-jp-type-hiragana rotate-list)))

      (if (yahoo-jp-context-transposing yc)
	  (let ((lst (member (yahoo-jp-context-transposing-type yc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (yahoo-jp-rotate-transposing-alnum-type
				 (yahoo-jp-context-transposing-type yc)
				 (car rotate-list))))))
	  (begin
	    (yahoo-jp-context-set-transposing! yc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state yahoo-jp-type-hiragana)
	      (= state yahoo-jp-type-katakana)
	      (= state yahoo-jp-type-halfkana)))
	(yahoo-jp-context-set-transposing-type! yc state))
       ((and state
	     (or
	      (= state yahoo-jp-type-halfwidth-alnum)
	      (= state yahoo-jp-candidate-type-upper-halfwidth-alnum)
	      (= state yahoo-jp-type-fullwidth-alnum)
	      (= state yahoo-jp-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (yahoo-jp-context-input-rule yc) yahoo-jp-input-rule-kana))
	    (yahoo-jp-context-set-transposing-type! yc state)))
       (else
	(and
	 ; commit
	 (if (yahoo-jp-commit-key? key key-state)
	     (begin
	       (im-commit yc (yahoo-jp-transposing-text yc))
	       (yahoo-jp-flush yc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (yahoo-jp-begin-conv-key? key key-state)
	     (begin
	       (yahoo-jp-context-set-transposing! yc #f)
	       (yahoo-jp-begin-conv yc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (yahoo-jp-cancel-key? key key-state)
	      (yahoo-jp-backspace-key? key key-state))
	     (begin
	       (yahoo-jp-context-set-transposing! yc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (yahoo-jp-prev-page-key? key key-state)
	      (yahoo-jp-next-page-key? key key-state)
	      (yahoo-jp-extend-segment-key? key key-state)
	      (yahoo-jp-shrink-segment-key? key key-state)
	      (yahoo-jp-next-segment-key? key key-state)
	      (yahoo-jp-beginning-of-preedit-key? key key-state)
	      (yahoo-jp-end-of-preedit-key? key key-state)
	      (yahoo-jp-next-candidate-key? key key-state)
	      (yahoo-jp-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit yc (yahoo-jp-transposing-text yc))
	   (yahoo-jp-flush yc)
	   (yahoo-jp-proc-input-state yc key key-state))))))))

(define (yahoo-jp-move-prediction yc offset)
  (let* ((nr (yahoo-jp-lib-get-nr-predictions yc))
         (idx (yahoo-jp-context-prediction-index yc))
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
    (im-select-candidate yc compensated-n)
    (yahoo-jp-context-set-prediction-index! yc compensated-n)))

(define (yahoo-jp-move-prediction-in-page yc numeralc)
  (let* ((nr (yahoo-jp-lib-get-nr-predictions yc))
	 (p-idx (yahoo-jp-context-prediction-index yc))
	 (n (if (not p-idx)
		0
		p-idx))
	 (cur-page (if (= yahoo-jp-nr-candidate-max 0)
		       0
		       (quotient n yahoo-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page yahoo-jp-nr-candidate-max) compensated-pageidx))
	 (compensated-idx (cond
			   ((>= idx nr)
			    #f)
			   (else
			    idx)))
	 (selected-pageidx (if (not p-idx)
			       #f
			       (if (= yahoo-jp-nr-candidate-max 0)
				   p-idx
				   (remainder p-idx
					      yahoo-jp-nr-candidate-max)))))
    (if (and
	 compensated-idx
	 (not (eqv? compensated-pageidx selected-pageidx)))
	(begin
	  (yahoo-jp-context-set-prediction-index! yc compensated-idx)
	  (im-select-candidate yc compensated-idx)
	  #t)
	#f)))

(define (yahoo-jp-prediction-select-non-existing-index? yc numeralc)
  (let* ((nr (yahoo-jp-lib-get-nr-predictions yc))
	 (p-idx (yahoo-jp-context-prediction-index yc))
	 (cur-page (if (= yahoo-jp-nr-candidate-max 0)
		       0
		       (quotient p-idx yahoo-jp-nr-candidate-max)))
	 (pageidx (- (numeric-ichar->integer numeralc) 1))
	 (compensated-pageidx (cond
			       ((< pageidx 0) ; pressing key_0
				(+ pageidx 10))
			       (else
				pageidx)))
	 (idx (+ (* cur-page yahoo-jp-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
        #t
        #f)))

(define (yahoo-jp-prediction-keys-handled? yc key key-state)
  (cond
   ((yahoo-jp-next-prediction-key? key key-state)
    (yahoo-jp-move-prediction yc 1)
    #t)
   ((yahoo-jp-prev-prediction-key? key key-state)
    (yahoo-jp-move-prediction yc -1)
    #t)
   ((and
     yahoo-jp-select-prediction-by-numeral-key?
     (ichar-numeric? key))
    (yahoo-jp-move-prediction-in-page yc key))
   ((and
     (yahoo-jp-context-prediction-index yc)
     (yahoo-jp-prev-page-key? key key-state))
    (im-shift-page-candidate yc #f)
    #t)
   ((and
     (yahoo-jp-context-prediction-index yc)
     (yahoo-jp-next-page-key? key key-state))
    (im-shift-page-candidate yc #t)
    #t)
   (else
    #f)))

(define (yahoo-jp-proc-prediction-state yc key key-state)
  (cond
   ;; prediction index change
   ((yahoo-jp-prediction-keys-handled? yc key key-state))

   ;; cancel
   ((yahoo-jp-cancel-key? key key-state)
    (if (yahoo-jp-context-prediction-index yc)
	(yahoo-jp-reset-prediction-window yc)
	(begin
	  (yahoo-jp-reset-prediction-window yc)
	  (yahoo-jp-proc-input-state yc key key-state))))

   ;; commit
   ((and
     (yahoo-jp-context-prediction-index yc)
     (yahoo-jp-commit-key? key key-state))
    (yahoo-jp-do-commit-prediction yc))
   (else
    (if (and
	 yahoo-jp-use-implicit-commit-prediction?
	 (yahoo-jp-context-prediction-index yc))
	(cond
	 ((or
	   ;; check keys used in yahoo-jp-proc-input-state-with-preedit
	   (yahoo-jp-begin-conv-key? key key-state)
	   (yahoo-jp-backspace-key? key key-state)
	   (yahoo-jp-delete-key? key key-state)
	   (yahoo-jp-kill-key? key key-state)
	   (yahoo-jp-kill-backward-key? key key-state)
	   (and
	    (not (yahoo-jp-context-alnum yc))
	    (yahoo-jp-commit-as-opposite-kana-key? key key-state))
	   (yahoo-jp-transpose-as-hiragana-key? key key-state)
	   (yahoo-jp-transpose-as-katakana-key? key key-state)
	   (yahoo-jp-transpose-as-halfkana-key? key key-state)
	   (and
	    (not (= (yahoo-jp-context-input-rule yc) yahoo-jp-input-rule-kana))
	    (or
	     (yahoo-jp-transpose-as-halfwidth-alnum-key? key key-state)
	     (yahoo-jp-transpose-as-fullwidth-alnum-key? key key-state)))
	   (yahoo-jp-hiragana-key? key key-state)
	   (yahoo-jp-katakana-key? key key-state)
	   (yahoo-jp-halfkana-key? key key-state)
	   (yahoo-jp-halfwidth-alnum-key? key key-state)
	   (yahoo-jp-fullwidth-alnum-key? key key-state)
	   (and
	    (not (yahoo-jp-context-alnum yc))
	    (yahoo-jp-kana-toggle-key? key key-state))
	   (yahoo-jp-alkana-toggle-key? key key-state)
	   (yahoo-jp-go-left-key? key key-state)
	   (yahoo-jp-go-right-key? key key-state)
	   (yahoo-jp-beginning-of-preedit-key? key key-state)
	   (yahoo-jp-end-of-preedit-key? key key-state)
	   (and
	    (modifier-key-mask key-state)
	    (not (shift-key-mask key-state))))
	  ;; go back to unselected prediction
	  (yahoo-jp-reset-prediction-window yc)
	  (yahoo-jp-check-prediction yc #f))
	 ((and
	   (ichar-numeric? key)
	   yahoo-jp-select-prediction-by-numeral-key?
	   (not (yahoo-jp-prediction-select-non-existing-index? yc key)))
	  (yahoo-jp-context-set-predicting! yc #f)
	  (yahoo-jp-context-set-prediction-index! yc #f)
	  (yahoo-jp-proc-input-state yc key key-state))
	 (else
	  ;; implicit commit
	  (yahoo-jp-do-commit-prediction yc)
	  (yahoo-jp-proc-input-state yc key key-state)))
	(begin
	  (yahoo-jp-context-set-predicting! yc #f)
	  (yahoo-jp-context-set-prediction-index! yc #f)
	  (if (not yahoo-jp-use-prediction?)
	      (yahoo-jp-reset-prediction-window yc))
	  (yahoo-jp-proc-input-state yc key key-state))))))

(define (yahoo-jp-proc-input-state-with-preedit yc key key-state)
  (define (check-auto-conv str)
    (and
      str
      yahoo-jp-auto-start-henkan?
      (string-find japanese-auto-start-henkan-keyword-list str)
      (begin
	(yahoo-jp-reset-prediction-window yc)
	(yahoo-jp-begin-conv yc))))
  (let ((preconv-str (yahoo-jp-context-preconv-ustr yc))
	(raw-str (yahoo-jp-context-raw-ustr yc))
	(rkc (yahoo-jp-context-rkc yc))
	(rule (yahoo-jp-context-input-rule yc))
	(kana (yahoo-jp-context-kana-mode yc)))
    (cond
     ;; begin conversion
     ((yahoo-jp-begin-conv-key? key key-state)
      (yahoo-jp-begin-conv yc))

     ;; prediction
     ((yahoo-jp-next-prediction-key? key key-state)
      (yahoo-jp-check-prediction yc #t))

     ;; backspace
     ((yahoo-jp-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (yahoo-jp-context-input-rule yc) yahoo-jp-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((yahoo-jp-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((yahoo-jp-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((yahoo-jp-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (yahoo-jp-context-alnum yc))
       (yahoo-jp-commit-as-opposite-kana-key? key key-state))
      (im-commit yc (yahoo-jp-make-whole-string yc #t (ja-opposite-kana kana)))
      (yahoo-jp-flush yc))

     ;; Transposing状態へ移行
     ((or (yahoo-jp-transpose-as-hiragana-key? key key-state)
	  (yahoo-jp-transpose-as-katakana-key? key key-state)
	  (yahoo-jp-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (yahoo-jp-context-input-rule yc) yahoo-jp-input-rule-kana))
	   (or
	    (yahoo-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (yahoo-jp-transpose-as-fullwidth-alnum-key? key key-state))))
      (yahoo-jp-reset-prediction-window yc)
      (yahoo-jp-proc-transposing-state yc key key-state))

     ((yahoo-jp-hiragana-key? key key-state)
      (if (not (= kana yahoo-jp-type-hiragana))
	  (begin
	    (im-commit yc (yahoo-jp-make-whole-string yc #t kana))
	    (yahoo-jp-flush yc)))
      (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-hiragana)
      (yahoo-jp-context-set-alnum! yc #f))

     ((yahoo-jp-katakana-key? key key-state)
      (if (not (= kana yahoo-jp-type-katakana))
	  (begin
	    (im-commit yc (yahoo-jp-make-whole-string yc #t kana))
	    (yahoo-jp-flush yc)))
      (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-katakana)
      (yahoo-jp-context-set-alnum! yc #f))

     ((yahoo-jp-halfkana-key? key key-state)
      (if (not (= kana yahoo-jp-type-halfkana))
	  (begin
	    (im-commit yc (yahoo-jp-make-whole-string yc #t kana))
	    (yahoo-jp-flush yc)))
      (yahoo-jp-context-set-kana-mode! yc yahoo-jp-type-halfkana)
      (yahoo-jp-context-set-alnum! yc #f))

     ((and
       (yahoo-jp-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-alnum-type yc) yahoo-jp-type-halfwidth-alnum)
	 (yahoo-jp-context-alnum yc))))
      (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-halfwidth-alnum)
      (yahoo-jp-context-set-alnum! yc #t))

     ((and
       (yahoo-jp-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (yahoo-jp-context-alnum-type yc) yahoo-jp-type-fullwidth-alnum)
	 (yahoo-jp-context-alnum yc))))
      (yahoo-jp-context-set-alnum-type! yc yahoo-jp-type-fullwidth-alnum)
      (yahoo-jp-context-set-alnum! yc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (yahoo-jp-context-alnum yc))
       (yahoo-jp-kana-toggle-key? key key-state))
      (im-commit yc (yahoo-jp-make-whole-string yc #t kana))
      (yahoo-jp-flush yc)
      (yahoo-jp-context-kana-toggle yc))

     ((yahoo-jp-alkana-toggle-key? key key-state)
      (yahoo-jp-context-alkana-toggle yc))

     ;; cancel
     ((yahoo-jp-cancel-key? key key-state)
      (yahoo-jp-flush yc))

     ;; commit
     ((yahoo-jp-commit-key? key key-state)
      (begin
	(im-commit
	 yc
	 (yahoo-jp-make-whole-string yc #t kana))
	(yahoo-jp-flush yc)))

     ;; left
     ((yahoo-jp-go-left-key? key key-state)
      (yahoo-jp-context-confirm-kana! yc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((yahoo-jp-go-right-key? key key-state)
      (yahoo-jp-context-confirm-kana! yc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((yahoo-jp-beginning-of-preedit-key? key key-state)
      (yahoo-jp-context-confirm-kana! yc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((yahoo-jp-end-of-preedit-key? key key-state)
      (yahoo-jp-context-confirm-kana! yc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      (if (yahoo-jp-context-alnum yc)
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
			       (if (= (yahoo-jp-context-alnum-type yc)
				      yahoo-jp-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str)
	    (check-auto-conv key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule yahoo-jp-input-rule-kana)
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

(define yahoo-jp-context-confirm-kana!
  (lambda (yc)
    (if (= (yahoo-jp-context-input-rule yc)
	   yahoo-jp-input-rule-kana)
	(let* ((preconv-str (yahoo-jp-context-preconv-ustr yc))
	       (rkc (yahoo-jp-context-rkc yc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
                (if (list? (car residual-kana))
                  (ustr-insert-seq! preconv-str residual-kana)
                  (ustr-insert-elem! preconv-str residual-kana))
		(rk-flush rkc)))))))

(define (yahoo-jp-reset-prediction-window yc)
  (if (yahoo-jp-context-prediction-window yc)
      (im-deactivate-candidate-selector yc))
  (yahoo-jp-context-set-predicting! yc #f)
  (yahoo-jp-context-set-prediction-window! yc #f)
  (yahoo-jp-context-set-prediction-index! yc #f))

(define (yahoo-jp-check-prediction yc force-check?)
  (if (and
       (not (yahoo-jp-context-state yc))
       (not (yahoo-jp-context-transposing yc))
       (not (yahoo-jp-context-predicting yc)))
      (let* ((use-pending-rk-for-prediction? #t)
             (preconv-str
              (yahoo-jp-make-whole-string
               yc
               (not use-pending-rk-for-prediction?)
               (yahoo-jp-context-kana-mode yc)))
             (preedit-len (+
                           (ustr-length (yahoo-jp-context-preconv-ustr yc))
                           (if (not use-pending-rk-for-prediction?)
                               0
                               (string-length (rk-pending
                                               (yahoo-jp-context-rkc
                                                yc)))))))
        (if (or
             (>= preedit-len yahoo-jp-prediction-start-char-count)
             force-check?)
            (begin
              (yahoo-jp-lib-set-prediction-src-string yc preconv-str)
              (let ((nr (yahoo-jp-lib-get-nr-predictions yc)))
                (if (and
                     nr
                     (> nr 0))
                    (begin
                     (im-activate-candidate-selector
                      yc nr yahoo-jp-nr-candidate-max)
                     (yahoo-jp-context-set-prediction-window! yc #t)
                     (yahoo-jp-context-set-predicting! yc #t))
                    (yahoo-jp-reset-prediction-window yc))))
            (yahoo-jp-reset-prediction-window yc)))))

(define (yahoo-jp-proc-input-state yc key key-state)
  (if (yahoo-jp-has-preedit? yc)
      (yahoo-jp-proc-input-state-with-preedit yc key key-state)
      (yahoo-jp-proc-input-state-no-preedit yc key key-state))
  (if yahoo-jp-use-prediction?
      (yahoo-jp-check-prediction yc #f)))

(define yahoo-jp-separator
  (lambda (yc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if yahoo-jp-show-segment-separator?
	  (cons attr yahoo-jp-segment-separator)
	  #f))))

(define yahoo-jp-context-transposing-state-preedit
  (lambda (yc)
    (let ((transposing-text (yahoo-jp-transposing-text yc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define yahoo-jp-transposing-text
  (lambda (yc)
    (let ((transposing-type (yahoo-jp-context-transposing-type yc)))
      (cond
       ((or
	 (= transposing-type yahoo-jp-type-hiragana)
	 (= transposing-type yahoo-jp-type-katakana)
	 (= transposing-type yahoo-jp-type-halfkana))
	(yahoo-jp-make-whole-string yc #t transposing-type))
       ((= transposing-type yahoo-jp-type-halfwidth-alnum)
	(yahoo-jp-make-whole-raw-string yc #f #f))
       ((= transposing-type yahoo-jp-candidate-type-upper-halfwidth-alnum)
	(yahoo-jp-make-whole-raw-string yc #f #t))
       ((= transposing-type yahoo-jp-type-fullwidth-alnum)
	(yahoo-jp-make-whole-raw-string yc #t #f))
       ((= transposing-type yahoo-jp-candidate-type-upper-fullwidth-alnum)
	(yahoo-jp-make-whole-raw-string yc #t #t))))))

(define yahoo-jp-get-raw-str-seq
  (lambda (yc)
    (let* ((rkc (yahoo-jp-context-rkc yc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (yahoo-jp-context-raw-ustr yc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
               (if (list? (car residual-kana))
                 (reverse (string-to-list pending))
		 (list pending))
               '())
	      right-str))))

(define yahoo-jp-get-raw-candidate
  (lambda (yc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (yahoo-jp-make-whole-string yc #t yahoo-jp-type-hiragana))))
	   (unconv-candidate (yahoo-jp-lib-get-unconv-candidate yc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (yahoo-jp-get-raw-str-seq yc))))
      (cond
       ((= cand-idx yahoo-jp-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx yahoo-jp-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) yahoo-jp-type-katakana))
       ((= cand-idx yahoo-jp-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) yahoo-jp-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if (and
                        start
                        (= (length raw-str) (length preconv))) ;; sanity check
		      (yahoo-jp-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx yahoo-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       yahoo-jp-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx yahoo-jp-candidate-type-halfwidth-alnum)
			    (= cand-idx yahoo-jp-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (yahoo-jp-predicting-state-preedit yc)
  (if (or
       (not yahoo-jp-use-implicit-commit-prediction?)
       (not (yahoo-jp-context-prediction-index yc)))
      (yahoo-jp-input-state-preedit yc)
      (let ((cand (yahoo-jp-get-prediction-string yc)))
       (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand)))))

(define (yahoo-jp-compose-state-preedit yc)
  (let* ((segments (yahoo-jp-context-segments yc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (yahoo-jp-separator yc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx yahoo-jp-candidate-type-katakana)
			(yahoo-jp-lib-get-nth-candidate yc seg-idx cand-idx)
			(yahoo-jp-get-raw-candidate yc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (yahoo-jp-input-state-preedit yc)
  (let* ((preconv-str (yahoo-jp-context-preconv-ustr yc))
	 (rkc (yahoo-jp-context-rkc yc))
	 (pending (rk-pending rkc))
	 (kana (yahoo-jp-context-kana-mode yc))
	 (rule (yahoo-jp-context-input-rule yc))
	 (extract-kana
	  (if (= rule yahoo-jp-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (yahoo-jp-has-preedit? yc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (yahoo-jp-get-commit-string yc)
  (let ((segments (yahoo-jp-context-segments yc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx yahoo-jp-candidate-type-katakana)
			     (yahoo-jp-lib-get-nth-candidate
			      yc seg-idx cand-idx)
			     (yahoo-jp-get-raw-candidate
			      yc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (yahoo-jp-commit-string yc)
    (let ((yx-ctx (yahoo-jp-context-yx-ctx yc))
          (segments (yahoo-jp-context-segments yc)))
      (if yx-ctx
          (begin
            (yahoo-jp-lib-commit-segments yc (ustr-whole-seq segments))
            (if (every (lambda (x) (<= x yahoo-jp-candidate-type-katakana))
                       (ustr-whole-seq segments))
                (yahoo-jp-lib-reset-conversion yc))))))

(define (yahoo-jp-do-commit yc)
    (im-commit yc (yahoo-jp-get-commit-string yc))
    (yahoo-jp-commit-string yc)
    (yahoo-jp-reset-candidate-window yc)
    (yahoo-jp-flush yc))

(define (yahoo-jp-get-prediction-string yc)
  (yahoo-jp-lib-get-nth-prediction
   yc
   (yahoo-jp-context-prediction-index yc)))

(define (yahoo-jp-learn-prediction-string yc)
  (yahoo-jp-lib-commit-nth-prediction
   yc
   (yahoo-jp-context-prediction-index yc)))

(define (yahoo-jp-do-commit-prediction yc)
  (im-commit yc (yahoo-jp-get-prediction-string yc))
  (yahoo-jp-learn-prediction-string yc)
  (yahoo-jp-reset-prediction-window yc)
  (yahoo-jp-flush yc))

(define yahoo-jp-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (yahoo-jp-move-segment yc dir)
  (yahoo-jp-reset-candidate-window yc)
  (let ((segments (yahoo-jp-context-segments yc)))
    (ustr-cursor-move! segments dir)
    (yahoo-jp-correct-segment-cursor segments)))

(define (yahoo-jp-resize-segment yc cnt)
  (let* ((segments (yahoo-jp-context-segments yc))
	 (cur-seg (ustr-cursor-pos segments)))
    (yahoo-jp-reset-candidate-window yc)
    (yahoo-jp-lib-resize-segment yc cur-seg cnt)
    (let* ((resized-nseg (yahoo-jp-lib-get-nr-segments yc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (yahoo-jp-move-candidate yc offset)
  (let* ((segments (yahoo-jp-context-segments yc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (yahoo-jp-lib-get-nr-candidates yc cur-seg))
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
	 (new-op-count (+ 1 (yahoo-jp-context-candidate-op-count yc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (yahoo-jp-context-set-candidate-op-count! yc new-op-count)
    (if (and
	 (= (yahoo-jp-context-candidate-op-count yc)
	    yahoo-jp-candidate-op-count)
	 yahoo-jp-use-candidate-window?)
	(begin
	  (yahoo-jp-context-set-candidate-window! yc #t)
	  (im-activate-candidate-selector yc max yahoo-jp-nr-candidate-max)))
    (if (yahoo-jp-context-candidate-window yc)
	(im-select-candidate yc compensated-n))))

(define yahoo-jp-move-candidate-in-page
  (lambda (yc numeralc)
    (let* ((segments (yahoo-jp-context-segments yc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (yahoo-jp-lib-get-nr-candidates yc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= yahoo-jp-nr-candidate-max 0)
			 0
			 (quotient n yahoo-jp-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page yahoo-jp-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (yahoo-jp-context-candidate-op-count yc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (yahoo-jp-context-set-candidate-op-count! yc new-op-count)
      (im-select-candidate yc compensated-idx))))

(define (yahoo-jp-reset-candidate-window yc)
  (if (yahoo-jp-context-candidate-window yc)
      (begin
	(im-deactivate-candidate-selector yc)
	(yahoo-jp-context-set-candidate-window! yc #f)))
  (yahoo-jp-context-set-candidate-op-count! yc 0))

(define yahoo-jp-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx yahoo-jp-candidate-type-halfwidth-alnum)
       (= state yahoo-jp-candidate-type-halfwidth-alnum))
      yahoo-jp-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx yahoo-jp-candidate-type-fullwidth-alnum)
       (= state yahoo-jp-candidate-type-fullwidth-alnum))
      yahoo-jp-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define yahoo-jp-set-segment-transposing
  (lambda (yc key key-state)
    (let ((segments (yahoo-jp-context-segments yc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(yahoo-jp-reset-candidate-window yc)
	(yahoo-jp-context-set-candidate-op-count! yc 0)

	(if (yahoo-jp-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons yahoo-jp-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (yahoo-jp-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons yahoo-jp-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (yahoo-jp-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons yahoo-jp-candidate-type-halfkana
				    rotate-list)))
	(if (yahoo-jp-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons yahoo-jp-candidate-type-katakana
				    rotate-list)))
	(if (yahoo-jp-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons yahoo-jp-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx yahoo-jp-candidate-type-hiragana)
	     (= idx yahoo-jp-candidate-type-katakana)
	     (= idx yahoo-jp-candidate-type-halfkana)
	     (= idx yahoo-jp-candidate-type-halfwidth-alnum)
	     (= idx yahoo-jp-candidate-type-fullwidth-alnum)
	     (= idx yahoo-jp-candidate-type-upper-halfwidth-alnum)
	     (= idx yahoo-jp-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (yahoo-jp-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (yahoo-jp-proc-compose-state yc key key-state)
  (cond
   ((yahoo-jp-prev-page-key? key key-state)
    (if (yahoo-jp-context-candidate-window yc)
        (im-shift-page-candidate yc #f)))

   ((yahoo-jp-next-page-key? key key-state)
    (if (yahoo-jp-context-candidate-window yc)
        (im-shift-page-candidate yc #t)))

   ((yahoo-jp-commit-key? key key-state)
    (yahoo-jp-do-commit yc))

   ((yahoo-jp-extend-segment-key? key key-state)
    (yahoo-jp-resize-segment yc 1))

   ((yahoo-jp-shrink-segment-key? key key-state)
    (yahoo-jp-resize-segment yc -1))

   ((yahoo-jp-next-segment-key? key key-state)
    (yahoo-jp-move-segment yc 1))

   ((yahoo-jp-prev-segment-key? key key-state)
    (yahoo-jp-move-segment yc -1))

   ((yahoo-jp-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (yahoo-jp-context-segments yc))
      (yahoo-jp-reset-candidate-window yc)))

   ((yahoo-jp-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (yahoo-jp-context-segments yc))
      (yahoo-jp-correct-segment-cursor (yahoo-jp-context-segments yc))
      (yahoo-jp-reset-candidate-window yc)))

   ((yahoo-jp-backspace-key? key key-state)
    (yahoo-jp-cancel-conv yc))

   ((yahoo-jp-next-candidate-key? key key-state)
    (yahoo-jp-move-candidate yc 1))

   ((yahoo-jp-prev-candidate-key? key key-state)
    (yahoo-jp-move-candidate yc -1))

   ((or (yahoo-jp-transpose-as-hiragana-key? key key-state)
        (yahoo-jp-transpose-as-katakana-key? key key-state)
        (yahoo-jp-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (yahoo-jp-context-input-rule yc) yahoo-jp-input-rule-kana))
         (or
          (yahoo-jp-transpose-as-halfwidth-alnum-key? key key-state)
          (yahoo-jp-transpose-as-fullwidth-alnum-key? key key-state))))
    (yahoo-jp-set-segment-transposing yc key key-state))

   ((yahoo-jp-cancel-key? key key-state)
    (yahoo-jp-cancel-conv yc))

   ((and yahoo-jp-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (yahoo-jp-context-candidate-window yc))
    (yahoo-jp-move-candidate-in-page yc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (yahoo-jp-do-commit yc)
      (yahoo-jp-proc-input-state yc key key-state)))))

(define (yahoo-jp-press-key-handler yc key key-state)
  (if (ichar-control? key)
      (im-commit-raw yc)
      (if (yahoo-jp-context-on yc)
          (if (yahoo-jp-context-transposing yc)
              (yahoo-jp-proc-transposing-state yc key key-state)
              (if (yahoo-jp-context-state yc)
                  (yahoo-jp-proc-compose-state yc key key-state)
                  (if (yahoo-jp-context-predicting yc)
                      (yahoo-jp-proc-prediction-state yc key key-state)
                      (yahoo-jp-proc-input-state yc key key-state))))
	  (yahoo-jp-proc-raw-state yc key key-state)))
  (yahoo-jp-update-preedit yc))

;;;
(define (yahoo-jp-release-key-handler yc key key-state)
  (if (or (ichar-control? key)
	  (not (yahoo-jp-context-on yc)))
      (yahoo-jp-commit-raw yc)))
;;;
(define (yahoo-jp-reset-handler yc)
  (if (yahoo-jp-context-on yc)
      (begin
	(if (yahoo-jp-context-state yc)
            (yahoo-jp-lib-reset-conversion yc))
	(yahoo-jp-flush yc))))

;;;
(define (yahoo-jp-get-candidate-handler yc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (yahoo-jp-context-segments yc)))
         (cand (if (yahoo-jp-context-state yc)
                   (yahoo-jp-lib-get-nth-candidate yc cur-seg idx)
                   (yahoo-jp-lib-get-nth-prediction yc idx))))
    (list cand (digit->string (+ idx 1)) "")))

(define (yahoo-jp-set-candidate-index-handler yc idx)
    (cond
     ((yahoo-jp-context-state yc)
      (ustr-cursor-set-frontside! (yahoo-jp-context-segments yc) idx)
      (yahoo-jp-update-preedit yc))
     ((yahoo-jp-context-predicting yc)
      (yahoo-jp-context-set-prediction-index! yc idx)
      (yahoo-jp-update-preedit yc))))

(define (yahoo-jp-proc-raw-state yc key key-state)
  (if (not (yahoo-jp-begin-input yc key key-state))
      (im-commit-raw yc)))

(yahoo-jp-configure-widgets)
(register-im
 'yahoo-jp
 "ja"
 "EUC-JP"
 yahoo-jp-im-name-label
 yahoo-jp-im-short-desc
 #f
 yahoo-jp-init-handler
 yahoo-jp-release-handler
 context-mode-handler
 yahoo-jp-press-key-handler
 yahoo-jp-release-key-handler
 yahoo-jp-reset-handler
 yahoo-jp-get-candidate-handler
 yahoo-jp-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
