;;; social-ime.scm: social-ime for uim.
;;;
;;; Copyright (c) 2008 uim Project http://code.google.com/p/uim/
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
(require "japanese-kana.scm")
(require "japanese-azik.scm")
(require-custom "generic-key-custom.scm")
(require-custom "social-ime-custom.scm")
(require-custom "social-ime-key-custom.scm")

(module-load "curl")

;;; implementations

;;
;; canna emulating functions
;;

(define social-ime-internal-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'str         "")
    (list 'candidates  '())
    (list 'seg-cnts '()))))
(define-record 'social-ime-internal-context social-ime-internal-context-rec-spec)
(define social-ime-internal-context-new-internal social-ime-internal-context-new)

(define (social-ime-conversion str opts)
  (define (make-query user)
        (format "~a?string=~a&charset=EUC-JP&applicartion=uim~a~a"
                social-ime-url
                (curl-url-escape str)
                user
                opts))
  (define (parse str)
    (and-let* ((ret1 (if (string? str)
                         (string-split str "\n")
                         '("")))
               (col (if (equal? '("") (take-right ret1 1))
                        (drop-right ret1 1)
                        ret1))
               (ret2 (map (lambda (s)
                            (and-let* ((ret (string-split s "\t"))
                                       (low (if (equal? '("") (take-right ret 1))
                                                (drop-right ret 1)
                                              ret)))
                                      low))
                          col)))
              ret2))
  (let* ((user (if (string=? social-ime-user "")
                   ""
                   (format "&user=~a" (curl-url-escape social-ime-user))))
         (ret (curl-fetch-simple (make-query user))))
    (if (string? ret)
        (parse ret)
        (list (list str)))))

(define (social-ime-conversion-make-resize-query seg-cnts)
  (apply string-append
         (map (lambda (idx)
                (let* ((cnt (list-ref seg-cnts idx))
                       (plusminus (if (< 0 cnt) "+" "")))
                  (if (= 0 cnt)
                      ""
                      (format "&resize[~a]=~a~a" idx plusminus cnt))))
              (iota (length seg-cnts)))))
(define (social-ime-conversion-resize str seg-cnts)
  (social-ime-conversion
   str
   (social-ime-conversion-make-resize-query seg-cnts)))

(define (social-ime-conversion-make-commit-query seg-cnts delta)
  (string-append
   (social-ime-conversion-make-resize-query seg-cnts)
   (apply string-append
          (map (lambda (idx seg)
                 (if (not (= seg 0))
                     (format "&commit[~a]=~a" idx seg)
                     ""))
               (iota (length delta))
               delta))))
(define (social-ime-send-commit str resize delta)
  (let ((ret (social-ime-conversion-make-commit-query resize delta)))
    (if (not (string=? ret ""))
        (social-ime-conversion str ret))))

(define (social-ime-lib-init)
  #t)
(define (social-ime-lib-alloc-context)
  #t)
(define (social-ime-lib-get-nth-candidate sc seg nth)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (cand (social-ime-internal-context-candidates sc-ctx)))
    (list-ref (list-ref cand seg) nth)))
(define (social-ime-lib-release-context sc)
  #t)
(define (social-ime-lib-get-unconv-candidate sc seg-idx)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (cand (social-ime-internal-context-candidates sc-ctx)))
    ;; XXX
    (car (take-right (list-ref cand seg-idx) 1))))
(define (social-ime-lib-get-nr-segments sc)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (cand (social-ime-internal-context-candidates sc-ctx)))
    (length cand)))
(define (social-ime-lib-get-nr-candidates sc seg)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (cand (social-ime-internal-context-candidates sc-ctx)))
    (length (list-ref cand seg))))
(define (social-ime-lib-resize-segment sc seg cnt)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (str (social-ime-internal-context-str sc-ctx))
         (cand (social-ime-internal-context-candidates sc-ctx))
         (seg-cnts (social-ime-internal-context-seg-cnts sc-ctx))
         (next-seg-cnts
          (map (lambda (idx)
                 (if (= idx seg)
                     (+ cnt (list-ref seg-cnts idx))
                     (list-ref seg-cnts idx)))
               (iota (length seg-cnts))))
         (next-cand (social-ime-conversion-resize str next-seg-cnts)))
    (if (and next-cand
             (not (equal? next-cand cand)))
        (begin
          (social-ime-internal-context-set-candidates!
           sc-ctx
           next-cand)
          (social-ime-internal-context-set-seg-cnts! sc-ctx next-seg-cnts)))
    #t))
(define (social-ime-lib-begin-conversion sc str)
  (let* ((cand (social-ime-conversion str ""))
         (sc-ctx (social-ime-internal-context-new-internal)))
    (social-ime-internal-context-set-str! sc-ctx str)
    (social-ime-internal-context-set-candidates! sc-ctx cand)
    (social-ime-internal-context-set-seg-cnts!
     sc-ctx
     (make-list (length cand) 0))
    (social-ime-context-set-sc-ctx! sc sc-ctx)
    (length cand)))
(define (social-ime-lib-commit-segments sc delta)
  (let* ((sc-ctx (social-ime-context-sc-ctx sc))
         (str (social-ime-internal-context-str sc-ctx))
         (seg-cnts (social-ime-internal-context-seg-cnts sc-ctx)))
    (social-ime-send-commit str seg-cnts delta)
    #t))
(define (social-ime-lib-reset-conversion sc)
  #f)


(define social-ime-init-lib-ok? #f)

(define social-ime-type-direct	   ja-type-direct)
(define social-ime-type-hiragana	   ja-type-hiragana)
(define social-ime-type-katakana	   ja-type-katakana)
(define social-ime-type-halfkana	   ja-type-halfkana)
(define social-ime-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define social-ime-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define social-ime-input-rule-roma 0)
(define social-ime-input-rule-kana 1)
(define social-ime-input-rule-azik 2)

(define social-ime-candidate-type-katakana -2)
(define social-ime-candidate-type-hiragana -3)
(define social-ime-candidate-type-halfkana -4)
(define social-ime-candidate-type-halfwidth-alnum -5)
(define social-ime-candidate-type-fullwidth-alnum -6)
(define social-ime-candidate-type-upper-halfwidth-alnum -7)
(define social-ime-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key social-ime-space-key? '(" "))

(define social-ime-prepare-input-rule-activation
  (lambda (sc)
    (cond
     ((social-ime-context-state sc)
      (social-ime-do-commit sc))
     ((social-ime-context-transposing sc)
      (im-commit sc (social-ime-transposing-text sc)))
     ((and
       (social-ime-context-on sc)
       (social-ime-has-preedit? sc))
      (im-commit
       sc (social-ime-make-whole-string sc #t (social-ime-context-kana-mode sc)))))
    (social-ime-flush sc)
    (social-ime-update-preedit sc)))

(define social-ime-prepare-input-mode-activation
  (lambda (sc new-mode)
    (let ((old-kana (social-ime-context-kana-mode sc)))
      (cond
       ((social-ime-context-state sc)
	(social-ime-do-commit sc))
       ((social-ime-context-transposing sc)
	(im-commit sc (social-ime-transposing-text sc))
	(social-ime-flush sc))
       ((and
	 (social-ime-context-on sc)
	 (social-ime-has-preedit? sc)
	 (not (= old-kana new-mode)))
	(im-commit
	 sc (social-ime-make-whole-string sc #t (social-ime-context-kana-mode sc)))
	(social-ime-flush sc)))
      (social-ime-update-preedit sc))))

(register-action 'action_social-ime_hiragana
		 (lambda (sc) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (sc) ;; activity predicate
		   (and (social-ime-context-on sc)
		        (not (social-ime-context-alnum sc))
			(= (social-ime-context-kana-mode sc)
			   social-ime-type-hiragana)))

		 (lambda (sc) ;; action handler
		   (social-ime-prepare-input-mode-activation sc social-ime-type-hiragana)
		   (social-ime-context-set-on! sc #t)
		   (social-ime-context-set-alnum! sc #f)
		   (social-ime-context-change-kana-mode! sc social-ime-type-hiragana)))

(register-action 'action_social-ime_katakana
		 (lambda (sc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (sc)
		   (and (social-ime-context-on sc)
		        (not (social-ime-context-alnum sc))
			(= (social-ime-context-kana-mode sc)
			   social-ime-type-katakana)))
		 (lambda (sc)
		   (social-ime-prepare-input-mode-activation sc social-ime-type-katakana)
		   (social-ime-context-set-on! sc #t)
		   (social-ime-context-set-alnum! sc #f)
		   (social-ime-context-change-kana-mode! sc social-ime-type-katakana)))

(register-action 'action_social-ime_halfkana
		 (lambda (sc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (sc)
		   (and (social-ime-context-on sc)
			(not (social-ime-context-alnum sc))
			(= (social-ime-context-kana-mode sc) social-ime-type-halfkana)))
		 (lambda (sc)
		   (social-ime-prepare-input-mode-activation sc social-ime-type-halfkana)
		   (social-ime-context-set-on! sc #t)
		   (social-ime-context-set-alnum! sc #f)
		   (social-ime-context-change-kana-mode! sc social-ime-type-halfkana)))

(register-action 'action_social-ime_halfwidth_alnum
		 (lambda (sc) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (sc) ;; activity predicate
		   (and (social-ime-context-on sc)
			(social-ime-context-alnum sc)
			(= (social-ime-context-alnum-type sc)
			   social-ime-type-halfwidth-alnum)))
		 (lambda (sc) ;; action handler
		   (social-ime-prepare-input-mode-activation
		    sc (social-ime-context-kana-mode sc))
		   (social-ime-context-set-on! sc #t)
		   (social-ime-context-set-alnum! sc #t)
		   (social-ime-context-set-alnum-type!
		    sc social-ime-type-halfwidth-alnum)))

(register-action 'action_social-ime_direct
		 (lambda (sc)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (sc)
		   (not (social-ime-context-on sc)))
		 (lambda (sc)
		   (social-ime-prepare-input-mode-activation sc social-ime-type-direct)
		   (social-ime-context-set-on! sc #f)))

(register-action 'action_social-ime_fullwidth_alnum
		 (lambda (sc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (sc)
		   (and (social-ime-context-on sc)
			(social-ime-context-alnum sc)
			(= (social-ime-context-alnum-type sc)
			   social-ime-type-fullwidth-alnum)))
		 (lambda (sc)
		   (social-ime-prepare-input-mode-activation
		    sc (social-ime-context-kana-mode sc))
		   (social-ime-context-set-on! sc #t)
		   (social-ime-context-set-alnum! sc #t)
		   (social-ime-context-set-alnum-type!
		    sc social-ime-type-fullwidth-alnum)))

(register-action 'action_social-ime_roma
		 (lambda (sc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (sc)
		   (= (social-ime-context-input-rule sc)
		      social-ime-input-rule-roma))
		 (lambda (sc)
		   (social-ime-prepare-input-rule-activation sc)
		   (rk-context-set-rule! (social-ime-context-rkc sc)
					 ja-rk-rule)
		   (social-ime-context-set-input-rule! sc social-ime-input-rule-roma)))

(register-action 'action_social-ime_kana
		 (lambda (sc)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (sc)
		   (= (social-ime-context-input-rule sc)
		      social-ime-input-rule-kana))
		 (lambda (sc)
		   (social-ime-prepare-input-rule-activation sc)
		   (social-ime-context-set-input-rule! sc social-ime-input-rule-kana)
                   (social-ime-context-change-kana-mode!
                     sc (social-ime-context-kana-mode sc))
		   (social-ime-context-set-alnum! sc #f)))

(register-action 'action_social-ime_azik
		 (lambda (sc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (sc)
		   (= (social-ime-context-input-rule sc)
		      social-ime-input-rule-azik))
		 (lambda (sc)
		   (social-ime-prepare-input-rule-activation sc)
		   (rk-context-set-rule! (social-ime-context-rkc sc)
					 ja-azik-rule)
		   (social-ime-context-set-input-rule! sc social-ime-input-rule-azik)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define social-ime-configure-widgets
  (lambda ()
    (register-widget 'widget_social-ime_input_mode
		     (activity-indicator-new social-ime-input-mode-actions)
		     (actions-new social-ime-input-mode-actions))

    (register-widget 'widget_social-ime_kana_input_method
		     (activity-indicator-new social-ime-kana-input-method-actions)
		     (actions-new social-ime-kana-input-method-actions))
    (context-list-replace-widgets! 'social-ime social-ime-widgets)))

(define social-ime-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'sc-ctx             ()) ;; social-ime-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'kana-mode          social-ime-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      social-ime-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         social-ime-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'social-ime-context social-ime-context-rec-spec)
(define social-ime-context-new-internal social-ime-context-new)

(define (social-ime-context-new id im)
  (let ((sc (social-ime-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (social-ime-context-set-sc-ctx! sc (if social-ime-init-lib-ok?
;				      (social-ime-lib-alloc-context) ()))
    (social-ime-context-set-sc-ctx! sc (social-ime-lib-alloc-context))
    (social-ime-context-set-widgets! sc social-ime-widgets)
    (social-ime-context-set-rkc! sc rkc)
    (social-ime-context-set-preconv-ustr! sc (ustr-new '()))
    (social-ime-context-set-raw-ustr! sc (ustr-new '()))
    (social-ime-context-set-segments! sc (ustr-new '()))
    (if using-kana-table?
        (social-ime-context-set-input-rule! sc social-ime-input-rule-kana)
        (social-ime-context-set-input-rule! sc social-ime-input-rule-roma))
    sc))

(define (social-ime-commit-raw sc)
  (im-commit-raw sc)
  (social-ime-context-set-commit-raw! sc #t))

(define (social-ime-context-kana-toggle sc)
  (let* ((kana (social-ime-context-kana-mode sc))
	 (opposite-kana (ja-opposite-kana kana)))
    (social-ime-context-change-kana-mode! sc opposite-kana)))

(define social-ime-context-alkana-toggle
  (lambda (sc)
    (let ((alnum-state (social-ime-context-alnum sc)))
      (social-ime-context-set-alnum! sc (not alnum-state)))))

(define social-ime-context-change-kana-mode!
  (lambda (sc kana-mode)
    (if (= (social-ime-context-input-rule sc)
           social-ime-input-rule-kana)
        (rk-context-set-rule!
	 (social-ime-context-rkc sc)
	 (cond
	  ((= kana-mode social-ime-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode social-ime-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode social-ime-type-halfkana) ja-kana-halfkana-rule))))
    (social-ime-context-set-kana-mode! sc kana-mode)))

(define social-ime-make-whole-string
  (lambda (sc convert-pending-into-kana? kana)
    (let* ((rkc (social-ime-context-rkc sc))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (social-ime-context-input-rule sc))
           (preconv-str (social-ime-context-preconv-ustr sc))
           (extract-kana
            (if (= rule social-ime-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule social-ime-input-rule-kana)
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

(define social-ime-make-raw-string
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
	     (social-ime-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (social-ime-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define social-ime-make-whole-raw-string
  (lambda (sc wide? upper?)
    (social-ime-make-raw-string (social-ime-get-raw-str-seq sc) wide? upper?)))

(define (social-ime-init-handler id im arg)
  (if (not social-ime-init-lib-ok?)
      (begin
	(social-ime-lib-init)
	(set! social-ime-init-lib-ok? #t)))
  (social-ime-context-new id im))

(define (social-ime-release-handler sc)
  (if sc
      (social-ime-lib-release-context sc)))

(define (social-ime-flush sc)
  (rk-flush (social-ime-context-rkc sc))
  (ustr-clear! (social-ime-context-preconv-ustr sc))
  (ustr-clear! (social-ime-context-raw-ustr sc))
  (ustr-clear! (social-ime-context-segments sc))
  (social-ime-context-set-transposing! sc #f)
  (social-ime-context-set-state! sc #f)
  (if (social-ime-context-candidate-window sc)
      (im-deactivate-candidate-selector sc))
  (social-ime-context-set-candidate-window! sc #f)
  (social-ime-context-set-candidate-op-count! sc 0))

(define (social-ime-begin-input sc key key-state)
  (if (cond
       ((social-ime-on-key? key key-state)
	#t)
       ((and
	 social-ime-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((social-ime-hiragana-key? key key-state)
	   (social-ime-context-set-kana-mode! sc social-ime-type-hiragana)
	   (social-ime-context-set-alnum! sc #f)
	   #t)
	  ((social-ime-katakana-key? key key-state)
	   (social-ime-context-set-kana-mode! sc social-ime-type-katakana)
	   (social-ime-context-set-alnum! sc #f)
	   #t)
	  ((social-ime-halfkana-key? key key-state)
	   (social-ime-context-set-kana-mode! sc social-ime-type-halfkana)
	   (social-ime-context-set-alnum! sc #f)
	   #t)
	  ((social-ime-halfwidth-alnum-key? key key-state)
	   (social-ime-context-set-alnum-type! sc social-ime-type-halfwidth-alnum)
	   (social-ime-context-set-alnum! sc #t)
	   #t)
	  ((social-ime-halfwidth-alnum-key? key key-state)
	   (social-ime-context-set-alnum-type! sc social-ime-type-fullwidth-alnum)
	   (social-ime-context-set-alnum! sc #t)
	   #t)
	  ((social-ime-kana-toggle-key? key key-state)
	   (social-ime-context-kana-toggle sc)
	   (social-ime-context-set-alnum! sc #f)
	   #t)
	  ((social-ime-alkana-toggle-key? key key-state)
	   (social-ime-context-alkana-toggle sc)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(social-ime-context-set-on! sc #t)
	(rk-flush (social-ime-context-rkc sc))
	(social-ime-context-set-state! sc #f)
	#t)
      #f))

(define (social-ime-update-preedit sc)
  (if (not (social-ime-context-commit-raw sc))
      (let ((segments (if (social-ime-context-on sc)
			  (if (social-ime-context-transposing sc)
			      (social-ime-context-transposing-state-preedit sc)
			      (if (social-ime-context-state sc)
				  (social-ime-compose-state-preedit sc)
				  (social-ime-input-state-preedit sc)))
			  ())))
	(context-update-preedit sc segments))
      (social-ime-context-set-commit-raw! sc #f)))

(define (social-ime-begin-conv sc)
  (let ((sc-ctx (social-ime-context-sc-ctx sc))
	(preconv-str (social-ime-make-whole-string sc #t social-ime-type-hiragana)))
    (if (and sc-ctx
             (> (string-length preconv-str) 0))
	(let ((num (social-ime-lib-begin-conversion sc preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (social-ime-context-segments sc)
		 (make-list num 0))
		(social-ime-context-set-state! sc #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; social-ime-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define social-ime-cancel-conv
  (lambda (sc)
    (social-ime-reset-candidate-window sc)
    (social-ime-context-set-state! sc #f)
    (ustr-clear! (social-ime-context-segments sc))
    (social-ime-lib-reset-conversion sc)))

(define (social-ime-proc-input-state-no-preedit sc key key-state)
  (let
      ((rkc (social-ime-context-rkc sc))
       (direct (ja-direct (charcode->string key)))
       (rule (social-ime-context-input-rule sc)))
    (cond
     ((and social-ime-use-with-vi?
           (social-ime-vi-escape-key? key key-state))
      (social-ime-flush sc)
      (social-ime-context-set-on! sc #f)
      (social-ime-commit-raw sc))

     ((social-ime-off-key? key key-state)
      (social-ime-flush sc)
      (social-ime-context-set-on! sc #f))

     ((social-ime-backspace-key? key key-state)
      (social-ime-commit-raw sc))
     
     ((social-ime-delete-key? key key-state)
      (social-ime-commit-raw sc))

     ((and
       (social-ime-hiragana-key? key key-state)
       (not
        (and
	 (= (social-ime-context-kana-mode sc) social-ime-type-hiragana)
	 (not (social-ime-context-alnum sc)))))
      (social-ime-context-change-kana-mode! sc social-ime-type-hiragana)
      (social-ime-context-set-alnum! sc #f))

     ((and
       (social-ime-katakana-key? key key-state)
       (not
        (and
	 (= (social-ime-context-kana-mode sc) social-ime-type-katakana)
	 (not (social-ime-context-alnum sc)))))
      (social-ime-context-change-kana-mode! sc social-ime-type-katakana)
      (social-ime-context-set-alnum! sc #f))
     
     ((and
       (social-ime-halfkana-key? key key-state)
       (not
        (and
	 (= (social-ime-context-kana-mode sc) social-ime-type-halfkana)
	 (not (social-ime-context-alnum sc)))))
      (social-ime-context-change-kana-mode! sc social-ime-type-halfkana)
      (social-ime-context-set-alnum! sc #f))
     
     ((and
       (social-ime-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (social-ime-context-alnum-type sc) social-ime-type-halfwidth-alnum)
	 (social-ime-context-alnum sc))))
      (social-ime-context-set-alnum-type! sc social-ime-type-halfwidth-alnum)
      (social-ime-context-set-alnum! sc #t))
     
     ((and
       (social-ime-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (social-ime-context-alnum-type sc) social-ime-type-fullwidth-alnum)
	 (social-ime-context-alnum sc))))
      (social-ime-context-set-alnum-type! sc social-ime-type-fullwidth-alnum)
      (social-ime-context-set-alnum! sc #t))
     
     ((and
       (not (social-ime-context-alnum sc))
       (social-ime-kana-toggle-key? key key-state))
      (social-ime-context-kana-toggle sc))

     ((social-ime-alkana-toggle-key? key key-state)
      (social-ime-context-alkana-toggle sc))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (social-ime-commit-raw sc))
     
     ;; direct key => commit
     (direct
      (im-commit sc direct))

     ;; space key
     ((social-ime-space-key? key key-state)
      (if (social-ime-context-alnum sc)
	  (im-commit sc (list-ref
			 ja-alnum-space
			 (- (social-ime-context-alnum-type sc)
			    social-ime-type-halfwidth-alnum)))
	  (im-commit sc (list-ref ja-space (social-ime-context-kana-mode sc)))))

     ((symbol? key)
      (social-ime-commit-raw sc))

     (else
      (if (social-ime-context-alnum sc)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (social-ime-context-preconv-ustr sc)
			       (if (= (social-ime-context-alnum-type sc)
				      social-ime-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (social-ime-context-raw-ustr sc) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule social-ime-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
	          (ustr-insert-elem! (social-ime-context-preconv-ustr sc) res)
	          (ustr-insert-elem! (social-ime-context-raw-ustr sc) key-str))
	        (if (null? (rk-context-seq rkc))
		    (social-ime-commit-raw sc)))))))))

(define (social-ime-has-preedit? sc)
  (or (not (ustr-empty? (social-ime-context-preconv-ustr sc)))
      (> (string-length (rk-pending (social-ime-context-rkc sc))) 0)))

(define social-ime-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type social-ime-type-halfwidth-alnum)
       (= state social-ime-type-halfwidth-alnum))
      social-ime-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type social-ime-type-fullwidth-alnum)
       (= state social-ime-type-fullwidth-alnum))
      social-ime-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define social-ime-proc-transposing-state
  (lambda (sc key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (social-ime-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons social-ime-type-fullwidth-alnum rotate-list)))
      (if (social-ime-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons social-ime-type-halfwidth-alnum rotate-list)))
      (if (social-ime-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons social-ime-type-halfkana rotate-list)))
      (if (social-ime-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons social-ime-type-katakana rotate-list)))
      (if (social-ime-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons social-ime-type-hiragana rotate-list)))

      (if (social-ime-context-transposing sc)
	  (let ((lst (member (social-ime-context-transposing-type sc) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (social-ime-rotate-transposing-alnum-type
				 (social-ime-context-transposing-type sc)
				 (car rotate-list))))))
	  (begin
	    (social-ime-context-set-transposing! sc #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state social-ime-type-hiragana)
	      (= state social-ime-type-katakana)
	      (= state social-ime-type-halfkana)))
	(social-ime-context-set-transposing-type! sc state))
       ((and state
	     (or
	      (= state social-ime-type-halfwidth-alnum)
	      (= state social-ime-candidate-type-upper-halfwidth-alnum)
	      (= state social-ime-type-fullwidth-alnum)
	      (= state social-ime-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (social-ime-context-input-rule sc) social-ime-input-rule-kana))
	    (social-ime-context-set-transposing-type! sc state)))
       (else
	(and
	 ; commit
	 (if (social-ime-commit-key? key key-state)
	     (begin
	       (im-commit sc (social-ime-transposing-text sc))
	       (social-ime-flush sc)
	       #f)
	     #t)
	 ; begin-conv
	 (if (social-ime-begin-conv-key? key key-state)
	     (begin
	       (social-ime-context-set-transposing! sc #f)
	       (social-ime-begin-conv sc)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (social-ime-cancel-key? key key-state)
	      (social-ime-backspace-key? key key-state))
	     (begin
	       (social-ime-context-set-transposing! sc #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (social-ime-prev-page-key? key key-state)
	      (social-ime-next-page-key? key key-state)
	      (social-ime-extend-segment-key? key key-state)
	      (social-ime-shrink-segment-key? key key-state)
	      (social-ime-next-segment-key? key key-state)
	      (social-ime-beginning-of-preedit-key? key key-state)
	      (social-ime-end-of-preedit-key? key key-state)
	      (social-ime-next-candidate-key? key key-state)
	      (social-ime-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit sc (social-ime-transposing-text sc))
	   (social-ime-flush sc)
	   (social-ime-proc-input-state sc key key-state))))))))

(define (social-ime-proc-input-state-with-preedit sc key key-state)
  (let ((preconv-str (social-ime-context-preconv-ustr sc))
	(raw-str (social-ime-context-raw-ustr sc))
	(rkc (social-ime-context-rkc sc))
	(rule (social-ime-context-input-rule sc))
	(kana (social-ime-context-kana-mode sc)))
    (cond
     ;; begin conversion
     ((social-ime-begin-conv-key? key key-state)
      (social-ime-begin-conv sc))

     ;; backspace
     ((social-ime-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (social-ime-context-input-rule sc) social-ime-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((social-ime-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((social-ime-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((social-ime-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (social-ime-context-alnum sc))
       (social-ime-commit-as-opposite-kana-key? key key-state))
      (im-commit sc (social-ime-make-whole-string sc #t (ja-opposite-kana kana)))
      (social-ime-flush sc))

     ;; Transposing状態へ移行
     ((or (social-ime-transpose-as-hiragana-key? key key-state)
	  (social-ime-transpose-as-katakana-key? key key-state)
	  (social-ime-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (social-ime-context-input-rule sc) social-ime-input-rule-kana))
	   (or
	    (social-ime-transpose-as-halfwidth-alnum-key? key key-state)
	    (social-ime-transpose-as-fullwidth-alnum-key? key key-state))))
      (social-ime-proc-transposing-state sc key key-state))

     ((social-ime-hiragana-key? key key-state)
      (if (not (= kana social-ime-type-hiragana))
	  (begin
	    (im-commit sc (social-ime-make-whole-string sc #t kana))
	    (social-ime-flush sc)))
      (social-ime-context-set-kana-mode! sc social-ime-type-hiragana)
      (social-ime-context-set-alnum! sc #f))

     ((social-ime-katakana-key? key key-state)
      (if (not (= kana social-ime-type-katakana))
	  (begin
	    (im-commit sc (social-ime-make-whole-string sc #t kana))
	    (social-ime-flush sc)))
      (social-ime-context-set-kana-mode! sc social-ime-type-katakana)
      (social-ime-context-set-alnum! sc #f))

     ((social-ime-halfkana-key? key key-state)
      (if (not (= kana social-ime-type-halfkana))
	  (begin
	    (im-commit sc (social-ime-make-whole-string sc #t kana))
	    (social-ime-flush sc)))
      (social-ime-context-set-kana-mode! sc social-ime-type-halfkana)
      (social-ime-context-set-alnum! sc #f))

     ((and
       (social-ime-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (social-ime-context-alnum-type sc) social-ime-type-halfwidth-alnum)
	 (social-ime-context-alnum sc))))
      (social-ime-context-set-alnum-type! sc social-ime-type-halfwidth-alnum)
      (social-ime-context-set-alnum! sc #t))

     ((and
       (social-ime-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (social-ime-context-alnum-type sc) social-ime-type-fullwidth-alnum)
	 (social-ime-context-alnum sc))))
      (social-ime-context-set-alnum-type! sc social-ime-type-fullwidth-alnum)
      (social-ime-context-set-alnum! sc #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (social-ime-context-alnum sc))
       (social-ime-kana-toggle-key? key key-state))
      (im-commit sc (social-ime-make-whole-string sc #t kana))
      (social-ime-flush sc)
      (social-ime-context-kana-toggle sc))

     ((social-ime-alkana-toggle-key? key key-state)
      (social-ime-context-alkana-toggle sc))

     ;; cancel
     ((social-ime-cancel-key? key key-state)
      (social-ime-flush sc))

     ;; commit
     ((social-ime-commit-key? key key-state)
      (begin
	(im-commit
	 sc
	 (social-ime-make-whole-string sc #t kana))
	(social-ime-flush sc)))

     ;; left
     ((social-ime-go-left-key? key key-state)
      (social-ime-context-confirm-kana! sc)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((social-ime-go-right-key? key key-state)
      (social-ime-context-confirm-kana! sc)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((social-ime-beginning-of-preedit-key? key key-state)
      (social-ime-context-confirm-kana! sc)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((social-ime-end-of-preedit-key? key key-state)
      (social-ime-context-confirm-kana! sc)
      (ustr-cursor-move-end! preconv-str)
      (ustr-cursor-move-end! raw-str))

     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	      (not (shift-key-mask key-state)))
      #f)

     ((symbol? key)
      #f)

     (else
      ;; handle "n1" sequence as "ん1"
      (if (and (not (social-ime-context-alnum sc))
	       (not (ichar-alphabetic? key))
	       (not (string-find
		     (rk-expect rkc)
		     (charcode->string
		      (if (= rule social-ime-input-rule-kana)
			  key
			  (ichar-downcase key))))))
	  (let ((pend (rk-pending rkc))
		(residual-kana (rk-push-key-last! rkc)))
	    (if residual-kana
		(begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))))

      (if (social-ime-context-alnum sc)
          (let ((key-str (charcode->string key))
	        (pend (rk-pending rkc))
		(residual-kana (rk-peek-terminal-match rkc)))
	    (rk-flush rkc) ;; OK to reset rkc here.
	    (if residual-kana
	        (begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))
	    (ustr-insert-elem! preconv-str
			       (if (= (social-ime-context-alnum-type sc)
				      social-ime-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule social-ime-input-rule-kana)
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
		      (ustr-insert-elem! raw-str pend)
		      (if (list? (car res))
		          (begin
			    (ustr-insert-elem! raw-str pend)
			    (ustr-insert-elem! raw-str key-str))
		          (ustr-insert-elem!
		           raw-str
		           (string-append pend key-str))))))))))))

(define social-ime-context-confirm-kana!
  (lambda (sc)
    (if (= (social-ime-context-input-rule sc)
	   social-ime-input-rule-kana)
	(let* ((preconv-str (social-ime-context-preconv-ustr sc))
	       (rkc (social-ime-context-rkc sc))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
		(ustr-insert-elem! preconv-str residual-kana)
		(rk-flush rkc)))))))

(define (social-ime-proc-input-state sc key key-state)
  (if (social-ime-has-preedit? sc)
      (social-ime-proc-input-state-with-preedit sc key key-state)
      (social-ime-proc-input-state-no-preedit sc key key-state)))

(define social-ime-separator
  (lambda (sc)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if social-ime-show-segment-separator?
	  (cons attr social-ime-segment-separator)
	  #f))))

(define social-ime-context-transposing-state-preedit
  (lambda (sc)
    (let ((transposing-text (social-ime-transposing-text sc)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define social-ime-transposing-text
  (lambda (sc)
    (let ((transposing-type (social-ime-context-transposing-type sc)))
      (cond
       ((or
	 (= transposing-type social-ime-type-hiragana)
	 (= transposing-type social-ime-type-katakana)
	 (= transposing-type social-ime-type-halfkana))
	(social-ime-make-whole-string sc #t transposing-type))
       ((= transposing-type social-ime-type-halfwidth-alnum)
	(social-ime-make-whole-raw-string sc #f #f))
       ((= transposing-type social-ime-candidate-type-upper-halfwidth-alnum)
	(social-ime-make-whole-raw-string sc #f #t))
       ((= transposing-type social-ime-type-fullwidth-alnum)
	(social-ime-make-whole-raw-string sc #t #f))
       ((= transposing-type social-ime-candidate-type-upper-fullwidth-alnum)
	(social-ime-make-whole-raw-string sc #t #t))))))

(define social-ime-get-raw-str-seq
  (lambda (sc)
    (let* ((rkc (social-ime-context-rkc sc))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (social-ime-context-raw-ustr sc))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
		 (list pending)
		 '())
	      right-str))))

(define social-ime-get-raw-candidate
  (lambda (sc seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (social-ime-make-whole-string sc #t social-ime-type-hiragana))))
	   (unconv-candidate (social-ime-lib-get-unconv-candidate sc seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (social-ime-get-raw-str-seq sc))))
      (cond
       ((= cand-idx social-ime-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx social-ime-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) social-ime-type-katakana))
       ((= cand-idx social-ime-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) social-ime-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if start
		      (social-ime-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx social-ime-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       social-ime-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx social-ime-candidate-type-halfwidth-alnum)
			    (= cand-idx social-ime-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (social-ime-compose-state-preedit sc)
  (let* ((segments (social-ime-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (social-ime-separator sc)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx social-ime-candidate-type-katakana)
			(social-ime-lib-get-nth-candidate sc seg-idx cand-idx)
			(social-ime-get-raw-candidate sc seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (social-ime-input-state-preedit sc)
  (let* ((preconv-str (social-ime-context-preconv-ustr sc))
	 (rkc (social-ime-context-rkc sc))
	 (pending (rk-pending rkc))
	 (kana (social-ime-context-kana-mode sc))
	 (rule (social-ime-context-input-rule sc))
	 (extract-kana
	  (if (= rule social-ime-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (social-ime-has-preedit? sc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (social-ime-get-commit-string sc)
  (let ((segments (social-ime-context-segments sc)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx social-ime-candidate-type-katakana)
			     (social-ime-lib-get-nth-candidate
			      sc seg-idx cand-idx)
			     (social-ime-get-raw-candidate
			      sc seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (social-ime-commit-string sc)
    (let ((sc-ctx (social-ime-context-sc-ctx sc))
          (segments (social-ime-context-segments sc)))
      (if sc-ctx
          (begin
            (social-ime-lib-commit-segments sc (ustr-whole-seq segments))
            (if (every (lambda (x) (<= x social-ime-candidate-type-katakana))
                       (ustr-whole-seq segments))
                (social-ime-lib-reset-conversion sc))))))

(define (social-ime-do-commit sc)
    (im-commit sc (social-ime-get-commit-string sc))
    (social-ime-commit-string sc)
    (social-ime-reset-candidate-window sc)
    (social-ime-flush sc))

(define social-ime-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (social-ime-move-segment sc dir)
  (social-ime-reset-candidate-window sc)
  (let ((segments (social-ime-context-segments sc)))
    (ustr-cursor-move! segments dir)
    (social-ime-correct-segment-cursor segments)))

(define (social-ime-resize-segment sc cnt)
  (let* ((segments (social-ime-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments)))
    (social-ime-reset-candidate-window sc)
    (social-ime-lib-resize-segment sc cur-seg cnt)
    (let* ((resized-nseg (social-ime-lib-get-nr-segments sc))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (social-ime-move-candidate sc offset)
  (let* ((segments (social-ime-context-segments sc))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (social-ime-lib-get-nr-candidates sc cur-seg))
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
	 (new-op-count (+ 1 (social-ime-context-candidate-op-count sc))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (social-ime-context-set-candidate-op-count! sc new-op-count)
    (if (and
	 (= (social-ime-context-candidate-op-count sc)
	    social-ime-candidate-op-count)
	 social-ime-use-candidate-window?)
	(begin
	  (social-ime-context-set-candidate-window! sc #t)
	  (im-activate-candidate-selector sc max social-ime-nr-candidate-max)))
    (if (social-ime-context-candidate-window sc)
	(im-select-candidate sc compensated-n))))

(define social-ime-move-candidate-in-page
  (lambda (sc numeralc)
    (let* ((segments (social-ime-context-segments sc))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (social-ime-lib-get-nr-candidates sc cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= social-ime-nr-candidate-max 0)
			 0
			 (quotient n social-ime-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page social-ime-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (social-ime-context-candidate-op-count sc))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (social-ime-context-set-candidate-op-count! sc new-op-count)
      (im-select-candidate sc compensated-idx))))

(define (social-ime-reset-candidate-window sc)
  (if (social-ime-context-candidate-window sc)
      (begin
	(im-deactivate-candidate-selector sc)
	(social-ime-context-set-candidate-window! sc #f)))
  (social-ime-context-set-candidate-op-count! sc 0))

(define social-ime-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx social-ime-candidate-type-halfwidth-alnum)
       (= state social-ime-candidate-type-halfwidth-alnum))
      social-ime-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx social-ime-candidate-type-fullwidth-alnum)
       (= state social-ime-candidate-type-fullwidth-alnum))
      social-ime-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define social-ime-set-segment-transposing
  (lambda (sc key key-state)
    (let ((segments (social-ime-context-segments sc)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(social-ime-reset-candidate-window sc)
	(social-ime-context-set-candidate-op-count! sc 0)

	(if (social-ime-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons social-ime-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (social-ime-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons social-ime-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (social-ime-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons social-ime-candidate-type-halfkana
				    rotate-list)))
	(if (social-ime-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons social-ime-candidate-type-katakana
				    rotate-list)))
	(if (social-ime-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons social-ime-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx social-ime-candidate-type-hiragana)
	     (= idx social-ime-candidate-type-katakana)
	     (= idx social-ime-candidate-type-halfkana)
	     (= idx social-ime-candidate-type-halfwidth-alnum)
	     (= idx social-ime-candidate-type-fullwidth-alnum)
	     (= idx social-ime-candidate-type-upper-halfwidth-alnum)
	     (= idx social-ime-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (social-ime-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (social-ime-proc-compose-state sc key key-state)
  (cond
   ((social-ime-prev-page-key? key key-state)
    (if (social-ime-context-candidate-window sc)
        (im-shift-page-candidate sc #f)))

   ((social-ime-next-page-key? key key-state)
    (if (social-ime-context-candidate-window sc)
        (im-shift-page-candidate sc #t)))

   ((social-ime-commit-key? key key-state)
    (social-ime-do-commit sc))

   ((social-ime-extend-segment-key? key key-state)
    (social-ime-resize-segment sc 1))

   ((social-ime-shrink-segment-key? key key-state)
    (social-ime-resize-segment sc -1))

   ((social-ime-next-segment-key? key key-state)
    (social-ime-move-segment sc 1))

   ((social-ime-prev-segment-key? key key-state)
    (social-ime-move-segment sc -1))

   ((social-ime-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (social-ime-context-segments sc))
      (social-ime-reset-candidate-window sc)))

   ((social-ime-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (social-ime-context-segments sc))
      (social-ime-correct-segment-cursor (social-ime-context-segments sc))
      (social-ime-reset-candidate-window sc)))

   ((social-ime-backspace-key? key key-state)
    (social-ime-cancel-conv sc))

   ((social-ime-next-candidate-key? key key-state)
    (social-ime-move-candidate sc 1))

   ((social-ime-prev-candidate-key? key key-state)
    (social-ime-move-candidate sc -1))

   ((or (social-ime-transpose-as-hiragana-key? key key-state)
        (social-ime-transpose-as-katakana-key? key key-state)
        (social-ime-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (social-ime-context-input-rule sc) social-ime-input-rule-kana))
         (or
          (social-ime-transpose-as-halfwidth-alnum-key? key key-state)
          (social-ime-transpose-as-fullwidth-alnum-key? key key-state))))
    (social-ime-set-segment-transposing sc key key-state))

   ((social-ime-cancel-key? key key-state)
    (social-ime-cancel-conv sc))

   ((and social-ime-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (social-ime-context-candidate-window sc))
    (social-ime-move-candidate-in-page sc key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (social-ime-do-commit sc)
      (social-ime-proc-input-state sc key key-state)))))

(define (social-ime-press-key-handler sc key key-state)
  (if (ichar-control? key)
      (im-commit-raw sc)
      (if (social-ime-context-on sc)
          (if (social-ime-context-transposing sc)
              (social-ime-proc-transposing-state sc key key-state)
              (if (social-ime-context-state sc)
                  (social-ime-proc-compose-state sc key key-state)
                  (social-ime-proc-input-state sc key key-state)))
	  (social-ime-proc-raw-state sc key key-state)))
  (social-ime-update-preedit sc))

;;;
(define (social-ime-release-key-handler sc key key-state)
  (if (or (ichar-control? key)
	  (not (social-ime-context-on sc)))
      (social-ime-commit-raw sc)))
;;;
(define (social-ime-reset-handler sc)
  (if (social-ime-context-on sc)
      (begin
	(if (social-ime-context-state sc)
            (social-ime-lib-reset-conversion sc))
	(social-ime-flush sc))))

;;;
(define (social-ime-get-candidate-handler sc idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (social-ime-context-segments sc)))
	 (cand (social-ime-lib-get-nth-candidate
		sc cur-seg idx)))
    (list cand (digit->string (+ idx 1)) "")))

(define (social-ime-set-candidate-index-handler sc idx)
  (ustr-cursor-set-frontside! (social-ime-context-segments sc) idx)
  (social-ime-update-preedit sc))

(define (social-ime-proc-raw-state sc key key-state)
  (if (not (social-ime-begin-input sc key key-state))
      (im-commit-raw sc)))

(social-ime-configure-widgets)
(register-im
 'social-ime
 "ja"
 "EUC-JP"
 social-ime-im-name-label
 social-ime-im-short-desc
 #f
 social-ime-init-handler
 social-ime-release-handler
 context-mode-handler
 social-ime-press-key-handler
 social-ime-release-key-handler
 social-ime-reset-handler
 social-ime-get-candidate-handler
 social-ime-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
