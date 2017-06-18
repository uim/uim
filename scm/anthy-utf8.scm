;;; anthy.scm: Anthy (UTF-8) for uim.
;;; charset: UTF-8
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

(require-extension (srfi 1 8))

(require "util.scm")
(require "ustr.scm")
(require "japanese.scm")
(require-custom "generic-key-custom.scm")
(require-custom "anthy-utf8-custom.scm")
(require-custom "anthy-key-custom.scm")


;;; implementations

(define anthy-utf8-lib-initialized? #f)
(define anthy-version #f)

(define anthy-type-direct          ja-type-direct)
(define anthy-type-hiragana        ja-type-hiragana)
(define anthy-type-katakana        ja-type-katakana)
(define anthy-type-halfkana        ja-type-halfkana)
(define anthy-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define anthy-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define anthy-input-rule-roma 0)
(define anthy-input-rule-kana 1)
(define anthy-input-rule-azik 2)
(define anthy-input-rule-act 3)
(define anthy-input-rule-kzik 4)

(define anthy-candidate-type-katakana -2)
(define anthy-candidate-type-hiragana -3)
(define anthy-candidate-type-halfkana -4)
;; below are not defined in Anthy
(define anthy-candidate-type-halfwidth-alnum -5)
(define anthy-candidate-type-fullwidth-alnum -6)
(define anthy-candidate-type-upper-halfwidth-alnum -7)
(define anthy-candidate-type-upper-fullwidth-alnum -8)

(define anthy-compiled-encoding 0)
(define anthy-euc-jp-encoding 1)
(define anthy-utf8-encoding 2)

;; I don't think the key needs to be customizable.
(define-key anthy-space-key? '(" "))

;; Handle Anthy's version scheme like 7100b, 8158memm.
(define anthy-utf8-version->major.minor
  (lambda (vstr)
    (if (string=? vstr "(unknown)")
	'("-1" . "")
	(receive (maj min) (span char-numeric? (string->list vstr))
	  (cons (list->string maj)
		(list->string min))))))

(define anthy-utf8-prepare-input-rule-activation
  (lambda (ac)
    (cond
     ((anthy-utf8-context-converting ac)
      (anthy-utf8-do-commit ac))
     ((anthy-utf8-context-transposing ac)
      (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-transposing-text ac))))
     ((and
       (anthy-utf8-context-on ac)
       (anthy-utf8-has-preedit? ac))
      (im-commit
       ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t (anthy-utf8-context-kana-mode ac))))))
    (anthy-utf8-flush ac)
    (anthy-utf8-update-preedit ac)))

(define anthy-utf8-prepare-input-mode-activation
  (lambda (ac new-mode)
    (let ((old-kana (anthy-utf8-context-kana-mode ac)))
      (cond
       ((anthy-utf8-context-converting ac)
	(anthy-utf8-do-commit ac))
       ((anthy-utf8-context-transposing ac)
	(im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-transposing-text ac)))
	(anthy-utf8-flush ac))
       ((and
	 (anthy-utf8-context-on ac)
	 (anthy-utf8-has-preedit? ac)
	 (not (= old-kana new-mode)))
	(im-commit
	 ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t (anthy-utf8-context-kana-mode ac))))
	(anthy-utf8-flush ac)))
      (anthy-utf8-update-preedit ac))))

(register-action 'action_anthy_utf8_hiragana
;;		 (indication-alist-indicator 'action_anthy_utf8_hiragana
;;					     anthy-utf8-input-mode-indication-alist)
		 (lambda (ac) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (ac) ;; activity predicate
		   (and (anthy-utf8-context-on ac)
			(not (anthy-utf8-context-alnum ac))
			(= (anthy-utf8-context-kana-mode ac)
			   anthy-type-hiragana)))

		 (lambda (ac) ;; action handler
		   (anthy-utf8-prepare-input-mode-activation ac anthy-type-hiragana)
		   (anthy-utf8-context-set-on! ac #t)
		   (anthy-utf8-context-set-alnum! ac #f)
		   (anthy-utf8-context-change-kana-mode! ac anthy-type-hiragana)))

(register-action 'action_anthy_utf8_katakana
;;		 (indication-alist-indicator 'action_anthy_utf8_katakana
;;					     anthy-utf8-input-mode-indication-alist)
		 (lambda (ac)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (ac)
		   (and (anthy-utf8-context-on ac)
			(not (anthy-utf8-context-alnum ac))
			(= (anthy-utf8-context-kana-mode ac)
			   anthy-type-katakana)))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-mode-activation ac anthy-type-katakana)
		   (anthy-utf8-context-set-on! ac #t)
		   (anthy-utf8-context-set-alnum! ac #f)
		   (anthy-utf8-context-change-kana-mode! ac anthy-type-katakana)))

(register-action 'action_anthy_utf8_halfkana
;;		 (indication-alist-indicator 'action_anthy_utf8_halfkana
;;					     anthy-utf8-input-mode-indication-alist)
		 (lambda (ac)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (ac)
		   (and (anthy-utf8-context-on ac)
			(not (anthy-utf8-context-alnum ac))
			(= (anthy-utf8-context-kana-mode ac)
			   anthy-type-halfkana)))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-mode-activation ac anthy-type-halfkana)
		   (anthy-utf8-context-set-on! ac #t)
		   (anthy-utf8-context-set-alnum! ac #f)
		   (anthy-utf8-context-change-kana-mode! ac anthy-type-halfkana)))

(register-action 'action_anthy_utf8_halfwidth_alnum
		 (lambda (ac)
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (ac)
		   (and (anthy-utf8-context-on ac)
			(anthy-utf8-context-alnum ac)
			(= (anthy-utf8-context-alnum-type ac)
			   anthy-type-halfwidth-alnum)))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-mode-activation
		    ac (anthy-utf8-context-kana-mode ac))
		   (anthy-utf8-context-set-on! ac #t)
		   (anthy-utf8-context-set-alnum! ac #t)
		   (anthy-utf8-context-set-alnum-type!
		    ac anthy-type-halfwidth-alnum)))

(register-action 'action_anthy_utf8_direct
;;		 (indication-alist-indicator 'action_anthy_utf8_direct
;;					     anthy-utf8-input-mode-indication-alist)
		 (lambda (ac)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (ac)
		   (not (anthy-utf8-context-on ac)))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-mode-activation ac anthy-type-direct)
		   (anthy-utf8-context-set-on! ac #f)))

(register-action 'action_anthy_utf8_fullwidth_alnum
;;		 (indication-alist-indicator 'action_anthy_utf8_fullwidth_alnum
;;					     anthy-utf8-input-mode-indication-alist)
		 (lambda (ac)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (ac)
		   (and (anthy-utf8-context-on ac)
			(anthy-utf8-context-alnum ac)
			(= (anthy-utf8-context-alnum-type ac)
			   anthy-type-fullwidth-alnum)))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-mode-activation
		    ac (anthy-utf8-context-kana-mode ac))
		   (anthy-utf8-context-set-on! ac #t)
		   (anthy-utf8-context-set-alnum! ac #t)
		   (anthy-utf8-context-set-alnum-type!
		    ac anthy-type-fullwidth-alnum)))

(register-action 'action_anthy_utf8_roma
;;		 (indication-alist-indicator 'action_anthy_utf8_roma
;;					     anthy-utf8-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-utf8-context-input-rule ac)
		      anthy-input-rule-roma))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-rule-activation ac)
		   (rk-context-set-rule! (anthy-utf8-context-rkc ac)
					 ja-rk-rule)
		   (japanese-roma-set-yen-representation)
		   (anthy-utf8-context-set-input-rule! ac anthy-input-rule-roma)))

(register-action 'action_anthy_utf8_kana
;;		 (indication-alist-indicator 'action_anthy_utf8_kana
;;					     anthy-utf8-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (ac)
		   (= (anthy-utf8-context-input-rule ac)
		      anthy-input-rule-kana))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-rule-activation ac)
                   (require "japanese-kana.scm")
		   (anthy-utf8-context-set-input-rule! ac anthy-input-rule-kana)
		   (anthy-utf8-context-change-kana-mode! ac (anthy-utf8-context-kana-mode ac))
		   (anthy-utf8-context-set-alnum! ac #f)
		   (japanese-roma-set-yen-representation)
		   ;;(define-key anthy-kana-toggle-key? "")
		   ;;(define-key anthy-on-key? generic-on-key?)
		   ;;(define-key anthy-fullwidth-alnum-key? "")
		   ))

(register-action 'action_anthy_utf8_azik
;;		 (indication-alist-indicator 'action_anthy_utf8_azik
;;					     anthy-utf8-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-utf8-context-input-rule ac)
		      anthy-input-rule-azik))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-rule-activation ac)
                   (require "japanese-azik.scm")
		   (rk-context-set-rule! (anthy-utf8-context-rkc ac)
					 ja-azik-rule)
		   (japanese-roma-set-yen-representation)
		   (anthy-utf8-context-set-input-rule! ac anthy-input-rule-azik)))

(register-action 'action_anthy_utf8_kzik
;;		 (indication-alist-indicator 'action_anthy_utf8_kzik
;;					     anthy-utf8-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-utf8-context-input-rule ac)
		      anthy-input-rule-kzik))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-rule-activation ac)
                   (require "japanese-kzik.scm")
		   (rk-context-set-rule! (anthy-utf8-context-rkc ac)
					 ja-kzik-rule)
		   (japanese-roma-set-yen-representation)
		   (anthy-utf8-context-set-input-rule! ac anthy-input-rule-kzik)))

(register-action 'action_anthy_utf8_act
;;		(indication-alist-indicator 'action_anthy_utf8_act
;;					     anthy-utf8-kana-input-method-indication-alist)
		 (lambda (ac)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (ac)
		   (= (anthy-utf8-context-input-rule ac)
		      anthy-input-rule-act))
		 (lambda (ac)
		   (anthy-utf8-prepare-input-rule-activation ac)
                   (require "japanese-act.scm")
		   (rk-context-set-rule! (anthy-utf8-context-rkc ac)
					 ja-act-rule)
		   (japanese-roma-set-yen-representation)
		   (anthy-utf8-context-set-input-rule! ac anthy-input-rule-act)))



;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define anthy-utf8-configure-widgets
  (lambda ()
    (register-widget 'widget_anthy_utf8_input_mode
		     (activity-indicator-new anthy-utf8-input-mode-actions)
		     (actions-new anthy-utf8-input-mode-actions))

    (register-widget 'widget_anthy_utf8_kana_input_method
		     (activity-indicator-new anthy-utf8-kana-input-method-actions)
		     (actions-new anthy-utf8-kana-input-method-actions))
    (context-list-replace-widgets! 'anthy-utf8 anthy-utf8-widgets)))

(define anthy-utf8-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'converting         #f)
    (list 'transposing        #f)
    (list 'predicting         #f)
    (list 'ac-id              #f) ;; anthy-utf8-context-id
    (list 'preconv-ustr       #f) ;; preedit strings
    (list 'rkc                #f)
    (list 'segments           #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'transposing-type   0)
    (list 'prediction-window  #f)
    (list 'prediction-index   #f)
    (list 'kana-mode          anthy-type-hiragana)
    (list 'alnum              #f)
    (list 'alnum-type         anthy-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         anthy-input-rule-roma)
    (list 'raw-ustr           #f))))
(define-record 'anthy-utf8-context anthy-utf8-context-rec-spec)
(define anthy-utf8-context-new-internal anthy-utf8-context-new)

(define anthy-utf8-context-new
 (lambda (id im)
   (let ((ac (anthy-utf8-context-new-internal id im))
	 (rkc (rk-context-new ja-rk-rule #t #f)))
     (if (symbol-bound? 'anthy-utf8-lib-init)
         (begin
	   (set! anthy-utf8-lib-initialized? (anthy-utf8-lib-init))
	   (set! anthy-version (anthy-utf8-version->major.minor
				(anthy-utf8-lib-get-anthy-version)))))
     (if anthy-utf8-lib-initialized?
	 (anthy-utf8-context-set-ac-id!
	  ac (anthy-utf8-lib-alloc-context anthy-utf8-encoding)))
     (anthy-utf8-context-set-widgets! ac anthy-utf8-widgets)
     (anthy-utf8-context-set-rkc! ac rkc)
     (anthy-utf8-context-set-preconv-ustr! ac (ustr-new '()))
     (anthy-utf8-context-set-raw-ustr! ac (ustr-new '()))
     (anthy-utf8-context-set-segments! ac (ustr-new '()))
     ac)))

(define anthy-utf8-commit-raw
  (lambda (ac)
    (im-commit-raw ac)
    (anthy-utf8-context-set-commit-raw! ac #t)))

(define anthy-utf8-context-kana-toggle
  (lambda (ac)
    (let* ((kana (anthy-utf8-context-kana-mode ac))
	   (opposite-kana (ja-opposite-kana kana)))
      (anthy-utf8-context-change-kana-mode! ac opposite-kana))))

(define anthy-utf8-context-alkana-toggle
  (lambda (ac)
    (let ((alnum-state (anthy-utf8-context-alnum ac)))
      (anthy-utf8-context-set-alnum! ac (not alnum-state)))))

(define anthy-utf8-context-change-kana-mode!
  (lambda (ac kana-mode)
    (if (= (anthy-utf8-context-input-rule ac)
           anthy-input-rule-kana)
        (rk-context-set-rule!
	 (anthy-utf8-context-rkc ac)
	 (cond
	  ((= kana-mode anthy-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode anthy-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode anthy-type-halfkana)  ja-kana-halfkana-rule))))
    (anthy-utf8-context-set-kana-mode! ac kana-mode)))

;; TODO: generarize as multi-segment procedure
;; side effect: none. rkc will not be altered
(define anthy-utf8-make-whole-string
  (lambda (ac convert-pending-into-kana? kana)
    (let* ((rkc (anthy-utf8-context-rkc ac))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (rule (anthy-utf8-context-input-rule ac))
	   (preconv-str (anthy-utf8-context-preconv-ustr ac))
	   (extract-kana
	    (if (= rule anthy-input-rule-kana)
		(lambda (entry) (car entry))
		(lambda (entry) (list-ref entry kana)))))

      (if (= rule anthy-input-rule-kana)
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

(define anthy-make-raw-string
  (lambda (raw-str-list wide? upper?)
    (if (not (null? raw-str-list))
	(if wide?
	    (string-append
	     (ja-string-list-to-wide-alphabet
	      (if upper?
		  (map
		   (lambda (x)
		     (if (ichar-alphabetic? (string->charcode x))
			 (charcode->string (ichar-upcase (string->charcode x)))
			 x))
		   (string-to-list (car raw-str-list)))
		  (string-to-list (car raw-str-list))))
	     (anthy-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map
		   (lambda (x)
		     (if (ichar-alphabetic? (string->charcode x))
			 (charcode->string (ichar-upcase (string->charcode x)))
			 x))
		   (string-to-list (car raw-str-list))))
		 (car raw-str-list))
	     (anthy-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define anthy-utf8-make-whole-raw-string
  (lambda (ac wide? upper?)
    (anthy-make-raw-string (anthy-utf8-get-raw-str-seq ac) wide? upper?)))

(define anthy-utf8-init-handler
  (lambda (id im arg)
    (anthy-utf8-context-new id im)))

(define anthy-utf8-release-handler
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac)))
      (if ac-id
	  (anthy-utf8-lib-free-context ac-id)))))

(define anthy-utf8-flush
  (lambda (ac)
    (rk-flush (anthy-utf8-context-rkc ac))
    (ustr-clear! (anthy-utf8-context-preconv-ustr ac))
    (ustr-clear! (anthy-utf8-context-raw-ustr ac))
    (ustr-clear! (anthy-utf8-context-segments ac))
    (anthy-utf8-context-set-transposing! ac #f)
    (anthy-utf8-context-set-converting! ac #f)
    (anthy-utf8-context-set-predicting! ac #f)
    (if (or
         (anthy-utf8-context-candidate-window ac)
         (anthy-utf8-context-prediction-window ac))
	(im-deactivate-candidate-selector ac))
    (anthy-utf8-context-set-candidate-window! ac #f)
    (anthy-utf8-context-set-prediction-window! ac #f)
    (anthy-utf8-context-set-candidate-op-count! ac 0)))

(define anthy-utf8-begin-input
  (lambda (ac key key-state)
    (if (cond
	 ((anthy-on-key? key key-state)
	  #t)
	 ((and
	   anthy-use-mode-transition-keys-in-off-mode?
	   (cond
	    ((anthy-hiragana-key? key key-state)
	     (anthy-utf8-context-set-kana-mode! ac anthy-type-hiragana)
	     (anthy-utf8-context-set-alnum! ac #f)
	     #t)
	    ((anthy-katakana-key? key key-state)
	     (anthy-utf8-context-set-kana-mode! ac anthy-type-katakana)
	     (anthy-utf8-context-set-alnum! ac #f)
	     #t)
	    ((anthy-halfkana-key? key key-state)
	     (anthy-utf8-context-set-kana-mode! ac anthy-type-halfkana)
	     (anthy-utf8-context-set-alnum! ac #f)
	     #t)
	    ((anthy-halfwidth-alnum-key? key key-state)
	     (anthy-utf8-context-set-alnum-type! ac anthy-type-halfwidth-alnum)
	     (anthy-utf8-context-set-alnum! ac #t)
	     #t)
	    ((anthy-fullwidth-alnum-key? key key-state)
	     (anthy-utf8-context-set-alnum-type! ac anthy-type-fullwidth-alnum)
	     (anthy-utf8-context-set-alnum! ac #t)
	     #t)
	    ((anthy-kana-toggle-key? key key-state)
	     (anthy-utf8-context-kana-toggle ac)
	     (anthy-utf8-context-set-alnum! ac #f)
	     #t)
	    ((anthy-alkana-toggle-key? key key-state)
	     (anthy-utf8-context-alkana-toggle ac)
	     #t)
	    (else
	     #f))))
	 (else
	  #f))
	(begin
	  (anthy-utf8-context-set-on! ac #t)
	  (rk-flush (anthy-utf8-context-rkc ac))
	  (anthy-utf8-context-set-converting! ac #f)
	  #t)
	#f)))

(define anthy-utf8-update-preedit
  (lambda (ac)
    (if (not (anthy-utf8-context-commit-raw ac))
	(let ((segments (if (anthy-utf8-context-on ac)
			    (if (anthy-utf8-context-transposing ac)
				(anthy-utf8-context-transposing-state-preedit ac)
				(if (anthy-utf8-context-converting ac)
				    (anthy-utf8-converting-state-preedit ac)
				    (if (anthy-utf8-context-predicting ac)
				        (anthy-utf8-predicting-state-preedit ac)
				        (anthy-utf8-input-state-preedit ac))))
			    ())))
	  (context-update-preedit ac segments))
	(anthy-utf8-context-set-commit-raw! ac #f))))
  
(define anthy-utf8-proc-raw-state
  (lambda (ac key key-state)
    (if (not (anthy-utf8-begin-input ac key key-state))
	(anthy-utf8-commit-raw ac))))

(define anthy-utf8-begin-conv
  (lambda (ac)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (kana (anthy-utf8-context-kana-mode ac))
	   (preconv-str (anthy-utf8-make-whole-string ac #t anthy-type-hiragana)))
      (if (and ac-id
	       (> (string-length preconv-str)
		  0))
	  (begin
	    (anthy-utf8-lib-set-string ac-id (anthy-utf8-lib-eucjp-to-utf8 preconv-str))
	    (let ((nr-segments (anthy-utf8-lib-get-nr-segments ac-id)))
	      (ustr-set-latter-seq! (anthy-utf8-context-segments ac)
				    (make-list nr-segments 0))
	      (anthy-utf8-context-set-converting! ac #t)
	      ;; Don't perform rk-flush here. The rkc must be restored when
	      ;; anthy-utf8-cancel-conv invoked -- YamaKen 2004-10-25
	      ))))))

(define anthy-utf8-cancel-conv
  (lambda (ac)
    (anthy-utf8-reset-candidate-window ac)
    (anthy-utf8-context-set-converting! ac #f)
    (ustr-clear! (anthy-utf8-context-segments ac))))

(define kana-keys?
  (lambda (key)
    (if (not (symbol? key))
	#f
	(cond
	 ((eq? 'kana-lock key)
	  #f)
	 ((eq? 'kana-shift key)
	  #f)
	 (else
	   (let ((name (symbol->string key)))
	     (if (> (string-length name) 5)
		 (let ((keysym-head
			(string-list-concat
		       (list-head (reverse (string-to-list name)) 5))))
		   (if (string=? keysym-head "-anak") ;; reverse
		       #t
		       #f))
		 #f)))))))

(define anthy-non-composing-symbol?
  (lambda (ac key)
    (if (and
	 (symbol? key)
	 (not (kana-keys? key))
	 (not (eq? key 'yen)))
	#t
	#f)))

(define anthy-utf8-proc-input-state-no-preedit
  (lambda (ac key key-state)
    (let ((rkc (anthy-utf8-context-rkc ac))
	  (direct (ja-direct (charcode->string key)))
	  (rule (anthy-utf8-context-input-rule ac)))
      (cond
       ((and anthy-use-with-vi?
             (anthy-vi-escape-key? key key-state))
	(anthy-utf8-flush ac)
	(anthy-utf8-context-set-on! ac #f)
	(anthy-utf8-commit-raw ac))

       ((anthy-off-key? key key-state)
	(anthy-utf8-flush ac)
	(anthy-utf8-context-set-on! ac #f))

       ((anthy-backspace-key? key key-state)
	(anthy-utf8-commit-raw ac))

       ((anthy-delete-key? key key-state)
	(anthy-utf8-commit-raw ac))
       
       ((and
         (anthy-hiragana-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-kana-mode ac) anthy-type-hiragana)
	   (not (anthy-utf8-context-alnum ac)))))
	(anthy-utf8-context-change-kana-mode! ac anthy-type-hiragana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((and
         (anthy-katakana-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-kana-mode ac) anthy-type-katakana)
	   (not (anthy-utf8-context-alnum ac)))))
	(anthy-utf8-context-change-kana-mode! ac anthy-type-katakana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((and
         (anthy-halfkana-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-kana-mode ac) anthy-type-halfkana)
	   (not (anthy-utf8-context-alnum ac)))))
	(anthy-utf8-context-change-kana-mode! ac anthy-type-halfkana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((and
         (anthy-halfwidth-alnum-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-alnum-type ac) anthy-type-halfwidth-alnum)
	   (anthy-utf8-context-alnum ac))))
	(anthy-utf8-context-set-alnum-type! ac anthy-type-halfwidth-alnum)
	(anthy-utf8-context-set-alnum! ac #t))

       ((and
         (anthy-fullwidth-alnum-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-alnum-type ac) anthy-type-fullwidth-alnum)
	   (anthy-utf8-context-alnum ac))))
	(anthy-utf8-context-set-alnum-type! ac anthy-type-fullwidth-alnum)
	(anthy-utf8-context-set-alnum! ac #t))

       ((and
	 (not (anthy-utf8-context-alnum ac))
	 (anthy-kana-toggle-key? key key-state))
	(anthy-utf8-context-kana-toggle ac))

       ((anthy-alkana-toggle-key? key key-state)
	(anthy-utf8-context-alkana-toggle ac))

       ;; modifiers (except shift) => ignore
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	(anthy-utf8-commit-raw ac))
       
       ;; direct key => commit
       (direct
	(im-commit ac (anthy-utf8-lib-eucjp-to-utf8 direct)))

       ;; space key => commit
       ((anthy-space-key? key key-state)
	(if (anthy-utf8-context-alnum ac)
	    (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (list-ref
			   ja-alnum-space
			   (- (anthy-utf8-context-alnum-type ac)
			      anthy-type-halfwidth-alnum))))
	    (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (list-ref ja-space (anthy-utf8-context-kana-mode ac))))))

       ((anthy-non-composing-symbol? ac key)
	(anthy-utf8-commit-raw ac))

       (else
	(if (anthy-utf8-context-alnum ac)
	    (let ((key-str (if (symbol? key)
			       (if (symbol-bound? key)
				   (symbol-value key)
				   "?") ;; shouldn't happen
			       (charcode->string key))))
	      (ustr-insert-elem! (anthy-utf8-context-preconv-ustr ac)
				 (if (= (anthy-utf8-context-alnum-type ac)
					anthy-type-halfwidth-alnum)
				     (list key-str key-str key-str)
				     (list (ja-wide key-str) (ja-wide key-str)
					   (ja-wide key-str))))
	      (ustr-insert-elem! (anthy-utf8-context-raw-ustr ac) key-str))
	    (let* ((key-str (if (= rule anthy-input-rule-kana)
	    			(if (symbol? key)
				    (symbol->string key)
				    (charcode->string key))
				(if (symbol? key)
				    (symbol->string key)
				    (charcode->string (ichar-downcase key)))))
		   (res (rk-push-key! rkc key-str)))
	      (if res
		  (begin
		    (if (list? (car res))
		      (ustr-insert-seq! (anthy-utf8-context-preconv-ustr ac) res)
		      (ustr-insert-elem! (anthy-utf8-context-preconv-ustr ac) res))
		    (ustr-insert-elem! (anthy-utf8-context-raw-ustr ac)
				       (if (and (intern-key-symbol key-str)
						(symbol-bound?
						 (string->symbol key-str)))
					   (symbol-value
					    (string->symbol key-str))
					   key-str)))
		  (if (null? (rk-context-seq rkc))
		      (anthy-utf8-commit-raw ac))))))))))

(define anthy-utf8-has-preedit?
  (lambda (ac)
    (or (not (ustr-empty? (anthy-utf8-context-preconv-ustr ac)))
	(> (string-length (rk-pending (anthy-utf8-context-rkc ac))) 0))))

(define anthy-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type anthy-type-halfwidth-alnum)
       (= state anthy-type-halfwidth-alnum))
      anthy-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type anthy-type-fullwidth-alnum)
       (= state anthy-type-fullwidth-alnum))
      anthy-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define anthy-utf8-learn-transposing-text
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac))
          (transposing-type (anthy-utf8-context-transposing-type ac))
          (preconv-str (anthy-utf8-make-whole-string ac #t anthy-type-hiragana))
          (type #f))
      (define (expand-segment)
        (if (not (= (anthy-utf8-lib-get-nr-segments ac-id) 1))
          (begin
            (anthy-utf8-lib-resize-segment ac-id 0 1)
            (expand-segment))))
      (cond
        ((= transposing-type anthy-type-hiragana)
         (set! type anthy-candidate-type-hiragana))
        ((= transposing-type anthy-type-katakana)
         (set! type anthy-candidate-type-katakana)))
      (if (and ac-id
               (> (string-length preconv-str) 0)
               type)
        (begin
          (anthy-utf8-lib-set-string ac-id (anthy-utf8-lib-eucjp-to-utf8 preconv-str))
          (expand-segment)
          (anthy-utf8-lib-commit-segment ac-id 0 type))))))

(define anthy-utf8-proc-transposing-state
  (lambda (ac key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (anthy-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons anthy-type-fullwidth-alnum rotate-list)))
      (if (anthy-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons anthy-type-halfwidth-alnum rotate-list)))
      (if (anthy-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons anthy-type-halfkana rotate-list)))
      (if (anthy-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons anthy-type-katakana rotate-list)))
      (if (anthy-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons anthy-type-hiragana rotate-list)))

      (if (anthy-utf8-context-transposing ac)
	  (let ((lst (member (anthy-utf8-context-transposing-type ac) rotate-list)))
	    (if (and lst
		     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (anthy-rotate-transposing-alnum-type
				 (anthy-utf8-context-transposing-type ac)
				 (car rotate-list))))))
	  (begin
	    (anthy-utf8-context-set-transposing! ac #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state anthy-type-hiragana)
	      (= state anthy-type-katakana)
	      (= state anthy-type-halfkana)))
	(anthy-utf8-context-set-transposing-type! ac state))
       ((and state
	     (or
	      (= state anthy-type-halfwidth-alnum)
	      (= state anthy-candidate-type-upper-halfwidth-alnum)
	      (= state anthy-type-fullwidth-alnum)
	      (= state anthy-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (anthy-utf8-context-input-rule ac) anthy-input-rule-kana))
	    (anthy-utf8-context-set-transposing-type! ac state)))
       (else
	(and
	 ; commit
	 (if (anthy-commit-key? key key-state)
	     (begin
	       (anthy-utf8-learn-transposing-text ac)
	       (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-transposing-text ac)))
	       (anthy-utf8-flush ac)
	       #f)
	     #t)
	 ; begin-conv
	 (if (anthy-begin-conv-key? key key-state)
	     (begin
	       (anthy-utf8-context-set-transposing! ac #f)
	       (anthy-utf8-begin-conv ac)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (anthy-cancel-key? key key-state)
	      (anthy-backspace-key? key key-state))
	     (begin
	       (anthy-utf8-context-set-transposing! ac #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (anthy-prev-page-key? key key-state)
	      (anthy-next-page-key? key key-state)
	      (anthy-extend-segment-key? key key-state)
	      (anthy-shrink-segment-key? key key-state)
	      (anthy-next-segment-key? key key-state)
	      (anthy-prev-segment-key? key key-state)
	      (anthy-beginning-of-preedit-key? key key-state)
	      (anthy-end-of-preedit-key? key key-state)
	      (anthy-next-candidate-key? key key-state)
	      (anthy-prev-candidate-key? key key-state)
	      (and (modifier-key-mask key-state)
		   (not (shift-key-mask key-state)))
	      (anthy-non-composing-symbol? ac key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-transposing-text ac)))
	   (anthy-utf8-flush ac)
	   (anthy-utf8-proc-input-state ac key key-state))))))))

(define anthy-utf8-move-prediction
  (lambda (ac offset)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (nr (anthy-utf8-lib-get-nr-predictions ac-id))
	   (idx (anthy-utf8-context-prediction-index ac))
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
      (im-select-candidate ac compensated-n)
      (anthy-utf8-context-set-prediction-index! ac compensated-n))))

(define anthy-utf8-move-prediction-in-page
  (lambda (ac numeralc)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (nr (anthy-utf8-lib-get-nr-predictions ac-id))
	   (p-idx (anthy-utf8-context-prediction-index ac))
	   (n (if (not p-idx)
		  0
		  p-idx))
	   (cur-page (if (= anthy-nr-candidate-max 0)
			 0
			 (quotient n anthy-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page anthy-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx nr)
			      #f)
			     (else
			      idx)))
	   (selected-pageidx (if (not p-idx)
				 #f
				 (if (= anthy-nr-candidate-max 0)
				     p-idx
				     (remainder p-idx
						anthy-nr-candidate-max)))))
      (if (and
	   compensated-idx
	   (not (eqv? compensated-pageidx selected-pageidx)))
	  (begin
	    (anthy-utf8-context-set-prediction-index! ac compensated-idx)
	    (im-select-candidate ac compensated-idx)
	    #t)
	  #f))))
    
(define anthy-utf8-prediction-select-non-existing-index?
  (lambda (ac numeralc)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (nr (anthy-utf8-lib-get-nr-predictions ac-id))
	   (p-idx (anthy-utf8-context-prediction-index ac))
	   (cur-page (if (= anthy-nr-candidate-max 0)
			 0
			 (quotient p-idx anthy-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page anthy-nr-candidate-max) compensated-pageidx)))
    (if (>= idx nr)
	#t
	#f))))

(define anthy-utf8-prediction-keys-handled?
  (lambda (ac key key-state)
    (cond
     ((anthy-next-prediction-key? key key-state)
      (anthy-utf8-move-prediction ac 1)
      #t)
     ((anthy-prev-prediction-key? key key-state)
      (anthy-utf8-move-prediction ac -1)
      #t)
     ((and
       anthy-select-prediction-by-numeral-key?
       (ichar-numeric? key))
      (anthy-utf8-move-prediction-in-page ac key))
     ((and
       (anthy-utf8-context-prediction-index ac)
       (anthy-prev-page-key? key key-state))
      (im-shift-page-candidate ac #f)
      #t)
     ((and
       (anthy-utf8-context-prediction-index ac)
       (anthy-next-page-key? key key-state))
      (im-shift-page-candidate ac #t)
      #t)
     (else
      #f))))

(define anthy-utf8-proc-prediction-state
  (lambda (ac key key-state)
    (cond
     ;; prediction index change
     ((anthy-utf8-prediction-keys-handled? ac key key-state))

     ;; cancel
     ((anthy-cancel-key? key key-state)
      (if (anthy-utf8-context-prediction-index ac)
	  (anthy-utf8-reset-prediction-window ac)
	  (begin
	    (anthy-utf8-reset-prediction-window ac)
	    (anthy-utf8-proc-input-state ac key key-state))))

     ;; commit
     ((and
       (anthy-utf8-context-prediction-index ac)
       (anthy-commit-key? key key-state))
      (anthy-utf8-do-commit-prediction ac))
     (else
      (if (and
	   anthy-use-implicit-commit-prediction?
	   (anthy-utf8-context-prediction-index ac))
	  (cond
	    ((or
	      ;; check keys used in anthy-utf8-proc-input-state-with-preedit
	      (anthy-begin-conv-key? key key-state)
	      (anthy-backspace-key? key key-state)
	      (anthy-delete-key? key key-state)
	      (anthy-kill-key? key key-state)
	      (anthy-kill-backward-key? key key-state)
	      (and
	       (not (anthy-utf8-context-alnum ac))
	       (anthy-commit-as-opposite-kana-key? key key-state))
	      (anthy-transpose-as-hiragana-key? key key-state)
	      (anthy-transpose-as-katakana-key? key key-state)
	      (anthy-transpose-as-halfkana-key? key key-state)
	      (and
	       (not (= (anthy-utf8-context-input-rule ac) anthy-input-rule-kana))
	       (or
		(anthy-transpose-as-halfwidth-alnum-key? key key-state)
		(anthy-transpose-as-fullwidth-alnum-key? key key-state)))
	      (anthy-hiragana-key? key key-state)
	      (anthy-katakana-key? key key-state)
	      (anthy-halfkana-key? key key-state)
	      (anthy-halfwidth-alnum-key? key key-state)
	      (anthy-fullwidth-alnum-key? key key-state)
	      (and
	       (not (anthy-utf8-context-alnum ac))
	       (anthy-kana-toggle-key? key key-state))
	      (anthy-alkana-toggle-key? key key-state)
	      (anthy-go-left-key? key key-state)
	      (anthy-go-right-key? key key-state)
	      (anthy-beginning-of-preedit-key? key key-state)
	      (anthy-end-of-preedit-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state))))
	     ;; go back to unselected prediction
	     (anthy-utf8-reset-prediction-window ac)
	     (anthy-utf8-check-prediction ac #f))
	    ((and
	      (ichar-numeric? key)
	      anthy-select-prediction-by-numeral-key?
	      (not (anthy-utf8-prediction-select-non-existing-index? ac key)))
	     (anthy-utf8-context-set-predicting! ac #f)
	     (anthy-utf8-context-set-prediction-index! ac #f)
	     (anthy-utf8-proc-input-state ac key key-state))
	    (else
	     ;; implicit commit
	     (anthy-utf8-do-commit-prediction ac)
	     (anthy-utf8-proc-input-state ac key key-state)))
	  (begin
	    (anthy-utf8-context-set-predicting! ac #f)
	    (anthy-utf8-context-set-prediction-index! ac #f)
	    (if (not anthy-use-prediction?)
		(anthy-utf8-reset-prediction-window ac))
	    (anthy-utf8-proc-input-state ac key key-state)))))))

(define anthy-utf8-proc-input-state-with-preedit
  (lambda (ac key key-state)
    (define (check-auto-conv str)
      (and
	str
	anthy-auto-start-henkan?
	(string-find japanese-auto-start-henkan-keyword-list str)
	(begin
	  (anthy-utf8-reset-prediction-window ac)
	  (anthy-utf8-begin-conv ac))))
    (let ((preconv-str (anthy-utf8-context-preconv-ustr ac))
	  (raw-str (anthy-utf8-context-raw-ustr ac))
	  (rkc (anthy-utf8-context-rkc ac))
	  (kana (anthy-utf8-context-kana-mode ac))
	  (rule (anthy-utf8-context-input-rule ac)))
      (cond

       ;; begin conversion
       ((anthy-begin-conv-key? key key-state)
	(anthy-utf8-reset-prediction-window ac)
	(anthy-utf8-begin-conv ac))

       ;; prediction 
       ((anthy-next-prediction-key? key key-state)
	(anthy-utf8-check-prediction ac #t))

       ;; backspace
       ((anthy-backspace-key? key key-state)
	(if (not (rk-backspace rkc))
            (begin
	      (ustr-cursor-delete-backside! preconv-str)
	      (ustr-cursor-delete-backside! raw-str)
	      ;; fix to valid roma
	      (if (and
		   (= (anthy-utf8-context-input-rule ac) anthy-input-rule-roma)
		   (not (null? (ustr-former-seq preconv-str)))
		   (not (ichar-printable?	;; check for kana
			 (string->ichar
			  (car (last (ustr-former-seq preconv-str)))))))
		  (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

       ;; delete
       ((anthy-delete-key? key key-state)
	(if (not (rk-delete rkc))
            (begin
	      (ustr-cursor-delete-frontside! preconv-str)
	      (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
       ((anthy-kill-key? key key-state)
	(ustr-clear-latter! preconv-str)
	(ustr-clear-latter! raw-str))
       
       ;; kill-backward
       ((anthy-kill-backward-key? key key-state)
	(rk-flush rkc)
	(ustr-clear-former! preconv-str)
	(ustr-clear-former! raw-str))

       ;; 現在とは逆のかなモードでかなを確定する
       ((and
         (not (anthy-utf8-context-alnum ac))
         (anthy-commit-as-opposite-kana-key? key key-state))
	(begin
	  (im-commit
	   ac
	   (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t (ja-opposite-kana kana))))
	  (anthy-utf8-flush ac)))

       ;; Transposing状態へ移行
       ((or (anthy-transpose-as-hiragana-key? key key-state)
	    (anthy-transpose-as-katakana-key? key key-state)
	    (anthy-transpose-as-halfkana-key? key key-state)
	    (and
	     (not (= (anthy-utf8-context-input-rule ac) anthy-input-rule-kana ))
	     (or
	      (anthy-transpose-as-halfwidth-alnum-key? key key-state)
	      (anthy-transpose-as-fullwidth-alnum-key? key key-state))))
	(anthy-utf8-reset-prediction-window ac)
	(anthy-utf8-proc-transposing-state ac key key-state))

       ((anthy-hiragana-key? key key-state)
        (if (not (= kana anthy-type-hiragana))
	  (begin
	    (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t kana)))
	    (anthy-utf8-flush ac)))
	(anthy-utf8-context-set-kana-mode! ac anthy-type-hiragana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((anthy-katakana-key? key key-state)
        (if (not (= kana anthy-type-katakana))
	  (begin
	    (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t kana)))
	    (anthy-utf8-flush ac)))
	(anthy-utf8-context-set-kana-mode! ac anthy-type-katakana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((anthy-halfkana-key? key key-state)
        (if (not (= kana anthy-type-halfkana))
	  (begin
	    (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t kana)))
	    (anthy-utf8-flush ac)))
	(anthy-utf8-context-set-kana-mode! ac anthy-type-halfkana)
	(anthy-utf8-context-set-alnum! ac #f))

       ((and
         (anthy-halfwidth-alnum-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-alnum-type ac) anthy-type-halfwidth-alnum)
	   (anthy-utf8-context-alnum ac))))
	(anthy-utf8-context-set-alnum-type! ac anthy-type-halfwidth-alnum)
	(anthy-utf8-context-set-alnum! ac #t))

       ((and
         (anthy-fullwidth-alnum-key? key key-state)
	 (not
	  (and
	   (= (anthy-utf8-context-alnum-type ac) anthy-type-fullwidth-alnum)
	   (anthy-utf8-context-alnum ac))))
	(anthy-utf8-context-set-alnum-type! ac anthy-type-fullwidth-alnum)
	(anthy-utf8-context-set-alnum! ac #t))

       ;; Commit current preedit string, then toggle hiragana/katakana mode.
       ((and
	 (not (anthy-utf8-context-alnum ac))
	 (anthy-kana-toggle-key? key key-state))
	(begin
	  (im-commit ac (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t kana)))
	  (anthy-utf8-flush ac)
	  (anthy-utf8-context-kana-toggle ac)))

       ((anthy-alkana-toggle-key? key key-state)
	(anthy-utf8-context-alkana-toggle ac))

       ;; cancel
       ((anthy-cancel-key? key key-state)
	(anthy-utf8-flush ac))

       ;; commit
       ((anthy-commit-key? key key-state)
	(begin
	  (im-commit
	   ac
	   (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-make-whole-string ac #t kana)))
	  (anthy-utf8-flush ac)))

       ;; left
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-go-left-key? key key-state)
	(anthy-utf8-context-confirm-kana! ac)
	(ustr-cursor-move-backward! preconv-str)
	(ustr-cursor-move-backward! raw-str))

       ;; right
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-go-right-key? key key-state)
	(anthy-utf8-context-confirm-kana! ac)
	(ustr-cursor-move-forward! preconv-str)
	(ustr-cursor-move-forward! raw-str))

       ;; beginning-of-preedit
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-beginning-of-preedit-key? key key-state)
	(anthy-utf8-context-confirm-kana! ac)
	(ustr-cursor-move-beginning! preconv-str)
	(ustr-cursor-move-beginning! raw-str))

       ;; end-of-preedit
       ;; 2004-08-27 Takuro Ashie <ashie@homa.ne.jp>
       ;;   * We should restore pending state of rk-context when the input-rule
       ;;     is kana mode.
       ((anthy-end-of-preedit-key? key key-state)
	(anthy-utf8-context-confirm-kana! ac)
	(ustr-cursor-move-end! preconv-str)
	(ustr-cursor-move-end! raw-str))

       ;; modifiers (except shift) => ignore
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	#f)

       ((anthy-non-composing-symbol? ac key)
	#f)

       (else	
	(if (anthy-utf8-context-alnum ac)
	    (let ((key-str (if (symbol? key)
			       (if (symbol-bound? key)
				   (symbol-value key)
				   "?") ;; shouldn't happen
			       (charcode->string key)))
		  (pend (rk-pending rkc))
		  (residual-kana (rk-peek-terminal-match rkc)))
	      (rk-flush rkc) ;; OK to reset rkc here.
	      (if residual-kana
		  (begin
                    (if (list? (car residual-kana))
                      (begin
                        (ustr-insert-seq! preconv-str residual-kana)
                        (ustr-insert-seq! raw-str (reverse
                                                    (string-to-list pend))))
                      (begin
                        (ustr-insert-elem! preconv-str residual-kana)
                        (ustr-insert-elem! raw-str pend)))))
	      (ustr-insert-elem! preconv-str 
				 (if (= (anthy-utf8-context-alnum-type ac)
					anthy-type-halfwidth-alnum)
				     (list key-str key-str key-str)
				     (list (ja-wide key-str) (ja-wide key-str)
					   (ja-wide key-str))))
	      (ustr-insert-elem! raw-str key-str)
	      (check-auto-conv key-str))
	    (let* ((key-str (if (= rule anthy-input-rule-kana)
	    			(if (symbol? key)
				    (symbol->string key)
				    (charcode->string key))
				(if (symbol? key)
				    (symbol->string key)
				    (charcode->string (ichar-downcase key)))))
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
                                (ustr-insert-seq! raw-str (reverse
                                                            (string-to-list
                                                              pend))))
                              ;; assume key-str as a vowel
			      (ustr-insert-elem!
			       raw-str (if (and (intern-key-symbol key-str)
						(symbol-bound?
						 (string->symbol key-str)))
					   (symbol-value
					    (string->symbol key-str))
					   key-str)))
			    (ustr-insert-elem!
			     raw-str
			     (string-append
			      pend
			      (if (and
				   (intern-key-symbol key-str)
				   (symbol-bound? (string->symbol key-str)))
				  (symbol-value (string->symbol key-str))
				  key-str)))))))
	      (check-auto-conv (if res (car res) #f)))))))))

(define anthy-utf8-context-confirm-kana!
  (lambda (ac)
    (if (= (anthy-utf8-context-input-rule ac)
	   anthy-input-rule-kana)
	(let* ((preconv-str (anthy-utf8-context-preconv-ustr ac))
	       (rkc (anthy-utf8-context-rkc ac))
	       (residual-kana (rk-peek-terminal-match rkc)))
	    (if residual-kana
		(begin
                  (if (list? (car residual-kana))
                    (ustr-insert-seq! preconv-str residual-kana)
                    (ustr-insert-elem! preconv-str residual-kana))
		  (rk-flush rkc)))))))

(define anthy-utf8-reset-prediction-window
  (lambda (ac)
    (if (anthy-utf8-context-prediction-window ac)
        (im-deactivate-candidate-selector ac))
    (anthy-utf8-context-set-predicting! ac #f)
    (anthy-utf8-context-set-prediction-window! ac #f)
    (anthy-utf8-context-set-prediction-index! ac #f)))

(define anthy-utf8-check-prediction
  (lambda (ac force-check?)
    (if (and
	 (not (anthy-utf8-context-converting ac))
	 (not (anthy-utf8-context-transposing ac))
	 (not (anthy-utf8-context-predicting ac)))
	(let* ((use-pending-rk-for-prediction? #f)
               (preconv-str
		(anthy-utf8-make-whole-string
		 ac
		 (not use-pending-rk-for-prediction?)
		 (anthy-utf8-context-kana-mode ac)))
	       (ac-id (anthy-utf8-context-ac-id ac))
	       (preedit-len
		(+
		 (ustr-length (anthy-utf8-context-preconv-ustr ac))
		 (if (not use-pending-rk-for-prediction?)
		     0
		     (string-length
		      (rk-pending (anthy-utf8-context-rkc ac)))))))
	  (if (or
	       (>= preedit-len anthy-prediction-start-char-count)
	       force-check?)
	      (begin
		(anthy-utf8-lib-set-prediction-src-string
		 ac-id (anthy-utf8-lib-eucjp-to-utf8 preconv-str))
		(let ((nr (anthy-utf8-lib-get-nr-predictions ac-id)))
		  (if (and
		       nr
		       (> nr 0))
		      (begin
			(im-activate-candidate-selector
			 ac nr anthy-nr-candidate-max)
			(anthy-utf8-context-set-prediction-window! ac #t)
			(anthy-utf8-context-set-predicting! ac #t))
		      (anthy-utf8-reset-prediction-window ac))))
	      (anthy-utf8-reset-prediction-window ac))))))

(define anthy-utf8-proc-input-state
  (lambda (ac key key-state)
    (if (anthy-utf8-has-preedit? ac)
	(anthy-utf8-proc-input-state-with-preedit ac key key-state)
	(anthy-utf8-proc-input-state-no-preedit ac key key-state))
    (if (and
         anthy-use-prediction?
         (not (anthy-utf8-context-predicting ac)))
	 (anthy-utf8-check-prediction ac #f))))

(define anthy-separator
  (lambda (ac)
    (let ((attr (bitwise-ior preedit-separator
			     preedit-underline)))
      (if anthy-show-segment-separator?
	  (cons attr anthy-segment-separator)
	  #f))))

(define anthy-utf8-context-transposing-state-preedit
  (lambda (ac)
    (let ((transposing-text (anthy-utf8-transposing-text ac)))
      (list (cons preedit-reverse (anthy-utf8-lib-eucjp-to-utf8 transposing-text))
	    (cons preedit-cursor "")))))

(define anthy-utf8-transposing-text
  (lambda (ac)
    (let* ((transposing-type (anthy-utf8-context-transposing-type ac)))
      (cond
       ((or
	 (= transposing-type anthy-type-hiragana)
	 (= transposing-type anthy-type-katakana)
	 (= transposing-type anthy-type-halfkana))
	(anthy-utf8-make-whole-string ac #t transposing-type))
       ((= transposing-type anthy-type-halfwidth-alnum)
	(anthy-utf8-make-whole-raw-string ac #f #f))
       ((= transposing-type anthy-candidate-type-upper-halfwidth-alnum)
	(anthy-utf8-make-whole-raw-string ac #f #t))
       ((= transposing-type anthy-type-fullwidth-alnum)
	(anthy-utf8-make-whole-raw-string ac #t #f))
       ((= transposing-type anthy-candidate-type-upper-fullwidth-alnum)
	(anthy-utf8-make-whole-raw-string ac #t #t))))))

(define anthy-utf8-get-raw-str-seq
  (lambda (ac)
    (let* ((rkc (anthy-utf8-context-rkc ac))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (anthy-utf8-context-raw-ustr ac))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
      (append left-str
	      (if residual-kana
                (if (list? (car residual-kana))
		  (reverse (string-to-list pending))
		  (list pending))
		  '())
	      right-str))))

(define anthy-utf8-get-raw-candidate
  (lambda (ac ac-id seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (anthy-utf8-make-whole-string ac #t anthy-type-hiragana))))
	   (unconv-candidate (anthy-utf8-lib-utf8-to-eucjp (anthy-utf8-lib-get-unconv-candidate ac-id seg-idx)))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (anthy-utf8-get-raw-str-seq ac))))
      (if (not (null? unconv))
	  (if (member (car unconv) preconv)
	      (let ((start (list-seq-contained? preconv unconv))
		    (len (length unconv)))
		(if (and
                      start
                      (= (length raw-str) (length preconv))) ;; sanity check
		    (anthy-make-raw-string
		     (reverse (sublist-rel raw-str start len))
		     (if (or
			  (= cand-idx anthy-candidate-type-halfwidth-alnum)
			  (= cand-idx
			     anthy-candidate-type-upper-halfwidth-alnum))
			 #f
			 #t)
		     (if (or
			  (= cand-idx anthy-candidate-type-halfwidth-alnum)
			  (= cand-idx anthy-candidate-type-fullwidth-alnum))
			 #f
			 #t))
		    "??")) ;; FIXME
	      "???") ;; FIXME
	  "????")))) ;; shouldn't happen

(define anthy-utf8-predicting-state-preedit
  (lambda (ac)
    (if (or 
	 (not anthy-use-implicit-commit-prediction?)
	 (not (anthy-utf8-context-prediction-index ac)))
        (anthy-utf8-input-state-preedit ac)
	(let ((cand (anthy-utf8-get-prediction-string ac)))
	  (list (cons (bitwise-ior preedit-reverse preedit-cursor) cand))))))

(define anthy-utf8-converting-state-preedit
  (lambda (ac)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (segments (anthy-utf8-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (separator (anthy-separator ac)))
      (append-map
       (lambda (seg-idx cand-idx)
	 (let* ((attr (if (= seg-idx cur-seg)
			  (bitwise-ior preedit-reverse
				       preedit-cursor)
			  preedit-underline))
		(cand (if (> cand-idx anthy-candidate-type-halfwidth-alnum)
			  (anthy-utf8-lib-get-nth-candidate ac-id seg-idx cand-idx)
			  (anthy-utf8-lib-eucjp-to-utf8 (anthy-utf8-get-raw-candidate ac ac-id seg-idx cand-idx))))
		(seg (list (cons attr cand))))
	   (if (and separator
		    (< 0 seg-idx))
	       (cons separator seg)
	       seg)))
       (iota (ustr-length segments))
       (ustr-whole-seq segments)))))

(define anthy-utf8-input-state-preedit
  (lambda (ac)
    (let* ((preconv-str (anthy-utf8-context-preconv-ustr ac))
	   (rkc (anthy-utf8-context-rkc ac))
	   (pending (rk-pending rkc))
	   (kana (anthy-utf8-context-kana-mode ac))
	   (rule (anthy-utf8-context-input-rule ac))
	   (extract-kana
	    (if (= rule anthy-input-rule-kana)
		(lambda (entry) (car entry))
		(lambda (entry) (list-ref entry kana)))))
      (list
       (and (not (ustr-cursor-at-beginning? preconv-str))
	    (cons preedit-underline
		  (anthy-utf8-lib-eucjp-to-utf8 (string-append-map-ustr-former extract-kana preconv-str))))
       (and (> (string-length pending) 0)
	    (cons preedit-underline (anthy-utf8-lib-eucjp-to-utf8 pending)))
       (and (anthy-utf8-has-preedit? ac)
	    (cons preedit-cursor ""))
       (and (not (ustr-cursor-at-end? preconv-str))
	    (cons
	     preedit-underline
	     (anthy-utf8-lib-eucjp-to-utf8 (string-append-map-ustr-latter extract-kana preconv-str))))))))

(define anthy-utf8-get-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac))
	  (segments (anthy-utf8-context-segments ac)))
      (string-append-map (lambda (seg-idx cand-idx)
			   (if (> cand-idx
				  anthy-candidate-type-halfwidth-alnum)
			       (anthy-utf8-lib-get-nth-candidate
				ac-id seg-idx cand-idx)
			       (anthy-utf8-lib-eucjp-to-utf8
                                 (anthy-utf8-get-raw-candidate
                                   ac ac-id seg-idx cand-idx))))
			 (iota (ustr-length segments))
			 (ustr-whole-seq segments)))))

(define anthy-utf8-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac))
	  (segments (anthy-utf8-context-segments ac)))
      (for-each (lambda (seg-idx cand-idx)
		  (if (> cand-idx anthy-candidate-type-halfwidth-alnum)
		      (anthy-utf8-lib-commit-segment ac-id seg-idx cand-idx)))
		(iota (ustr-length segments))
		(ustr-whole-seq segments)))))

(define anthy-utf8-do-commit
  (lambda (ac)
    (im-commit ac (anthy-utf8-get-commit-string ac))
    (anthy-utf8-commit-string ac)
    (anthy-utf8-reset-candidate-window ac)
    (anthy-utf8-flush ac)))

(define anthy-utf8-get-prediction-string
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac)))
      (anthy-utf8-lib-get-nth-prediction
       ac-id (anthy-utf8-context-prediction-index ac)))))

(define anthy-utf8-learn-prediction-string
  (lambda (ac)
    (let ((ac-id (anthy-utf8-context-ac-id ac)))
      (anthy-utf8-lib-commit-nth-prediction
       ac-id (anthy-utf8-context-prediction-index ac)))))

(define anthy-utf8-do-commit-prediction
  (lambda (ac)
    (im-commit ac (anthy-utf8-get-prediction-string ac))
    (anthy-utf8-learn-prediction-string ac)
    (anthy-utf8-reset-prediction-window ac)
    (anthy-utf8-flush ac)))

(define anthy-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define anthy-utf8-move-segment
  (lambda (ac offset)
    (anthy-utf8-reset-candidate-window ac)
    (let ((segments (anthy-utf8-context-segments ac)))
      (ustr-cursor-move! segments offset)
      (anthy-correct-segment-cursor segments))))

(define anthy-utf8-resize-segment
  (lambda (ac cnt)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (segments (anthy-utf8-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments)))
      (anthy-utf8-reset-candidate-window ac)
      (anthy-utf8-lib-resize-segment ac-id cur-seg cnt)
      (let* ((resized-nseg (anthy-utf8-lib-get-nr-segments ac-id))
	     (latter-nseg (- resized-nseg cur-seg)))
	(ustr-set-latter-seq! segments (make-list latter-nseg 0))))))

(define anthy-utf8-move-candidate
  (lambda (ac offset)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (segments (anthy-utf8-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-utf8-lib-get-nr-candidates ac-id cur-seg))
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
	   (new-op-count (+ 1 (anthy-utf8-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-n)
      (anthy-utf8-context-set-candidate-op-count! ac new-op-count)
      (if (and anthy-use-candidate-window?
	       (= (anthy-utf8-context-candidate-op-count ac)
		  anthy-candidate-op-count))
	  (begin
	    (anthy-utf8-context-set-candidate-window! ac #t)
	    (im-activate-candidate-selector ac max anthy-nr-candidate-max)))
      (if (anthy-utf8-context-candidate-window ac)
	  (im-select-candidate ac compensated-n)))))

(define anthy-utf8-move-candidate-in-page
  (lambda (ac numeralc)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (segments (anthy-utf8-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-utf8-lib-get-nr-candidates ac-id cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= anthy-nr-candidate-max 0)
	   		 0
			 (quotient n anthy-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
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
	   (new-op-count (+ 1 (anthy-utf8-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (anthy-utf8-context-set-candidate-op-count! ac new-op-count)
      (im-select-candidate ac compensated-idx))))

(define anthy-utf8-reset-candidate-window
  (lambda (ac)
    (if (anthy-utf8-context-candidate-window ac)
	(begin
	  (im-deactivate-candidate-selector ac)
	  (anthy-utf8-context-set-candidate-window! ac #f)))
    (anthy-utf8-context-set-candidate-op-count! ac 0)))

(define anthy-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx anthy-candidate-type-halfwidth-alnum)
       (= state anthy-candidate-type-halfwidth-alnum))
      anthy-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx anthy-candidate-type-fullwidth-alnum)
       (= state anthy-candidate-type-fullwidth-alnum))
      anthy-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define anthy-utf8-set-segment-transposing
  (lambda (ac key key-state)
    (let ((segments (anthy-utf8-context-segments ac)))
      (if (and
	   anthy-version
	   (>= (string->number (car anthy-version)) 7802))
	  ;; anthy-7802 and upward
	  (let ((rotate-list '())
		(state #f)
		(idx (ustr-cursor-frontside segments)))
	    (anthy-utf8-reset-candidate-window ac)
	    (anthy-utf8-context-set-candidate-op-count! ac 0)

	    (if (anthy-transpose-as-fullwidth-alnum-key? key key-state)
		(set! rotate-list (cons anthy-candidate-type-fullwidth-alnum
					rotate-list)))
	    (if (anthy-transpose-as-halfwidth-alnum-key? key key-state)
		(set! rotate-list (cons anthy-candidate-type-halfwidth-alnum
					rotate-list)))
	    (if (anthy-transpose-as-halfkana-key? key key-state)
		(set! rotate-list (cons anthy-candidate-type-halfkana
					rotate-list)))
	    (if (anthy-transpose-as-katakana-key? key key-state)
		(set! rotate-list (cons anthy-candidate-type-katakana
					rotate-list)))
	    (if (anthy-transpose-as-hiragana-key? key key-state)
		(set! rotate-list (cons anthy-candidate-type-hiragana
					rotate-list)))
	    (if (or
		 (= idx anthy-candidate-type-hiragana)
		 (= idx anthy-candidate-type-katakana)
		 (= idx anthy-candidate-type-halfkana)
		 (= idx anthy-candidate-type-halfwidth-alnum)
		 (= idx anthy-candidate-type-fullwidth-alnum)
		 (= idx anthy-candidate-type-upper-halfwidth-alnum)
		 (= idx anthy-candidate-type-upper-fullwidth-alnum))
		(let ((lst (member idx rotate-list)))
		  (if (and lst
			   (not (null? (cdr lst))))
		      (set! state (car (cdr lst)))
		      (set! state (anthy-rotate-segment-transposing-alnum-type
				   idx (car rotate-list)))))
		(set! state (car rotate-list)))
	     (ustr-cursor-set-frontside! segments state))
	  ;; below anthy-7802
	  (begin
	    ;; FIXME: don't cancel conversion
	    (anthy-utf8-cancel-conv ac)
	    (anthy-utf8-proc-transposing-state ac key key-state))))))

(define anthy-utf8-proc-converting-state
  (lambda (ac key key-state)
    (cond
     ((anthy-prev-page-key? key key-state)
      (if (anthy-utf8-context-candidate-window ac)
	  (im-shift-page-candidate ac #f)))

     ((anthy-next-page-key? key key-state)
      (if (anthy-utf8-context-candidate-window ac)
	  (im-shift-page-candidate ac #t)))

     ((anthy-commit-key? key key-state)
      (anthy-utf8-do-commit ac))
     
     ((anthy-extend-segment-key? key key-state)
      (anthy-utf8-resize-segment ac 1))
     
     ((anthy-shrink-segment-key? key key-state)
      (anthy-utf8-resize-segment ac -1))
     
     ((anthy-next-segment-key? key key-state)
      (anthy-utf8-move-segment ac 1))
     
     ((anthy-prev-segment-key? key key-state)
      (anthy-utf8-move-segment ac -1))

     ((anthy-beginning-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-beginning! (anthy-utf8-context-segments ac))
	(anthy-utf8-reset-candidate-window ac)))

     ((anthy-end-of-preedit-key? key key-state)
      (begin
	(ustr-cursor-move-end! (anthy-utf8-context-segments ac))
	(anthy-correct-segment-cursor (anthy-utf8-context-segments ac))
	(anthy-utf8-reset-candidate-window ac)))

     ((anthy-backspace-key? key key-state)
      (anthy-utf8-cancel-conv ac))

     ((anthy-next-candidate-key? key key-state)
      (anthy-utf8-move-candidate ac 1))

     ((anthy-prev-candidate-key? key key-state)
      (anthy-utf8-move-candidate ac -1))

     ((or (anthy-transpose-as-hiragana-key? key key-state)
	  (anthy-transpose-as-katakana-key? key key-state)
	  (anthy-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (anthy-utf8-context-input-rule ac) anthy-input-rule-kana))
	   (or
	    (anthy-transpose-as-halfwidth-alnum-key? key key-state)
	    (anthy-transpose-as-fullwidth-alnum-key? key key-state))))
	(anthy-utf8-set-segment-transposing ac key key-state))

     ((anthy-cancel-key? key key-state)
      (anthy-utf8-cancel-conv ac))

     ((and anthy-select-candidate-by-numeral-key?
	   (ichar-numeric? key)
	   (anthy-utf8-context-candidate-window ac))
      (anthy-utf8-move-candidate-in-page ac key))

     ;; don't discard shift-modified keys. Some of them ("?", "~",
     ;; etc) are used to implicit commit. Reported by [Anthy-dev 745]
     ;; -- YamaKen 2004-04-08
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      #f)  ;; use #f rather than () to conform to R5RS

     ((anthy-non-composing-symbol? ac key)
      #f)

     (else
      (begin
	(anthy-utf8-do-commit ac)
	(anthy-utf8-proc-input-state ac key key-state))))))

(define anthy-utf8-press-key-handler
  (lambda (ac key key-state)
    (if (ichar-control? key)
	(im-commit-raw ac)
	(if (anthy-utf8-context-on ac)
	    (if (anthy-utf8-context-transposing ac)
		(anthy-utf8-proc-transposing-state ac key key-state)
		(if (anthy-utf8-context-converting ac)
		    (anthy-utf8-proc-converting-state ac key key-state)
		    (if (anthy-utf8-context-predicting ac)
		        (anthy-utf8-proc-prediction-state ac key key-state)
		        (anthy-utf8-proc-input-state ac key key-state))))
	    (anthy-utf8-proc-raw-state ac key key-state)))
    ;; preedit
    (anthy-utf8-update-preedit ac)))


(define anthy-utf8-release-key-handler
  (lambda (ac key key-state)
    (if (or (ichar-control? key)
	    (not (anthy-utf8-context-on ac)))
	;; don't discard key release event for apps
	(anthy-utf8-commit-raw ac))))

(define anthy-utf8-reset-handler
  (lambda (ac)
    (if (anthy-utf8-context-on ac)
	(anthy-utf8-flush ac))
    ;; code to commit pending string must not be added to here.
    ;; -- YamaKen 2004-10-21
    ))

(define anthy-utf8-get-candidate-handler
  (lambda (ac idx accel-enum-hint)
    (let* ((ac-id (anthy-utf8-context-ac-id ac))
	   (cur-seg (ustr-cursor-pos (anthy-utf8-context-segments ac)))
	   (cand (if (anthy-utf8-context-converting ac)
	             (anthy-utf8-lib-get-nth-candidate ac-id cur-seg idx)
	             (anthy-utf8-lib-get-nth-prediction ac-id idx))))
      (list cand (digit->string (+ idx 1)) ""))))

(define anthy-utf8-set-candidate-index-handler
  (lambda (ac idx)
    (cond
     ((anthy-utf8-context-converting ac)
       (ustr-cursor-set-frontside! (anthy-utf8-context-segments ac) idx)
       (anthy-utf8-update-preedit ac))
     ((anthy-utf8-context-predicting ac)
       (anthy-utf8-context-set-prediction-index! ac idx)
       (anthy-utf8-update-preedit ac)))))

(anthy-utf8-configure-widgets)

(register-im
 'anthy-utf8
 "ja"
 "UTF-8"
 anthy-utf8-im-name-label
 anthy-utf8-im-short-desc
 #f
 anthy-utf8-init-handler
 anthy-utf8-release-handler
 context-mode-handler
 anthy-utf8-press-key-handler
 anthy-utf8-release-key-handler
 anthy-utf8-reset-handler
 anthy-utf8-get-candidate-handler
 anthy-utf8-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
)
