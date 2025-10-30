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

;; SKK is a Japanese input method
;;
;; EUC-JP
;;
;; SKKの入力は下記の状態で構成される
;; Following is list of SKK input state
;;  直接入力 direct
;;  漢字入力 kanji
;;  見出し語補完 completion
;;  変換中 converting
;;  送りがな okuri
;;  英数 latin
;;  全角英数 wide-latin
;;  漢字コード入力 kcode
;;
;;
(require "japanese.scm")
(require-custom "generic-key-custom.scm")
(require-custom "skk-custom.scm")
(require-custom "skk-key-custom.scm")


;;; user config

;; TODO: Support new custom type string-list. It involves character
;; encoding conversion problem.  -- YamaKen 2005-02-02
(define skk-auto-start-henkan-keyword-list '("を" "、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";" ":" "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}" "]" "?" "." "," "!"))

(define skk-ddskk-like-heading-label-char-list '("a" "s" "d" "f" "j" "k" "l"))
(define skk-uim-heading-label-char-list '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

(define skk-ja-rk-rule (append ja-rk-rule-basic ja-rk-rule-additional))
(define skk-okuri-char-alist '())
(define skk-downcase-alist '())
(define skk-set-henkan-point-key '())

(define skk-ichar-downcase
  (lambda (x)
    (or (cdr (or (assoc x skk-downcase-alist)
                 '(#f . #f)))
        (ichar-downcase x))))

(define skk-ichar-upper-case?
  (lambda (x)
    (or (if (assoc x skk-downcase-alist) #t #f) (ichar-upper-case? x))))

(define skk-context-set-okuri-head-using-alist!
  (lambda (sc s)
    (skk-context-set-okuri-head!
      sc
      (or (cdr (or (assoc s skk-okuri-char-alist)
                   '(#f . #f)))
          s))))

;; style specification
(define skk-style-spec
  '(;; (style-element-name . validator)
    (skk-preedit-attr-mode-mark		   . preedit-attr?)
    (skk-preedit-attr-head		   . preedit-attr?)
    (skk-preedit-attr-okuri		   . preedit-attr?)
    (skk-preedit-attr-pending-rk	   . preedit-attr?)
    (skk-preedit-attr-conv-body		   . preedit-attr?)
    (skk-preedit-attr-conv-okuri	   . preedit-attr?)
    (skk-preedit-attr-conv-appendix	   . preedit-attr?)
    (skk-preedit-attr-direct-pending-rk    . preedit-attr?)
    (skk-preedit-attr-child-beginning-mark . preedit-attr?)
    (skk-preedit-attr-child-end-mark       . preedit-attr?)
    (skk-preedit-attr-child-committed      . preedit-attr?)
    (skk-preedit-attr-child-dialog	   . preedit-attr?)
    (skk-preedit-attr-dcomp		   . preedit-attr?)
    (skk-child-context-beginning-mark      . string?)
    (skk-child-context-end-mark		   . string?)
    (skk-show-cursor-on-preedit?	   . boolean?)
    (skk-show-candidates-with-okuri?       . boolean?)))
;; predefined styles
(define skk-style-uim
  '((skk-preedit-attr-mode-mark		   . preedit-reverse)
    (skk-preedit-attr-head		   . preedit-reverse)
    (skk-preedit-attr-okuri		   . preedit-reverse)
    (skk-preedit-attr-pending-rk	   . preedit-reverse)
    (skk-preedit-attr-conv-body		   . preedit-reverse)
    (skk-preedit-attr-conv-okuri	   . preedit-reverse)
    (skk-preedit-attr-conv-appendix	   . preedit-reverse)
    (skk-preedit-attr-direct-pending-rk    . preedit-underline)
    (skk-preedit-attr-child-beginning-mark . preedit-reverse)
    (skk-preedit-attr-child-end-mark       . preedit-reverse)
    (skk-preedit-attr-child-committed      . preedit-reverse)
    (skk-preedit-attr-child-dialog	   . preedit-none)
    (skk-preedit-attr-dcomp		   . preedit-none)
    (skk-child-context-beginning-mark      . "[")
    (skk-child-context-end-mark		   . "]")
    (skk-show-cursor-on-preedit?	   . #f)
    (skk-show-candidates-with-okuri?       . #t)))
(define skk-style-ddskk-like
  '((skk-preedit-attr-mode-mark		   . preedit-underline)
    (skk-preedit-attr-head		   . preedit-underline)
    (skk-preedit-attr-okuri		   . preedit-underline)
    (skk-preedit-attr-pending-rk	   . preedit-underline)
    (skk-preedit-attr-conv-body		   . preedit-reverse)
    (skk-preedit-attr-conv-okuri	   . preedit-underline)
    (skk-preedit-attr-conv-appendix	   . preedit-underline)
    (skk-preedit-attr-direct-pending-rk    . preedit-underline)
    (skk-preedit-attr-child-beginning-mark . preedit-underline)
    (skk-preedit-attr-child-end-mark       . preedit-underline)
    (skk-preedit-attr-child-committed      . preedit-underline)
    (skk-preedit-attr-child-dialog	   . preedit-none)
    (skk-preedit-attr-dcomp		   . preedit-underline)
    (skk-child-context-beginning-mark      . "【")
    (skk-child-context-end-mark		   . "】")
    (skk-show-cursor-on-preedit?	   . #t)
    (skk-show-candidates-with-okuri?       . #f)))

;;; implementations

(define skk-type-hiragana 0)
(define skk-type-katakana 1)
(define skk-type-hankana 2)

(define skk-input-rule-roma 0)
(define skk-input-rule-azik 1)
(define skk-input-rule-act 2)
(define skk-input-rule-kzik 3)

(define skk-child-type-editor 0)
(define skk-child-type-dialog 1)

;; style elements
(define skk-preedit-attr-mode-mark #f)
(define skk-preedit-attr-head #f)
(define skk-preedit-attr-okuri #f)
(define skk-preedit-attr-pending-rk #f)
(define skk-preedit-attr-conv-body #f)
(define skk-preedit-attr-conv-okuri #f)
(define skk-preedit-attr-conv-appendix #f)
(define skk-preedit-attr-direct-pending-rk #f)
(define skk-preedit-attr-child-beginning-mark #f)
(define skk-preedit-attr-child-end-mark #f)
(define skk-preedit-attr-child-committed #f)
(define skk-preedit-attr-child-dialog #f)
(define skk-preedit-attr-dcomp #f)
(define skk-child-context-beginning-mark #f)
(define skk-child-context-end-mark #f)
(define skk-show-cursor-on-preedit? #f)
(define skk-show-candidates-with-okuri? #f)

(define skk-dic #f)
(define skk-context-list '())

(define skk-prepare-activation
  (lambda (sc)
    (skk-flush sc)
    (skk-update-preedit sc)))

(register-action 'action_skk_hiragana
		 (lambda (sc)
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (and (not (skk-latin-state? dsc))
			  (= (skk-context-kana-mode dsc)
			     skk-type-hiragana))))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-context-set-state! dsc 'skk-state-direct)
		     (skk-context-set-kana-mode! dsc skk-type-hiragana))))

(register-action 'action_skk_katakana
		 (lambda (sc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (and (not (skk-latin-state? dsc))
			  (= (skk-context-kana-mode dsc)
			     skk-type-katakana))))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-context-set-state! dsc 'skk-state-direct)
		     (skk-context-set-kana-mode! dsc skk-type-katakana))))

(register-action 'action_skk_hankana
		 (lambda (sc)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (and (not (skk-latin-state? dsc))
			  (= (skk-context-kana-mode dsc)
			     skk-type-hankana))))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-context-set-state! dsc 'skk-state-direct)
		     (skk-context-set-kana-mode! dsc skk-type-hankana))))

(register-action 'action_skk_latin
		 (lambda (sc)
		   '(ja_halfwidth_alnum
		     "a"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (eq? (skk-context-state dsc)
			'skk-state-latin)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-context-set-state! dsc 'skk-state-latin))))

(register-action 'action_skk_wide_latin
		 (lambda (sc)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (eq? (skk-context-state dsc)
			'skk-state-wide-latin)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-context-set-state! dsc 'skk-state-wide-latin))))

(register-action 'action_skk_roma
		 (lambda (sc)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (= (skk-context-input-rule dsc)
			skk-input-rule-roma)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-set-rule! dsc skk-input-rule-roma))))

(register-action 'action_skk_azik
		 (lambda (sc)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (= (skk-context-input-rule dsc)
			skk-input-rule-azik)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-set-rule! dsc skk-input-rule-azik))))

(register-action 'action_skk_act
		 (lambda (sc)
		   '(ja_act
		     "Ｃ"
		     "ACT"
		     "ACT拡張ローマ字入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (= (skk-context-input-rule dsc)
			skk-input-rule-act)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-set-rule! dsc skk-input-rule-act))))


(register-action 'action_skk_kzik
		 (lambda (sc)
		   '(ja_kzik
		     "Ｋ"
		     "KZIK"
		     "KZIK拡張ローマ字入力モード"))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (= (skk-context-input-rule dsc)
			skk-input-rule-kzik)))
		 (lambda (sc)
		   (let ((dsc (skk-find-descendant-context sc)))
		     (skk-prepare-activation dsc)
		     (skk-set-rule! dsc skk-input-rule-kzik))))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define skk-configure-widgets
  (lambda ()
    (register-widget 'widget_skk_input_mode
		     (activity-indicator-new skk-input-mode-actions)
		     (actions-new skk-input-mode-actions))

    (register-widget 'widget_skk_kana_input_method
		     (activity-indicator-new skk-kana-input-method-actions)
		     (actions-new skk-kana-input-method-actions))
    (context-list-replace-widgets! 'skk skk-widgets)))

(define skk-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'state	      'skk-state-latin)
    (list 'kana-mode	      skk-type-hiragana)
    (list 'input-rule	      skk-input-rule-roma)
    (list 'head		      '())
    (list 'okuri-head	      "")
    (list 'okuri	      '())
    (list 'appendix	      '())
    (list 'dcomp-word	      "")
    ;(list 'candidates	      '())
    (list 'nth		      0)
    (list 'nr-candidates      0)
    (list 'rk-context	      '())
    (list 'candidate-op-count 0)
    (list 'candidate-window   #f)
    (list 'child-context      '())
    (list 'child-type	      '())
    (list 'parent-context     '())
    (list 'editor	      '())
    (list 'dialog	      '())
    (list 'latin-conv	      #f)
    (list 'commit-raw	      #f)
    (list 'completion-nth     0))))
(define-record 'skk-context skk-context-rec-spec)
(define skk-context-new-internal skk-context-new)

(define skk-set-rule!
  (lambda (sc input-rule)
    (let ((rkc (skk-context-rk-context sc))
	  (rule (cond
		 ((= input-rule skk-input-rule-roma)
                  (set! skk-okuri-char-alist '())
                  (set! skk-downcase-alist '()) 
                  (set! skk-set-henkan-point-key '())
		  skk-ja-rk-rule)
		 ((= input-rule skk-input-rule-azik)
		  (require "japanese-azik.scm")
                  (set! skk-okuri-char-alist ja-azik-skk-okuri-char-alist)
                  (set! skk-downcase-alist ja-azik-skk-downcase-alist) 
                  (set! skk-set-henkan-point-key ja-azik-skk-set-henkan-point-key)
		  ja-azik-rule)
		 ((= input-rule skk-input-rule-act)
		  (require "japanese-act.scm")
                  (set! skk-okuri-char-alist ja-act-skk-okuri-char-alist)
                  (set! skk-downcase-alist ja-act-skk-downcase-alist)
                  (set! skk-set-henkan-point-key ja-act-skk-set-henkan-point-key)
		  ja-act-rule)
		 ((= input-rule skk-input-rule-kzik)
		  (require "japanese-kzik.scm")
                  (set! skk-okuri-char-alist '())
                  (set! skk-downcase-alist '())
                  (set! skk-set-henkan-point-key '())
		  ja-kzik-rule))))
      (skk-context-set-input-rule! sc input-rule)
      (rk-context-set-rule! rkc rule))))

(define skk-find-root-context
  (lambda (sc)
    (let ((pc (skk-context-parent-context sc)))
      (if (not (null? pc))
	  (skk-find-root-context pc)
	  sc))))

(define skk-find-descendant-context
  (lambda (sc)
    (let ((csc (skk-context-child-context sc)))
      (if (not (null? csc))
	  (skk-find-descendant-context csc)
	  sc))))

(define skk-read-personal-dictionary
  (lambda ()
    (if (not (setugid?))
        (or (skk-lib-read-personal-dictionary skk-dic
                                              skk-uim-personal-dic-filename)
            (skk-lib-read-personal-dictionary skk-dic
                                              skk-personal-dic-filename)))))

(define skk-save-personal-dictionary
  (lambda ()
    (if (not (setugid?))
        (skk-lib-save-personal-dictionary skk-dic
                                          skk-uim-personal-dic-filename))))

(define skk-flush
  (lambda (sc)
    (let ((csc (skk-context-child-context sc)))
      (rk-flush (skk-context-rk-context sc))
      (if skk-use-recursive-learning?
	  (skk-editor-flush (skk-context-editor sc)))
      (skk-dialog-flush (skk-context-dialog sc))
      (if (not (skk-latin-state? sc))
	  (skk-context-set-state! sc 'skk-state-direct))
      (skk-context-set-head! sc '())
      (skk-context-set-okuri-head! sc "")
      (skk-context-set-okuri! sc '())
      (skk-context-set-appendix! sc '())
      (skk-reset-dcomp-word sc)
      (skk-reset-candidate-window sc)
      (skk-context-set-nr-candidates! sc 0)
      (skk-context-set-latin-conv! sc #f)
      (skk-context-set-child-context! sc '())
      (skk-context-set-child-type! sc '())
      (if (not (null? csc))
	  (skk-flush csc)))))

(define skk-context-new
  (lambda (id im)
    (if (not skk-dic)
	(let ((hostname (if skk-skkserv-use-env?
			    (or (getenv "SKKSERVER") "localhost")
			    skk-skkserv-hostname)))
	  (if skk-use-recursive-learning?
	      (require "skk-editor.scm"))
	  (require "skk-dialog.scm")
          (set! skk-dic (skk-lib-dic-open skk-dic-file-name
                                          skk-use-skkserv?
                                          hostname
                                          skk-skkserv-portnum
                                          skk-skkserv-address-family))
          (if skk-use-look?
              (skk-lib-look-open skk-look-dict))
	  (skk-read-personal-dictionary)))
    (let ((sc (skk-context-new-internal id im))
	  (rkc (rk-context-new skk-ja-rk-rule #t #f)))
      (skk-context-set-widgets! sc skk-widgets)
      (skk-context-set-head! sc '())
      (skk-context-set-rk-context! sc rkc)
      (skk-context-set-child-context! sc '())
      (skk-context-set-parent-context! sc '())
      (if skk-use-recursive-learning?
	  (skk-context-set-editor! sc (skk-editor-new sc)))
      (skk-context-set-dialog! sc (skk-dialog-new sc))
      (skk-flush sc)
      (skk-context-set-state! sc 'skk-state-latin)
      sc)))

(define skk-latin-state?
  (lambda (sc)
    (case (skk-context-state sc)
      ((skk-state-latin skk-state-wide-latin) #t)
      (else #f))))

(define skk-make-string
  (lambda (sl kana)
    (let ((get-str-by-type 
	   (lambda (l)
	     (cond
	      ((= kana skk-type-hiragana)
	       (caar l))
	      ((= kana skk-type-katakana)
	       (car (cdar l)))
	      ((= kana skk-type-hankana)
	       (cadr (cdar l)))))))
    (if (not (null? sl))
	(string-append (skk-make-string (cdr sl) kana)
		       (get-str-by-type sl))
	""))))

(define skk-conv-wide-latin
  (lambda (sl)
    (let ((get-wide-latin-str
	   (lambda (l)
	     (ja-wide (caar l)))))
    (if (not (null? sl))
	(string-append (skk-conv-wide-latin (cdr sl))
		       (get-wide-latin-str sl))
	""))))

(define skk-conv-opposite-case
  (lambda (sl)
    (let ((get-opposite-case-str
	   (lambda (l)
	     (let ((c (string->charcode (caar l))))
	       (cond
		((ichar-upper-case? c)
		 (charcode->string (+ c 32)))
		((ichar-lower-case? c)
		 (charcode->string (- c 32)))
		(else
		 (caar l)))))))
      (if (not (null? sl))
	  (string-append (skk-conv-opposite-case (cdr sl))
			 (get-opposite-case-str sl))
	  ""))))

(define skk-opposite-kana
  (lambda (kana)
    (cond
     ((= kana skk-type-hiragana)
      skk-type-katakana)
     ((= kana skk-type-katakana)
      skk-type-hiragana)
     ((= kana skk-type-hankana)
      skk-type-hiragana))))  ; different to ddskk's behavior

(define skk-context-kana-toggle
  (lambda (sc)
    (let* ((kana (skk-context-kana-mode sc))
	   (opposite-kana (skk-opposite-kana kana)))
      (skk-context-set-kana-mode! sc opposite-kana))))

(define skk-get-string-mode-part
  (lambda (sc res type)
    (let ((get-str-by-type 
	   (lambda (l)
	     (cond
	      ((= type skk-type-hiragana)
	       (car l))
	      ((= type skk-type-katakana)
	       (car (cdr l)))
	      ((= type skk-type-hankana)
	       (cadr (cdr l)))))))
      (get-str-by-type res))))

(define skk-do-get-string
  (lambda (sc str kana)
    (if (not (null? str))
	(if (string? (car str))
	    (skk-get-string-mode-part sc str kana)
	    (string-append
	     (skk-do-get-string sc (car str) kana)
	     (skk-do-get-string sc (cdr str) kana)))
	"")))

(define skk-get-string
  (lambda (sc str kana)
    (let ((res (skk-do-get-string sc str kana)))
      (if (and res (> (string-length res) 0))
	  res
	  #f))))

;;; no longer used 
(define skk-get-string-by-mode
  (lambda (sc str)
    (let ((kana (skk-context-kana-mode sc)))
      (skk-get-string sc str kana))))

(define skk-get-nth-candidate
  (lambda (sc n)
    (let* ((head (skk-context-head sc))
    	   (cand (skk-lib-get-nth-candidate
                  skk-dic
                  n
                  (cons (skk-make-string head skk-type-hiragana)
                        (skk-context-okuri-head sc))
                  (skk-make-string (skk-context-okuri sc) skk-type-hiragana)
                  skk-use-numeric-conversion?)))
      (if skk-show-annotation?
	  cand
	  (skk-lib-remove-annotation cand)))))

(define skk-get-current-candidate
  (lambda (sc)
    (skk-get-nth-candidate
     sc
     (skk-context-nth sc))))

(define skk-get-nth-completion
  (lambda (sc n)
    (skk-lib-get-nth-completion
     skk-dic
     n
     (skk-make-string (skk-context-head sc) skk-type-hiragana)
     skk-use-numeric-conversion?
     skk-use-look?)))

(define skk-get-current-completion
  (lambda (sc)
    (skk-get-nth-completion
     sc
     (skk-context-completion-nth sc))))

(define skk-commit-raw
  (lambda (sc key key-state)
    (let ((psc (skk-context-parent-context sc)))
      (if (not (null? psc))
	  (begin
	    (if (= (skk-context-child-type psc)
		   skk-child-type-editor)
		(skk-editor-commit-raw (skk-context-editor psc) key key-state)
		(skk-dialog-commit-raw (skk-context-dialog psc) key key-state)))
	  (begin
	    (skk-context-set-commit-raw! sc #t)
	    (im-commit-raw sc))))))

(define skk-commit-raw-with-preedit-update
  (lambda (sc key key-state)
    (let ((psc (skk-context-parent-context sc)))
      (if (not (null? psc))
	  (begin
	    (if (= (skk-context-child-type psc)
	    	   skk-child-type-editor)
		(skk-editor-commit-raw (skk-context-editor psc) key key-state)
		(skk-dialog-commit-raw (skk-context-dialog psc) key key-state)))
	  (begin
	    (skk-context-set-commit-raw! sc #f)
	    (im-commit-raw sc))))))

;; commit string
(define skk-commit
  (lambda (sc str)
    (let ((psc (skk-context-parent-context sc)))
      (if (not (null? psc))
	  (begin
	    (if (= (skk-context-child-type psc)
	    	   skk-child-type-editor)
		(skk-editor-commit (skk-context-editor psc) str)
		(skk-dialog-commit (skk-context-dialog psc) str)))
	  (im-commit sc str)))))

(define skk-prepare-commit-string
  (lambda (sc)
    (let* ((cand (skk-lib-eval-candidate (skk-lib-remove-annotation (skk-get-current-candidate sc))))
	   (okuri (skk-make-string (skk-context-okuri sc)
				   (skk-context-kana-mode sc)))
	   (appendix (skk-make-string (skk-context-appendix sc)
				   (skk-context-kana-mode sc)))
	   (res (string-append cand okuri appendix))
	   (head (skk-context-head sc)))
      (skk-lib-commit-candidate
       skk-dic
       (cons (skk-make-string head skk-type-hiragana)
             (skk-context-okuri-head sc))
       (skk-make-string (skk-context-okuri sc) skk-type-hiragana)
       (skk-context-nth sc)
       skk-use-numeric-conversion?)
      (if (> (skk-context-nth sc) 0)
	  (skk-save-personal-dictionary))
      (skk-reset-candidate-window sc)
      (skk-flush sc)
      res)))

(define skk-purge-candidate
  (lambda (sc)
    (let ((res (skk-lib-purge-candidate
                skk-dic
                (cons
                 (skk-make-string (skk-context-head sc) skk-type-hiragana)
                 (skk-context-okuri-head sc))
                (skk-make-string (skk-context-okuri sc) skk-type-hiragana)
                (skk-context-nth sc)
                skk-use-numeric-conversion?)))
      (if res
	  (skk-save-personal-dictionary))
      (skk-reset-candidate-window sc)
      (skk-flush sc)
      res)))

(define skk-reset-dcomp-word
  (lambda (sc)
    (if skk-dcomp-activate?
	(skk-context-set-dcomp-word! sc ""))))

(define skk-append-string
  (lambda (sc str)
    (and
     (not (null? str))
     (if (not (string? (car str)))
	 (begin
	   (skk-append-string sc (car str))
	   (skk-append-string sc (cdr str)))
	 #t)
     (skk-context-set-head! sc (cons str (skk-context-head sc)))
     ;;; dcomp
     (if skk-dcomp-activate?
	 (skk-context-set-dcomp-word!
	  sc
	  (skk-lib-get-dcomp-word
           skk-dic
           (skk-make-string
            (skk-context-head sc) (skk-context-kana-mode sc))
           skk-use-numeric-conversion?
           skk-use-look?))))))

(define skk-append-okuri-string
  (lambda (sc str)
    (and
     (not (null? str))
     (if (not (string? (car str)))
	 (begin
	   (skk-append-okuri-string sc (car str))
	   (skk-append-okuri-string sc (cdr str))
	   )
	 #t)
     (skk-context-set-okuri!
      sc
      (cons str (skk-context-okuri sc))))))

(define skk-append-residual-kana
  (lambda (sc)
    (let* ((rkc (skk-context-rk-context sc))
	   (residual-kana (rk-push-key-last! rkc)))
      (if residual-kana
	  (skk-append-string sc residual-kana)))))

(define skk-begin-conversion
  (lambda (sc)
    (let ((res (skk-lib-get-entry
                skk-dic
                (skk-make-string (skk-context-head sc) skk-type-hiragana)
                (skk-context-okuri-head sc)
                (skk-make-string (skk-context-okuri sc)
                                 skk-type-hiragana)
                skk-use-numeric-conversion?)))
      (if res
	  (begin
	    (skk-context-set-nth! sc 0)
	    (skk-context-set-nr-candidates! sc 0)
	    (skk-check-candidate-window-begin sc)
	    (if (skk-context-candidate-window sc)
		(im-select-candidate sc 0))
	    (skk-context-set-state! sc 'skk-state-converting))
	  (if skk-use-recursive-learning?
	      (skk-setup-child-context sc skk-child-type-editor)
	      (skk-flush sc))))))

(define skk-begin-completion
  (lambda (sc)
    ;; get residual 'n'
    (if (eq? (skk-context-state sc) 'skk-state-kanji)
	(skk-append-residual-kana sc))
    (skk-lib-get-completion
     skk-dic
     (skk-make-string (skk-context-head sc) (skk-context-kana-mode sc))
     skk-use-numeric-conversion?
     skk-use-look?)
    (skk-context-set-completion-nth! sc 0)
    (skk-context-set-state! sc 'skk-state-completion)))

(define skk-dcomp-word-tail
  (lambda (sc)
   (let ((h (skk-make-string (skk-context-head sc) skk-type-hiragana))
	 (w (skk-context-dcomp-word sc)))
     (skk-lib-substring w (string-length h) (string-length w)))))


(define skk-do-update-preedit
  (lambda (sc)
    (let ((rkc (skk-context-rk-context sc))
	  (stat (skk-context-state sc))
	  (csc (skk-context-child-context sc))
	  (with-dcomp-word? #f))
      ;; mark
      (if (and
	   (null? csc)
	   (or
	    (eq? stat 'skk-state-kanji)
	    (eq? stat 'skk-state-completion)
	    (eq? stat 'skk-state-okuri)))
	  (im-pushback-preedit sc skk-preedit-attr-mode-mark "▽"))
      (if (and
	   (null? csc)
	   (eq? stat 'skk-state-kcode))
	  (im-pushback-preedit sc skk-preedit-attr-mode-mark "JIS "))
      (if (or
	   (not (null? csc))
	   (eq? stat 'skk-state-converting))
	  (im-pushback-preedit sc skk-preedit-attr-mode-mark "▼"))
      ;; head without child context
      (if (and
	   (null? csc)
	   (or
	    (eq? stat 'skk-state-kanji)
	    (eq? stat 'skk-state-okuri)
	    (eq? stat 'skk-state-kcode)))
	  (let ((h (skk-make-string 
		    (skk-context-head sc)
		    (skk-context-kana-mode sc))))
	    (if (string? h)
		(im-pushback-preedit
		 sc skk-preedit-attr-head
		 h))))
      ;; dcomp
      (if (and
	   skk-dcomp-activate?
	   (null? csc)
	   (eq? stat 'skk-state-kanji)
	   (not (skk-rk-pending? sc))
	   (not (string=? (skk-context-dcomp-word sc) "")))
	  (begin
	    (if skk-show-cursor-on-preedit?
	        (im-pushback-preedit sc preedit-cursor ""))
	    (im-pushback-preedit
	     sc skk-preedit-attr-dcomp
	     (skk-dcomp-word-tail sc))
	    (set! with-dcomp-word? #t)
	     ))
      ;; conv-body + okuri
      (if (and
	   (eq? stat 'skk-state-converting)
	   (or
	    (null? csc)
	    (and
	     (not (null? csc))
	     (= (skk-context-child-type sc) skk-child-type-dialog))))
	  (begin
	    (if (or
		 (eq? skk-candidate-selection-style 'uim)
		 (and
		  (eq? skk-candidate-selection-style 'ddskk-like)
		  (not (skk-context-candidate-window sc))))
		(im-pushback-preedit
		 sc
		 (bitwise-ior skk-preedit-attr-conv-body
			      (if skk-show-cursor-on-preedit?
				  preedit-cursor
				  preedit-none))
		 (if skk-show-annotation-in-preedit?
		     (skk-lib-eval-candidate (skk-get-current-candidate sc))
		     (skk-lib-eval-candidate
		      (skk-lib-remove-annotation
		       (skk-get-current-candidate sc)))))
		(im-pushback-preedit
		 sc
		 (bitwise-ior skk-preedit-attr-conv-body
			      (if skk-show-cursor-on-preedit?
				  preedit-cursor
				  preedit-none))
		 ""))
	    (im-pushback-preedit
	     sc skk-preedit-attr-conv-okuri
	     (skk-make-string (skk-context-okuri sc)
			      (skk-context-kana-mode sc)))
	    (im-pushback-preedit
	     sc skk-preedit-attr-conv-appendix
	     (skk-make-string (skk-context-appendix sc)
			      (skk-context-kana-mode sc)))))
      ;; head with child context
      (if (and
	   (not (null? csc))
	   (or
	     (eq? stat 'skk-state-kanji)
	     (eq? stat 'skk-state-okuri)
	     (and
	      (eq? stat 'skk-state-converting)
	      (eq? (skk-context-child-type sc) skk-child-type-editor))))
	  (let ((h '()))
	    (if skk-use-numeric-conversion?
	      ;; replace numeric string with #
	      (set! h (skk-lib-replace-numeric
			(skk-make-string 
			 (skk-context-head sc)
			 (skk-context-kana-mode sc))))
	      (set! h (skk-make-string 
			(skk-context-head sc)
			(skk-context-kana-mode sc))))
	    (if (string? h)
		(im-pushback-preedit
		 sc skk-preedit-attr-head
		 h))))
      ;; completion
      (if (and
	   (eq? stat 'skk-state-completion)
	   (null? csc))
	  (let ((comp (skk-get-current-completion sc)))
	    (im-pushback-preedit
	     sc skk-preedit-attr-head
	     (if (not (string=? comp ""))
		 comp
		 (skk-make-string 
		  (skk-context-head sc)
		  (skk-context-kana-mode sc))))))
      ;; okuri mark
      (if (or
	   (eq? stat 'skk-state-okuri)
	   (and
	    (not (null? csc))
	    (eq? stat 'skk-state-converting)
	    (not (null? (skk-context-okuri sc)))
	    (= (skk-context-child-type sc) skk-child-type-editor)))
	  (begin
	    (im-pushback-preedit 
	     sc skk-preedit-attr-okuri
	     (string-append
	      "*" (skk-make-string (skk-context-okuri sc)
				   (skk-context-kana-mode sc))))))
      ;; pending rk
      (if (or
	   (eq? stat 'skk-state-direct)
	   (eq? stat 'skk-state-latin)
	   (eq? stat 'skk-state-wide-latin))
	  (begin
	    (im-pushback-preedit sc skk-preedit-attr-direct-pending-rk
				 (rk-pending rkc))
	    (if skk-show-cursor-on-preedit?
	        (im-pushback-preedit sc preedit-cursor "")))
	  (begin
	    (im-pushback-preedit sc skk-preedit-attr-pending-rk
				 (rk-pending rkc))
	    (if (and
		 (or
		  (eq? stat 'skk-state-kanji)
		  (eq? stat 'skk-state-completion)
		  (eq? stat 'skk-state-okuri)
		  (eq? stat 'skk-state-kcode))
		 skk-show-cursor-on-preedit?
		 (not with-dcomp-word?))
		(im-pushback-preedit sc preedit-cursor ""))))
      ;; child context's preedit
      (if (not (null? csc))
	  (let ((editor (skk-context-editor sc))
		(dialog (skk-context-dialog sc)))
	    (if (= (skk-context-child-type sc) skk-child-type-editor)
		(begin
		  (im-pushback-preedit sc
		  		       skk-preedit-attr-child-beginning-mark
				       skk-child-context-beginning-mark)
		  (im-pushback-preedit sc
				       skk-preedit-attr-child-committed
				       (skk-editor-get-left-string editor)))
		(begin
		  (im-pushback-preedit sc
				       skk-preedit-attr-child-dialog
				       skk-child-context-beginning-mark)
		  (im-pushback-preedit sc
		  		       skk-preedit-attr-child-dialog
				       (skk-dialog-get-left-string dialog))))
	    (skk-do-update-preedit csc)
	    (if (= (skk-context-child-type sc) skk-child-type-editor)
	    	(begin
		  (im-pushback-preedit sc
				     skk-preedit-attr-child-committed
				     (skk-editor-get-right-string editor))
		  (im-pushback-preedit sc
		  		       skk-preedit-attr-child-end-mark
				       skk-child-context-end-mark))
		(begin
		  (im-pushback-preedit sc
				       skk-preedit-attr-child-dialog
				       (skk-dialog-get-right-string dialog))
		  (im-pushback-preedit sc
		  		       skk-preedit-attr-child-dialog
				       skk-child-context-end-mark)))))
	    )))

(define skk-update-preedit
  (lambda (sc)
    (if (not (skk-context-commit-raw sc))
	(begin
	  (im-clear-preedit sc)
	  (skk-do-update-preedit (skk-find-root-context sc))
	  (im-update-preedit sc))
	(skk-context-set-commit-raw! sc #f))))


;; called from skk-editor
(define skk-commit-editor-context
  (lambda (sc str)
    (let* ((psc (skk-context-parent-context sc))
	   (okuri (skk-make-string (skk-context-okuri sc)
				   (skk-context-kana-mode sc)))
	   (appendix (skk-make-string (skk-context-appendix sc)
				   (skk-context-kana-mode sc)))
	   (str (if (not (null? psc))
		    str
		    (string-append str okuri appendix))))
      (skk-flush sc)
      (skk-context-set-child-context! sc '())
      (skk-context-set-child-type! sc '())
      (skk-commit sc str))))

(define skk-commit-dialog-context
  (lambda (sc str)
    (let* ((psc (skk-context-parent-context sc))
	   (okuri (skk-make-string (skk-context-okuri sc)
				   (skk-context-kana-mode sc)))
	   (appendix (skk-make-string (skk-context-appendix sc)
				   (skk-context-kana-mode sc)))
	   (str (if (not (null? psc))
		    str
		    (string-append str okuri appendix))))
      (skk-flush sc)
      (skk-context-set-child-context! sc '())
      (skk-context-set-child-type! sc '())
      (skk-commit sc str))))

;; experimental coding style. discussions are welcome -- YamaKen
(define skk-proc-state-direct-no-preedit
  (lambda (key key-state sc rkc)
    (if skk-use-with-vi?
	(if (skk-vi-escape-key? key key-state)
	    (begin
	      (skk-context-set-state! sc 'skk-state-latin)
	      (rk-flush rkc))))
    (cond
     ((or (skk-cancel-key? key key-state)
	  (skk-backspace-key? key key-state)
	  (skk-return-key? key key-state))
      (skk-commit-raw sc key key-state)
      #f)
     ((skk-wide-latin-key? key key-state)
      (skk-context-set-state! sc 'skk-state-wide-latin)
      (rk-flush rkc)
      #f)
     ((skk-latin-key? key key-state)
      (skk-context-set-state! sc 'skk-state-latin)
      (rk-flush rkc)
      #f)
     ((skk-kcode-input-key? key key-state)
      (skk-context-set-state! sc 'skk-state-kcode)
      (rk-flush rkc)
      #f)
     ((skk-latin-conv-key? key key-state)
      (skk-context-set-state! sc 'skk-state-kanji)
      (skk-context-set-latin-conv! sc #t)
      #f)
     ((skk-sticky-key? key key-state)
      (skk-context-set-state! sc 'skk-state-kanji)
      (skk-context-set-latin-conv! sc #f)
      #f)
     ((skk-kanji-mode-key? key key-state)
      (skk-context-set-state! sc 'skk-state-kanji)
      (skk-context-set-latin-conv! sc #f)
      #f)
     ((skk-hankaku-kana-key? key key-state)
      (let* ((kana (skk-context-kana-mode sc))
	     (new-kana (if (= kana skk-type-hankana)
			   skk-type-hiragana
			   skk-type-hankana)))
	(skk-context-set-kana-mode! sc new-kana))
      #f)
     ((skk-kana-toggle-key? key key-state)
      (skk-context-kana-toggle sc)
      #f)
     ;; bad strategy. see bug #528
     ((symbol? key)
      (skk-commit-raw sc key key-state)
      #f)
     ;; bad strategy. see bug #528
     ((or
       (and
	(shift-key-mask key-state)
	(not (ichar-graphic? key)))
       (control-key-mask key-state)
       (alt-key-mask key-state)
       (meta-key-mask key-state)
       (super-key-mask key-state)
       (hyper-key-mask key-state))
      (if (not (skk-state-direct-no-preedit-nop-key? key key-state))
	  (skk-commit-raw sc key key-state))
      #f)
     (else
      #t))))

(define skk-rk-pending?
  (lambda (sc)
    (if (null? (rk-context-seq (skk-context-rk-context sc)))
	#f
	#t)))

(define skk-proc-state-direct
  (lambda (c key key-state)
    (let* ((sc (skk-find-descendant-context c))
	   (key-str (charcode->string (skk-ichar-downcase key)))
	   (rkc (skk-context-rk-context sc))
	   (res #f)
	   (kana (skk-context-kana-mode sc)))
      (and
       ;; at first, no preedit mode
       (if (not (skk-rk-pending? sc))
	   (skk-proc-state-direct-no-preedit key key-state sc rkc)
	   #t)
       (if (skk-cancel-key? key key-state)
	   (begin
	     (skk-flush sc)
	     #f)
	   #t)
       (if (skk-backspace-key? key key-state)
	   (begin
	     (rk-backspace rkc)
	     #f)
	   #t)
       ;; commits "n" as kana according to kana-mode. This is
       ;; ddskk-compatible behavior.
       (if (skk-commit-key? key key-state)
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     #f)
	   #t)
       ;; commits "n" as kana according to kana-mode, and send
       ;; native return
       (if (skk-return-key? key key-state)
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-commit-raw-with-preedit-update sc key key-state)
	     #f)
	   #t)
       ;; Handles "n{L,l,/,\,Q,C-q,C-Q,q}" key sequence as below. This is
       ;; ddskk-compatible behavior.
       ;; 1. commits "n" as kana according to kana-mode
       ;; 2. switch mode by "{L,l,/,\,Q,C-q,C-Q,q}"
       (if (and (skk-wide-latin-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-context-set-state! sc 'skk-state-wide-latin)
	     #f)
	   #t)
       (if (and (skk-latin-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-context-set-state! sc 'skk-state-latin)
	     #f)
	   #t)
       (if (and (skk-kcode-input-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-context-set-state! sc 'skk-state-kcode)
	     #f)
	   #t)
       (if (and (skk-latin-conv-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (let* ((residual-kana (rk-push-key-last! rkc)))
	     (if residual-kana
		 (skk-commit sc (skk-get-string sc residual-kana kana)))
	     (skk-context-set-state! sc 'skk-state-kanji)
	     (skk-context-set-latin-conv! sc #t)
	     #f)
	   #t)
       (if (and (skk-sticky-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (let* ((residual-kana (rk-push-key-last! rkc)))
	     (if residual-kana
		 (skk-commit sc (skk-get-string sc residual-kana kana)))
	     (skk-context-set-state! sc 'skk-state-kanji)
	     (skk-context-set-latin-conv! sc #f)
	     #f)
	   #t)
       (if (and (skk-kanji-mode-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (let* ((residual-kana (rk-push-key-last! rkc)))
	     (if residual-kana
		 (skk-commit sc (skk-get-string sc residual-kana kana)))
	     (skk-context-set-state! sc 'skk-state-kanji)
	     (skk-context-set-latin-conv! sc #f)
	     #f)
	   #t)
       (if (and (skk-hankaku-kana-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (let* ((kana (skk-context-kana-mode sc))
		  (new-kana (if (= kana skk-type-hankana)
		    		  skk-type-hiragana
				  skk-type-hankana)))
	     (set! res (rk-push-key-last! rkc))
	     (skk-context-set-kana-mode! sc new-kana)
	     #f)
	   #t)
       (if (and (skk-kana-toggle-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-context-kana-toggle sc)
	     #f)
	   #t)
       ;; Handles "n " key sequence as below. This is ddskk-compatible
       ;; behavior.
       ;; 1. commits "n" as kana according to kana-mode
       ;; 2. commits " " as native space (such as Qt::Key_Space)
       ;;    unless expected rkc list includes " "
       (if (and (skk-plain-space-key? key key-state)
		(not (rk-expect-key? rkc key-str)))
	   (begin
	     (set! res (rk-push-key-last! rkc))
	     (skk-commit-raw-with-preedit-update sc key key-state)
	     #f)
	   #t)
       ;; bad strategy. see bug #528
       ;; "<Control>a", "<Alt> ", "<Meta>b" and so on
       (if (or
	    (and
	     (shift-key-mask key-state)
	     (not (ichar-graphic? key)))
	    (control-key-mask key-state)
	    (alt-key-mask key-state)
	    (meta-key-mask key-state)
	    (super-key-mask key-state)
	    (hyper-key-mask key-state))
	   (begin
	     (skk-flush sc)
	     (skk-commit-raw-with-preedit-update sc key key-state)
	     #f)
	   #t)
       (if (skk-ichar-upper-case? key)
	   (if (and 
		(skk-rk-pending? sc)
		(not (rk-current-seq rkc)))
	       ;; ddskk compatible behavior but not in SKK speciation
	       (let ((str (rk-push-key! rkc (charcode->string
					      (skk-ichar-downcase key)))))
		 (skk-context-set-state! sc 'skk-state-kanji)
		 (if str
		   (skk-append-string sc str))
		 #f)
	       (let* ((residual-kana (rk-push-key-last! rkc)))
		 ;; handle preceding "n"
		 (if residual-kana
		     (skk-commit sc (skk-get-string sc residual-kana kana)))
		 (skk-context-set-state! sc 'skk-state-kanji)
		 (set! key (skk-ichar-downcase key))
		 #t))
	   #t)
       ;; bad strategy. see bug #528
       (if (symbol? key)
	   (begin
	     (skk-flush sc)
	     (skk-commit-raw-with-preedit-update sc key key-state)
	     #f)
	   #t)
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		key-str))
	 #t));;and
      ;; update state
      (if (eq? (skk-context-state sc) 'skk-state-kanji)
	  (begin
	    (if res
		(skk-append-string sc res))))
      (if (or
	   (eq? (skk-context-state sc) 'skk-state-direct)
	   (eq? (skk-context-state sc) 'skk-state-latin)
	   (eq? (skk-context-state sc) 'skk-state-wide-latin)
	   (eq? (skk-context-state sc) 'skk-state-kcode))
	  (if (and res
		   (or
		    (list? (car res))
		    (not (string=? (car res) ""))))
	      (skk-get-string sc res kana)
	      #f)
	  #f))))

(define skk-sokuon-shiin-char?
  (lambda (c)
    (and (ichar-alphabetic? c)
	 (and
	  (not (= c 97))	;; a
	  (not (= c 105))	;; i
	  (not (= c 117))	;; u
	  (not (= c 101))	;; e
	  (not (= c 111))	;; o
	  (not (= c 110))))))	;; n

(define skk-rk-push-key-match-without-new-seq
  (lambda (rkc key)
    (let* ((s (rk-context-seq rkc))
	   (s (cons key s))
	   (rule (rk-context-rule rkc))
	   (seq (rk-lib-find-seq (reverse s) rule)))
	 (if (and
	      seq
	      (null? (cdar seq)))
	     (cadr seq)
	     #f))))

; see [Anthy-dev: 2646, 2654]
(define skk-commit-with-conv-completion
  (lambda (sc)
    (cond
     ((and skk-dcomp-activate?
	   (not (skk-rk-pending? sc))
	   (not (string=? (skk-context-dcomp-word sc) "")))
      (if (skk-lib-get-entry
           skk-dic
           (skk-context-dcomp-word sc) "" "" skk-use-numeric-conversion?)
	  (begin
	    (skk-string-list-to-context-head
	     sc
	     (string-to-list (skk-context-dcomp-word sc)))
	    (skk-context-set-nth! sc 0)
	    (skk-commit sc (skk-prepare-commit-string sc)))
	  (begin
	    (skk-commit sc (skk-context-dcomp-word sc))
	    (skk-flush sc))))
     ((and skk-dcomp-activate?
	   (skk-rk-pending? sc)
	   (not (string=? (skk-context-dcomp-word sc) "")))
      (skk-append-residual-kana sc)
      (if (not (null? (skk-context-head sc)))
	  (let ((dcomp (skk-lib-get-dcomp-word
                        skk-dic
                        (skk-make-string
                         (skk-context-head sc)
                         (skk-context-kana-mode sc))
                        skk-use-numeric-conversion?
                        skk-use-look?)))
	    (if (not (string=? dcomp ""))
		(begin
		  (skk-string-list-to-context-head
		   sc
		   (string-to-list dcomp))
		  (if (skk-lib-get-entry
                       skk-dic
                       (skk-make-string
                        (skk-context-head sc) skk-type-hiragana)
                       ""
                       ""
                       skk-use-numeric-conversion?)
		      (begin
			(skk-context-set-nth! sc 0)
			(skk-commit sc (skk-prepare-commit-string sc)))
		      (begin
			(skk-commit sc dcomp)
			(skk-flush sc))))
		(begin
		  (if (skk-lib-get-entry
                       skk-dic
                       (skk-make-string
                        (skk-context-head sc) skk-type-hiragana)
                       ""
                       ""
                       skk-use-numeric-conversion?)
		      (begin
			(skk-context-set-nth! sc 0)
			(skk-commit sc (skk-prepare-commit-string sc)))
		      (begin
			(skk-commit sc (skk-make-string
					(skk-context-head sc)
					(skk-context-kana-mode sc)))
			(skk-flush sc))))))))
     (else
      (skk-append-residual-kana sc)
      (if (not (null? (skk-context-head sc)))
	  (begin
	    (if (skk-lib-get-entry
                 skk-dic
                 (skk-make-string
                  (skk-context-head sc) skk-type-hiragana)
                 ""
                 ""
                 skk-use-numeric-conversion?)
		(begin
		  (skk-context-set-nth! sc 0)
		  (skk-commit sc (skk-prepare-commit-string sc)))
		(begin
		  (skk-commit sc (skk-make-string
				  (skk-context-head sc)
				  (skk-context-kana-mode sc)))
		  (skk-flush sc))))
	  (skk-flush sc))))))

(define skk-proc-state-kanji
  (lambda (c key key-state)
    (let* ((sc (skk-find-descendant-context c))
	   (rkc (skk-context-rk-context sc))
	   (stat (skk-context-state sc))
	   (res #f))
      (and
       ;; First, check begin-conv, completion, cancel, backspace,
       ;; commit, and return keys
       (if (skk-begin-conv-key? key key-state)
	   (begin
	     (skk-append-residual-kana sc)
	     (if (not (null? (skk-context-head sc)))
		 (skk-begin-conversion sc)
		 (skk-flush sc))
	     #f)
	   #t)
       (if (skk-begin-completion-key? key key-state)
	   (begin
	     (skk-begin-completion sc)
	     #f)
	   #t)
       (if (skk-cancel-key? key key-state)
	   (begin
	     (skk-flush sc)
	     #f)
	   #t)
       (if (skk-backspace-key? key key-state)
	   (begin
	     (if (not (rk-backspace rkc))
		 (if (> (length (skk-context-head sc)) 0)
		     (skk-context-set-head! sc (cdr (skk-context-head sc)))
		     (skk-flush sc)))
	     ;;; dcomp
	     (if (and
		  skk-dcomp-activate?
		  (eq? (skk-context-state sc) 'skk-state-kanji))
		 (skk-context-set-dcomp-word!
		  sc
		  (if (not (skk-rk-pending? sc))
		      (skk-lib-get-dcomp-word
                       skk-dic
                       (skk-make-string
                        (skk-context-head sc)
                        (skk-context-kana-mode sc))
                       skk-use-numeric-conversion?
                       skk-use-look?)
                      "")))
	     #f)
	   #t)
       (if (or
	    (skk-commit-key? key key-state)
	    (skk-return-key? key key-state))
	   (begin
	     (skk-append-residual-kana sc)
	     (skk-commit sc (skk-make-string
			     (skk-context-head sc)
			     (skk-context-kana-mode sc)))
	     (skk-flush sc)
	     (if (not skk-egg-like-newline?)
		 (if (skk-return-key? key key-state)
		     (if skk-commit-newline-explicitly?
			 (skk-commit sc "\n")
			 (begin
			   (skk-update-preedit sc)
			   (skk-proc-state-direct c key key-state)))))
 	     #f)
	   #t)
       (if (skk-begin-conv-with-completion-key? key key-state)
	   ; do uim's own way --ekato. see [Anthy-dev: 2646, 2654]
	   (begin
	     (cond
	      ((and skk-dcomp-activate?
		    (not (skk-rk-pending? sc))
		    (not (string=? (skk-context-dcomp-word sc) "")))
	       (let ((sl (string-to-list (skk-context-dcomp-word sc))))
		 (skk-string-list-to-context-head sc sl)
		 (skk-begin-conversion sc)))
	      ((and skk-dcomp-activate?
		    (skk-rk-pending? sc)
		    (not (string=? (skk-context-dcomp-word sc) "")))
	       (skk-append-residual-kana sc)
	       (let ((sl (string-to-list
			  (skk-lib-get-dcomp-word
                           skk-dic
                           (skk-make-string
                            (skk-context-head sc)
                            (skk-context-kana-mode sc))
                           skk-use-numeric-conversion?
                           skk-use-look?))))
		 (if (not (null? sl))
		     (begin
		       (skk-string-list-to-context-head sc sl)
		       (skk-begin-conversion sc))
		     (begin
		       (if (not (null? (skk-context-head sc)))
			   (skk-begin-conversion sc)
			   (skk-flush sc))))))
	      (else
	       (skk-append-residual-kana sc)
	       (if (not (null? (skk-context-head sc)))
		   (skk-begin-conversion sc)
		   (skk-flush sc))))
	     #f)
	   #t)
       (if (skk-commit-with-conv-completion-key? key key-state)
	   (begin
	     (skk-commit-with-conv-completion sc)
	     #f)
	   #t)
       ;; Then check latin-conv status before key handling of hiragana/katakana
       (if (skk-context-latin-conv sc)
	   (begin
	     (cond
	      ((skk-conv-wide-latin-key? key key-state) 
	       ;; wide latin conversion
	       (if (not (null? (skk-context-head sc)))
		   (begin
		     (skk-commit sc (skk-conv-wide-latin
				     (skk-context-head sc)))
		     (skk-flush sc))))
	      ((skk-conv-opposite-case-key? key key-state) 
	       ;; alternative case conversion
	       (if (not (null? (skk-context-head sc)))
		   (begin
		     (skk-commit sc (skk-conv-opposite-case
				     (skk-context-head sc)))
		     (skk-flush sc))))
	      (else
	       ;; append latin string
	       (begin
		 (if (ichar-graphic? key)
		     (let* ((s (charcode->string key))
			    (p (cons s (cons s (cons s s)))))
		       (skk-append-string sc p))))))
	     #f)
	   #t)
       (if (skk-kanji-mode-key? key key-state)
	   (begin
	     (skk-append-residual-kana sc)
	     (if (not (null? (skk-context-head sc)))
		 (begin
		   (skk-commit sc (skk-make-string
				   (skk-context-head sc)
				   (skk-context-kana-mode sc)))
		   (skk-flush sc)
		   (skk-context-set-state! sc 'skk-state-kanji)
		   (skk-context-set-latin-conv! sc #f)))
	     #f)
	   #t)
       ;; handle Settou-ji
       (if (skk-special-midashi-key? key key-state)
	   (begin
	     (skk-append-residual-kana sc)
	     (skk-append-string sc '(">" ">" ">"))
	     (skk-begin-conversion sc)
	     #f)
	   #t)
       (if (skk-sticky-key? key key-state)
	   (if (null? (skk-context-head sc))
	     (begin
	       (skk-commit sc (charcode->string key))
	       (skk-flush sc)
	       #f)
	     (begin
	       (skk-context-set-state! sc 'skk-state-okuri)
	       #f))
	   #t)
       (if (and (skk-ichar-upper-case? key)
		(not (null? (skk-context-head sc))))
	   (let ((key-str (charcode->string (skk-ichar-downcase key))))
	     (set! res (skk-rk-push-key-match-without-new-seq rkc key-str))
	     (if (and
		  (skk-rk-pending? sc)
		  (not (rk-current-seq rkc))
		  res)
		 ;; ddskk compatible behavior but not in SKK speciation
		 (begin
		   (skk-context-set-state! sc 'skk-state-okuri)
		   (skk-context-set-okuri-head-using-alist!
		    sc
		    (car (reverse (rk-context-seq rkc))))
		   (rk-context-set-seq! rkc '())
		   (skk-append-okuri-string sc res)
		   (skk-begin-conversion sc)
		   #f)
		 (begin
		   (skk-context-set-state! sc 'skk-state-okuri)
		   (set! key (skk-ichar-downcase key))
		   (skk-context-set-okuri-head-using-alist! sc key-str)
		   (if (and (not (member key skk-set-henkan-point-key)) (skk-sokuon-shiin-char? key))
		       (begin
			 (set! res (rk-push-key! rkc key-str))
			 (if res
			     (skk-context-set-head! sc
						    (cons
						     res
						     (skk-context-head sc))))))
		   (skk-append-residual-kana sc)
		   #t)))
	   #t)
       (if (skk-kana-toggle-key? key key-state)
	   (begin
	     (skk-append-residual-kana sc)
	     (if (not (null? (skk-context-head sc)))
		 (begin
		   (skk-commit sc (skk-make-string
				   (skk-context-head sc)
				   (skk-opposite-kana
				    (skk-context-kana-mode sc))))
	     	   (skk-flush sc)))
	     #f)
	   #t)
       (if (skk-hankaku-kana-key? key key-state)
	   (begin
	     (skk-append-residual-kana sc)
	     (if (not (null? (skk-context-head sc)))
		 (begin
		   (skk-commit sc (skk-make-string (skk-context-head sc)
						   skk-type-hankana))
		   (skk-flush sc)))
	     #f)
	   #t)
       (begin
	 (set! key (skk-ichar-downcase key))  
	 (set! stat (skk-context-state sc))
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
	 (and
	  (if (and
	       res
	       skk-auto-start-henkan?
	       (string-find skk-auto-start-henkan-keyword-list (car res))
	       (not (null? (skk-context-head sc))))
	      (begin
		(skk-context-set-appendix! sc (list res))
		(skk-begin-conversion sc)
		#f)
	      #t)
	  (if (and res
		   (eq? stat 'skk-state-kanji)
		   (or
		    (list? (car res))
		    (not (string=? (car res) ""))))
	      (begin
		(skk-append-string sc res)
		#t)
	      #t)
	   (if (and res
	 	    (eq? stat 'skk-state-okuri)
		    (or
		     (list? (car res))
		     (not (string=? (car res) ""))))
	       (begin
		 (skk-append-okuri-string sc res)
		 (skk-begin-conversion sc))))))
      #f)))

(define skk-setup-child-context
  (lambda (sc type)
    (let ((csc (skk-context-new (skk-context-uc sc)
				(skk-context-im sc)))
	  (input-rule (skk-context-input-rule sc)))
      (skk-context-set-child-context! sc csc)
      (skk-context-set-child-type! sc type)
      (skk-context-set-parent-context! csc sc)
      (if (= type skk-child-type-editor)
	  (skk-context-set-state! csc 'skk-state-direct)
	  (skk-context-set-state! csc 'skk-state-latin))
      (skk-set-rule! csc input-rule))))

(define skk-check-candidate-window-begin
  (lambda (sc)
    (if (and
	 (not (skk-context-candidate-window sc))
	 skk-use-candidate-window?
	 (> (skk-context-nth sc) (- skk-candidate-op-count 2)))
	(begin
	  (skk-context-set-candidate-window! sc #t)
	  (skk-context-set-nr-candidates!
	   sc
	   (skk-lib-get-nr-candidates
            skk-dic
            (skk-make-string (skk-context-head sc) skk-type-hiragana)
            (skk-context-okuri-head sc)
            (skk-make-string (skk-context-okuri sc) skk-type-hiragana)
            skk-use-numeric-conversion?))
	  (im-activate-candidate-selector
	   sc
	   (cond
	    ((eq? skk-candidate-selection-style 'uim)
		  (skk-context-nr-candidates sc))
	    ((eq? skk-candidate-selection-style 'ddskk-like)
		  (- (skk-context-nr-candidates sc)
		     (- skk-candidate-op-count 1))))
	   skk-nr-candidate-max)))))

(define skk-commit-by-label-key
  (lambda (sc key)
    (let ((nr (skk-context-nr-candidates sc))
	  (cur-page (if (= skk-nr-candidate-max 0)
			0
			(cond
			 ((eq? skk-candidate-selection-style 'uim)
			    (quotient (skk-context-nth sc)
				      skk-nr-candidate-max))
			 ((eq? skk-candidate-selection-style 'ddskk-like)
			    (quotient (- (skk-context-nth sc)
					 (- skk-candidate-op-count 1))
				      skk-nr-candidate-max)))))
	  (idx -1)
	  (res #f))
      (cond
       ((eq? skk-candidate-selection-style 'uim)
	(let ((num (- (length skk-uim-heading-label-char-list)
		      (length
		       (member (charcode->string key)
			       skk-uim-heading-label-char-list)))))
	  (if (or (< num skk-nr-candidate-max)
		  (= skk-nr-candidate-max 0))
	      (set! idx (+ (* cur-page skk-nr-candidate-max) num)))))
       ((eq? skk-candidate-selection-style 'ddskk-like)
	(let ((num (- (length skk-ddskk-like-heading-label-char-list)
		      (length
		       (member (charcode->string key)
			       skk-ddskk-like-heading-label-char-list)))))
	  (if (or (< num skk-nr-candidate-max)
		  (= skk-nr-candidate-max 0))
	      (set! idx (+ (* cur-page skk-nr-candidate-max)
			   num (- skk-candidate-op-count 1)))))))
      (if (and (>= idx 0)
	       (< idx nr))
	  (begin
	    (skk-context-set-nth! sc idx)
	    (set! res (skk-prepare-commit-string sc))))
      res)))
      
(define skk-incr-candidate-index
  (lambda (sc)
    (cond
     ((eq? skk-candidate-selection-style 'uim)
      (skk-context-set-nth! sc (+ 1 (skk-context-nth sc))))
     ((eq? skk-candidate-selection-style 'ddskk-like)
      (if (> (+ (skk-context-nth sc) 1) (- skk-candidate-op-count 1))
	  (if (> (+ (skk-context-nth sc) skk-nr-candidate-max)
		 (- (skk-context-nr-candidates sc) 1))
	      ;; go into recursive learning state
	      (skk-context-set-nth! sc (skk-context-nr-candidates sc))
	      ;; just shift to next page
	      (im-shift-page-candidate sc #t))
	  ;; just increment index unless candidate window exist
	  (skk-context-set-nth! sc (+ 1 (skk-context-nth sc))))))
    (skk-context-set-candidate-op-count!
     sc
     (+ 1 (skk-context-candidate-op-count sc)))
    #t))

(define skk-decr-candidate-index
  (lambda (sc)
    (cond
     ((eq? skk-candidate-selection-style 'uim)
      (if (> (skk-context-nth sc) 0)
	  (begin
	    (skk-context-set-nth! sc (- (skk-context-nth sc) 1))
	    #t)
	  (begin
	    (if (= (skk-context-nr-candidates sc) 0)
		(begin
		  (skk-back-to-kanji-state sc)
		  #f)
		(begin
		  (skk-context-set-nth!
		   sc
		   (- (skk-context-nr-candidates sc) 1))
		  #t)))))
     ((eq? skk-candidate-selection-style 'ddskk-like)
      (if (> (skk-context-nth sc)
	     (+ skk-nr-candidate-max (- skk-candidate-op-count 2)))
	  (begin
	    (im-shift-page-candidate sc #f)
	    #t)
	  (if (= (skk-context-nth sc) 0)
	      (begin
		(skk-back-to-kanji-state sc)
		#f)
	      (begin
		(if (> (skk-context-nth sc) (- skk-candidate-op-count 2))
		    (begin
		      (skk-reset-candidate-window sc)
		      (skk-context-set-nth! sc
					    (- skk-candidate-op-count 1))))
		(skk-context-set-nth! sc (- (skk-context-nth sc) 1))
		#t)))))))

(define skk-change-candidate-index
  (lambda (sc incr)
    (let ((head (skk-context-head sc)))
      (and
       (if incr
	   (skk-incr-candidate-index sc)
	   (skk-decr-candidate-index sc))
       (if (null? (skk-get-current-candidate sc))
	   (begin
	     (skk-context-set-nth! sc 0)
	     (if skk-use-recursive-learning?
		 (begin
		   (skk-reset-candidate-window sc)
		   (skk-setup-child-context sc skk-child-type-editor)))
	     #t)
	   #t)
       (if (null? (skk-context-child-context sc))
	   (begin
	     ;; 候補Windowの表示を開始するか
	     (skk-check-candidate-window-begin sc)
	     ;;
	     (if (skk-context-candidate-window sc)
		 (cond
		  ((eq? skk-candidate-selection-style 'uim)
		   (im-select-candidate sc (skk-context-nth sc)))
		  ((eq? skk-candidate-selection-style 'ddskk-like)
		   (im-select-candidate
		    sc
		    (- (skk-context-nth sc) (- skk-candidate-op-count 1))))))
	     #t)
	   #t))
      #f)))

(define skk-reset-candidate-window
  (lambda (sc)
    (if (skk-context-candidate-window sc)
	(begin
	  (im-deactivate-candidate-selector sc)
	  (skk-context-set-candidate-window! sc #f)))
    (skk-context-set-candidate-op-count! sc 0)))

(define skk-back-to-kanji-state
  (lambda (sc)
    (skk-reset-candidate-window sc)
    (skk-context-set-state! sc 'skk-state-kanji)
    (skk-context-set-okuri-head! sc "")
    (if (not (null? (skk-context-okuri sc)))
	(begin
	  (skk-context-set-head! sc
				 (append (skk-context-okuri sc)
					 (skk-context-head sc)))
	  (skk-reset-dcomp-word sc)))
    (if (not (null? (skk-context-appendix sc)))
	(begin
	  (skk-context-set-head! sc
				 (append (skk-context-appendix sc)
					 (skk-context-head sc)))
	  (skk-reset-dcomp-word sc)))
    (skk-context-set-okuri! sc '())
    (skk-context-set-appendix! sc '())
    ;; don't clear dcomp (not compatible with ddskk's behavior)
    ;;(skk-reset-dcomp-word sc )
    (skk-context-set-nr-candidates! sc 0)))

(define skk-back-to-converting-state
  (lambda (sc)
    (skk-context-set-nth! sc (- (skk-context-nr-candidates sc) 1))
    (skk-check-candidate-window-begin sc)
    (if (skk-context-candidate-window sc)
	(cond
	 ((eq? skk-candidate-selection-style 'uim)
	  (im-select-candidate sc (skk-context-nth sc)))
	 ((eq? skk-candidate-selection-style 'ddskk-like)
	  (im-select-candidate
	   sc
	   (- (skk-context-nth sc) (- skk-candidate-op-count 1))))))
    (skk-context-set-state! sc 'skk-state-converting)))

(define skk-change-completion-index
  (lambda (sc incr)
    (if incr
	(begin
	  (if (> (- (skk-lib-get-nr-completions
                     skk-dic
                     (skk-make-string (skk-context-head sc) skk-type-hiragana)
                     skk-use-numeric-conversion?
                     skk-use-look?)
		    1)
		 (skk-context-completion-nth sc))
	      (skk-context-set-completion-nth!
	       sc
	       (+ 1 (skk-context-completion-nth sc)))))
	(begin
	  (if (> (skk-context-completion-nth sc) 0)
	      (skk-context-set-completion-nth!
	       sc
	       (- (skk-context-completion-nth sc) 1)))))
    #f))

(define find-kana-list-from-rule
  (lambda (rule str)
    (if (not (null? rule))
	(if (pair? (member str (car (cdr (car rule)))))
	    (car (cdr (car rule)))
	    (find-kana-list-from-rule (cdr rule) str))
	(list str str str))))

(define skk-string-list-to-context-head
  (lambda (sc sl)
    (skk-context-set-head! sc '())
    (skk-append-string-list-to-context-head sc sl)))

(define skk-append-string-list-to-context-head
  (lambda (sc sl)
    (let ((append-list-to-context-head
    	    (lambda (sc sl)
	      (skk-context-set-head! sc (append (skk-context-head sc)
	      (list sl))))))
      (if (not (null? sl))
	  (begin
	    (append-list-to-context-head
	      sc
	      (if (or
		   (skk-context-latin-conv sc)
		   ;; handle Setsubi-ji and Settou-ji
		   (string=? ">" (car sl))
		   (and
		    skk-use-numeric-conversion?
		    (string=? "#" (car sl))))
	       (list (car sl) (car sl) (car sl))
	       (find-kana-list-from-rule ja-rk-rule-basic (car sl))))
	  (skk-append-string-list-to-context-head sc (cdr sl)))
	#f))))

(define skk-proc-state-completion
  (lambda (c key key-state)
    (let ((sc (skk-find-descendant-context c)))
      (and
       (if (skk-next-completion-key? key key-state)
	   (skk-change-completion-index sc #t)
	   #t)
       (if (skk-prev-completion-key? key key-state)
	   (skk-change-completion-index sc #f)
	   #t)
       (if (skk-new-completion-from-current-comp-key? key key-state)
	   (let* ((comp (skk-get-current-completion sc))
		  (sl (string-to-list comp)))
	     (if (not (null? sl))
		 (begin (skk-lib-get-completion
                         skk-dic
                         (skk-get-current-completion sc)
                         skk-use-numeric-conversion?
                         skk-use-look?)))
	     (skk-lib-clear-completions
	      (skk-make-string
	       (skk-context-head sc)
	       skk-type-hiragana)
	      skk-use-numeric-conversion?)
	     (if (not (null? sl))
		 (skk-string-list-to-context-head sc sl))
	     (skk-context-set-completion-nth! sc 0)
	     (if skk-dcomp-activate?
		 (skk-context-set-dcomp-word!
		  sc
		  (skk-get-current-completion sc)))
	     #f)
	   #t)
       (if (skk-cancel-key? key key-state)
	   (begin
	     (skk-lib-clear-completions
	       (skk-make-string (skk-context-head sc) skk-type-hiragana)
	       skk-use-numeric-conversion?)
	     (skk-context-set-state! sc 'skk-state-kanji)
	     ;; don't clear dcomp (not compatible with ddskk's behavior)
	     ;;(skk-reset-dcomp-word sc)
	     #f)
	   #t)
       (let ((sl (string-to-list (skk-get-current-completion sc))))
	 (skk-lib-clear-completions
	  (skk-make-string (skk-context-head sc) (skk-context-kana-mode sc))
	  skk-use-numeric-conversion?)
	 (if (not (null? sl))
	     (skk-string-list-to-context-head sc sl))
	 (skk-reset-dcomp-word sc)
	 (skk-context-set-state! sc 'skk-state-kanji)
	 (skk-proc-state-kanji c key key-state)))
      #f)))

(define skk-heading-label-char?
  (lambda (key)
    (cond
     ((eq? skk-candidate-selection-style 'uim)
      (if (member (charcode->string key)
      		  skk-uim-heading-label-char-list)
	  #t
	  #f))
     ((eq? skk-candidate-selection-style 'ddskk-like)
      (if (member (charcode->string key)
		  skk-ddskk-like-heading-label-char-list)
	  #t
	  #f)))))

(define skk-proc-state-converting
  (lambda (c key key-state)
    (let ((sc (skk-find-descendant-context c))
	  (res #f))
      (and
       (if (skk-next-candidate-key? key key-state)
	   (skk-change-candidate-index sc #t)
	   #t)
       (if (skk-prev-candidate-key? key key-state)
	   (skk-change-candidate-index sc #f)
	   #t)
       (if (skk-cancel-key? key key-state)
	   (begin
	     ;; back to kanji state
	     (skk-back-to-kanji-state sc)
	     #f)
	   #t)
       (if (skk-next-page-key? key key-state)
	   (begin
	     (if (skk-context-candidate-window sc)
		 (im-shift-page-candidate sc #t))
	     #f)
	   #t)
       (if (skk-prev-page-key? key key-state)
	   (begin
	     (if (skk-context-candidate-window sc)
		 (im-shift-page-candidate sc #f))
	     #f)
	   #t)
       (if (or
	    (skk-commit-key? key key-state)
	    (skk-return-key? key key-state))
	   (begin
	     (set! res (skk-prepare-commit-string sc))
	     (if (skk-return-key? key key-state)
		 (begin
		   (skk-commit sc res)
		   (set! res #f)
		   (if (not skk-egg-like-newline?)
		       (if skk-commit-newline-explicitly?
			   (skk-commit sc "\n")
			   (begin
			     (skk-update-preedit sc)
			     (skk-proc-state-direct c key key-state))))))
	     #f)
	   #t)
       (if (and skk-commit-candidate-by-label-key?
       		(skk-heading-label-char? key)
		(skk-context-candidate-window sc))
	   (begin
	     (set! res (skk-commit-by-label-key sc key))
	     (if res
		 #f
		 #t))
	   #t)
       (if (skk-purge-candidate-key? key key-state)
	   (if (not
		(and (eq? skk-candidate-selection-style 'ddskk-like)
		     (skk-context-candidate-window sc)))
	       (begin
		 (skk-reset-candidate-window sc)
		 (skk-setup-child-context sc skk-child-type-dialog)
		 #f))
	   #t)
       (begin
	 (skk-context-set-state! sc 'skk-state-direct)
	 (set! res (skk-prepare-commit-string sc))
	 (skk-commit sc res)
	 (skk-update-preedit sc)
	 ;; handle Setsubi-ji
	 (if (skk-special-midashi-key? key key-state)
	     (begin
	       (skk-context-set-state! sc 'skk-state-kanji)
	       (skk-append-string sc '(">" ">" ">"))
	       (set! res #f))
	     (set! res (skk-proc-state-direct c key key-state)))))
      res)))

(define skk-proc-state-okuri
  (lambda (c key key-state)
    (let* ((sc (skk-find-descendant-context c))
	   (rkc (skk-context-rk-context sc))
	   (res #f))
      (and
       (if (skk-cancel-key? key key-state)
	   (begin
	     (rk-flush rkc)
	     (skk-back-to-kanji-state sc)
	     #f)
	   #t)
       (if (skk-backspace-key? key key-state)
	   (begin
	     (rk-backspace rkc)
	     (skk-back-to-kanji-state sc)
	     #f)
	   #t)
       ;; committing incomplete head: conformed the behavior to ddskk
       (if (or
	    (skk-commit-key? key key-state)
	    (skk-return-key? key key-state))
	   (begin
	     (skk-commit sc (skk-make-string
			     (skk-context-head sc)
			     (skk-context-kana-mode sc)))
	     (skk-flush sc)
	     (if (skk-return-key? key key-state)
		 (begin
		   (skk-update-preedit sc)
		   (skk-proc-state-direct c key key-state)))
	     #f)
	   #t)
       (begin
	 (if (string=? (skk-context-okuri-head sc) "")
             (if (skk-rk-pending? sc)
               (skk-context-set-okuri-head-using-alist!
                 sc
                 (car (reverse (rk-context-seq rkc))))
               (skk-context-set-okuri-head-using-alist!
                 sc
                 (charcode->string (skk-ichar-downcase key)))))
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string (skk-ichar-downcase key))))
	 (if (and res
	 	  (or
		   (list? (car res))
		   (not (string=? (car res) ""))))
	     (begin
	       (skk-append-okuri-string sc res)
	       (if (not (skk-rk-pending? sc))
		   (skk-begin-conversion sc)))
	     (begin
	       (if (= (length (rk-context-seq rkc)) 1)
		   (skk-context-set-okuri-head-using-alist! sc (charcode->string key)))))))
      #f)))

(define skk-proc-state-latin
  (lambda (c key key-state)
    (let ((sc (skk-find-descendant-context c)))
      (if
       (skk-on-key? key key-state)
       (begin
	 (skk-context-set-state! sc 'skk-state-direct)
	 (skk-context-set-kana-mode! sc skk-type-hiragana))
       (skk-commit-raw sc key key-state))
      #f)))

(define skk-proc-state-wide-latin
  (lambda (c key key-state)
    (let* ((char (charcode->string key))
	   (w (if (symbol? key) #f (ja-wide char)))
	   (sc (skk-find-descendant-context c)))
      (if skk-use-with-vi?
	  (if (skk-vi-escape-key? key key-state)
	      (skk-context-set-state! sc 'skk-state-latin)))
      (cond
       ((skk-on-key? key key-state)
	(skk-flush sc)
	(skk-context-set-state! sc 'skk-state-direct)
	(skk-context-set-kana-mode! sc skk-type-hiragana))
       ((and (modifier-key-mask key-state)
	     (not (shift-key-mask key-state)))
	(skk-commit-raw sc key key-state))
       (w
	(skk-commit sc w))
       (else
	(skk-commit-raw sc key key-state)))
      #f)))

(define skk-proc-state-kcode
  (lambda (c key key-state)
    (let ((sc (skk-find-descendant-context c)))
      (and
       (if (skk-cancel-key? key key-state)
	   (begin
	     (skk-flush sc)
	     #f)
	   #t)
       (if (skk-backspace-key? key key-state)
	   (begin
	     (if (> (length (skk-context-head sc)) 0)
		 (skk-context-set-head! sc (cdr (skk-context-head sc)))
		 (skk-flush sc))
	     #f)
	   #t)
       (if (or
	    (skk-commit-key? key key-state)
	    (skk-return-key? key key-state))
	   (begin
	     (if (> (length (skk-context-head sc)) 0)
	       (let* ((str-list (string-to-list
				  (skk-make-string
				     (skk-context-head sc)
				     (skk-context-kana-mode sc))))
		      (kanji (ja-kanji-code-input str-list)))
		 (if (and kanji (> (string-length kanji) 0))
		   (begin
		     (skk-commit sc kanji)
		     (skk-flush sc))))
	       (skk-flush sc))
	     #f)
	   #t)
       ;; append latin string
       (if (ichar-graphic? key)
	   (let* ((s (charcode->string key))
		  (p (cons s (cons s (cons s s)))))
	     (skk-append-string sc p)
	     #f)
	   #t))
      #f)))

(define skk-push-key
  (lambda (c key key-state)
    (let* ((sc (skk-find-descendant-context c))
	   (state (skk-context-state sc))
	   (fun (cond
		 ((eq? state 'skk-state-direct)
		  skk-proc-state-direct)
		 ((eq? state 'skk-state-kanji)
		  skk-proc-state-kanji)
		 ((eq? state 'skk-state-completion)
		  skk-proc-state-completion)
		 ((eq? state 'skk-state-converting)
		  skk-proc-state-converting)
		 ((eq? state 'skk-state-okuri)
		  skk-proc-state-okuri)
		 ((eq? state 'skk-state-latin)
		  skk-proc-state-latin)
		 ((eq? state 'skk-state-wide-latin)
		  skk-proc-state-wide-latin)
		 ((eq? state 'skk-state-kcode)
		  skk-proc-state-kcode)))
	   (res (fun c key key-state)))
      (if res
	  (skk-commit sc res))
      (skk-update-preedit sc))))

(define skk-init-handler
  (lambda (id im arg)
    (let ((sc (skk-context-new id im)))
      (update-style skk-style-spec (symbol-value skk-style))
      (set! skk-context-list (cons sc skk-context-list))
      sc)))

(define skk-release-handler
  (lambda (sc)
    (skk-save-personal-dictionary)
    (set! skk-context-list (delete! sc skk-context-list))
    (if (null? skk-context-list)
      (begin
        (skk-lib-look-close)
        (skk-lib-free-dic skk-dic)
        (set! skk-dic #f)))))

(define skk-press-key-handler
  (lambda (sc key state)
    (if (ichar-control? key)
	(im-commit-raw sc)
	(skk-push-key sc key state))))

(define skk-release-key-handler
  (lambda (c key state)
    (let* ((sc (skk-find-descendant-context c))
	   (state (skk-context-state sc)))
      (if (eq? state 'skk-state-latin)
	  ;; don't discard key release event for apps
	  (begin
	    (skk-context-set-commit-raw! sc #f)
	    (im-commit-raw sc))))))

(define skk-reset-handler
  (lambda (sc)
    (skk-flush sc)))

(define skk-get-candidate-with-okuri
  (lambda (cand okuri)
    (let ((pos (string-contains cand ";" 0)))
      (if pos
	  (string-append
	   (substring cand 0 pos)
	   (skk-make-string okuri skk-type-hiragana)
	   (substring cand pos (string-length cand)))
	  (string-append
	   cand
	   (skk-make-string okuri skk-type-hiragana))))))

(define skk-get-candidate-handler
  (lambda (sc idx accel-enum-hint)
    (let* ((dcsc (skk-find-descendant-context sc))
	   (cand (skk-lib-eval-candidate
		  (skk-get-nth-candidate
		   dcsc
		   (cond
		    ((eq? skk-candidate-selection-style 'uim)
		       idx)
		    ((eq? skk-candidate-selection-style 'ddskk-like)
		       (+ idx (- skk-candidate-op-count 1)))))))
	   (okuri (skk-context-okuri dcsc)))
      (list
       (if (and
	    (not (null? okuri))
	    skk-show-candidates-with-okuri?)
	   (skk-get-candidate-with-okuri cand okuri)
	   cand)
       (cond
	((eq? skk-candidate-selection-style 'uim)
	 (if (= skk-nr-candidate-max 0)
	     (digit->string (+ idx 1))
	     (begin
	       (set! idx (remainder idx skk-nr-candidate-max))
	       (if (< idx (length skk-uim-heading-label-char-list))
		   (charcode->string
		    (ichar-upcase
		     (string->charcode
		      (nth idx skk-uim-heading-label-char-list))))
		   ""))))
	((eq? skk-candidate-selection-style 'ddskk-like)
	 (if (> skk-nr-candidate-max 0)
	     (set! idx (remainder idx skk-nr-candidate-max)))
	 (if (< idx (length skk-ddskk-like-heading-label-char-list))
	     (charcode->string
	      (ichar-upcase
	       (string->charcode
		(nth idx skk-ddskk-like-heading-label-char-list))))
	     "")))
       ""))))

(define skk-set-candidate-index-handler
  (lambda (c idx)
    (let ((sc (skk-find-descendant-context c)))
      (if (skk-context-candidate-window sc)
	  (begin
	    (cond
	     ((eq? skk-candidate-selection-style 'uim)
	      (skk-context-set-nth! sc idx))
	     ((eq? skk-candidate-selection-style 'ddskk-like)
	      (skk-context-set-nth! sc (+ idx (- skk-candidate-op-count 1)))))
	    (skk-update-preedit sc))))))

(skk-configure-widgets)

(register-im
 'skk
 "ja"
 "EUC-JP"
 skk-im-name-label
 skk-im-short-desc
 #f
 skk-init-handler
 skk-release-handler
 context-mode-handler
 skk-press-key-handler
 skk-release-key-handler
 skk-reset-handler
 skk-get-candidate-handler
 skk-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
