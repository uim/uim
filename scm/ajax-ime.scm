;;; ajax-ime.scm: ajax-ime for uim.
;;;
;;; Copyright (c) 2008-2009 uim Project http://code.google.com/p/uim/
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
(require "input-parse.scm")
(require "http-client.scm")
(require-custom "generic-key-custom.scm")
(require-custom "ajax-ime-custom.scm")
(require-custom "ajax-ime-key-custom.scm")

;;; implementations

;;
;; canna emulating functions
;;

(define ajax-ime-internal-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'str         "")
    (list 'candidates  '())
    (list 'seg-cnts '()))))
(define-record 'ajax-ime-internal-context ajax-ime-internal-context-rec-spec)
(define ajax-ime-internal-context-new-internal ajax-ime-internal-context-new)

(define ajax-ime-url-alist
  '((ajax-ime . ("api.chasen.org" . "/ajaxime/"))
    (cha-ime  . ("cl.naist.jp" . "/~mamoru-k/chaime/api.cgi"))))

(define (ajax-ime-parse str)
  (define (ajax-ime:parse-quoted-word1 port)
    (skip-while '(#\' #\space #\tab *eof*) port)
    (let ((w (next-token '(#\') '(#\' *eof*) "reading word" port)))
      (if (eof-object? (read-char port))
          #f
          w)))
  (define (ajax-ime:parse-quoted-word2 port)
    (let ((parsed #f))
      (and
       (skip-while '(#\' #\, #\space #\tab *eof*) port)
       (set! parsed (next-token '(#\') '(#\' *eof*) "reading word" port))
       (skip-while '(#\' #\space #\tab *eof*) port))
      parsed))
  (define (ajax-ime:parse-quoted-word2* port)
    (let loop ((parsed (ajax-ime:parse-quoted-word2 port))
               (rest '()))
      (if (or (not parsed) (eof-object? (read-char port)))
          (reverse rest)
          (loop (ajax-ime:parse-quoted-word2 port) (cons parsed rest)))))

  (call-with-input-string
   str
   (lambda (port)
     (find-string-from-port? "ImeRequestCallback([" port)
     (let ((w1 (ajax-ime:parse-quoted-word1 port))
           (w2 (ajax-ime:parse-quoted-word2* port)))
       (if (and (string? w1) (list? w2))
           (list (append (list w1) w2))
           '(""))))))

(define (ajax-ime-conversion str opts)
  (define (icovn-convert to-code from-code from-str)
    (and-let* ((ic (iconv-open to-code from-code))
               (to-str (iconv-code-conv ic from-str)))
              (if ic
                  (iconv-release ic))
              to-str))
  (define (make-query)
    (let ((utf8-str (icovn-convert "UTF-8" "EUC-JP" str)))
      (if utf8-str
          (format "~a?action=conv&to=ime&query=~a~a"
                  (cdr (assq-cdr ajax-ime-url ajax-ime-url-alist))
                  (http:encode-uri-string utf8-str)
                  opts
                  )
          str)))
  (define proxy (and (eq? http-proxy-setting 'user)
                     (make-http-proxy http-proxy-hostname http-proxy-port)))
  (define (fetch url)
    (and-let* ((utf8-str (http:get (car (assq-cdr ajax-ime-url ajax-ime-url-alist))
                                   (make-query)
                                   80
                                   proxy))
               (euc-str (icovn-convert "EUC-JP" "UTF-8" utf8-str)))
              euc-str))
  (ajax-ime-parse (fetch (make-query))))

(define (ajax-ime-lib-init)
  #t)
(define (ajax-ime-lib-alloc-context)
  #t)
(define (ajax-ime-lib-get-nth-candidate ac seg nth)
  (let* ((ac-ctx (ajax-ime-context-ac-ctx ac))
         (cand (ajax-ime-internal-context-candidates ac-ctx)))
    (list-ref (list-ref cand seg) nth)))
(define (ajax-ime-lib-release-context ac)
  #t)
(define (ajax-ime-lib-get-unconv-candidate ac seg-idx)
  (let* ((ac-ctx (ajax-ime-context-ac-ctx ac))
         (str (ajax-ime-internal-context-str ac-ctx)))
    ;; XXX
    str))
(define (ajax-ime-lib-get-nr-segments ac)
  (let* ((ac-ctx (ajax-ime-context-ac-ctx ac))
         (cand (ajax-ime-internal-context-candidates ac-ctx)))
    (length cand)))
(define (ajax-ime-lib-get-nr-candidates ac seg)
  (let* ((ac-ctx (ajax-ime-context-ac-ctx ac))
         (cand (ajax-ime-internal-context-candidates ac-ctx)))
    (length (list-ref cand seg))))
(define (ajax-ime-lib-resize-segment ac seg cnt)
  #t)
(define (ajax-ime-lib-begin-conversion ac str)
  (let* ((cand (ajax-ime-conversion str ""))
         (ac-ctx (ajax-ime-internal-context-new-internal)))
    (ajax-ime-internal-context-set-str! ac-ctx str)
    (ajax-ime-internal-context-set-candidates! ac-ctx cand)
    (ajax-ime-internal-context-set-seg-cnts!
     ac-ctx
     (make-list (length cand) 0))
    (ajax-ime-context-set-ac-ctx! ac ac-ctx)
    (length cand)))
(define (ajax-ime-lib-commit-segment ac seg delta)
  #t)
(define (ajax-ime-lib-reset-conversion ac)
  #f)


(define ajax-ime-init-lib-ok? #f)

(define ajax-ime-type-direct	   ja-type-direct)
(define ajax-ime-type-hiragana	   ja-type-hiragana)
(define ajax-ime-type-katakana	   ja-type-katakana)
(define ajax-ime-type-halfkana	   ja-type-halfkana)
(define ajax-ime-type-halfwidth-alnum ja-type-halfwidth-alnum)
(define ajax-ime-type-fullwidth-alnum ja-type-fullwidth-alnum)

(define ajax-ime-input-rule-roma 0)
(define ajax-ime-input-rule-kana 1)
(define ajax-ime-input-rule-azik 2)

(define ajax-ime-candidate-type-katakana -2)
(define ajax-ime-candidate-type-hiragana -3)
(define ajax-ime-candidate-type-halfkana -4)
(define ajax-ime-candidate-type-halfwidth-alnum -5)
(define ajax-ime-candidate-type-fullwidth-alnum -6)
(define ajax-ime-candidate-type-upper-halfwidth-alnum -7)
(define ajax-ime-candidate-type-upper-fullwidth-alnum -8)


;; I don't think the key needs to be customizable.
(define-key ajax-ime-space-key? '(" "))

(define ajax-ime-prepare-input-rule-activation
  (lambda (ac)
    (cond
     ((ajax-ime-context-state ac)
      (ajax-ime-do-commit ac))
     ((ajax-ime-context-transposing ac)
      (im-commit ac (ajax-ime-transposing-text ac)))
     ((and
       (ajax-ime-context-on ac)
       (ajax-ime-has-preedit? ac))
      (im-commit
       ac (ajax-ime-make-whole-string ac #t (ajax-ime-context-kana-mode ac)))))
    (ajax-ime-flush ac)
    (ajax-ime-update-preedit ac)))

(define ajax-ime-prepare-input-mode-activation
  (lambda (ac new-mode)
    (let ((old-kana (ajax-ime-context-kana-mode ac)))
      (cond
       ((ajax-ime-context-state ac)
	(ajax-ime-do-commit ac))
       ((ajax-ime-context-transposing ac)
	(im-commit ac (ajax-ime-transposing-text ac))
	(ajax-ime-flush ac))
       ((and
	 (ajax-ime-context-on ac)
	 (ajax-ime-has-preedit? ac)
	 (not (= old-kana new-mode)))
	(im-commit
	 ac (ajax-ime-make-whole-string ac #t (ajax-ime-context-kana-mode ac)))
	(ajax-ime-flush ac)))
      (ajax-ime-update-preedit ac))))

(register-action 'action_ajax-ime_hiragana
		 (lambda (ac) ;; indication handler
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがな入力モード"))

		 (lambda (ac) ;; activity predicate
		   (and (ajax-ime-context-on ac)
		        (not (ajax-ime-context-alnum ac))
			(= (ajax-ime-context-kana-mode ac)
			   ajax-ime-type-hiragana)))

		 (lambda (ac) ;; action handler
		   (ajax-ime-prepare-input-mode-activation ac ajax-ime-type-hiragana)
		   (ajax-ime-context-set-on! ac #t)
		   (ajax-ime-context-set-alnum! ac #f)
		   (ajax-ime-context-change-kana-mode! ac ajax-ime-type-hiragana)))

(register-action 'action_ajax-ime_katakana
		 (lambda (ac)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナ入力モード"))
		 (lambda (ac)
		   (and (ajax-ime-context-on ac)
		        (not (ajax-ime-context-alnum ac))
			(= (ajax-ime-context-kana-mode ac)
			   ajax-ime-type-katakana)))
		 (lambda (ac)
		   (ajax-ime-prepare-input-mode-activation ac ajax-ime-type-katakana)
		   (ajax-ime-context-set-on! ac #t)
		   (ajax-ime-context-set-alnum! ac #f)
		   (ajax-ime-context-change-kana-mode! ac ajax-ime-type-katakana)))

(register-action 'action_ajax-ime_halfkana
		 (lambda (ac)
		   '(ja_halfkana
		     "ｱ"
		     "半角カタカナ"
		     "半角カタカナ入力モード"))
		 (lambda (ac)
		   (and (ajax-ime-context-on ac)
			(not (ajax-ime-context-alnum ac))
			(= (ajax-ime-context-kana-mode ac) ajax-ime-type-halfkana)))
		 (lambda (ac)
		   (ajax-ime-prepare-input-mode-activation ac ajax-ime-type-halfkana)
		   (ajax-ime-context-set-on! ac #t)
		   (ajax-ime-context-set-alnum! ac #f)
		   (ajax-ime-context-change-kana-mode! ac ajax-ime-type-halfkana)))

(register-action 'action_ajax-ime_halfwidth_alnum
		 (lambda (ac) ;; indication handler
		   '(ja_halfwidth_alnum
		     "a"
		     "半角英数"
		     "半角英数入力モード"))
		 (lambda (ac) ;; activity predicate
		   (and (ajax-ime-context-on ac)
			(ajax-ime-context-alnum ac)
			(= (ajax-ime-context-alnum-type ac)
			   ajax-ime-type-halfwidth-alnum)))
		 (lambda (ac) ;; action handler
		   (ajax-ime-prepare-input-mode-activation
		    ac (ajax-ime-context-kana-mode ac))
		   (ajax-ime-context-set-on! ac #t)
		   (ajax-ime-context-set-alnum! ac #t)
		   (ajax-ime-context-set-alnum-type!
		    ac ajax-ime-type-halfwidth-alnum)))

(register-action 'action_ajax-ime_direct
		 (lambda (ac)
		   '(ja_direct
		     "-"
		     "直接入力"
		     "直接(無変換)入力モード"))
		 (lambda (ac)
		   (not (ajax-ime-context-on ac)))
		 (lambda (ac)
		   (ajax-ime-prepare-input-mode-activation ac ajax-ime-type-direct)
		   (ajax-ime-context-set-on! ac #f)))

(register-action 'action_ajax-ime_fullwidth_alnum
		 (lambda (ac)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角英数入力モード"))
		 (lambda (ac)
		   (and (ajax-ime-context-on ac)
			(ajax-ime-context-alnum ac)
			(= (ajax-ime-context-alnum-type ac)
			   ajax-ime-type-fullwidth-alnum)))
		 (lambda (ac)
		   (ajax-ime-prepare-input-mode-activation
		    ac (ajax-ime-context-kana-mode ac))
		   (ajax-ime-context-set-on! ac #t)
		   (ajax-ime-context-set-alnum! ac #t)
		   (ajax-ime-context-set-alnum-type!
		    ac ajax-ime-type-fullwidth-alnum)))

(register-action 'action_ajax-ime_roma
		 (lambda (ac)
		   '(ja_romaji
		     "Ｒ"
		     "ローマ字"
		     "ローマ字入力モード"))
		 (lambda (ac)
		   (= (ajax-ime-context-input-rule ac)
		      ajax-ime-input-rule-roma))
		 (lambda (ac)
		   (ajax-ime-prepare-input-rule-activation ac)
		   (rk-context-set-rule! (ajax-ime-context-rkc ac)
					 ja-rk-rule)
		   (ajax-ime-context-set-input-rule! ac ajax-ime-input-rule-roma)))

(register-action 'action_ajax-ime_kana
		 (lambda (ac)
		   '(ja_kana
		     "か"
		     "かな"
		     "かな入力モード"))
		 (lambda (ac)
		   (= (ajax-ime-context-input-rule ac)
		      ajax-ime-input-rule-kana))
		 (lambda (ac)
		   (ajax-ime-prepare-input-rule-activation ac)
		   (ajax-ime-context-set-input-rule! ac ajax-ime-input-rule-kana)
                   (ajax-ime-context-change-kana-mode!
                     ac (ajax-ime-context-kana-mode ac))
		   (ajax-ime-context-set-alnum! ac #f)))

(register-action 'action_ajax-ime_azik
		 (lambda (ac)
		   '(ja_azik
		     "Ｚ"
		     "AZIK"
		     "AZIK拡張ローマ字入力モード"))
		 (lambda (ac)
		   (= (ajax-ime-context-input-rule ac)
		      ajax-ime-input-rule-azik))
		 (lambda (ac)
		   (ajax-ime-prepare-input-rule-activation ac)
		   (rk-context-set-rule! (ajax-ime-context-rkc ac)
					 ja-azik-rule)
		   (ajax-ime-context-set-input-rule! ac ajax-ime-input-rule-azik)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define ajax-ime-configure-widgets
  (lambda ()
    (register-widget 'widget_ajax-ime_input_mode
		     (activity-indicator-new ajax-ime-input-mode-actions)
		     (actions-new ajax-ime-input-mode-actions))

    (register-widget 'widget_ajax-ime_kana_input_method
		     (activity-indicator-new ajax-ime-kana-input-method-actions)
		     (actions-new ajax-ime-kana-input-method-actions))
    (context-list-replace-widgets! 'ajax-ime ajax-ime-widgets)))

(define ajax-ime-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'state              #f)
    (list 'transposing        #f)
    (list 'transposing-type    0)
    (list 'ac-ctx             ()) ;; ajax-ime-internal-context
    (list 'preconv-ustr	      #f) ;; preedit strings
    (list 'rkc                ())
    (list 'segments	      #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'kana-mode          ajax-ime-type-hiragana)
    (list 'alnum	      #f)
    (list 'alnum-type	      ajax-ime-type-halfwidth-alnum)
    (list 'commit-raw         #t)
    (list 'input-rule         ajax-ime-input-rule-roma)
    (list 'raw-ustr	      #f))))
(define-record 'ajax-ime-context ajax-ime-context-rec-spec)
(define ajax-ime-context-new-internal ajax-ime-context-new)

(define (ajax-ime-context-new id im)
  (let ((ac (ajax-ime-context-new-internal id im))
	(rkc (rk-context-new ja-rk-rule #t #f)))
;    (ajax-ime-context-set-ac-ctx! ac (if ajax-ime-init-lib-ok?
;				      (ajax-ime-lib-alloc-context) ()))
    (ajax-ime-context-set-ac-ctx! ac (ajax-ime-lib-alloc-context))
    (ajax-ime-context-set-widgets! ac ajax-ime-widgets)
    (ajax-ime-context-set-rkc! ac rkc)
    (ajax-ime-context-set-preconv-ustr! ac (ustr-new '()))
    (ajax-ime-context-set-raw-ustr! ac (ustr-new '()))
    (ajax-ime-context-set-segments! ac (ustr-new '()))
    (if using-kana-table?
        (ajax-ime-context-set-input-rule! ac ajax-ime-input-rule-kana)
        (ajax-ime-context-set-input-rule! ac ajax-ime-input-rule-roma))
    ac))

(define (ajax-ime-commit-raw ac)
  (im-commit-raw ac)
  (ajax-ime-context-set-commit-raw! ac #t))

(define (ajax-ime-context-kana-toggle ac)
  (let* ((kana (ajax-ime-context-kana-mode ac))
	 (opposite-kana (ja-opposite-kana kana)))
    (ajax-ime-context-change-kana-mode! ac opposite-kana)))

(define ajax-ime-context-alkana-toggle
  (lambda (ac)
    (let ((alnum-state (ajax-ime-context-alnum ac)))
      (ajax-ime-context-set-alnum! ac (not alnum-state)))))

(define ajax-ime-context-change-kana-mode!
  (lambda (ac kana-mode)
    (if (= (ajax-ime-context-input-rule ac)
           ajax-ime-input-rule-kana)
        (rk-context-set-rule!
	 (ajax-ime-context-rkc ac)
	 (cond
	  ((= kana-mode ajax-ime-type-hiragana) ja-kana-hiragana-rule)
	  ((= kana-mode ajax-ime-type-katakana) ja-kana-katakana-rule)
	  ((= kana-mode ajax-ime-type-halfkana) ja-kana-halfkana-rule))))
    (ajax-ime-context-set-kana-mode! ac kana-mode)))

(define ajax-ime-make-whole-string
  (lambda (ac convert-pending-into-kana? kana)
    (let* ((rkc (ajax-ime-context-rkc ac))
           (pending (rk-pending rkc))
           (residual-kana (rk-peek-terminal-match rkc))
           (rule (ajax-ime-context-input-rule ac))
           (preconv-str (ajax-ime-context-preconv-ustr ac))
           (extract-kana
            (if (= rule ajax-ime-input-rule-kana)
                (lambda (entry) (car entry))
                (lambda (entry) (list-ref entry kana)))))

      (if (= rule ajax-ime-input-rule-kana)
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

(define ajax-ime-make-raw-string
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
	     (ajax-ime-make-raw-string (cdr raw-str-list) wide? upper?))
	    (string-append
	     (if upper?
		 (string-list-concat
		  (map charcode->string
		       (map ichar-upcase
			    (map string->charcode
				 (string-to-list (car raw-str-list))))))
		 (car raw-str-list))
	     (ajax-ime-make-raw-string (cdr raw-str-list) wide? upper?)))
	"")))

(define ajax-ime-make-whole-raw-string
  (lambda (ac wide? upper?)
    (ajax-ime-make-raw-string (ajax-ime-get-raw-str-seq ac) wide? upper?)))

(define (ajax-ime-init-handler id im arg)
  (if (not ajax-ime-init-lib-ok?)
      (begin
	(ajax-ime-lib-init)
	(set! ajax-ime-init-lib-ok? #t)))
  (ajax-ime-context-new id im))

(define (ajax-ime-release-handler ac)
  (if ac
      (ajax-ime-lib-release-context ac)))

(define (ajax-ime-flush ac)
  (rk-flush (ajax-ime-context-rkc ac))
  (ustr-clear! (ajax-ime-context-preconv-ustr ac))
  (ustr-clear! (ajax-ime-context-raw-ustr ac))
  (ustr-clear! (ajax-ime-context-segments ac))
  (ajax-ime-context-set-transposing! ac #f)
  (ajax-ime-context-set-state! ac #f)
  (if (ajax-ime-context-candidate-window ac)
      (im-deactivate-candidate-selector ac))
  (ajax-ime-context-set-candidate-window! ac #f)
  (ajax-ime-context-set-candidate-op-count! ac 0))

(define (ajax-ime-begin-input ac key key-state)
  (if (cond
       ((ajax-ime-on-key? key key-state)
	#t)
       ((and
	 ajax-ime-use-mode-transition-keys-in-off-mode?
	 (cond
	  ((ajax-ime-hiragana-key? key key-state)
	   (ajax-ime-context-set-kana-mode! ac ajax-ime-type-hiragana)
	   (ajax-ime-context-set-alnum! ac #f)
	   #t)
	  ((ajax-ime-katakana-key? key key-state)
	   (ajax-ime-context-set-kana-mode! ac ajax-ime-type-katakana)
	   (ajax-ime-context-set-alnum! ac #f)
	   #t)
	  ((ajax-ime-halfkana-key? key key-state)
	   (ajax-ime-context-set-kana-mode! ac ajax-ime-type-halfkana)
	   (ajax-ime-context-set-alnum! ac #f)
	   #t)
	  ((ajax-ime-halfwidth-alnum-key? key key-state)
	   (ajax-ime-context-set-alnum-type! ac ajax-ime-type-halfwidth-alnum)
	   (ajax-ime-context-set-alnum! ac #t)
	   #t)
	  ((ajax-ime-halfwidth-alnum-key? key key-state)
	   (ajax-ime-context-set-alnum-type! ac ajax-ime-type-fullwidth-alnum)
	   (ajax-ime-context-set-alnum! ac #t)
	   #t)
	  ((ajax-ime-kana-toggle-key? key key-state)
	   (ajax-ime-context-kana-toggle ac)
	   (ajax-ime-context-set-alnum! ac #f)
	   #t)
	  ((ajax-ime-alkana-toggle-key? key key-state)
	   (ajax-ime-context-alkana-toggle ac)
	   #t)
	  (else
	   #f))))
       (else
	#f))
      (begin
	(ajax-ime-context-set-on! ac #t)
	(rk-flush (ajax-ime-context-rkc ac))
	(ajax-ime-context-set-state! ac #f)
	#t)
      #f))

(define (ajax-ime-update-preedit ac)
  (if (not (ajax-ime-context-commit-raw ac))
      (let ((segments (if (ajax-ime-context-on ac)
			  (if (ajax-ime-context-transposing ac)
			      (ajax-ime-context-transposing-state-preedit ac)
			      (if (ajax-ime-context-state ac)
				  (ajax-ime-compose-state-preedit ac)
				  (ajax-ime-input-state-preedit ac)))
			  ())))
	(context-update-preedit ac segments))
      (ajax-ime-context-set-commit-raw! ac #f)))

(define (ajax-ime-begin-conv ac)
  (let ((ac-ctx (ajax-ime-context-ac-ctx ac))
	(preconv-str (ajax-ime-make-whole-string ac #t ajax-ime-type-hiragana)))
    (if (and ac-ctx
             (> (string-length preconv-str) 0))
	(let ((num (ajax-ime-lib-begin-conversion ac preconv-str)))
	  (if num
	      (begin
		(ustr-set-latter-seq!
		 (ajax-ime-context-segments ac)
		 (make-list num 0))
		(ajax-ime-context-set-state! ac #t)
		;; Don't perform rk-flush here. The rkc must be restored when
		;; ajax-ime-cancel-conv invoked -- YamaKen 2004-10-25
		))))))

(define ajax-ime-cancel-conv
  (lambda (ac)
    (ajax-ime-reset-candidate-window ac)
    (ajax-ime-context-set-state! ac #f)
    (ustr-clear! (ajax-ime-context-segments ac))
    (ajax-ime-lib-reset-conversion ac)))

(define (ajax-ime-proc-input-state-no-preedit ac key key-state)
  (let
      ((rkc (ajax-ime-context-rkc ac))
       (direct (ja-direct (charcode->string key)))
       (rule (ajax-ime-context-input-rule ac)))
    (cond
     ((and ajax-ime-use-with-vi?
           (ajax-ime-vi-escape-key? key key-state))
      (ajax-ime-flush ac)
      (ajax-ime-context-set-on! ac #f)
      (ajax-ime-commit-raw ac))

     ((ajax-ime-off-key? key key-state)
      (ajax-ime-flush ac)
      (ajax-ime-context-set-on! ac #f))

     ((ajax-ime-backspace-key? key key-state)
      (ajax-ime-commit-raw ac))
     
     ((ajax-ime-delete-key? key key-state)
      (ajax-ime-commit-raw ac))

     ((and
       (ajax-ime-hiragana-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-kana-mode ac) ajax-ime-type-hiragana)
	 (not (ajax-ime-context-alnum ac)))))
      (ajax-ime-context-change-kana-mode! ac ajax-ime-type-hiragana)
      (ajax-ime-context-set-alnum! ac #f))

     ((and
       (ajax-ime-katakana-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-kana-mode ac) ajax-ime-type-katakana)
	 (not (ajax-ime-context-alnum ac)))))
      (ajax-ime-context-change-kana-mode! ac ajax-ime-type-katakana)
      (ajax-ime-context-set-alnum! ac #f))
     
     ((and
       (ajax-ime-halfkana-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-kana-mode ac) ajax-ime-type-halfkana)
	 (not (ajax-ime-context-alnum ac)))))
      (ajax-ime-context-change-kana-mode! ac ajax-ime-type-halfkana)
      (ajax-ime-context-set-alnum! ac #f))
     
     ((and
       (ajax-ime-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-alnum-type ac) ajax-ime-type-halfwidth-alnum)
	 (ajax-ime-context-alnum ac))))
      (ajax-ime-context-set-alnum-type! ac ajax-ime-type-halfwidth-alnum)
      (ajax-ime-context-set-alnum! ac #t))
     
     ((and
       (ajax-ime-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-alnum-type ac) ajax-ime-type-fullwidth-alnum)
	 (ajax-ime-context-alnum ac))))
      (ajax-ime-context-set-alnum-type! ac ajax-ime-type-fullwidth-alnum)
      (ajax-ime-context-set-alnum! ac #t))
     
     ((and
       (not (ajax-ime-context-alnum ac))
       (ajax-ime-kana-toggle-key? key key-state))
      (ajax-ime-context-kana-toggle ac))

     ((ajax-ime-alkana-toggle-key? key key-state)
      (ajax-ime-context-alkana-toggle ac))
     
     ;; modifiers (except shift) => ignore
     ((and (modifier-key-mask key-state)
	   (not (shift-key-mask key-state)))
      (ajax-ime-commit-raw ac))
     
     ;; direct key => commit
     (direct
      (im-commit ac direct))

     ;; space key
     ((ajax-ime-space-key? key key-state)
      (if (ajax-ime-context-alnum ac)
	  (im-commit ac (list-ref
			 ja-alnum-space
			 (- (ajax-ime-context-alnum-type ac)
			    ajax-ime-type-halfwidth-alnum)))
	  (im-commit ac (list-ref ja-space (ajax-ime-context-kana-mode ac)))))

     ((symbol? key)
      (ajax-ime-commit-raw ac))

     (else
      (if (ajax-ime-context-alnum ac)
          (let ((key-str (charcode->string key)))
	    (ustr-insert-elem! (ajax-ime-context-preconv-ustr ac)
			       (if (= (ajax-ime-context-alnum-type ac)
				      ajax-ime-type-halfwidth-alnum)
			       (list key-str key-str key-str)
			       (list (ja-wide key-str) (ja-wide key-str)
				     (ja-wide key-str))))
	    (ustr-insert-elem! (ajax-ime-context-raw-ustr ac) key-str))
	  (let* ((key-str (charcode->string
		           (if (= rule ajax-ime-input-rule-kana)
			       key
			       (ichar-downcase key))))
	         (res (rk-push-key! rkc key-str)))
	    (if res
	        (begin
	          (ustr-insert-elem! (ajax-ime-context-preconv-ustr ac) res)
	          (ustr-insert-elem! (ajax-ime-context-raw-ustr ac) key-str))
	        (if (null? (rk-context-seq rkc))
		    (ajax-ime-commit-raw ac)))))))))

(define (ajax-ime-has-preedit? ac)
  (or (not (ustr-empty? (ajax-ime-context-preconv-ustr ac)))
      (> (string-length (rk-pending (ajax-ime-context-rkc ac))) 0)))

(define ajax-ime-rotate-transposing-alnum-type
  (lambda (cur-type state)
    (cond
     ((and
       (= cur-type ajax-ime-type-halfwidth-alnum)
       (= state ajax-ime-type-halfwidth-alnum))
      ajax-ime-candidate-type-upper-halfwidth-alnum)
     ((and
       (= cur-type ajax-ime-type-fullwidth-alnum)
       (= state ajax-ime-type-fullwidth-alnum))
      ajax-ime-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define ajax-ime-proc-transposing-state
  (lambda (ac key key-state)
    (let ((rotate-list '())
	  (state #f))
      (if (ajax-ime-transpose-as-fullwidth-alnum-key? key key-state)
	  (set! rotate-list (cons ajax-ime-type-fullwidth-alnum rotate-list)))
      (if (ajax-ime-transpose-as-halfwidth-alnum-key? key key-state)
	  (set! rotate-list (cons ajax-ime-type-halfwidth-alnum rotate-list)))
      (if (ajax-ime-transpose-as-halfkana-key? key key-state)
	  (set! rotate-list (cons ajax-ime-type-halfkana rotate-list)))
      (if (ajax-ime-transpose-as-katakana-key? key key-state)
	  (set! rotate-list (cons ajax-ime-type-katakana rotate-list)))
      (if (ajax-ime-transpose-as-hiragana-key? key key-state)
	  (set! rotate-list (cons ajax-ime-type-hiragana rotate-list)))

      (if (ajax-ime-context-transposing ac)
	  (let ((lst (member (ajax-ime-context-transposing-type ac) rotate-list)))
	    (if (and lst
	    	     (not (null? (cdr lst))))
		(set! state (car (cdr lst)))
		(if (not (null? rotate-list))
		    (set! state (ajax-ime-rotate-transposing-alnum-type
				 (ajax-ime-context-transposing-type ac)
				 (car rotate-list))))))
	  (begin
	    (ajax-ime-context-set-transposing! ac #t)
	    (set! state (car rotate-list))))

      (cond
       ((and state
	     (or
	      (= state ajax-ime-type-hiragana)
	      (= state ajax-ime-type-katakana)
	      (= state ajax-ime-type-halfkana)))
	(ajax-ime-context-set-transposing-type! ac state))
       ((and state
	     (or
	      (= state ajax-ime-type-halfwidth-alnum)
	      (= state ajax-ime-candidate-type-upper-halfwidth-alnum)
	      (= state ajax-ime-type-fullwidth-alnum)
	      (= state ajax-ime-candidate-type-upper-fullwidth-alnum)))
	(if (not (= (ajax-ime-context-input-rule ac) ajax-ime-input-rule-kana))
	    (ajax-ime-context-set-transposing-type! ac state)))
       (else
	(and
	 ; commit
	 (if (ajax-ime-commit-key? key key-state)
	     (begin
	       (im-commit ac (ajax-ime-transposing-text ac))
	       (ajax-ime-flush ac)
	       #f)
	     #t)
	 ; begin-conv
	 (if (ajax-ime-begin-conv-key? key key-state)
	     (begin
	       (ajax-ime-context-set-transposing! ac #f)
	       (ajax-ime-begin-conv ac)
	       #f)
	     #t)
	 ; cancel
	 (if (or
	      (ajax-ime-cancel-key? key key-state)
	      (ajax-ime-backspace-key? key key-state))
	     (begin
	       (ajax-ime-context-set-transposing! ac #f)
	       #f)
	     #t)
	 ; ignore
	 (if (or
	      (ajax-ime-prev-page-key? key key-state)
	      (ajax-ime-next-page-key? key key-state)
	      (ajax-ime-extend-segment-key? key key-state)
	      (ajax-ime-shrink-segment-key? key key-state)
	      (ajax-ime-next-segment-key? key key-state)
	      (ajax-ime-beginning-of-preedit-key? key key-state)
	      (ajax-ime-end-of-preedit-key? key key-state)
	      (ajax-ime-next-candidate-key? key key-state)
	      (ajax-ime-prev-candidate-key? key key-state)
	      (and
	       (modifier-key-mask key-state)
	       (not (shift-key-mask key-state)))
	      (symbol? key))
	     #f
	     #t)
	 ; implicit commit
	 (begin
	   (im-commit ac (ajax-ime-transposing-text ac))
	   (ajax-ime-flush ac)
	   (ajax-ime-proc-input-state ac key key-state))))))))

(define (ajax-ime-proc-input-state-with-preedit ac key key-state)
  (let ((preconv-str (ajax-ime-context-preconv-ustr ac))
	(raw-str (ajax-ime-context-raw-ustr ac))
	(rkc (ajax-ime-context-rkc ac))
	(rule (ajax-ime-context-input-rule ac))
	(kana (ajax-ime-context-kana-mode ac)))
    (cond
     ;; begin conversion
     ((ajax-ime-begin-conv-key? key key-state)
      (ajax-ime-begin-conv ac))

     ;; backspace
     ((ajax-ime-backspace-key? key key-state)
      (if (not (rk-backspace rkc))
	  (begin
	    (ustr-cursor-delete-backside! preconv-str)
	    (ustr-cursor-delete-backside! raw-str)
	    ;; fix to valid roma
	    (if (and
		 (= (ajax-ime-context-input-rule ac) ajax-ime-input-rule-roma)
		 (not (null? (ustr-former-seq preconv-str)))
		 (not (ichar-printable?
		       (string->ichar
			(car (last (ustr-former-seq preconv-str)))))))
	        (ja-fix-deleted-raw-str-to-valid-roma! raw-str)))))

     ;; delete
     ((ajax-ime-delete-key? key key-state)
      (if (not (rk-delete rkc))
	  (begin
	    (ustr-cursor-delete-frontside! preconv-str)
	    (ustr-cursor-delete-frontside! raw-str))))

       ;; kill
     ((ajax-ime-kill-key? key key-state)
      (ustr-clear-latter! preconv-str)
      (ustr-clear-latter! raw-str))
     
     ;; kill-backward
     ((ajax-ime-kill-backward-key? key key-state)
      (rk-flush rkc)
      (ustr-clear-former! preconv-str)
      (ustr-clear-former! raw-str))
       
     ;; 現在とは逆のかなモードでかなを確定する
     ((and
       (not (ajax-ime-context-alnum ac))
       (ajax-ime-commit-as-opposite-kana-key? key key-state))
      (im-commit ac (ajax-ime-make-whole-string ac #t (ja-opposite-kana kana)))
      (ajax-ime-flush ac))

     ;; Transposing状態へ移行
     ((or (ajax-ime-transpose-as-hiragana-key? key key-state)
	  (ajax-ime-transpose-as-katakana-key? key key-state)
	  (ajax-ime-transpose-as-halfkana-key? key key-state)
	  (and
	   (not (= (ajax-ime-context-input-rule ac) ajax-ime-input-rule-kana))
	   (or
	    (ajax-ime-transpose-as-halfwidth-alnum-key? key key-state)
	    (ajax-ime-transpose-as-fullwidth-alnum-key? key key-state))))
      (ajax-ime-proc-transposing-state ac key key-state))

     ((ajax-ime-hiragana-key? key key-state)
      (if (not (= kana ajax-ime-type-hiragana))
	  (begin
	    (im-commit ac (ajax-ime-make-whole-string ac #t kana))
	    (ajax-ime-flush ac)))
      (ajax-ime-context-set-kana-mode! ac ajax-ime-type-hiragana)
      (ajax-ime-context-set-alnum! ac #f))

     ((ajax-ime-katakana-key? key key-state)
      (if (not (= kana ajax-ime-type-katakana))
	  (begin
	    (im-commit ac (ajax-ime-make-whole-string ac #t kana))
	    (ajax-ime-flush ac)))
      (ajax-ime-context-set-kana-mode! ac ajax-ime-type-katakana)
      (ajax-ime-context-set-alnum! ac #f))

     ((ajax-ime-halfkana-key? key key-state)
      (if (not (= kana ajax-ime-type-halfkana))
	  (begin
	    (im-commit ac (ajax-ime-make-whole-string ac #t kana))
	    (ajax-ime-flush ac)))
      (ajax-ime-context-set-kana-mode! ac ajax-ime-type-halfkana)
      (ajax-ime-context-set-alnum! ac #f))

     ((and
       (ajax-ime-halfwidth-alnum-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-alnum-type ac) ajax-ime-type-halfwidth-alnum)
	 (ajax-ime-context-alnum ac))))
      (ajax-ime-context-set-alnum-type! ac ajax-ime-type-halfwidth-alnum)
      (ajax-ime-context-set-alnum! ac #t))

     ((and
       (ajax-ime-fullwidth-alnum-key? key key-state)
       (not
        (and
	 (= (ajax-ime-context-alnum-type ac) ajax-ime-type-fullwidth-alnum)
	 (ajax-ime-context-alnum ac))))
      (ajax-ime-context-set-alnum-type! ac ajax-ime-type-fullwidth-alnum)
      (ajax-ime-context-set-alnum! ac #t))

     ;; Commit current preedit string, then toggle hiragana/katakana mode.
     ((and
       (not (ajax-ime-context-alnum ac))
       (ajax-ime-kana-toggle-key? key key-state))
      (im-commit ac (ajax-ime-make-whole-string ac #t kana))
      (ajax-ime-flush ac)
      (ajax-ime-context-kana-toggle ac))

     ((ajax-ime-alkana-toggle-key? key key-state)
      (ajax-ime-context-alkana-toggle ac))

     ;; cancel
     ((ajax-ime-cancel-key? key key-state)
      (ajax-ime-flush ac))

     ;; commit
     ((ajax-ime-commit-key? key key-state)
      (begin
	(im-commit
	 ac
	 (ajax-ime-make-whole-string ac #t kana))
	(ajax-ime-flush ac)))

     ;; left
     ((ajax-ime-go-left-key? key key-state)
      (ajax-ime-context-confirm-kana! ac)
      (ustr-cursor-move-backward! preconv-str)
      (ustr-cursor-move-backward! raw-str))

     ;; right
     ((ajax-ime-go-right-key? key key-state)
      (ajax-ime-context-confirm-kana! ac)
      (ustr-cursor-move-forward! preconv-str)
      (ustr-cursor-move-forward! raw-str))

     ;; beginning-of-preedit
     ((ajax-ime-beginning-of-preedit-key? key key-state)
      (ajax-ime-context-confirm-kana! ac)
      (ustr-cursor-move-beginning! preconv-str)
      (ustr-cursor-move-beginning! raw-str))

     ;; end-of-preedit
     ((ajax-ime-end-of-preedit-key? key key-state)
      (ajax-ime-context-confirm-kana! ac)
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
      (if (and (not (ajax-ime-context-alnum ac))
	       (not (ichar-alphabetic? key))
	       (not (string-find
		     (rk-expect rkc)
		     (charcode->string
		      (if (= rule ajax-ime-input-rule-kana)
			  key
			  (ichar-downcase key))))))
	  (let ((pend (rk-pending rkc))
		(residual-kana (rk-push-key-last! rkc)))
	    (if residual-kana
		(begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))))

      (if (ajax-ime-context-alnum ac)
          (let ((key-str (charcode->string key))
	        (pend (rk-pending rkc))
		(residual-kana (rk-peek-terminal-match rkc)))
	    (rk-flush rkc) ;; OK to reset rkc here.
	    (if residual-kana
	        (begin
		  (ustr-insert-elem! preconv-str residual-kana)
		  (ustr-insert-elem! raw-str pend)))
	    (ustr-insert-elem! preconv-str
			       (if (= (ajax-ime-context-alnum-type ac)
				      ajax-ime-type-halfwidth-alnum)
				   (list key-str key-str key-str)
				   (list (ja-wide key-str) (ja-wide key-str)
					 (ja-wide key-str))))
	    (ustr-insert-elem! raw-str key-str))
	  (let* ((key-str (charcode->string
			   (if (= rule ajax-ime-input-rule-kana)
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

(define ajax-ime-context-confirm-kana!
  (lambda (ac)
    (if (= (ajax-ime-context-input-rule ac)
	   ajax-ime-input-rule-kana)
	(let* ((preconv-str (ajax-ime-context-preconv-ustr ac))
	       (rkc (ajax-ime-context-rkc ac))
	       (residual-kana (rk-peek-terminal-match rkc)))
	  (if residual-kana
	      (begin
		(ustr-insert-elem! preconv-str residual-kana)
		(rk-flush rkc)))))))

(define (ajax-ime-proc-input-state ac key key-state)
  (if (ajax-ime-has-preedit? ac)
      (ajax-ime-proc-input-state-with-preedit ac key key-state)
      (ajax-ime-proc-input-state-no-preedit ac key key-state)))

(define ajax-ime-separator
  (lambda (ac)
    (let ((attr (bitwise-ior preedit-separator preedit-underline)))
      (if ajax-ime-show-segment-separator?
	  (cons attr ajax-ime-segment-separator)
	  #f))))

(define ajax-ime-context-transposing-state-preedit
  (lambda (ac)
    (let ((transposing-text (ajax-ime-transposing-text ac)))
      (list (cons preedit-reverse transposing-text)
	    (cons preedit-cursor "")))))

(define ajax-ime-transposing-text
  (lambda (ac)
    (let ((transposing-type (ajax-ime-context-transposing-type ac)))
      (cond
       ((or
	 (= transposing-type ajax-ime-type-hiragana)
	 (= transposing-type ajax-ime-type-katakana)
	 (= transposing-type ajax-ime-type-halfkana))
	(ajax-ime-make-whole-string ac #t transposing-type))
       ((= transposing-type ajax-ime-type-halfwidth-alnum)
	(ajax-ime-make-whole-raw-string ac #f #f))
       ((= transposing-type ajax-ime-candidate-type-upper-halfwidth-alnum)
	(ajax-ime-make-whole-raw-string ac #f #t))
       ((= transposing-type ajax-ime-type-fullwidth-alnum)
	(ajax-ime-make-whole-raw-string ac #t #f))
       ((= transposing-type ajax-ime-candidate-type-upper-fullwidth-alnum)
	(ajax-ime-make-whole-raw-string ac #t #t))))))

(define ajax-ime-get-raw-str-seq
  (lambda (ac)
    (let* ((rkc (ajax-ime-context-rkc ac))
	   (pending (rk-pending rkc))
	   (residual-kana (rk-peek-terminal-match rkc))
	   (raw-str (ajax-ime-context-raw-ustr ac))
	   (right-str (ustr-latter-seq raw-str))
	   (left-str (ustr-former-seq raw-str)))
     (append left-str
	     (if residual-kana
		 (list pending)
		 '())
	      right-str))))

(define ajax-ime-get-raw-candidate
  (lambda (ac seg-idx cand-idx)
    (let* ((preconv
	    (ja-join-vu (string-to-list
			 (ajax-ime-make-whole-string ac #t ajax-ime-type-hiragana))))
	   (unconv-candidate (ajax-ime-lib-get-unconv-candidate ac seg-idx))
	   (unconv (if unconv-candidate
		       (ja-join-vu (string-to-list unconv-candidate))
		       '()))
	   (raw-str (reverse (ajax-ime-get-raw-str-seq ac))))
      (cond
       ((= cand-idx ajax-ime-candidate-type-hiragana)
	(string-list-concat unconv))
       ((= cand-idx ajax-ime-candidate-type-katakana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) ajax-ime-type-katakana))
       ((= cand-idx ajax-ime-candidate-type-halfkana)
	(ja-make-kana-str (ja-make-kana-str-list unconv) ajax-ime-type-halfkana))
       (else
	(if (not (null? unconv))
	    (if (member (car unconv) preconv)
		(let ((start (list-seq-contained? preconv unconv))
		      (len (length unconv)))
		  (if start
		      (ajax-ime-make-raw-string
		       (reverse (sublist-rel raw-str start len))
		       (if (or
			    (= cand-idx ajax-ime-candidate-type-halfwidth-alnum)
			    (= cand-idx
			       ajax-ime-candidate-type-upper-halfwidth-alnum))
			   #f
			   #t)
		       (if (or
			    (= cand-idx ajax-ime-candidate-type-halfwidth-alnum)
			    (= cand-idx ajax-ime-candidate-type-fullwidth-alnum))
			   #f
			   #t))
		      "??")) ;; FIXME
		"???") ;; FIXME
	    "????")))))) ;; shouldn't happen

(define (ajax-ime-compose-state-preedit ac)
  (let* ((segments (ajax-ime-context-segments ac))
	 (cur-seg (ustr-cursor-pos segments))
	 (separator (ajax-ime-separator ac)))
    (append-map
     (lambda (seg-idx cand-idx)
       (let* ((attr (if (= seg-idx cur-seg)
			(bitwise-ior preedit-reverse
				     preedit-cursor)
			preedit-underline))
	      (cand (if (> cand-idx ajax-ime-candidate-type-katakana)
			(ajax-ime-lib-get-nth-candidate ac seg-idx cand-idx)
			(ajax-ime-get-raw-candidate ac seg-idx cand-idx)))
	      (seg (list (cons attr cand))))
	 (if (and separator
		  (< 0 seg-idx))
	     (cons separator seg)
	     seg)))
     (iota (ustr-length segments))
     (ustr-whole-seq segments))))

(define (ajax-ime-input-state-preedit ac)
  (let* ((preconv-str (ajax-ime-context-preconv-ustr ac))
	 (rkc (ajax-ime-context-rkc ac))
	 (pending (rk-pending rkc))
	 (kana (ajax-ime-context-kana-mode ac))
	 (rule (ajax-ime-context-input-rule ac))
	 (extract-kana
	  (if (= rule ajax-ime-input-rule-kana)
	      (lambda (entry) (car entry))
	      (lambda (entry) (list-ref entry kana)))))
    (list
     (and (not (ustr-cursor-at-beginning? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-former extract-kana preconv-str)))
     (and (> (string-length pending) 0)
	  (cons preedit-underline pending))
     (and (ajax-ime-has-preedit? ac)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? preconv-str))
	  (cons preedit-underline
		(string-append-map-ustr-latter extract-kana preconv-str))))))

(define (ajax-ime-get-commit-string ac)
  (let ((segments (ajax-ime-context-segments ac)))
    (string-append-map (lambda (seg-idx cand-idx)
			 (if (> cand-idx ajax-ime-candidate-type-katakana)
			     (ajax-ime-lib-get-nth-candidate
			      ac seg-idx cand-idx)
			     (ajax-ime-get-raw-candidate
			      ac seg-idx cand-idx)))
		       (iota (ustr-length segments))
		       (ustr-whole-seq segments))))

(define (ajax-ime-commit-string ac)
    (let ((ac-ctx (ajax-ime-context-ac-ctx ac))
          (segments (ajax-ime-context-segments ac)))
      (if ac-ctx
          (begin
            (for-each (lambda (seg-idx cand-idx)
                        (if (> cand-idx ajax-ime-candidate-type-katakana)
                            (ajax-ime-lib-commit-segment ac seg-idx cand-idx)))
                      (iota (ustr-length segments))
                      (ustr-whole-seq segments))
            (if (every (lambda (x) (<= x ajax-ime-candidate-type-katakana))
                       (ustr-whole-seq segments))
                (ajax-ime-lib-reset-conversion ac))))))

(define (ajax-ime-do-commit ac)
    (im-commit ac (ajax-ime-get-commit-string ac))
    (ajax-ime-commit-string ac)
    (ajax-ime-reset-candidate-window ac)
    (ajax-ime-flush ac))

(define ajax-ime-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define (ajax-ime-move-segment ac dir)
  (ajax-ime-reset-candidate-window ac)
  (let ((segments (ajax-ime-context-segments ac)))
    (ustr-cursor-move! segments dir)
    (ajax-ime-correct-segment-cursor segments)))

(define (ajax-ime-resize-segment ac cnt)
  (let* ((segments (ajax-ime-context-segments ac))
	 (cur-seg (ustr-cursor-pos segments)))
    (ajax-ime-reset-candidate-window ac)
    (ajax-ime-lib-resize-segment ac cur-seg cnt)
    (let* ((resized-nseg (ajax-ime-lib-get-nr-segments ac))
           (latter-nseg (- resized-nseg cur-seg)))
      (ustr-set-latter-seq! segments (make-list latter-nseg 0)))))

(define (ajax-ime-move-candidate ac offset)
  (let* ((segments (ajax-ime-context-segments ac))
	 (cur-seg (ustr-cursor-pos segments))
	 (max (ajax-ime-lib-get-nr-candidates ac cur-seg))
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
	 (new-op-count (+ 1 (ajax-ime-context-candidate-op-count ac))))
    (ustr-cursor-set-frontside! segments compensated-n)
    (ajax-ime-context-set-candidate-op-count! ac new-op-count)
    (if (and
	 (= (ajax-ime-context-candidate-op-count ac)
	    ajax-ime-candidate-op-count)
	 ajax-ime-use-candidate-window?)
	(begin
	  (ajax-ime-context-set-candidate-window! ac #t)
	  (im-activate-candidate-selector ac max ajax-ime-nr-candidate-max)))
    (if (ajax-ime-context-candidate-window ac)
	(im-select-candidate ac compensated-n))))

(define ajax-ime-move-candidate-in-page
  (lambda (ac numeralc)
    (let* ((segments (ajax-ime-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (ajax-ime-lib-get-nr-candidates ac cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= ajax-ime-nr-candidate-max 0)
			 0
			 (quotient n ajax-ime-nr-candidate-max)))
	   (pageidx (- (numeric-ichar->integer numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page ajax-ime-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (ajax-ime-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (ajax-ime-context-set-candidate-op-count! ac new-op-count)
      (im-select-candidate ac compensated-idx))))

(define (ajax-ime-reset-candidate-window ac)
  (if (ajax-ime-context-candidate-window ac)
      (begin
	(im-deactivate-candidate-selector ac)
	(ajax-ime-context-set-candidate-window! ac #f)))
  (ajax-ime-context-set-candidate-op-count! ac 0))

(define ajax-ime-rotate-segment-transposing-alnum-type
  (lambda (idx state)
    (cond
     ((and
       (= idx ajax-ime-candidate-type-halfwidth-alnum)
       (= state ajax-ime-candidate-type-halfwidth-alnum))
      ajax-ime-candidate-type-upper-halfwidth-alnum)
     ((and
       (= idx ajax-ime-candidate-type-fullwidth-alnum)
       (= state ajax-ime-candidate-type-fullwidth-alnum))
      ajax-ime-candidate-type-upper-fullwidth-alnum)
     (else
      state))))

(define ajax-ime-set-segment-transposing
  (lambda (ac key key-state)
    (let ((segments (ajax-ime-context-segments ac)))
      (let ((rotate-list '())
	    (state #f)
	    (idx (ustr-cursor-frontside segments)))
	(ajax-ime-reset-candidate-window ac)
	(ajax-ime-context-set-candidate-op-count! ac 0)

	(if (ajax-ime-transpose-as-fullwidth-alnum-key? key key-state)
	    (set! rotate-list (cons ajax-ime-candidate-type-fullwidth-alnum
				    rotate-list)))
	(if (ajax-ime-transpose-as-halfwidth-alnum-key? key key-state)
	    (set! rotate-list (cons ajax-ime-candidate-type-halfwidth-alnum
				    rotate-list)))
	(if (ajax-ime-transpose-as-halfkana-key? key key-state)
	    (set! rotate-list (cons ajax-ime-candidate-type-halfkana
				    rotate-list)))
	(if (ajax-ime-transpose-as-katakana-key? key key-state)
	    (set! rotate-list (cons ajax-ime-candidate-type-katakana
				    rotate-list)))
	(if (ajax-ime-transpose-as-hiragana-key? key key-state)
	    (set! rotate-list (cons ajax-ime-candidate-type-hiragana
				    rotate-list)))
	(if (or
	     (= idx ajax-ime-candidate-type-hiragana)
	     (= idx ajax-ime-candidate-type-katakana)
	     (= idx ajax-ime-candidate-type-halfkana)
	     (= idx ajax-ime-candidate-type-halfwidth-alnum)
	     (= idx ajax-ime-candidate-type-fullwidth-alnum)
	     (= idx ajax-ime-candidate-type-upper-halfwidth-alnum)
	     (= idx ajax-ime-candidate-type-upper-fullwidth-alnum))
	    (let ((lst (member idx rotate-list)))
	      (if (and lst
		       (not (null? (cdr lst))))
		  (set! state (car (cdr lst)))
		  (set! state (ajax-ime-rotate-segment-transposing-alnum-type
			       idx (car rotate-list)))))
	    (set! state (car rotate-list)))
	(ustr-cursor-set-frontside! segments state)))))

(define (ajax-ime-proc-compose-state ac key key-state)
  (cond
   ((ajax-ime-prev-page-key? key key-state)
    (if (ajax-ime-context-candidate-window ac)
        (im-shift-page-candidate ac #f)))

   ((ajax-ime-next-page-key? key key-state)
    (if (ajax-ime-context-candidate-window ac)
        (im-shift-page-candidate ac #t)))

   ((ajax-ime-commit-key? key key-state)
    (ajax-ime-do-commit ac))

   ((ajax-ime-extend-segment-key? key key-state)
    (ajax-ime-resize-segment ac 1))

   ((ajax-ime-shrink-segment-key? key key-state)
    (ajax-ime-resize-segment ac -1))

   ((ajax-ime-next-segment-key? key key-state)
    (ajax-ime-move-segment ac 1))

   ((ajax-ime-prev-segment-key? key key-state)
    (ajax-ime-move-segment ac -1))

   ((ajax-ime-beginning-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-beginning! (ajax-ime-context-segments ac))
      (ajax-ime-reset-candidate-window ac)))

   ((ajax-ime-end-of-preedit-key? key key-state)
    (begin
      (ustr-cursor-move-end! (ajax-ime-context-segments ac))
      (ajax-ime-correct-segment-cursor (ajax-ime-context-segments ac))
      (ajax-ime-reset-candidate-window ac)))

   ((ajax-ime-backspace-key? key key-state)
    (ajax-ime-cancel-conv ac))

   ((ajax-ime-next-candidate-key? key key-state)
    (ajax-ime-move-candidate ac 1))

   ((ajax-ime-prev-candidate-key? key key-state)
    (ajax-ime-move-candidate ac -1))

   ((or (ajax-ime-transpose-as-hiragana-key? key key-state)
        (ajax-ime-transpose-as-katakana-key? key key-state)
        (ajax-ime-transpose-as-halfkana-key? key key-state)
        (and
         (not (= (ajax-ime-context-input-rule ac) ajax-ime-input-rule-kana))
         (or
          (ajax-ime-transpose-as-halfwidth-alnum-key? key key-state)
          (ajax-ime-transpose-as-fullwidth-alnum-key? key key-state))))
    (ajax-ime-set-segment-transposing ac key key-state))

   ((ajax-ime-cancel-key? key key-state)
    (ajax-ime-cancel-conv ac))

   ((and ajax-ime-select-candidate-by-numeral-key?
         (ichar-numeric? key)
         (ajax-ime-context-candidate-window ac))
    (ajax-ime-move-candidate-in-page ac key))

   ((and (modifier-key-mask key-state)
         (not (shift-key-mask key-state)))
    #f)

   ((symbol? key)
    #f)

   (else
    (begin
      (ajax-ime-do-commit ac)
      (ajax-ime-proc-input-state ac key key-state)))))

(define (ajax-ime-press-key-handler ac key key-state)
  (if (ichar-control? key)
      (im-commit-raw ac)
      (if (ajax-ime-context-on ac)
          (if (ajax-ime-context-transposing ac)
              (ajax-ime-proc-transposing-state ac key key-state)
              (if (ajax-ime-context-state ac)
                  (ajax-ime-proc-compose-state ac key key-state)
                  (ajax-ime-proc-input-state ac key key-state)))
	  (ajax-ime-proc-raw-state ac key key-state)))
  (ajax-ime-update-preedit ac))

;;;
(define (ajax-ime-release-key-handler ac key key-state)
  (if (or (ichar-control? key)
	  (not (ajax-ime-context-on ac)))
      (ajax-ime-commit-raw ac)))
;;;
(define (ajax-ime-reset-handler ac)
  (if (ajax-ime-context-on ac)
      (begin
	(if (ajax-ime-context-state ac)
            (ajax-ime-lib-reset-conversion ac))
	(ajax-ime-flush ac))))

;;;
(define (ajax-ime-get-candidate-handler ac idx ascel-enum-hint)
  (let* ((cur-seg (ustr-cursor-pos (ajax-ime-context-segments ac)))
	 (cand (ajax-ime-lib-get-nth-candidate
		ac cur-seg idx)))
    (list cand (digit->string (+ idx 1)) "")))

(define (ajax-ime-set-candidate-index-handler ac idx)
  (ustr-cursor-set-frontside! (ajax-ime-context-segments ac) idx)
  (ajax-ime-update-preedit ac))

(define (ajax-ime-proc-raw-state ac key key-state)
  (if (not (ajax-ime-begin-input ac key key-state))
      (im-commit-raw ac)))

(ajax-ime-configure-widgets)
(register-im
 'ajax-ime
 "ja"
 "EUC-JP"
 ajax-ime-im-name-label
 ajax-ime-im-short-desc
 #f
 ajax-ime-init-handler
 ajax-ime-release-handler
 context-mode-handler
 ajax-ime-press-key-handler
 ajax-ime-release-key-handler
 ajax-ime-reset-handler
 ajax-ime-get-candidate-handler
 ajax-ime-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
