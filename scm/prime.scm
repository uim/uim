;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;;TODO
;;    ::単語登録
;;
;;モード一覧
;;  日本語入力モード(カタカナ日本語入力モードも必要？)
;;  英数モード
;;  全角英数モード
;;  
;;ステート一覧
;;  *日本語入力モード
;;    待機状態,入力状態,変換状態
;;  *単語登録モード
;;    読み入力状態,単語入力状態
;;

(require "japanese.scm")
(require "generic-key.scm")
(require "util.scm")

;;; user configs

;; configs
(define prime-nr-candidate-max 10)
(define prime-always-show-window? #t)
(define prime-auto-register-mode? #t)
(define prime-pseudo-mode-cursor? #f)
(define prime-char-annotation?    #t)

(define prime-mask-pending-preedit? #f)

(define prime-engine-command-lookup     "lookup_compact")
;(define prime-engine-command-lookup-all "lookup_compact")
(define prime-engine-command-lookup-all "lookup_compact_all")
;(define prime-engine-command-lookup     "lookup_prefix")
;(define prime-engine-command-lookup-all "lookup_prefix")

;; config function
(define prime-dont-use-numeral-key-to-select-cand
  (lambda ()
    (set! prime-cand-select-key?
	  (lambda (key key-state)
	    (and (numeral-char? key)
		 (control-key-mask key-state))))))

;; key
(define-key prime-latin-key?          '("<Control>l" generic-off-key?))
(define-key prime-wide-latin-key?     "<Control>L")
(define-key prime-begin-conv-key?     'generic-begin-conv-key?)
(define-key prime-on-key?         '("<Control>j" "<Control>J" generic-on-key?))
(define-key prime-commit-key?         'generic-commit-key?)
(define-key prime-next-candidate-key? 'generic-next-candidate-key?)
(define-key prime-prev-candidate-key? 'generic-prev-candidate-key?)
(define-key prime-next-page-key?      'generic-next-page-key?)
(define-key prime-prev-page-key?      'generic-prev-page-key?)
(define-key prime-cancel-key?         'generic-cancel-key?)
(define-key prime-backspace-key?      'generic-backspace-key?)
(define-key prime-delete-key?         'generic-delete-key?)
(define-key prime-go-left-key?        'generic-go-left-key?)
(define-key prime-go-right-key?       'generic-go-right-key?)
(define-key prime-go-left-edge-key?   '("<Control>a" "<Control>left"))
(define-key prime-go-right-edge-key?  '("<Control>e" "<Control>right"))
(define-key prime-register-key?       '("<Control>w"))

(define-key prime-space-key?          '(" "))
(define-key prime-altspace-key?       '("<Control> " "<Alt> "))


(define prime-cand-select-key?
  (lambda (key key-state)
    (numeral-char? key)))

(define prime-symbol-key?
  (lambda (key key-state)
    (symbol? key)))

(define prime-with-control-key?
  (lambda (key key-state)
    (control-key-mask key-state)))

(define prime-command-key?
  (lambda (key key-state)
    (and (modifier-key-mask key-state)
	 ;; Pressing a shift key only is not a command key.
	 (not (= (cdr (assoc 'Shift_key key-state-alist)) key-state)))))

(define prime-normal-key?
  (lambda (key key-state)
    (not (or (prime-command-key? key key-state)
	     (prime-symbol-key? key key-state)))))

(define prime-any-key?
  (lambda (key key-state)
    #t))

(define prime-capital-key?
  (lambda (key key-state)
    (and (shift-key-mask key-state)
	 (alphabet-char? key))))

(define prime-ja-direct-key?
  (lambda (key key-state)
    (ja-direct (charcode->string key))))

;;;; ------------------------------------------------------------
;;;; prime-keymap: Keymaps binding a key stroke to a command.
;;;; ------------------------------------------------------------

(define prime-keymap-get-command
  (lambda (keymap key key-state)
    (let ((command-key? (car (car keymap)))
	  (command      (cdr (car keymap))))
      (if ((symbol-value command-key?) key key-state)
	  command
	  (if (null? (cdr keymap))
	      #f
	      (prime-keymap-get-command (cdr keymap) key key-state))
      ))))
    
(define prime-keymap-register-fund-state
  '(
   (prime-prev-candidate-key?  . prime-command-register-fund-cancel)
   (prime-next-candidate-key?  . prime-command-register-fund-cancel)
   (prime-wide-latin-key?      . prime-command-wide-latin-mode)
   (prime-latin-key?           . prime-command-latin-mode)
;; FIXME: A register-fund-state also needs the following key bindings
;; FIMXE: as well as a fund-state.
;; FIXME: (2004-06-15) <komatsu@taiyaki.org>

   (prime-space-key?         . prime-command-pass)
   (prime-altspace-key?      . prime-command-pass)
;   (prime-space-key?         . prime-command-fund-space)
;   (prime-altspace-key?    . prime-command-fund-altspace)
;   (prime-ja-direct-key?   . prime-command-fund-commit-ja-direct)
   (prime-backspace-key?     . prime-command-register-fund-backspace)
   (prime-delete-key?        . prime-command-register-fund-delete)
   (prime-cancel-key?        . prime-command-register-fund-cancel)
   (prime-commit-key?        . prime-command-register-fund-commit)
   (prime-go-left-key?       . prime-command-register-fund-cursor-left)
   (prime-go-right-key?      . prime-command-register-fund-cursor-right)
   (prime-go-left-edge-key?  . prime-command-register-fund-cursor-left-edge)
   (prime-go-right-edge-key? . prime-command-register-fund-cursor-right-edge)
;   (prime-with-control-key? . prime-command-commit-raw)
   (prime-with-control-key? . prime-command-pass)
   (prime-symbol-key?       . prime-command-pass)
   (prime-any-key?          . prime-command-fund-input)
   ))

(define prime-keymap-latin-mode
  '(
   (prime-on-key?  . prime-command-japanese-mode)
   (prime-any-key? . prime-command-latin-input)
   ))

(define prime-keymap-wide-latin-mode
  '(
   (prime-on-key?     . prime-command-japanese-mode)
   (prime-normal-key? . prime-command-wide-latin-input)
   (prime-any-key?    . prime-command-commit-raw)
   ))

(define prime-keymap-fund-state
  '(
   (prime-wide-latin-key?   . prime-command-wide-latin-mode)
   (prime-latin-key?        . prime-command-latin-mode)
   (prime-space-key?        . prime-command-fund-space)
   (prime-altspace-key?     . prime-command-fund-altspace)
   (prime-with-control-key? . prime-command-commit-raw)
   (prime-ja-direct-key?    . prime-command-fund-commit-ja-direct)
   (prime-symbol-key?       . prime-command-commit-raw)
   (prime-any-key?          . prime-command-fund-input)
   ))

(define prime-keymap-preedit-state
  '(
   (prime-register-key?       . prime-command-register-mode)
   (prime-begin-conv-key?     . prime-command-preedit-convert)
   (prime-next-candidate-key? . prime-command-preedit-convert)
   (prime-prev-candidate-key? . prime-command-preedit-convert-reversely)
   (prime-delete-key?         . prime-command-preedit-delete)
   (prime-cancel-key?         . prime-command-preedit-cancel)
   (prime-backspace-key?      . prime-command-preedit-backspace)
   (prime-commit-key?         . prime-command-preedit-commit)
   (prime-go-left-edge-key?   . prime-command-preedit-cursor-left-edge)
   (prime-go-right-edge-key?  . prime-command-preedit-cursor-right-edge)
   (prime-go-left-key?        . prime-command-preedit-cursor-left)
   (prime-go-right-key?       . prime-command-preedit-cursor-right)
   (prime-cand-select-key?    . prime-command-preedit-commit-candidate)
   (prime-command-key?        . prime-command-pass)
   (prime-symbol-key?         . prime-command-pass)
   (prime-any-key?            . prime-command-preedit-input)
   ))

(define prime-keymap-register-preedit-state
  '(
   (prime-begin-conv-key?     . prime-command-preedit-convert)
   (prime-next-candidate-key? . prime-command-preedit-convert)
   (prime-prev-candidate-key? . prime-command-preedit-convert-reversely)
   (prime-delete-key?         . prime-command-preedit-delete)
   (prime-cancel-key?         . prime-command-preedit-cancel)
   (prime-backspace-key?      . prime-command-preedit-backspace)
   (prime-commit-key?         . prime-command-register-preedit-commit)
   (prime-go-left-edge-key?   . prime-command-preedit-cursor-left-edge)
   (prime-go-right-edge-key?  . prime-command-preedit-cursor-right-edge)
   (prime-go-left-key?        . prime-command-preedit-cursor-left)
   (prime-go-right-key?       . prime-command-preedit-cursor-right)
   (prime-cand-select-key?    . prime-command-register-preedit-commit-candidate)
   (prime-command-key?        . prime-command-pass)
   (prime-symbol-key?         . prime-command-pass)
   (prime-any-key?            . prime-command-preedit-input)
   ))

(define prime-keymap-register-conv-state
  '(
   (prime-next-candidate-key? . prime-command-register-conv-next)
   (prime-prev-candidate-key? . prime-command-conv-prev)
   (prime-cancel-key?         . prime-command-conv-cancel)
   (prime-backspace-key?      . prime-command-conv-cancel)
   (prime-commit-key?         . prime-command-register-conv-commit)
   (prime-cand-select-key?    . prime-command-register-conv-select)
   (prime-symbol-key?         . prime-command-pass)
   (prime-with-control-key?   . prime-command-pass)
   (prime-any-key?            . prime-command-register-conv-input)
   ))

(define prime-keymap-conv-state
  '(
   (prime-register-key?       . prime-command-register-mode)
   (prime-next-candidate-key? . prime-command-conv-next)
   (prime-prev-candidate-key? . prime-command-conv-prev)
   (prime-cancel-key?         . prime-command-conv-cancel)
   (prime-backspace-key?      . prime-command-conv-cancel)
   (prime-commit-key?         . prime-command-conv-commit)
   (prime-cand-select-key?    . prime-command-conv-select)
   (prime-symbol-key?         . prime-command-pass)
   (prime-with-control-key?   . prime-command-pass)
   (prime-any-key?            . prime-command-conv-input)
   ))

;;;; ------------------------------------------------------------

;; widgets and actions

;; widgets
(define prime-widgets '(widget_prime_input_mode))

;; default activity for each widgets
(define default-widget_prime_input_mode 'action_prime_mode_latin)

;; actions of widget_prime_input_mode
(define prime-input-mode-actions
  '(action_prime_mode_latin
    action_prime_mode_hiragana
    action_prime_mode_wide_latin))


;;;; ------------------------------------------------------------

;;; implementations

(define prime-mode-latin      0)
(define prime-mode-hiragana   1)
(define prime-mode-wide-latin 2)

(register-action 'action_prime_mode_latin
		 (lambda (pc)
		   '(figure_prime_mode_latin
		     "P"
		     "直接入力"
		     "PRIME オフ"))
		 (lambda (pc)
		   (= (prime-context-mode pc)
		      prime-mode-latin))
		 (lambda (pc)
		   (prime-mode-set pc prime-mode-latin)))

(register-action 'action_prime_mode_hiragana
		 (lambda (pc)
		   '(figure_prime_mode_hiragana
		     "ぷ"
		     "日本語"
		     "PRIME オン"))
		 (lambda (pc)
		   (= (prime-context-mode pc)
		      prime-mode-hiragana))
		 (lambda (pc)
		   (prime-mode-set pc prime-mode-hiragana)))

(register-action 'action_prime_mode_wide_latin
		 (lambda (pc)
		   '(figure_prime_mode_wide_latin
		     "Ｐ"
		     "全角英数"
		     "全角を入力"))
		 (lambda (pc)
		   (= (prime-context-mode pc)
		      prime-mode-wide-latin))
		 (lambda (pc)
		   (prime-mode-set pc prime-mode-wide-latin)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define prime-configure-widgets
  (lambda ()
    (register-widget 'widget_prime_input_mode
		     (activity-indicator-new prime-input-mode-actions)
		     (actions-new prime-input-mode-actions))))

(define prime-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'state         'prime-state-no-preedit)
    ;;(list 'unused-1st    #t)
    (list 'learning-word ())
    ;;(list 'unused-3rd    ())
    ;;(list 'unused-4th    ())
    ;;(list 'unused-5th    ())
    (list 'nth           0)
    ;;(list 'unused-7th    ())
    ;;(list 'unused-8th    ())
    (list 'candidates    ())
    ;;(list 'unused-10th   ())
    ;;(list 'unused-11th   ())
    (list 'mode          prime-mode-latin)
    (list 'last-word     "") ;;PRIMEやPOBoxの用語でいうContext
    ;;(list 'unused-14th   #t)
    ;;(list 'unused-15th   #f)
    (list 'preedit-line  '(() . ()))
    (list 'register-line '(() . ()))
    (list 'history       '(prime-state-no-preedit (() . ()) (() . ()) 0)))))
(define-record 'prime-context prime-context-rec-spec)
(define prime-context-new-internal prime-context-new)

(define prime-context-new
  (lambda (id im)
    (print "prime-context-new")
    (let ((pc (prime-context-new-internal id im)))
      (prime-context-set-widgets! pc prime-widgets)
      pc)))

(define prime-context-history-set!
  (lambda (sc)
    (prime-context-set-history! sc (list
				    (prime-context-state sc)
				    (prime-context-copy-preedit-line  sc)
				    (prime-context-copy-register-line sc)
				    (prime-context-nth sc)))))
(define prime-context-history-get prime-context-history)
(define prime-context-history-compare
  (lambda (sc)
    (print "prime-context-history-compare")
    (let ((prev-data (prime-context-history-get sc)))
      (cond
       ((not (equal? (prime-context-state sc) (nth 0 prev-data)))
	'state)
       ((not (equal? (prime-context-get-preedit-line sc)  (nth 1 prev-data)))
	'preedit)
       ((not (equal? (prime-context-get-register-line sc) (nth 2 prev-data)))
	'cursor)
       ((not (equal? (prime-context-nth sc) (nth 3 prev-data)))
	'nth)
       ))))

(define prime-context-copy-register-line
  (lambda (sc)
    (let ((line (prime-context-get-register-line sc)))
      (cons (copy-list (car line)) (copy-list (cdr line)))
      )))
(define prime-context-reset-register-line!
 (lambda (sc)
   (prime-editor-set-left  (prime-context-get-register-line sc) '())
   (prime-editor-set-right (prime-context-get-register-line sc) '())
   ))
(define prime-context-get-register-line prime-context-register-line)
;; prime-context-set-register-line! is implicitly defined by define-record

(define prime-context-reset-preedit-line!
 (lambda (sc)
   (prime-editor-set-left  (prime-context-get-preedit-line sc) '())
   (prime-editor-set-right (prime-context-get-preedit-line sc) '())
   ))

(define prime-context-copy-preedit-line
  (lambda (sc)
    (let ((preedit (prime-context-get-preedit-line sc)))
      (cons (copy-list (car preedit)) (copy-list (cdr preedit)))
      )))
(define prime-context-get-preedit-line prime-context-preedit-line)
;; prime-context-set-preedit-line! is implicitly defined by define-record


(define prime-send-command
  (lambda (command)
    (let ((result (prime-lib-send-command command)))
      (let loop ((res result))
	(if (string=? res "")
	    (loop (prime-lib-send-command ""))
	    res
	    )))))

(define prime-preedit-reset!
  (lambda (sc)
    (print "prime-preedit-reset!")

    (prime-context-set-state! sc 'prime-state-no-preedit)
    (prime-context-reset-preedit-line!  sc)
    (prime-context-set-nth! sc 0)
    ))

(define prime-get-nth-candidate
  (lambda (sc n)
    (if (> n (prime-get-nr-candidates sc))
	#f
	(car (cdr (car (nthcdr n (prime-context-candidates sc))))))))

(define prime-get-nr-candidates
  (lambda (sc)
    (length (prime-context-candidates sc))))

(define prime-get-current-candidate
  (lambda (sc)
    (prime-get-nth-candidate
     sc
     (prime-context-nth sc))))

(define prime-get-candidates! ;;もうちょっと関数名をどうにかしたい
  (lambda (sc preedit context)
    (prime-engine-set-context context)
    (prime-context-set-candidates!
     sc
     (prime-engine-lookup prime-engine-command-lookup preedit))
    ))

(define prime-get-all-candidates! ;;これももうちょっと関数名をどうにかしたい
  (lambda (sc preedit context)
    (prime-engine-set-context context)
    (prime-context-set-candidates!
     sc
     (prime-engine-lookup prime-engine-command-lookup-all preedit))
    ))


;;;; ------------------------------------------------------------
;;;; prime-util: General purpose functions
;;;; ------------------------------------------------------------

(define prime-util-string-concat 
  (lambda (string-list glue)
    (if (null? (cdr string-list))
	(car string-list)
	(string-append (car string-list)
		       glue
		       (prime-util-string-concat (cdr string-list) glue))
	)))

(define prime-util-assoc-list
  (lambda (lst)
    (mapcar 
     (lambda (str)
       (string-split str "="))
     lst)))

;;;; ------------------------------------------------------------
;;;; prime-uim:
;;;; ------------------------------------------------------------

(define prime-uim-candwin-get-range
  (lambda (sc)
    (let* ((beginning (* (/ (prime-context-nth sc) prime-nr-candidate-max)
			 prime-nr-candidate-max))
	   (end       (min (+ beginning prime-nr-candidate-max)
			   (prime-get-nr-candidates sc))))
      (cons beginning end))))

;;;; ------------------------------------------------------------
;;;; prime-engine: Functions to connect with a prime server.
;;;; ------------------------------------------------------------

(define prime-engine-send-command
  (lambda (arg-list)
    (cdr 
     (string-split
      (prime-send-command
       (string-append (prime-util-string-concat arg-list "\t") "\n"))
      "\n"))))

(define prime-engine-lookup
  (lambda (command string)
    (mapcar
     (lambda (string-line)
       (string-split string-line "\t"))
     (delq "" (prime-engine-send-command (list command string))))))

(define prime-engine-set-context
  (lambda (string)
    (if (string=? string "")
	(prime-engine-reset-context)
	(prime-engine-send-command (list "set_context" string)))))

(define prime-engine-reset-context
  (lambda ()
    (prime-engine-send-command (list "reset_context"))))

(define prime-engine-get-label
  (lambda (string)
    (if (string=? string "")
	""
	(car (prime-engine-send-command (list "get_label" string))))))

(define prime-engine-preedit-convert-input
  (lambda (string)
    (print "prime-engine-preedit-convert-input")
    (if (string=? string "")
	'("")
	(let ((conversion (car (prime-engine-send-command
				(list "preedit_convert_input" string)))))
	  (cond
	   ;; counversion could be (), in case a suikyo table is broken.
	   ((not conversion)
	    '(""))
	   ;; Check the charcode of the beginning char of conversion
	   ((= (string->charcode conversion) (string->charcode "\t"))
	    (cons "" (string-split conversion "\t")))
	   (else
	    (string-split conversion "\t")))))))

(define prime-engine-learn-word
  (lambda (pron literal pos context suffix rest)
    (prime-engine-send-command (list "learn_word"
				     pron literal pos context suffix rest))))

(define prime-engine-get-env
  (lambda (env-name)
;    (print "prime-engine-get-env")
    (let* ((result (string-split (car (prime-engine-send-command
				       (list "get_env" env-name)))
 				 "\t"))
	   (result-type (car result)))
      (cond
       ((string=? result-type "nil")
	'nil)
       ((string=? result-type "string")
	(nth 1 result))
       ((string=? result-type "array")
	(string-split (cdr result) "\t"))
       ((string=? result-type "boolean")
	(string=? (nth 1 result) "true"))
       (t
	'unknown))
      )))

(define prime-engine-get-env-typing-method
  (lambda ()
;    (print "prime-engine-get-env-typing-method")
    (prime-engine-get-env "typing_method")
    ))

;;;; ------------------------------------------------------------
;;;; prime-command: User commands for general purpose.
;;;; ------------------------------------------------------------
(define prime-command-pass
  (lambda (context key key-state)
    #t))

(define prime-command-commit-raw
  (lambda (context key key-state)
    (prime-commit-raw context)))

;;;; prime-command: modes

(define prime-command-japanese-mode
  (lambda (context key key-state)
    (prime-mode-set context prime-mode-hiragana)))

(define prime-command-wide-latin-mode
  (lambda (context key key-state)
    (prime-mode-set context prime-mode-wide-latin)))

(define prime-command-latin-mode
  (lambda (context key key-state)
    (prime-mode-set context prime-mode-latin)))


(define prime-command-register-mode
  (lambda (context key key-state)
    (let ((learning-word (prime-context-learning-word context)))
      (if (not learning-word)
	  (prime-word-learning-start! context))
      )))


;;;; ------------------------------------------------------------
;;;; prime-command-latin: User commands in a latin-mode
;;;; ------------------------------------------------------------
(define prime-command-latin-input
  (lambda (context key key-state)
    (prime-command-commit-raw context key key-state)))

;;;; ------------------------------------------------------------
;;;; prime-command-wide-latin: User commands in a wide-latin-mode
;;;; ------------------------------------------------------------
(define prime-command-wide-latin-input
  (lambda (context key key-state)
    (let ((wide-char (ja-wide (charcode->string key))))
      (if wide-char
	  (im-commit context wide-char)
	  (prime-command-commit-raw context key key-state)))))

;;;; ------------------------------------------------------------
;;;; prime-command-conv: User commands in a conversion state
;;;; ------------------------------------------------------------
(define prime-command-conv-next
  (lambda (context key key-state)
    (prime-convert-selection-move context (+ 1 (prime-context-nth context)))
    ))

(define prime-command-register-conv-next
  (lambda (context key key-state)
    (prime-context-set-nth! context (+ 1 (prime-context-nth context)))
    (cond
     ((prime-get-current-candidate context)
      #f)
     (else
      (prime-context-set-nth! context 0)))
    ))

(define prime-command-conv-prev
  (lambda (context key key-state)
    (if (> (prime-context-nth context) 0)
	(prime-context-set-nth! context (- (prime-context-nth       context) 1))
	(prime-context-set-nth! context (- (prime-get-nr-candidates context) 1)))
    ))

(define prime-command-conv-cancel
  (lambda (context key key-state)
    (prime-context-set-state! context 'prime-state-preedit)
    (prime-context-set-nth! context 0)
    ))

(define prime-command-conv-commit
  (lambda (context key key-state)
    (print "prime-command-conv-commit")
    (prime-commit-candidate context (prime-context-nth context))
    ))

(define prime-command-register-conv-commit
  (lambda (context key key-state)
    (print "prime-command-conv-commit")
    (prime-commit-to-register-buffer context (prime-get-current-candidate context))
    ))

(define prime-command-conv-select
  (lambda (context key key-state)
    (print "prime-command-conv-select")
    (let* ((nth0 (number->candidate-index (numeral-char->number key)))
	   (cand-range (prime-uim-candwin-get-range context))
	   (nth (min (+ (car cand-range) nth0) (cdr cand-range)))
	   (cand (prime-get-nth-candidate context nth)))
      (if cand
	  (prime-commit-candidate context nth))
      )))

(define prime-command-register-conv-select
  (lambda (context key key-state)
    (print "prime-command-conv-select")
    (let* ((nth0 (number->candidate-index (numeral-char->number key)))
	   (cand-range (prime-uim-candwin-get-range context))
	   (nth (min (+ (car cand-range) nth0) (cdr cand-range)))
	   (cand (prime-get-nth-candidate context nth)))
      (if cand
	  (begin
	    (prime-context-set-nth! context nth)
	    (prime-commit-to-register-buffer context cand)))
      )))

(define prime-command-conv-input
  (lambda (context key key-state)
    (print "prime-command-conv-input")
    (prime-commit-candidate context (prime-context-nth context))
    (prime-push-key context key key-state)
    ))

(define prime-command-register-conv-input
  (lambda (context key key-state)
    (print "prime-command-register-conv-input")
    (prime-commit-to-register-buffer context (prime-get-current-candidate context))
    (prime-push-key context key key-state)
    ))

;;;; ------------------------------------------------------------
;;;; prime-command-preedit: User commands in a preedit state.
;;;; ------------------------------------------------------------

(define prime-command-preedit-cancel
  (lambda (context key key-state)
    (prime-context-reset-preedit-line! context)
    ))

(define prime-command-preedit-backspace
  (lambda (context key key-state)
    (prime-editor-backspace-char (prime-context-get-preedit-line context))
    ))

(define prime-command-preedit-delete
  (lambda (context key key-state)
    (prime-editor-delete-char (prime-context-get-preedit-line context))
    ))

(define prime-command-preedit-commit
  (lambda (context key key-state)
    (let* ((word-committed (prime-preedit-get-string-label context))
	   (word-data (list (list "basekey" word-committed)
			    (list "base"    word-committed))))
      (prime-commit-word-data context word-data)
      )))

(define prime-command-register-preedit-commit
  (lambda (context key key-state)
    (let ((word-committed (prime-preedit-get-string-label context)))
      (prime-commit-to-register-buffer context word-committed)
      )))

(define prime-command-preedit-cursor-left-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-left-edge
     (prime-context-get-preedit-line context))))

(define prime-command-preedit-cursor-right-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-right-edge
     (prime-context-get-preedit-line context))))

(define prime-command-preedit-cursor-left
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-get-preedit-line context) -1)))

(define prime-command-preedit-cursor-right
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-get-preedit-line context) 1)))

(define prime-command-preedit-input
  (lambda (context key key-state)
    (print "prime-command-preedit-input")
    (let ((line (prime-context-get-preedit-line context)))
      (prime-editor-insert-char line (charcode->string key))
      )))

(define prime-command-preedit-commit-candidate
  (lambda (context key key-state)
    (print "prime-command-preedit-commit-candidate")
    (if #f
	(let* ((nth (number->candidate-index (numeral-char->number key)))
	       (cand (prime-get-nth-candidate context nth)))
	  (if cand
	      (prime-commit-candidate context nth))
	  )
	(if (prime-normal-key? key key-state)
	    (prime-command-preedit-input context key key-state))
	)))

(define prime-command-register-preedit-commit-candidate
  (lambda (context key key-state)
    (print "prime-command-register-preedit-commit-candidate")
    (if #f
	(let* ((nth (number->candidate-index (numeral-char->number key)))
	       (cand (prime-get-nth-candidate context nth)))
	  (if cand
	      (begin
		(prime-context-set-nth! context nth)
		(prime-commit-to-register-buffer context cand)))
	  )
	(if (prime-normal-key? key key-state)
	    (prime-command-preedit-input context key key-state))
	)))

(define prime-command-preedit-convert
  (lambda (context key key-state)
    (print "prime-command-preedit-convert")
    (prime-begin-conversion context)
    ))

(define prime-command-preedit-convert-reversely
  (lambda (context key key-state)
    (prime-begin-conversion-reversely context)
    ))

;;;; ------------------------------------------------------------
;;;; prime-command-fund: User commands in a fundamental state.
;;;; ------------------------------------------------------------
(define prime-command-fund-input
  (lambda (context key key-state)
    (print "prime-command-fund-input")
    (prime-context-set-state! context 'prime-state-preedit)
    (prime-command-preedit-input context key key-state)
    ))

(define prime-command-fund-space
  (lambda (context key key-state)
    (let ((space  (ja-direct " ")))
      (prime-commit-without-learning context space)
      )))

(define prime-command-fund-altspace
  (lambda (context key key-state)
    (let ((space  (if (string=? (ja-direct " ") " ") "　" " ")))
      (prime-commit-without-learning context space)
      )))

(define prime-command-fund-commit-ja-direct
  (lambda (context key key-state)
    (let ((direct (ja-direct (charcode->string key))))
      (prime-commit-without-learning context direct)
      )))

;;;; ------------------------------------------------------------
;;;; prime-command-register-fund: User commands in a register fundamental state
;;;; ------------------------------------------------------------
(define prime-command-register-fund-backspace
  (lambda (context key key-state)
    (prime-editor-backspace-char (prime-context-get-register-line context))
    ))

(define prime-command-register-fund-delete
  (lambda (context key key-state)
    (prime-editor-delete-char (prime-context-get-register-line context))
    ))

(define prime-command-register-fund-cancel
  (lambda (context key key-state)
    (prime-context-set-preedit-line!
     context (cons (prime-context-learning-word context) '()))
    (prime-context-set-learning-word! context '())
    (prime-context-reset-register-line! context)

    (prime-context-set-nth! context 0)
    (prime-context-set-state! context 'prime-state-preedit)
    ))

(define prime-command-register-fund-commit
  (lambda (context key key-state)
    (print "prime-command-register-fund-commit")
    (let* ((learning-word
	    (string-list-concat (prime-context-learning-word context)))
	   (registered (prime-register-get-string-label context)))
      (if (not (string=? registered ""))
	  (let ((word-data (list (list "basekey" learning-word)
				 (list "base"    registered))))
	    (prime-commit-word-data context word-data)
	    (prime-context-set-learning-word! context '()))))
    ))

(define prime-command-register-fund-cursor-left-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-left-edge
     (prime-context-get-register-line context))))

(define prime-command-register-fund-cursor-right-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-right-edge
     (prime-context-get-register-line context))))

(define prime-command-register-fund-cursor-left
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-get-register-line context) -1)))

(define prime-command-register-fund-cursor-right
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-get-register-line context) 1)))

;;;; ------------------------------------------------------------
;;;; prime-proc:
;;;; ------------------------------------------------------------

(define prime-proc-call-command
  (lambda (keymap context key key-state)
    (let ((command (prime-keymap-get-command keymap key key-state)))
      (if command
	  (begin ((symbol-value command) context key key-state) #t)
	  #f))))

(define prime-push-key
  (lambda (context key key-state)
    (print "prime-push-key")
    (let* ((state (prime-context-state context))
	   (mode  (prime-context-mode context))
	   (learning-word (prime-context-learning-word context))
	   (keymap))
      (cond
       ((= state 'prime-state-converting)
	(print ":prime-push-key: converting")
	(if learning-word
	    (set! keymap prime-keymap-register-conv-state)
	    (set! keymap prime-keymap-conv-state)))

       ((= mode prime-mode-latin)
	(set! keymap prime-keymap-latin-mode))
       ((= mode prime-mode-wide-latin)
	(set! keymap prime-keymap-wide-latin-mode))
       ((= mode prime-mode-hiragana)
	(if (prime-preedit-exist? context)
	    (if learning-word
		(set! keymap prime-keymap-register-preedit-state)
		(set! keymap prime-keymap-preedit-state))
	    (if learning-word
		(set! keymap prime-keymap-register-fund-state)
		(set! keymap prime-keymap-fund-state))))
       )
       (prime-proc-call-command keymap context key key-state)

       (prime-update context)
       )))

;;;; ------------------------------------------------------------
;;;; prime-preedit:
;;;; ------------------------------------------------------------

(define prime-editor-get-left  (lambda (line) (car line)))
(define prime-editor-set-left  (lambda (line new-line-left)
				 (set-car! line new-line-left)))

(define prime-editor-get-right (lambda (line) (cdr line)))
(define prime-editor-set-right (lambda (line new-line-right)
				 (set-cdr! line new-line-right)))

(define prime-editor-get-line
  (lambda (line)
    (append
     (reverse (prime-editor-get-right line))
     (prime-editor-get-left line))))

(define prime-editor-cursor-move-right-edge
  (lambda (line)
    (let ((new-line-left (prime-editor-get-line line)))
      (prime-editor-set-right line '())
      (prime-editor-set-left  line new-line-left))))

(define prime-editor-cursor-move-left-edge
  (lambda (line)
    (let ((new-line-right (reverse (prime-editor-get-line line))))
      (prime-editor-set-right line new-line-right)
      (prime-editor-set-left  line '()))))

(define prime-editor-cursor-move
  (lambda (line motion-arg)
    (cond
     ;; right motion
     ((and (> motion-arg 0)
	   (not (null? (cdr line))))
      (let ((line-left  (cons (car (prime-editor-get-right line))
			      (prime-editor-get-left line)))
	    (line-right (cdr (prime-editor-get-right line))))
	(prime-editor-set-left  line line-left)
	(prime-editor-set-right line line-right))
      (prime-editor-cursor-move line (- motion-arg 1)))
     ;; left motion
     ((and (< motion-arg 0)
	   (not (null? (car line))))
      (let ((line-left  (cdr (prime-editor-get-left line)))
	    (line-right (cons (car (prime-editor-get-left line))
			      (prime-editor-get-right line))))
	(prime-editor-set-left  line line-left)
	(prime-editor-set-right line line-right))
      (prime-editor-cursor-move line (+ motion-arg 1)))
     (else line))))

(define prime-editor-insert-char
  (lambda (line char)
    (prime-editor-set-left  line (cons char (prime-editor-get-left line)))))

(define prime-editor-backspace-char
  (lambda (line)
    (prime-editor-set-left  line (cdr (prime-editor-get-left line)))))

(define prime-editor-delete-char
  (lambda (line)
    (prime-editor-set-right line (cdr (prime-editor-get-right line)))))


(define prime-preedit-exist?
  (lambda (sc)
    (let ((line (prime-context-get-preedit-line sc)))
      (or
       (> (length (prime-editor-get-left  line)) 0)
       (> (length (prime-editor-get-right line)) 0)
       ))))

(define prime-preedit-get-string-label
  (lambda (sc)
    (let ((line (prime-context-get-preedit-line sc)))
      (string-append
       (prime-engine-get-label
	(string-list-concat (prime-editor-get-left line)))
       (string-list-concat (reverse (prime-editor-get-right line)))))))

(define prime-preedit-get-string-raw
  (lambda (sc)
    (let ((line (prime-context-get-preedit-line sc)))
      (string-append
       (string-list-concat (prime-editor-get-left line))
       (string-list-concat (reverse (prime-editor-get-right line)))))))

(define prime-preedit-mask
  (lambda (preedition)
    (print "prime-preedit-mask")
    (let ((conv-list (prime-engine-preedit-convert-input preedition)))
      (if (car (cdr conv-list))
	  (string-append (car conv-list) "*")
	  (car conv-list)))))

(define prime-register-get-string-label
  (lambda (sc)
    (let ((line (prime-context-get-register-line sc)))
      (string-append
       (string-list-concat (prime-editor-get-left line))
       (string-list-concat (reverse (prime-editor-get-right line)))))))

;;;; ------------------------------------------------------------
;;;; prime-custom
;;;; ------------------------------------------------------------

(define prime-custom-init
  (lambda ()
;    (print "prime-custom-init")
    (let ((typing-method (prime-engine-get-env-typing-method)))
      (cond
       ((string=? typing-method "kana")
	(prime-dont-use-numeral-key-to-select-cand))
       ((string=? typing-method "tcode")
	(prime-dont-use-numeral-key-to-select-cand)
	(set! prime-mask-pending-preedit? #t)
	(set! prime-engine-command-lookup     "lookup")
	(set! prime-engine-command-lookup-all "lookup_all")
	)
       ))
    ))

;;;; ------------------------------------------------------------
;;;; prime-commit
;;;; ------------------------------------------------------------

(define prime-commit-raw
  (lambda (sc)
    (print "prime-commit-raw")
    (im-commit-raw sc)
    (prime-context-set-last-word! sc "")
    (prime-preedit-reset! sc)
    ))

(define prime-commit-without-learning
  (lambda (sc word)
    (im-commit sc word)
    (prime-context-set-last-word! sc "")
    ))

(define prime-commit-word-data
  (lambda (sc word-data)
    (im-commit sc (string-append (or (cadr (assoc "base" word-data)) "")
				 (or (cadr (assoc "conjugation" word-data)) "")
				 (or (cadr (assoc "suffix" word-data)) "")))
    (prime-learn-word sc word-data)
    (prime-preedit-reset! sc)))

(define prime-commit-candidate
  (lambda (sc nth)
    (let ((word-data
	   (prime-util-assoc-list
	    (cdr (cdar (nthcdr nth (prime-context-candidates sc)))))))
      (prime-commit-word-data sc word-data))))

(define prime-commit-to-register-buffer
  (lambda (sc word)
    (let ((line (prime-context-get-register-line sc)))
      (prime-editor-set-left line (append (string-to-list word)
					  (prime-editor-get-left line)))
      (prime-preedit-reset! sc)
      )))

;;;; ------------------------------------------------------------

(define prime-learn-word
  (lambda (sc assoc-list)
    (let ((key     (or (cadr (assoc "basekey"     assoc-list)) ""))
	  (value   (or (cadr (assoc "base"        assoc-list)) ""))
	  (part    (or (cadr (assoc "part"        assoc-list)) ""))
	  (context (or (prime-context-last-word sc) ""))
	  (suffix  (or (cadr (assoc "conjugation" assoc-list)) ""))
	  (rest    (or (cadr (assoc "suffix"      assoc-list)) "")))
      
      (prime-engine-learn-word key value part context suffix rest)
      (prime-context-set-last-word! sc (string-append value suffix rest)))))

(define prime-convert-selection-move
  (lambda (sc selection-index)
    (prime-context-set-nth! sc selection-index)
    (if (prime-get-current-candidate sc)
	#f
	(if prime-auto-register-mode?
	    (prime-word-learning-start! sc)
	    (prime-context-set-nth! sc 0)))
    ))

(define prime-begin-conversion-internal
  (lambda (sc init-idx)
    (print "prime-begin-conversion-internal")
    (print (prime-preedit-get-string-raw sc))
    (let ((res))
      (prime-get-all-candidates! sc
				 (prime-preedit-get-string-raw sc)
				 (prime-context-last-word sc))
      (set! res (prime-get-nth-candidate sc init-idx))
      (if res
	  (begin
	    (prime-context-set-nth!   sc init-idx)
	    (prime-context-set-state! sc 'prime-state-converting))
	  )
      (prime-convert-selection-move sc init-idx)
      )))

(define prime-begin-conversion-reversely
  (lambda (sc)
    (let ((last-idx (- (prime-get-nr-candidates sc)
		       1)))
      (prime-begin-conversion-internal sc last-idx))))

(define prime-begin-conversion
  (lambda (sc)
    (prime-begin-conversion-internal sc 0)))


;;;; ------------------------------------------------------------
;;;; prime-commit
;;;; ------------------------------------------------------------

(define prime-update
  (lambda (sc)
    (print "prime-update")

    (prime-update-state sc)
    (prime-update-prediction sc)
    
    (prime-update-candidate-window sc)
    (prime-update-preedit sc)

    (prime-update-history sc)
    ))

(define prime-update-state
  (lambda (sc)
    (if (not (prime-preedit-exist? sc))
	(begin
	  (print "  prime-update-state: set-state no-preedit")
	  (prime-context-set-state! sc 'prime-state-no-preedit)))
    ))

(define prime-update-history
  (lambda (sc)
    (print "prime-update-history")
    (prime-context-history-set! sc)))

(define prime-update-preedit
  (lambda (sc)
    (print "prime-update-preedit")

    (if (prime-context-history-compare sc)
	(let ((learning-word (prime-context-learning-word sc)))
	  (im-clear-preedit sc)
	  (prime-display-preedit
	   sc
	   (if learning-word
	       (prime-register-state-update-preedit sc)
	       (prime-preedit-state-update-preedit  sc)))
	  (im-update-preedit sc)
	  ))
    ))

(define prime-register-state-update-preedit
  (lambda (sc)
    (let* ((learning-word  (prime-context-learning-word     sc))
	   (line           (prime-context-get-register-line sc))
	   (register-left  (prime-editor-get-left           line))
	   (register-right (reverse (prime-editor-get-right line))))
      (append
       (list
	(cons 'register-label  "単語登録")
	(cons 'register-border "[")
	(cons 'register-word   (prime-engine-get-label
				(string-list-concat learning-word)))
	(cons 'register-border "|")
	(cons 'committed (string-list-concat register-left)))

       (prime-preedit-state-update-preedit sc)
       (list
	(cons 'committed (string-list-concat register-right))
	(cons 'register-border "]"))))))

(define prime-preedit-state-update-preedit
  (lambda (sc)
    (print "prime-preedit-state-update-preedit")
    (let* ((state (prime-context-state            sc))
	   (line  (prime-context-get-preedit-line sc))
	   (left  (string-list-concat (prime-editor-get-left line)))
	   (right (string-list-concat (reverse (prime-editor-get-right line))))
	   )
      (cond
       ((= state 'prime-state-converting)
	(list (cons 'converting (prime-get-current-candidate sc))))

       ((prime-preedit-exist? sc)
	(if prime-mask-pending-preedit?
	    (list (cons 'preedit (prime-preedit-mask left))
		  (cons 'cursor "")
		  (cons 'preedit right))
	    (list (cons 'preedit (prime-engine-get-label left))
		  (cons 'cursor "")
		  (cons 'preedit right))))
       (else
	(list (cons 'cursor "")))))
    ))

(define prime-display-preedit-format
  (list (cons 'committed        preedit-none)
	(cons 'cursor           preedit-cursor)
	(cons 'pseudo-cursor    preedit-reverse)
	(cons 'preedit          preedit-underline)
	(cons 'converting       preedit-reverse)
	(cons 'register-border  preedit-reverse)
	(cons 'register-label   preedit-reverse)
	(cons 'register-word    preedit-reverse)
	))


(define prime-display-preedit
  (lambda (sc preedit-list)
    (if preedit-list
	(let ((type   (car (car preedit-list)))
	      (string (cdr (car preedit-list))))
	  (cond
	   ((eq? type 'cursor)
	    (prime-display-preedit-cursor sc))
	   ((not (string=? string ""))
	    (im-pushback-preedit
	     sc (cdr (assoc type prime-display-preedit-format)) string))
	   )
	  (prime-display-preedit sc (cdr preedit-list))))))

(define prime-display-preedit-cursor
  (lambda (sc)
    (im-pushback-preedit
     sc (cdr (assoc 'cursor prime-display-preedit-format)) "")
    (if (and prime-pseudo-mode-cursor?
	     (= (prime-context-mode sc) prime-mode-hiragana)
	     (eq? (prime-context-state sc) 'prime-state-no-preedit))
	(im-pushback-preedit
	 sc (cdr (assoc 'pseudo-cursor prime-display-preedit-format)) " "))
    ))

(define prime-update-prediction
  (lambda (sc)
    (print "prime-update-prediction")
    (let ((diff (prime-context-history-compare sc)))
      (cond
       ((= diff 'state)
	(let ((state     (prime-context-state sc))
	      (last-word (prime-context-last-word sc)))
	  (cond
	   ((= state 'prime-state-preedit)
	    (prime-get-candidates! sc
				   (prime-preedit-get-string-raw sc)
				   (prime-context-last-word sc)))
	   ((= state 'prime-state-converting)
	    (prime-get-all-candidates! sc
				       (prime-preedit-get-string-raw sc)
				       (prime-context-last-word sc)))
	   ((= state 'prime-state-no-preedit)
	    (prime-context-set-candidates! sc '()))
	    )))
       ((= diff 'preedit)
	(prime-get-candidates! sc
			       (prime-preedit-get-string-raw sc)
			       (prime-context-last-word sc)))
       ))))

(define prime-update-candidate-window
  (lambda (sc)
    (print "prime-update-candidate-window")
    (let ((diff (prime-context-history-compare sc)))
      (cond
       ((= diff 'state)
	(let ((state (prime-context-state sc)))
	  (cond
	   ((= state 'prime-state-no-preedit)
	    (im-deactivate-candidate-selector sc))
	   ((= state 'prime-state-preedit)
	    (if (> (prime-get-nr-candidates sc) 0)
		(im-activate-candidate-selector
		 sc (prime-get-nr-candidates sc) prime-nr-candidate-max)))
	   ((= state 'prime-state-converting)
 	    (im-activate-candidate-selector
 	     sc (prime-get-nr-candidates sc) prime-nr-candidate-max)
	    (im-select-candidate sc (prime-context-nth sc)))
	    )))
       ((= diff 'nth)
	(im-select-candidate sc (prime-context-nth sc)))
       ((= diff 'preedit)
	(if (> (prime-get-nr-candidates sc) 0)
	    (im-activate-candidate-selector
	     sc (prime-get-nr-candidates sc) prime-nr-candidate-max)
	    (im-deactivate-candidate-selector sc)))
       ))))

;;;; ------------------------------------------------------------

(define prime-word-learning-start!
  (lambda (sc)
    (prime-context-set-learning-word!
     sc (prime-editor-get-line (prime-context-get-preedit-line sc)))
    (prime-preedit-reset! sc)
    ))

(define prime-init-handler
  (lambda (id im arg)
    (print "prime-init-handler")
    ;; pc stands for 'prime context'. all other 'sc' in this file
    ;; should be renamed to 'pc' or 'context'
    (let ((pc (prime-context-new id im)))
      (set! candidate-window-position "left")
      (prime-custom-init)
      pc)))

(define prime-press-key-handler
  (lambda (sc key state)
    (if (control-char? key)
	(im-commit-raw sc)
	(prime-push-key sc key state))))

(define prime-release-key-handler
  (lambda (sc key state)
    (if (or (control-char? key)
	    (= (prime-context-mode sc)
	       prime-mode-latin))
	(im-commit-raw sc))))

(define prime-reset-handler
  (lambda (sc)
    (print "prime-reset-handler")
    ))

(define prime-mode-set
  (lambda (context mode)
    (prime-context-set-mode! context mode)
    (prime-preedit-reset! context)
    (prime-update context)
    ))

(define prime-get-candidate-handler
  (lambda (sc idx accel-enum-hint)
    (let* ((cand (prime-get-nth-candidate sc idx))
	   (annotation (assoc cand prime-char-annotation-alist)))
      (if (and prime-char-annotation? annotation)
	  (list (string-append cand "  (" (cdr annotation) ")")
		(digit->string (+ idx 1)) "")
	  (list cand
		(digit->string (+ idx 1)) "")))))

(define prime-set-candidate-index-handler
  (lambda (sc selection-index)
    (print "prime-set-candidate-index-handler")
    (prime-convert-selection-move sc selection-index)
    (prime-update sc)
    ))

;; unused
(define prime-mode
  (list
   (list prime-mode-latin      "P"  "直接入力" "PRIME オフ" "prop_prime_mode_latin")
   (list prime-mode-hiragana   "ぷ" "日本語"   "PRIME オン" "prop_prime_mode_hiragana")
   (list prime-mode-wide-latin "Ｐ" "全角英数" "全角を入力" "prop_prime_mode_wide_latin")))

(define prime-char-annotation-alist
  '(("-"  . "半角「マイナス」")
    ("−" . "全角「マイナス」")
    ("ー" . "長音")
    ("─" . "罫線")
    ("一" . "漢数「いち」")
    ("―" . "記号「ダッシュ」")
    ("‐" . "記号「ハイフン」")
    ("x"  . "半角「エックス」")
    ("X"  . "半角「エックス」")
    ("ｘ" . "全角「エックス」")
    ("Ｘ" . "全角「エックス」")
    ("×" . "記号「かける」")
    ("o"  . "半角「オー」")
    ("O"  . "半角「オー」")
    ("0"  . "半角「ゼロ」")
    ("ｏ" . "全角「オー」")
    ("Ｏ" . "全角「オー」")
    ("０" . "全角「ゼロ」")
    ("〇" . "漢数「ゼロ」")
    ("○" . "記号「まる」")
    ("◯" . "記号「まる(2)」")
    ("ニ" . "全角カタカナ")
    ("二" . "漢数「に」")
    ("|"  . "半角")
    ("｜" . "全角")
    ))

(prime-configure-widgets)

(register-im
 'prime
 "ja"
 "EUC-JP"
 (N_ "Japanese predictable input method")
 #f
 prime-init-handler
 #f
 context-mode-handler
 prime-press-key-handler
 prime-release-key-handler
 prime-reset-handler
 prime-get-candidate-handler
 prime-set-candidate-index-handler
 context-prop-activate-handler)
