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

(require "util.scm")
(require "japanese.scm")
(require "i18n.scm")
(require "process.scm")
(require "socket.scm")
(require-custom "generic-key-custom.scm")
(require-custom "prime-custom.scm")
(require-custom "prime-key-custom.scm")
(require-extension (srfi 1 2 48))

;; config function
;; should be replaced with boolean custom variable  -- YamaKen 2005-01-15
(define prime-dont-use-numeral-key-to-select-cand
  (lambda ()
    (set! prime-cand-select-key?
	  (lambda (key key-state)
	    (and (ichar-numeric? key)
		 (control-key-mask key-state))))))

;; keys without custom
(define-key prime-escape-key?         '("escape" "<Control>["))
(define-key prime-space-key?          '(" "))

(define prime-app-mode-end-stroke-list '())
;;;; If you're a Vi user, modify the lines below.
(define prime-configure-app-mode-vi
  (lambda ()
    (if prime-custom-app-mode-vi?
	(begin
	  ;; For Vi users
	  (define-key prime-app-mode-start-key? prime-escape-key?)
	  (set! prime-app-mode-end-stroke-list
		'("i" "I" "a" "A" "o" "O" "C" "s" "S" ("c" . ("l" "w" "e" "c" "G")))))
	(begin
	  ;; Default
	  (define-key prime-app-mode-start-key?  #f)
	  (set! prime-app-mode-end-stroke-list '())))))

(prime-configure-app-mode-vi)

(define prime-cand-select-key?
  (lambda (key key-state)
    (ichar-numeric? key)))

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
	 (ichar-alphabetic? key))))

(define prime-ja-direct-key?
  (lambda (key key-state)
    (ja-direct (charcode->string key))))

;;;; ------------------------------------------------------------
;;;; prime-keymap: Keymaps binding a key stroke to a command.
;;;; ------------------------------------------------------------

(define prime-keymap-get-keymap
  (lambda (context key key-state)
    (let ((mode (prime-context-mode context))
	  (keymap #f))
      (cond 
       ((= mode prime-mode-latin)
	(set! keymap prime-keymap-latin-mode))
       ((= mode prime-mode-hiragana)
	(set! keymap (prime-keymap-get-keymap-hiragana context key key-state)))
       ((= mode prime-mode-wide-latin)
	(set! keymap prime-keymap-wide-latin-mode))
       ((= mode prime-mode-application)
	(set! keymap prime-keymap-app-mode)))
      keymap)))

(define prime-keymap-get-keymap-hiragana
  (lambda (context key key-state)
    (let ((state    (prime-context-state context))
	  (language (prime-context-language context))
	  (keymap #f))
      (cond
       ((eq? state 'prime-state-segment)
	(set! keymap prime-keymap-segment-state))

       ((eq? state 'prime-state-modifying)
	(set! keymap prime-keymap-modify-state))

       ((eq? state 'prime-state-converting)
	(if (eq? language 'Japanese)
	    (set! keymap prime-keymap-conv-state)
	    (set! keymap prime-keymap-english-conv-state)))

       ((eq? state 'prime-state-preedit)
	(if (eq? language 'Japanese)
	    (set! keymap prime-keymap-preedit-state)
	    (set! keymap prime-keymap-english-preedit-state)))

       ((eq? state 'prime-state-fund)
	(if (eq? language 'Japanese)
	    (if (prime-context-parent-context context)
		(set! keymap prime-keymap-child-fund-state)
		(set! keymap prime-keymap-fund-state))
	    (set! keymap prime-keymap-english-fund-state))))
      keymap)))

(define prime-keymap-get-command
  (lambda (keymap key key-state)
    (let* ((command-key? (car (car keymap)))
	   (command      (cdr (car keymap)))
	   (key-pred (symbol-value command-key?)))
      (if (and
	   key-pred
	   (key-pred key key-state))
	  command
	  (if (null? (cdr keymap))
	      #f
	      (prime-keymap-get-command (cdr keymap) key key-state))
      ))))
    
(define prime-keymap-latin-mode
  '(
   (prime-on-key?             . prime-command-japanese-mode)
   (prime-app-mode-start-key? . prime-command-app-mode-start)
   (prime-any-key?            . prime-command-commit-raw)
   ))

(define prime-subkeymap-child-context
  '(
   (prime-prev-candidate-key? . prime-command-fund-cancel)
   (prime-next-candidate-key? . prime-command-fund-cancel)
   (prime-backspace-key?      . prime-command-fund-backspace)
   (prime-delete-key?         . prime-command-fund-delete)
   (prime-cancel-key?         . prime-command-fund-cancel)
   (prime-commit-key?         . prime-command-child-finish)
   (prime-go-left-key?        . prime-command-fund-cursor-left)
   (prime-go-right-key?       . prime-command-fund-cursor-right)
   (prime-go-left-edge-key?   . prime-command-fund-cursor-left-edge)
   (prime-go-right-edge-key?  . prime-command-fund-cursor-right-edge)

   (prime-space-key?          . prime-command-fund-space)
   (prime-altspace-key?       . prime-command-fund-altspace)
   (prime-with-control-key?   . prime-command-pass)
   (prime-ja-direct-key?      . prime-command-fund-commit-ja-direct)
   (prime-symbol-key?         . prime-command-pass)
   (prime-any-key?            . prime-command-commit)
   ))

(define prime-keymap-wide-latin-mode
  '(
   (prime-on-key?             . prime-command-japanese-mode)
   (prime-app-mode-start-key? . prime-command-app-mode-start)
   (prime-normal-key?         . prime-command-wide-latin-input)
   (prime-any-key?            . prime-command-commit-raw)
   ))

(define prime-keymap-app-mode
  '((prime-any-key? . prime-command-app-mode)))

(define prime-keymap-fund-state
  '(
   (prime-wide-latin-key?     . prime-command-wide-latin-mode)
   (prime-latin-key?          . prime-command-latin-mode)
   (prime-app-mode-start-key? . prime-command-app-mode-start)
   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)
   (prime-language-toggle-key?       . prime-command-language-toggle)

   (prime-space-key?        . prime-command-fund-space)
   (prime-altspace-key?     . prime-command-fund-altspace)
   (prime-with-control-key? . prime-command-commit-raw)
   (prime-ja-direct-key?    . prime-command-fund-commit-ja-direct)
   (prime-symbol-key?       . prime-command-commit-raw)
   (prime-any-key?          . prime-command-fund-input)
   ))

(define prime-keymap-child-fund-state
  '(
   (prime-wide-latin-key?     . prime-command-wide-latin-mode)
   (prime-latin-key?          . prime-command-latin-mode)
;   (prime-app-mode-start-key? . prime-command-app-mode-start)
   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)

   (prime-prev-candidate-key? . prime-command-fund-cancel)
   (prime-next-candidate-key? . prime-command-fund-cancel)
   (prime-backspace-key?      . prime-command-fund-backspace)
   (prime-delete-key?         . prime-command-fund-delete)
   (prime-cancel-key?         . prime-command-fund-cancel)
   (prime-commit-key?         . prime-command-child-finish)
   (prime-go-left-key?        . prime-command-fund-cursor-left)
   (prime-go-right-key?       . prime-command-fund-cursor-right)
   (prime-go-left-edge-key?   . prime-command-fund-cursor-left-edge)
   (prime-go-right-edge-key?  . prime-command-fund-cursor-right-edge)

   (prime-space-key?          . prime-command-fund-space)
   (prime-altspace-key?       . prime-command-fund-altspace)
   (prime-with-control-key?   . prime-command-pass)
   (prime-ja-direct-key?      . prime-command-fund-commit-ja-direct)
   (prime-symbol-key?         . prime-command-pass)
   (prime-any-key?            . prime-command-fund-input)
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
   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)

   (prime-command-key?        . prime-command-pass)
   (prime-symbol-key?         . prime-command-pass)
   (prime-any-key?            . prime-command-preedit-input)
   ))

(define prime-keymap-conv-state
  '(
    (prime-register-key?       . prime-command-register-mode)
    (prime-next-candidate-key? . prime-command-conv-next)
    (prime-prev-candidate-key? . prime-command-conv-prev)
    (prime-cancel-key?         . prime-command-conv-cancel)
    (prime-backspace-key?      . prime-command-conv-cancel)
    (prime-commit-key?         . prime-command-conv-commit)
    (prime-go-left-edge-key?   . prime-command-modify-cursor-left-edge)
    (prime-go-right-edge-key?  . prime-command-modify-cursor-right-edge)
    (prime-go-left-key?        . prime-command-modify-cursor-left)
    (prime-go-right-key?       . prime-command-modify-cursor-right)
    (prime-expand-segment-key? . prime-command-modify-cursor-right)
    (prime-shrink-segment-key? . prime-command-modify-cursor-left)
    (prime-cand-select-key?    . prime-command-conv-select)
    ;; Typing mode key bindings
    (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
    (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
    (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
    (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
    (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)
    (prime-symbol-key?         . prime-command-pass)
    (prime-with-control-key?   . prime-command-pass)
    (prime-any-key?            . prime-command-conv-input)
    ))

(define prime-keymap-modify-state
  '(
;    (prime-register-key?       . prime-command-register-mode)
   (prime-begin-conv-key?      . prime-command-modify-convert)
   (prime-next-candidate-key?  . prime-command-modify-convert)
   (prime-prev-candidate-key?  . prime-command-modify-convert-reversely)
    (prime-cancel-key?         . prime-command-conv-cancel)
;    (prime-backspace-key?      . prime-command-conv-cancel)
    (prime-commit-key?         . prime-command-modify-commit)
    (prime-go-left-edge-key?   . prime-command-modify-cursor-left-edge)
    (prime-go-right-edge-key?  . prime-command-modify-cursor-right-edge)
    (prime-go-left-key?        . prime-command-modify-cursor-left)
    (prime-go-right-key?       . prime-command-modify-cursor-right)
    (prime-expand-segment-key? . prime-command-modify-cursor-expand)
    (prime-shrink-segment-key? . prime-command-modify-cursor-shrink)
;    ;; Typing mode key bindings
;    (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
;    (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
;    (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
;    (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
;    (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)
    (prime-symbol-key?         . prime-command-pass)
    (prime-with-control-key?   . prime-command-pass)
    (prime-any-key?            . prime-command-conv-input)
   ))

(define prime-keymap-segment-state
  '(
    (prime-cancel-key?         . prime-command-segment-cancel)
    (prime-commit-key?         . prime-command-modify-commit)
    (prime-next-candidate-key? . prime-command-segment-next)
    (prime-prev-candidate-key? . prime-command-segment-prev)
    (prime-cand-select-key?    . prime-command-segment-select)
    (prime-go-left-edge-key?   . prime-command-modify-cursor-left-edge)
    (prime-go-right-edge-key?  . prime-command-modify-cursor-right-edge)
    (prime-go-left-key?        . prime-command-modify-cursor-left)
    (prime-go-right-key?       . prime-command-modify-cursor-right)
    (prime-expand-segment-key? . prime-command-modify-cursor-expand)
    (prime-shrink-segment-key? . prime-command-modify-cursor-shrink)
    (prime-symbol-key?         . prime-command-pass)
    (prime-with-control-key?   . prime-command-pass)
    (prime-any-key?            . prime-command-conv-input)
;    (prime-any-key?            . prime-command-pass)
    ))

;; Keymaps for English

(define prime-keymap-english-fund-state
  '(
    (prime-space-key? . prime-command-commit-raw)
    (prime-english-direct-key?  . prime-command-commit-raw)
    (prime-wide-latin-key?      . prime-command-wide-latin-mode)
    (prime-latin-key?           . prime-command-latin-mode)
    (prime-app-mode-start-key?  . prime-command-app-mode-start)
    (prime-language-toggle-key? . prime-command-language-toggle)

    (prime-with-control-key? . prime-command-commit-raw)
    (prime-symbol-key?       . prime-command-commit-raw)
    (prime-any-key?          . prime-command-fund-input)
    ))

(define prime-keymap-english-preedit-state
  '(
    (prime-space-key?          . prime-command-preedit-commit-and-space)
    (prime-english-direct-key? . prime-command-preedit-commit-and-commit-raw)
    (prime-begin-conv-key?     . prime-command-preedit-convert)
    (prime-english-next-candidate-key? . prime-command-preedit-convert)
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

(define prime-keymap-english-conv-state
  '(
    (prime-space-key?          . prime-command-conv-commit-and-space)
    (prime-english-direct-key? . prime-command-conv-commit-and-commit-raw)
    (prime-english-next-candidate-key? . prime-command-conv-next)
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

;;; Implementations

(define prime-mode-latin       0)
(define prime-mode-hiragana    1)
(define prime-mode-wide-latin  2)
(define prime-mode-application 3)

(register-action 'action_prime_mode_latin
		 (lambda (context)
		   '(ja_direct
		     "--"
		     "通常入力"
		     "PRIMEをオフ"))
		 (lambda (context)
		   (= (prime-context-mode context)
		      prime-mode-latin))
		 (lambda (context)
		   (prime-mode-set context prime-mode-latin)))

(register-action 'action_prime_mode_hiragana
		 (lambda (context)
		   '(ja_hiragana
		     "あ"
		     "日本語"
		     "PRIMEをオン"))
		 (lambda (context)
		   (and
		    (eq? (prime-context-language context) 'Japanese)
		    (= (prime-context-mode context) prime-mode-hiragana)))
		 (lambda (context)
		   (prime-mode-language-set context 'Japanese)
		   (prime-mode-set context prime-mode-hiragana)))

(register-action 'action_prime_mode_english
		 (lambda (context)
		   '(ja_halfwidth_alnum
		     "A"
		     "英語"
		     "PRIMEをオン"))
		 (lambda (context)
		   (and
		    (eq? (prime-context-language context) 'English)
		    (= (prime-context-mode context) prime-mode-hiragana)))
		 (lambda (context)
		   (prime-mode-language-set context 'English)
		   (prime-mode-set context prime-mode-hiragana)))


(register-action 'action_prime_mode_wide_latin
		 (lambda (context)
		   '(ja_fullwidth_alnum
		     "Ａ"
		     "全角英数"
		     "全角を入力"))
		 (lambda (context)
		   (= (prime-context-mode context)
		      prime-mode-wide-latin))
		 (lambda (context)
		   (prime-mode-set context prime-mode-wide-latin)))

(register-action 'action_prime_mode_application
		 (lambda (context)
		   '(prime_mode_application
		     "！"
		     "特殊"
		     "アプリケーション依存"))
		 (lambda (context)
		   (= (prime-context-mode context)
		      prime-mode-application))
		 (lambda (context)
		   (prime-mode-set context prime-mode-application)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define prime-configure-widgets
  (lambda ()
    (register-widget 'widget_prime_input_mode
		     (activity-indicator-new prime-input-mode-actions)
		     (actions-new prime-input-mode-actions))
    (context-list-replace-widgets! 'prime prime-widgets)))


(define prime-context-rec-spec
  (append
   context-rec-spec
   (list
    ;; Upper level context which is used for registering a word.
    (list 'parent-context     #f)  
    (list 'display-head       '())
    (list 'display-tail       '())
    (list 'fund-line          '(() . ()))
    (list 'preedit-line       '("" "" ""))
    (list 'state              'prime-state-fund)
    (list 'nth                0)
    (list 'candidates         ())
    (list 'mode               prime-mode-latin)
    (list 'last-word          "")  ;; PRIMEやPOBoxの用語でいうContext
    (list 'connection         #f)
    (list 'session            #f)  ; the actual value is -default or -register.
					; language of the current session.
    (list 'language           prime-custom-default-language)
    (list 'lang-session-list  ())  ; session data of each language
    (list 'modification       '("" "" ""))
    (list 'segment-nth        0)
    (list 'segment-candidates ())
    (list 'history            ())
    (list 'previous-mode      prime-mode-latin)
    (list 'app-mode-key-list  ())
    )))
(define-record 'prime-context prime-context-rec-spec)
(define prime-context-new-internal prime-context-new)

(define prime-context-new
  (lambda (id im)
    (let ((context (prime-context-new-internal id im)))
      (prime-context-set-widgets! context prime-widgets)
      context)))

(define prime-context-new2
  (lambda (id im)
    (let ((context (prime-context-new-internal id im)))
      (prime-context-initialize! context)
      context)))

;; This initializes an empty context, and also initializes the prime server.
;; This returns context.
(define prime-context-initialize!
  (lambda (context)
    ;(print "prime-context-initialize!")
    (if (not (prime-context-session context))
	(begin
	  ;; The prime server is initialized here.
	  (prime-context-set-connection! context (prime-connection-init))
	  (let* ((connection (prime-context-connection context))
                 (session (prime-engine-session-start connection)))
	    (prime-custom-init connection)
	    (prime-context-set-fund-line!  context (cons () ()))
	    (prime-context-set-session!    context session)
	    (prime-context-set-lang-session-list!
	     context
	     (list (cons (prime-engine-session-language-get connection session) session)))
	    (prime-context-history-update! context))))
    context))

;; This function pushs the current context-data to the stack of
;; uim-contexts and create a new context-data.
(define prime-context-push
  (lambda (context)
    ;(print "prime-context-push")
    (let* ((im (prime-context-im context))
	   (uc (prime-context-uc context))
           (root-context (im-retrieve-context uc))
	   (new-context (prime-context-new2 uc im)))

      (prime-context-set-history! new-context (prime-context-history context))
      (set-cdr! (assoc 'state (prime-context-history new-context))
		'prime-state-pushed)
      (prime-context-set-parent-context! new-context (cons uc (cdr context)))
      ;; FIXME: Directly overwriting root-context prevents flexible input
      ;; context composition such as <im-switcher context> -> <prime
      ;; context>.  In such case, this code break both im-switcher context
      ;; and prime context. Do not assume and access superordinate input
      ;; context management scheme. -- YamaKen 2007-01-11
      (set-cdr! root-context (cdr new-context))
      new-context)))

;; This function destructs the current context-data and pops the tail
;; context-data from the stack of uim-contexts.
(define prime-context-pop
  (lambda (context)
    (let* ((parent-context (prime-context-parent-context context))
           (uc (prime-context-uc context))
           (root-context (im-retrieve-context uc)))
      (map
       (lambda (lang-pair)
	 (prime-engine-session-end (prime-context-connection context)
                                   (cdr lang-pair)))
       (prime-context-lang-session-list context))
      (if parent-context
	  (begin
 	    (prime-context-set-history! context
 					(prime-context-history parent-context))
	    (set-cdr! (assoc 'state (prime-context-history context))
		      'prime-state-poped)
            ;; FIXME: Directly overwriting root-context. See the comment
            ;; in prime-context-push.
	    (set-cdr! root-context
		      (cdr parent-context))
	    ))
      parent-context)))

(define prime-context-history-update!
  (lambda (context)
    (let* ((state          (prime-context-state context))
	   (selected-index (if (eq? state 'prime-state-segment)
			       (prime-context-segment-nth context)
			       (prime-context-nth context))))
      (prime-context-set-history!
       context
       (list (cons 'state           state)
	     (cons 'preedit-line    (prime-context-copy-preedit-line context))
	     (cons 'fund-line       (prime-context-copy-fund-line    context))
	     (cons 'selected-index  selected-index)
	     (cons 'conversion-line (copy-list
				     (prime-context-modification context)))
	     )))))

(define prime-context-history-compare
  (lambda (context)
    (let* ((prev-data      (prime-context-history context))
	   (state          (prime-context-state context))
	   (selected-index (if (eq? state 'prime-state-segment)
			       (prime-context-segment-nth context)
			       (prime-context-nth context))))
      (cond
       ((not (equal? state
		     (safe-cdr (assoc 'state prev-data))))
	'state)
       ((not (equal? (prime-context-get-preedit-line context)
		     (safe-cdr (assoc 'preedit-line prev-data))))
	'preedit)
       ((not (equal? (prime-context-fund-line context)
		     (safe-cdr (assoc 'fund-line prev-data))))
	'cursor)
       ((not (equal? selected-index
		     (safe-cdr (assoc 'selected-index prev-data))))
	'nth)
       ((not (equal? (prime-context-modification context)
		     (safe-cdr (assoc 'conversion-line prev-data))))
	'cursor)
       (else
        #f)
       ))))


(define prime-context-reset-fund-line!
 (lambda (context)
   (prime-editor-set-left  (prime-context-fund-line context) '())
   (prime-editor-set-right (prime-context-fund-line context) '())
   ))
(define prime-context-copy-fund-line
  (lambda (context)
    (let ((line (prime-context-fund-line context)))
      (cons (copy-list (car line)) (copy-list (cdr line)))
      )))

(define prime-context-reset-preedit-line!
 (lambda (context)
   (prime-engine-edit-erase (prime-context-connection context) (prime-context-session context))))

;; This returns a duplicated list of the current preedition.
(define prime-context-copy-preedit-line
  (lambda (context)
    (copy-list (prime-context-get-preedit-line context))))

;; This returns a list of the current preedition.
;; The structure of the list is [left, cursor, right]. ex. ["ab", "c", "de"].
(define prime-context-get-preedit-line
  (lambda (context)
    (prime-context-preedit-line context)))

(define prime-preedit-reset!
  (lambda (context)
    (prime-context-set-state! context 'prime-state-fund)
    (prime-context-reset-preedit-line!  context)
    (prime-context-set-nth! context 0)
    ))

(define prime-candidates-get-nth
  (lambda (context index-no)
    (if (>= index-no (prime-candidates-get-length context))
	#f
	(let ((state (prime-context-state context)))
	  (if (eq? state 'prime-state-segment)
	      (car (nth index-no (prime-context-segment-candidates context)))
	      (car (nth index-no (prime-context-candidates context))))))))

(define prime-candidates-get-length
  (lambda (context)
    (let ((state (prime-context-state context)))
      (if (eq? state 'prime-state-segment)
	  (length (prime-context-segment-candidates context))
	  (length (prime-context-candidates context))))))

(define prime-candidates-get-index
  (lambda (context)
    (let ((state (prime-context-state context)))
      (if (eq? state 'prime-state-segment)
	  (prime-context-segment-nth context)
	  (prime-context-nth context)))))

(define prime-get-nth-candidate
  (lambda (context n)
    (if (>= n (prime-get-nr-candidates context))
	#f
	(car (nth n (prime-context-candidates context))))))

(define prime-get-nr-candidates
  (lambda (context)
    (length (prime-context-candidates context))))

(define prime-get-current-candidate
  (lambda (context)
    (prime-get-nth-candidate context (prime-context-nth context))))

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
    (map
     (lambda (str)
       (string-split str "="))
     lst)))

;; This splits the string by the separator.  The difference from string-split
;; is the result of spliting "\t\t" by "\t".
;; (string-split "\t\t" "\t") => ().
;; (prime-util-string-split "\t\t" "\t") => ("" "" "").
;; The second argument separator must be a single character string.
;;
;; uim 1.5.0 revised the specification of string-split as
;; follows. Replace prime-util-string-split with the new string-split
;; if no other problems are remaining.  -- YamaKen 2007-07-11
;;   (string-split "\t\t" "\t")  => ("" "" "")
;;   (string-split "\t1\t" "\t") => ("" "1" "")
(define prime-util-string-split
  (lambda (string separator)
    (let ((result (list))
	  (node-string ""))
      (map (lambda (target)
	     (if (equal? target separator)
		 (begin
		   (set! result (cons node-string result))
		   (set! node-string ""))
		 (set! node-string (string-append node-string target))))
	   (reverse (string-to-list string)))
      (set! result (cons node-string result))
      (reverse result))))

(define prime-util-string-to-integer
  (lambda (string)
    (let ((integer 0)
	  (figure  1))
      (map
       (lambda (digit-string)
	 (if (string=? digit-string "-")
	     (set! integer (- integer))
	     (set! integer (+ integer (* (- (string->charcode digit-string)
					    (string->charcode "0"))
					 figure))))
	 (set! figure (* figure 10)))
       (string-to-list string))
      integer)))

;; This returns #t, if the argument command like "<Control>j" reflects
;; a pair of key and key-state.  The type of both key and key-stae is integer.
(define prime-util-command-match?
  (lambda (command key key-state)
    ((make-key-predicate (modify-key-strs-implicitly command)) key key-state)))

;;;; ------------------------------------------------------------
;;;; prime-uim:
;;;; ------------------------------------------------------------

;; This returns a pair of the beginning index and the end index of displayed
;; candidates.
(define prime-uim-candwin-get-range
  (lambda (context)
    (let* ((beginning (* (/ (prime-candidates-get-index context)
			    prime-nr-candidate-max)
			 prime-nr-candidate-max))
	   (end       (min (+ beginning prime-nr-candidate-max)
			   (prime-candidates-get-length context))))
      (cons beginning end))))

;;;; ------------------------------------------------------------
;;;; prime-engine: Functions to connect with a prime server.
;;;; ------------------------------------------------------------

(define prime-open-unix-domain-socket
  (lambda (socket-path)
    (and-let* ((fd (unix-domain-socket-connect socket-path)))
      (cons fd fd))))

(define prime-open-with-unix-domain-socket
  (lambda (socket-path)
    (let ((fds (prime-open-unix-domain-socket socket-path)))
      (or fds
          (begin
            (unlink socket-path)
            (process-with-daemon prime-command-path (list prime-command-path "-u" socket-path))
            (let loop ((fds (prime-open-unix-domain-socket socket-path))
                       (giveup 10))
              (cond ((= giveup 0)
                     (uim-notify-fatal
                      (format (N_ "cannot create socket file \"~a\"") socket-path))
                     #f ;; XXX
                     )
                    ((not fds)
                     (sleep 1)
                     (loop (prime-open-unix-domain-socket socket-path)
                           (dec giveup)))
                    (else
                     fds))))))))

(define prime-socket-path!
  (lambda ()
    (let ((config-path (get-config-path! #f)))
      (if (create/check-directory! (format "~a/socket" config-path))
          (format "~a/socket/uim-prime" config-path)
          (begin
            (uim-notify-fatal (N_ "cannot create socket directory"))
            #f)))))

(define prime-open-with-tcp-socket
  (lambda (hostname servname)
    (and-let* ((fd (tcp-connect prime-tcpserver-name prime-tcpserver-port)))
      (cons fd fd))))

(define prime-open-with-pipe
  (lambda (path)
    (process-io path)))

(define prime-connection-init
  (lambda ()
    (let ((fds (cond ((eq? prime-server-setting? 'unixdomain)
                      (prime-open-with-unix-domain-socket (prime-socket-path!)))
                     ((eq? prime-server-setting? 'tcpserver)
                      (prime-open-with-tcp-socket prime-tcpserver-name prime-tcpserver-port))
                     ((eq? prime-server-setting? 'pipe)
                      (prime-open-with-pipe prime-command-path))
                     (else
                      (uim-notify-fatal (N_ "Prime connection is not defined"))
                      #f))))
      (if fds
        (cons (open-file-port (car fds))
              (open-file-port (cdr fds)))
        #f))))

(define prime-send-command
  (lambda (connection msg)
    (if (pair? connection)
        (let ((iport (car connection))
              (oport (cdr connection)))
          (file-display msg oport)
          (let loop ((line (file-read-line iport))
                     (rest '()))
            (if (or (not line)
                    (eof-object? line)
                    (= 0 (string-length line))
                    (string=? line ""))
                (reverse rest) ;; drop last "\n"
                (loop (file-read-line iport) (cons line rest)))))
        #f)))

;; Don't append "\n" to arg-list in this function. That will cause a
;; problem with unix domain socket.
(define prime-engine-send-command
  (lambda (connection arg-list)
    ;; result       ==> ("ok" "1")
    (let* ((result (if connection
                     (prime-send-command
                       connection
                       (string-append (prime-util-string-concat arg-list "\t")
                                      "\n"))
                     '())))
      (if (not (null? result))
        (cdr result) ;; drop status line
        (list "\t\t")))))

(define prime-engine-close
  (lambda (prime-connection)
    (if (pair? prime-connection)
        (let ((iport (car prime-connection))
              (oport (cdr prime-connection)))
          (file-display "close\n" oport)
          (close-file-port (car prime-connection))
          (close-file-port (cdr prime-connection))))))

(define prime-engine-conv-predict
  (lambda (prime-connection prime-session)
    (cdr (prime-engine-conv-convert-internal prime-connection prime-session "conv_predict"))))

(define prime-engine-conv-convert
  (lambda (prime-connection prime-session)
    (cdr (prime-engine-conv-convert-internal prime-connection prime-session "conv_convert"))))

(define prime-engine-conv-convert-internal
  (lambda (prime-connection prime-session command)
    (let* ((result
	    (prime-engine-send-command prime-connection (list command prime-session)))
	   (index (prime-util-string-to-integer (car result)))
	   (words (map
		   (lambda (string-line)
		     (let ((word-data (prime-util-string-split string-line
							       "\t")))
		       (list (car word-data)  ; literal
			     (prime-util-assoc-list (cdr word-data)))))
		   (cdr result))))
      (cons index words))))

(define prime-engine-conv-select
  (lambda (prime-connection prime-session index-no)
    (prime-engine-send-command prime-connection
                               (list "conv_select"
				     prime-session
				     (digit->string index-no)))))

;; This sends a conv_commit command to the server and returns the commited
;; string.
(define prime-engine-conv-commit
  (lambda (prime-connection prime-session)
    (car (prime-engine-send-command prime-connection (list "conv_commit" prime-session)))))

(define prime-engine-modify-cursor-internal
  (lambda (prime-connection prime-session command)
    (prime-util-string-split
     (car (prime-engine-send-command prime-connection (list command prime-session)))
     "\t")))

(define prime-engine-modify-cursor-right
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection prime-session "modify_cursor_right")))
(define prime-engine-modify-cursor-left
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection prime-session "modify_cursor_left")))
(define prime-engine-modify-cursor-right-edge
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection
                                         prime-session
					 "modify_cursor_right_edge")))
(define prime-engine-modify-cursor-left-edge
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection
                                         prime-session
					 "modify_cursor_left_edge")))
(define prime-engine-modify-cursor-expand
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection
                                         prime-session
					 "modify_cursor_expand")))
(define prime-engine-modify-cursor-shrink
  (lambda (prime-connection prime-session)
    (prime-engine-modify-cursor-internal prime-connection
                                         prime-session
					 "modify_cursor_shrink")))

(define prime-engine-segment-select
  (lambda (prime-connection prime-session index-no)
    (prime-util-string-split
     (car (prime-engine-send-command prime-connection
                                     (list "segment_select"
					   prime-session
					   (digit->string index-no))))
     "\t")))

(define prime-engine-segment-reconvert
  (lambda (prime-connection prime-session)
    (prime-engine-conv-convert-internal prime-connection prime-session "segment_reconvert")))

(define prime-engine-context-reset
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "context_reset" prime-session))))


;; session operations
(define prime-engine-session-start
  (lambda (prime-connection)
    (car (prime-engine-send-command prime-connection (list "session_start")))))
(define prime-engine-session-end
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "session_end" prime-session))))

(define prime-engine-session-language-set
  (lambda (prime-connection language)
    (let ((language-string (if (eq? language 'English) "English" "Japanese")))
      (car (prime-engine-send-command
            prime-connection
	    (list "session_start" language-string))))))

(define prime-engine-session-language-get
  (lambda (prime-connection prime-session)
    (let ((language-string
	   (nth 1 (prime-util-string-split 
		   (car (prime-engine-send-command
                         prime-connection
			 (list "session_get_env" prime-session "language")))
		   "\t"))))
      (if (string=? language-string "English")
	  'English 'Japanese))))

;; composing operations
(define prime-engine-edit-insert
  (lambda (prime-connection prime-session string)
    (prime-engine-send-command prime-connection (list "edit_insert"    prime-session string))))
(define prime-engine-edit-delete
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_delete"    prime-session))))
(define prime-engine-edit-backspace
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_backspace" prime-session))))
(define prime-engine-edit-erase
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_erase"     prime-session))))

;; This sends a edit_commit command to the server and returns the commited
;; string.
(define prime-engine-edit-commit
  (lambda (prime-connection prime-session)
    (car (prime-engine-send-command prime-connection (list "edit_commit" prime-session)))))

;; cursor operations
(define prime-engine-edit-cursor-left
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_cursor_left" prime-session))))
(define prime-engine-edit-cursor-right
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_cursor_right" prime-session))))
(define prime-engine-edit-cursor-left-edge
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_cursor_left_edge" prime-session))))
(define prime-engine-edit-cursor-right-edge
  (lambda (prime-connection prime-session)
    (prime-engine-send-command prime-connection (list "edit_cursor_right_edge" prime-session))))

;; preedition-getting operations
(define prime-engine-edit-get-preedition
  (lambda (prime-connection prime-session)
    (prime-util-string-split (car (prime-engine-send-command
                                   prime-connection
				   (list "edit_get_preedition" prime-session)))
			     "\t")))
(define prime-engine-edit-get-query-string
  (lambda (prime-connection prime-session)
    (car (prime-engine-send-command
          prime-connection
	  (list "edit_get_query_string" prime-session)))))

;; mode operations
(define prime-engine-edit-set-mode
  (lambda (prime-connection prime-session mode)
    (prime-engine-send-command prime-connection (list "edit_set_mode" prime-session mode))))

(define prime-engine-preedit-convert-input
  (lambda (prime-connection string)
    (if (string=? string "")
	'("")
	(let ((conversion (car (prime-engine-send-command
                                prime-connection
				(list "preedit_convert_input" string)))))
	  (cond
	   ;; counversion could be (), in case a suikyo table is broken.
	   ((not conversion)
	    '(""))
	   ;; Check the charcode of the beginning char of conversion
	   (else
 	    (prime-util-string-split conversion "\t")))))))

(define prime-engine-learn-word
  (lambda (prime-connection pron literal pos context suffix rest)
    (prime-engine-send-command prime-connection
                               (list "learn_word"
				     pron literal pos context suffix rest))))

;; This returns a version string of the PRIME server.
(define prime-engine-get-version
  (lambda (prime-connection)
    (car (prime-engine-send-command prime-connection '("get_version")))))

(define prime-engine-get-env
  (lambda (prime-connection env-name)
    (let* ((result (prime-util-string-split
		    (car (prime-engine-send-command prime-connection (list "get_env" env-name)))
		    "\t"))
	   (result-type (car result)))
      (cond
       ((string=? result-type "nil")
	'nil)
       ((string=? result-type "string")
	(nth 1 result))
       ((string=? result-type "array")
	(prime-util-string-split (cdr result) "\t"))
       ((string=? result-type "boolean")
	(string=? (nth 1 result) "true"))
       (else
	'unknown))
      )))

(define prime-engine-get-env-typing-method
  (lambda (prime-connection)
    (prime-engine-get-env prime-connection "typing_method")
    ))

;;;; ------------------------------------------------------------
;;;; prime-command: User commands for general purpose.
;;;; ------------------------------------------------------------
(define prime-command-pass
  (lambda (context key key-state)
    #t))

(define prime-command-commit
  (lambda (context key key-state)
    (prime-commit-without-learning context (charcode->string key))))

(define prime-command-commit-raw
  (lambda (context key key-state)
    (if (prime-context-parent-context context)
	(prime-proc-call-command prime-subkeymap-child-context
				 context key key-state)
	(prime-commit-raw context))))

;;;; prime-command: modes

;; This changes the typing mode specified by mode-string.
(define prime-mode-set-mode
  (lambda (context mode-string)
    (if (eq? (prime-context-state context) 'prime-state-converting)
	(prime-convert-cancel context))
    (prime-engine-edit-set-mode (prime-context-connection context)
                                (prime-context-session context) mode-string)))
    
;; This sets the typing mode to the default/Hiragana mode.
(define prime-command-mode-hiragana
  (lambda (context key key-state)
    (prime-mode-set-mode context "default")))

;; This sets the typing mode to the Katakana mode.
(define prime-command-mode-katakana
  (lambda (context key key-state)
    (prime-mode-set-mode context "katakana")))

;; This sets the typing mode to the hankaku(half-width) Katakana mode.
(define prime-command-mode-hankana
  (lambda (context key key-state)
    (prime-mode-set-mode context "half_katakana")))

;; This sets the typing mode to the zenkaku(wide-width) ASCII mode.
(define prime-command-mode-wideascii
  (lambda (context key key-state)
    (prime-mode-set-mode context "wide_ascii")))

;; This sets the typing mode to the raw/ASCII mode.
(define prime-command-mode-ascii
  (lambda (context key key-state)
    (prime-mode-set-mode context "raw")))

(define prime-command-language-toggle
  (lambda (context key key-state)
    (let ((next-language (if (eq? (prime-context-language context) 'English)
			     'Japanese 'English)))
      (prime-mode-language-set context next-language))))

(define prime-command-japanese-mode
  (lambda (context key key-state)
    (prime-context-initialize! context)
    (prime-mode-language-set context (prime-context-language context))
    (prime-mode-set context prime-mode-hiragana)))

(define prime-command-wide-latin-mode
  (lambda (context key key-state)
    (prime-mode-set context prime-mode-wide-latin)))

(define prime-command-latin-mode
  (lambda (context key key-state)
    (prime-mode-set context prime-mode-latin)))


(define prime-command-register-mode
  (lambda (context key key-state)
    ;(print "prime-command-register-mode")
    (prime-register-mode-on context)))


;;;; ------------------------------------------------------------
;;;; prime-command-wide-latin: User commands in a wide-latin-mode
;;;; ------------------------------------------------------------
(define prime-command-wide-latin-input
  (lambda (context key key-state)
    (let ((wide-char (ja-wide (charcode->string key))))
      (if wide-char
	  (prime-commit-without-learning context wide-char)
	  (prime-command-commit-raw context key key-state)))))

;;;; ------------------------------------------------------------
;;;; prime-command-conv: User commands in a conversion state
;;;; ------------------------------------------------------------
(define prime-command-conv-next
  (lambda (context key key-state)
    (prime-convert-selection-move context (+ 1 (prime-context-nth context)))
    ))

(define prime-command-conv-prev
  (lambda (context key key-state)
    (if (> (prime-context-nth context) 0)
	(prime-convert-selection-move context
				      (- (prime-context-nth context) 1))
	(prime-convert-selection-move context
				      (- (prime-get-nr-candidates context) 1)))
    ))

(define prime-command-conv-cancel
  (lambda (context key key-state)
    (prime-convert-cancel context)))

(define prime-command-conv-commit
  (lambda (context key key-state)
    (prime-commit-conversion context)))

(define prime-command-conv-commit-and-commit-raw
  (lambda (context key key-state)
    (prime-commit-conversion context)
    (prime-command-commit-raw context key key-state)))

(define prime-command-conv-commit-and-space
  (lambda (context key key-state)
    (prime-commit-conversion context)
    (prime-commit-string context " ")))

(define prime-command-conv-select
  (lambda (context key key-state)
    (let* ((nth0 (number->candidate-index (numeric-ichar->integer key)))
	   (cand-range (prime-uim-candwin-get-range context))
	   (nth (min (+ (car cand-range) nth0) (cdr cand-range)))
	   (cand (prime-candidates-get-nth context nth)))
      (if cand
	  (prime-commit-candidate context nth))
      )))

;; FIXME: Integrate into the above prime-command-conv-select.
;; FIXME: <Hiroyuki Komatsu> (2005-03-30)
(define prime-command-segment-select
  (lambda (context key key-state)
    (let* ((nth0 (number->candidate-index (numeric-ichar->integer key)))
	   (cand-range (prime-uim-candwin-get-range context))
	   (nth (min (+ (car cand-range) nth0) (cdr cand-range)))
	   (cand (prime-candidates-get-nth context nth)))
      ;(print cand-range)
      (if cand
	  (prime-commit-segment-nth context nth))
      )))

(define prime-command-conv-input
  (lambda (context key key-state)
    (prime-commit-candidate context (prime-context-nth context))
    (prime-command-fund-input context key key-state)
    ))

;;;; ------------------------------------------------------------
;;;; prime-command-modify: User commands in a modification state.
;;;; ------------------------------------------------------------

(define prime-command-modify-commit
  (lambda (context key key-state)
    (prime-commit-conversion context)))

(define prime-command-modify-convert
  (lambda (context key key-state)
    (prime-context-set-state! context 'prime-state-segment)
    (let ((conversion (prime-engine-segment-reconvert
                       (prime-context-connection context)
		       (prime-context-session context))))
      (prime-context-set-segment-nth!        context (car conversion))
      (prime-context-set-segment-candidates! context (cdr conversion)))))

(define prime-command-modify-convert-reversely
  (lambda (context key key-state)
    (prime-command-modify-convert context key key-state)
    (prime-command-segment-prev   context key key-state)))

(define prime-command-modify-cursor-right
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-right (prime-context-connection context)
                                       (prime-context-session context)))
    ))

(define prime-command-modify-cursor-left
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-left (prime-context-connection context)
                                      (prime-context-session context)))
    ))

(define prime-command-modify-cursor-right-edge
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-right-edge (prime-context-connection context)
                                            (prime-context-session context)))
    ))

(define prime-command-modify-cursor-left-edge
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-left-edge (prime-context-connection context)
                                           (prime-context-session context)))
    ))

(define prime-command-modify-cursor-expand
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-expand (prime-context-connection context)
                                        (prime-context-session context)))
    ))

(define prime-command-modify-cursor-shrink
  (lambda (context key key-state)
    (prime-modify-reset! context)
    (prime-context-set-modification!
     context
     (prime-engine-modify-cursor-shrink (prime-context-connection context)
                                        (prime-context-session context)))
    ))

(define prime-modify-reset!
  (lambda (context)
    (prime-context-set-state!              context 'prime-state-modifying)
    (prime-context-set-segment-nth!        context 0)
    (prime-context-set-segment-candidates! context ())))

;;;; ------------------------------------------------------------
;;;; prime-command-segment: User commands in a segment state.
;;;; ------------------------------------------------------------
(define prime-command-segment-cancel
  (lambda (context key key-state)
    (prime-modify-reset! context)))

(define prime-command-segment-commit
  (lambda (context key key-state)
    (prime-commit-segment context)))

(define prime-command-segment-next
  (lambda (context key key-state)
    (prime-segment-selection-move context
				  (+ (prime-context-segment-nth context) 1))))

(define prime-command-segment-prev
  (lambda (context key key-state)
    (prime-segment-selection-move context
				  (- (prime-context-segment-nth context) 1))))

;; TODO: Add a auto-register-mode function.
;; TODO: (2005-01-12) <Hiroyuki Komatsu>
(define prime-segment-selection-move
  (lambda (context selection-index)
    (cond
     ((<  selection-index 0)
      (set! selection-index (- (prime-segment-get-candidates-length context)
			       1)))
     ((>= selection-index (prime-segment-get-candidates-length context))
      (set! selection-index 0)))
    (prime-context-set-segment-nth! context selection-index)
    (prime-context-set-modification! context
				     (prime-engine-segment-select
                                      (prime-context-connection context)
				      (prime-context-session context)
				      selection-index))))

(define prime-segment-get-candidates-length
  (lambda (context)
    (length (prime-context-segment-candidates context))))

;;;; ------------------------------------------------------------
;;;; prime-command-preedit: User commands in a preedit state.
;;;; ------------------------------------------------------------

(define prime-command-preedit-cancel
  (lambda (context key key-state)
    (prime-engine-edit-erase (prime-context-connection context)
                             (prime-context-session context))))

(define prime-command-preedit-backspace
  (lambda (context key key-state)
    (prime-engine-edit-backspace (prime-context-connection context)
                                 (prime-context-session context))))

(define prime-command-preedit-delete
  (lambda (context key key-state)
    (prime-engine-edit-delete (prime-context-connection context)
                              (prime-context-session context))))

(define prime-command-preedit-commit
  (lambda (context key key-state)
    (prime-commit-preedition context)))

(define prime-command-preedit-commit-and-commit-raw
  (lambda (context key key-state)
    (prime-commit-preedition context)
    (prime-command-commit-raw context key key-state)))

(define prime-command-preedit-commit-and-space
  (lambda (context key key-state)
    (prime-commit-preedition context)
    (prime-commit-string context " ")))

(define prime-command-preedit-cursor-left-edge
  (lambda (context key key-state)
    (prime-engine-edit-cursor-left-edge (prime-context-connection context)
                                        (prime-context-session context))))

(define prime-command-preedit-cursor-right-edge
  (lambda (context key key-state)
    (prime-engine-edit-cursor-right-edge (prime-context-connection context)
                                         (prime-context-session context))))

(define prime-command-preedit-cursor-left
  (lambda (context key key-state)
    (prime-engine-edit-cursor-left (prime-context-connection context)
                                   (prime-context-session context))))

(define prime-command-preedit-cursor-right
  (lambda (context key key-state)
    (prime-engine-edit-cursor-right (prime-context-connection context)
                                    (prime-context-session context))))

(define prime-command-preedit-input
  (lambda (context key key-state)
    (prime-engine-edit-insert (prime-context-connection context)
                              (prime-context-session context)
			      (charcode->string key))))

(define prime-command-preedit-commit-candidate
  (lambda (context key key-state)
    (if prime-custom-number-selection?
	(let* ((nth (number->candidate-index (numeric-ichar->integer key)))
	       (cand (prime-get-nth-candidate context nth)))
	  (if cand
	      (prime-commit-candidate context nth))
	  )
	(if (prime-normal-key? key key-state)
	    (prime-command-preedit-input context key key-state))
	)))

(define prime-command-preedit-convert
  (lambda (context key key-state)
    (prime-convert-start context)
    ))

(define prime-command-preedit-convert-reversely
  (lambda (context key key-state)
    (prime-convert-start-reversely context)
    ))

;;;; ------------------------------------------------------------
;;;; prime-command-fund: User commands in a fundamental state.
;;;; ------------------------------------------------------------
(define prime-command-fund-input
  (lambda (context key key-state)
    (prime-context-set-state! context 'prime-state-preedit)
    (prime-command-preedit-input context key key-state)
    ))

(define prime-command-fund-space
  (lambda (context key key-state)
    (cond
     ((eq? (prime-context-language context) 'Japanese)
      (let ((space (if (eq? prime-custom-japanese-space 'wide) "　" " ")))
	(prime-commit-without-learning context space)))
     (else
      (prime-commit-without-learning context " ")))))

(define prime-command-fund-altspace
  (lambda (context key key-state)
    (cond
     ((eq? (prime-context-language context) 'Japanese)
      (let ((space (if (eq? prime-custom-japanese-space 'wide) " " "　")))
	(prime-commit-without-learning context space)))
     (else
      (prime-commit-without-learning context " ")))))

(define prime-command-fund-commit-ja-direct
  (lambda (context key key-state)
    (let ((direct (ja-direct (charcode->string key))))
      (prime-commit-without-learning context direct)
      )))

;;;; ------------------------------------------------------------
;;;; prime-command-register-fund: User commands in a register fundamental state
;;;; ------------------------------------------------------------
(define prime-command-fund-backspace
  (lambda (context key key-state)
    (prime-editor-backspace-char (prime-context-fund-line context))
    ))

(define prime-command-fund-delete
  (lambda (context key key-state)
    (prime-editor-delete-char (prime-context-fund-line context))
    ))

(define prime-command-fund-cancel
  (lambda (context key key-state)
    (prime-context-pop context)
    ))

(define prime-command-child-finish
  (lambda (context key key-state)
    (let ((parent-context (prime-context-parent-context context)))
      (if parent-context
	  (let* ((reading (prime-preedit-get-string-label parent-context))
		 (literal (prime-fund-get-line-string context))
		 (word-data (list (list "basekey" reading)
				  (list "base"    literal))))
	    (prime-commit-word-data parent-context word-data)
	    (prime-context-pop context)))
      )))

(define prime-command-fund-cursor-left-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-left-edge
     (prime-context-fund-line context))))

(define prime-command-fund-cursor-right-edge
  (lambda (context key key-state)
    (prime-editor-cursor-move-right-edge
     (prime-context-fund-line context))))

(define prime-command-fund-cursor-left
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-fund-line context) -1)))

(define prime-command-fund-cursor-right
  (lambda (context key key-state)
    (prime-editor-cursor-move (prime-context-fund-line context) 1)))

;; ------------------------------------------------------------
;; prime-command-app: commands for specific applications
;; ------------------------------------------------------------

(define prime-command-app-mode-start
  (lambda (context key key-state)
    ;(print "prime-command-app-mode-start")
    (prime-context-set-previous-mode! context (prime-context-mode context))
    (prime-context-set-app-mode-key-list! context
					  prime-app-mode-end-stroke-list)
    (prime-mode-set context prime-mode-application)
    (prime-commit-raw context)))

(define prime-command-app-mode-end
  (lambda (context key key-state)
    (prime-mode-set context (prime-context-previous-mode context))
    (prime-context-set-previous-mode! context prime-mode-latin)))

(define prime-command-app-mode
  (lambda (context key key-state)
    ;(print "prime-command-app-mode")
    (prime-command-app-mode-internal
     context key key-state
     (prime-context-app-mode-key-list context))))

(define prime-command-app-mode-internal
  (lambda (context key key-state key-list)
    (let ((key-data (safe-car key-list)))
      (cond
       ;; there's no speficied command then pressed key is passed.
       ((eq? key-list '())
	(prime-context-set-app-mode-key-list! context
					      prime-app-mode-end-stroke-list)
	(prime-commit-raw context))

       ;; key-data is a string like "i" then this app-mode ends.
       ((and (string? key-data)
	     (prime-util-command-match? key-data key key-state))
	(prime-command-app-mode-end context key key-state)
	(prime-commit-raw context))

       ;; key-data is a stroke of keys like ("c" . ("l" "w" ...))
       ;; then the key-list data goes to a next stage.
       ((and (list? key-data)
	     (prime-util-command-match? (car key-data) key key-state))
	(prime-context-set-app-mode-key-list! context (cdr key-data))
	(prime-commit-raw context))

       ;; call this command recursively.
       (else
	(prime-command-app-mode-internal context
					 key key-state (cdr key-list)))))))

;;;; ------------------------------------------------------------
;;;; prime-proc: procedure
;;;; ------------------------------------------------------------

(define prime-proc-call-command
  (lambda (keymap context key key-state)
    (let ((command (prime-keymap-get-command keymap key key-state)))
      (if command
	  (begin
	    ((symbol-value command) context key key-state)
	    #t)
	  #f))))

;;;; ------------------------------------------------------------
;;;; prime-preedit:
;;;; ------------------------------------------------------------

(define prime-editor-get-left  (lambda (line) (car line)))
(define prime-editor-set-left  (lambda (line new-line-left)
				 (set-car! line new-line-left)))
(define prime-editor-get-left-string
  (lambda (line)
    (string-list-concat (prime-editor-get-left line))))

(define prime-editor-get-right (lambda (line) (cdr line)))
(define prime-editor-set-right (lambda (line new-line-right)
				 (set-cdr! line new-line-right)))
(define prime-editor-get-right-string
  (lambda (line)
    (string-list-concat (reverse (prime-editor-get-right line)))))

(define prime-editor-get-line
  (lambda (line)
    (append
     (reverse (prime-editor-get-right line))
     (prime-editor-get-left line))))
(define prime-editor-get-line-string
  (lambda (line)
    (string-list-concat (prime-editor-get-line line))))

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
      (let ((line-left  (cons (or (safe-car (prime-editor-get-right line))
				  '())
			      (prime-editor-get-left line)))
	    (line-right (or (safe-cdr (prime-editor-get-right line))
			    '())))
	(prime-editor-set-left  line line-left)
	(prime-editor-set-right line line-right))
      (prime-editor-cursor-move line (- motion-arg 1)))
     ;; left motion
     ((and (< motion-arg 0)
	   (not (null? (car line))))
      (let ((line-left  (or (safe-cdr (prime-editor-get-left line))
			    '()))
	    (line-right (cons (or (safe-car (prime-editor-get-left line))
				  '())
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
    (prime-editor-set-left  line (or (safe-cdr (prime-editor-get-left line))
				     '()))))

(define prime-editor-delete-char
  (lambda (line)
    (prime-editor-set-right line (or (safe-cdr (prime-editor-get-right line))
				     '()))))


;; This returns a preediting string.
(define prime-preedit-get-string-label
  (lambda (context)
    (apply string-append (prime-context-preedit-line context))))

;; This returns #t if the preediting string is not empty.  Or #f.
(define prime-preedit-exist?
  (lambda (context)
    (> (string-length (prime-preedit-get-string-label context)) 0)))

;; This returns a query string for PRIME server.
(define prime-preedit-get-string-raw
  (lambda (context)
    (prime-engine-edit-get-query-string (prime-context-connection context)
                                        (prime-context-session context))))

;; This returns a commited string of register mode.
(define prime-fund-get-line-string
  (lambda (context)
    (let ((line (prime-context-fund-line context)))
      (prime-editor-get-line-string line))))

;;;; ------------------------------------------------------------
;;;; prime-custom
;;;; ------------------------------------------------------------

(define prime-custom-init
  (lambda (prime-connection)
    (let ((typing-method (prime-engine-get-env-typing-method prime-connection)))
      (cond
       ((eq? typing-method 'unknown)
        #f)
       ((string=? typing-method "kana")
	(prime-dont-use-numeral-key-to-select-cand))
       ((string=? typing-method "tcode")
	(prime-dont-use-numeral-key-to-select-cand)
;	(set! prime-mask-pending-preedit? #t)
	)
       ))
    ))

;;;; ------------------------------------------------------------
;;;; prime-commit
;;;; ------------------------------------------------------------

(define prime-commit-raw
  (lambda (context)
    (if (member (prime-context-mode context) 
		(list prime-mode-latin prime-mode-application))
	(im-commit-raw context)
	(begin
	  ;; Reset the current prime-context
	  (prime-engine-context-reset (prime-context-connection context)
                                      (prime-context-session context))

	  (im-commit-raw context)
	  (prime-context-set-last-word! context "")
	  (prime-preedit-reset! context)
	  ))))

(define prime-commit-without-learning
  (lambda (context string)
    ;; Reset the current prime-context
    (prime-engine-context-reset (prime-context-connection context)
                                (prime-context-session context))

    (if (prime-context-parent-context context)
	(prime-commit-to-fund-line context string)
	(im-commit context string))
    (prime-context-set-last-word! context "")
    ))

(define prime-commit-string
  (lambda (context string)
    (if (prime-context-parent-context context)
	(prime-commit-to-fund-line context string)
	(im-commit context string))
    (prime-preedit-reset! context)))

;; obsolete
(define prime-commit-word-data
  (lambda (context word-data)
    (prime-learn-word context word-data)
    (prime-commit-string
     context
     (string-append (or (safe-car (safe-cdr (assoc "base"        word-data)))
			"")
		    (or (safe-car (safe-cdr (assoc "conjugation" word-data)))
			"")
		    (or (safe-car (safe-cdr (assoc "suffix"      word-data)))
			"")))))

(define prime-commit-preedition
  (lambda (context)
    (let ((commited-string (prime-engine-edit-commit
                            (prime-context-connection context)
			    (prime-context-session context))))
      (prime-commit-string context commited-string))))

(define prime-commit-conversion
  (lambda (context)
    (let ((commited-string (prime-engine-conv-commit
                            (prime-context-connection context)
			    (prime-context-session context))))
      (prime-commit-string context commited-string))))

(define prime-commit-segment
  (lambda (context)
;    (prime-engine-modify-commit (prime-context-connection context) (prime-context-session-default context))
    (prime-context-set-state! context 'prime-state-modifying)))

(define prime-commit-segment-nth
  (lambda (context selection-index)
    (prime-segment-selection-move context selection-index)
    (prime-commit-segment context)))

(define prime-commit-candidate
  (lambda (context index-no)
    (prime-engine-conv-select (prime-context-connection context)
                              (prime-context-session context)
                              index-no)
    (prime-commit-conversion context)))

(define prime-commit-to-fund-line
  (lambda (context word)
    (let ((line (prime-context-fund-line context)))
      (prime-editor-set-left line (append (string-to-list word)
					  (prime-editor-get-left line)))
      )))

;;;; ------------------------------------------------------------

(define prime-learn-word
  (lambda (context assoc-list)
    (let ((key     (or (safe-car (safe-cdr (assoc "basekey"     assoc-list)))
		       ""))
	  (value   (or (safe-car (safe-cdr (assoc "base"        assoc-list)))
		       ""))
	  (part    (or (safe-car (safe-cdr (assoc "part"        assoc-list)))
		       ""))
	  (prime-context (or (prime-context-last-word context) ""))
	  (suffix  (or (safe-car (safe-cdr (assoc "conjugation" assoc-list)))
		       ""))
	  (rest    (or (safe-car (safe-cdr (assoc "suffix"      assoc-list)))
		       "")))
      
      (prime-engine-learn-word (prime-context-connection context) key value part prime-context suffix rest)
      (prime-context-set-last-word! context
				    (string-append value suffix rest))
      )))


;;;; ------------------------------------------------------------
;;;; prime-convert
;;;; ------------------------------------------------------------

(define prime-convert-start
  (lambda (context)
    (prime-convert-start-internal context 0)))

(define prime-convert-start-reversely
  (lambda (context)
    (let ((last-idx (- (prime-get-nr-candidates context)
		       1)))
      (prime-convert-start-internal context last-idx))))

(define prime-convert-start-internal
  (lambda (context init-idx)
    (let ((res #f))
      (prime-convert-get-conversion context)
      (set! res (prime-get-nth-candidate context init-idx))
      (if res
	  (begin
	    (prime-context-set-nth!   context init-idx)
	    (prime-context-set-state! context 'prime-state-converting))
	  )
      (prime-convert-selection-move context init-idx)
      )))


;; This function moves the cursor of candidate words.  If the cursor is out of
;; the range and the variable prime-auto-register-mode? is #t, the mode is
;; changed to register-mode.
(define prime-convert-selection-move
  (lambda (context selection-index)
    (prime-context-set-nth! context selection-index)
    (if (prime-get-current-candidate context)
	;; If the selection-index is a valid number, sends the number
	;; to the server.
	(prime-engine-conv-select (prime-context-connection context)
                                  (prime-context-session context)
				  selection-index)
	(begin
	  (prime-context-set-nth! context 0)
	  (prime-engine-conv-select (prime-context-connection context)
                                    (prime-context-session context)
                                    0)
	  (if prime-auto-register-mode?
	      (prime-register-mode-on context))))))

;; This resets the converting mode and goes to the preediting mode.
(define prime-convert-cancel
  (lambda (context)
    (prime-context-set-state! context 'prime-state-preedit)
    (prime-context-set-nth! context 0)
    (im-deactivate-candidate-selector context)))

;; This executes 'conv_predict' to predict candidate words and stores them.
(define prime-convert-get-prediction
  (lambda (context)
    (prime-context-set-candidates!  ;; FIXME: candidates -> conversions
     context
     (prime-engine-conv-predict (prime-context-connection context)
                                (prime-context-session context)))))

;; This executes 'conv_convert' to get candidate words and stores them.
(define prime-convert-get-conversion
  (lambda (context)
    (prime-context-set-candidates!  ;; FIXME: candidates -> conversions
     context
     (prime-engine-conv-convert (prime-context-connection context)
                                (prime-context-session context)))))

;;;; ------------------------------------------------------------
;;;; prime-commit
;;;; ------------------------------------------------------------

(define prime-update
  (lambda (context)
    (prime-update-key-press   context)
    (prime-update-key-release context)))

(define prime-update-key-press
  (lambda (context)
    (let ((session (prime-context-session context))
	  (mode (prime-context-mode context)))
      (cond
       ((not session)
	#f)  ;; Do nothing.

       (else
	(if (and
	     (not (= mode prime-mode-latin))
	     (not (= mode prime-mode-application)))
	    ;; Store the current preedition into the context
	    (prime-context-set-preedit-line!
	     context
	     (prime-engine-edit-get-preedition (prime-context-connection context)
                                               session)))

	(prime-update-state context)
	(prime-update-preedit context)
	)))))

(define prime-update-key-release
  (lambda (context)
    (let ((session (prime-context-session context)))
      (cond
       ((not session)
	#f)  ;; Do nothing.

       (else
	(prime-update-prediction context)
	(prime-update-candidate-window context)
	(prime-update-history context)
	)))))

(define prime-update-state
  (lambda (context)
    (if (not (prime-preedit-exist? context))
	(prime-context-set-state! context 'prime-state-fund))))

(define prime-update-history
  (lambda (context)
    (prime-context-history-update! context)))

(define prime-update-prediction
  (lambda (context)
    (let ((diff (prime-context-history-compare context)))
      (cond
       ((eq? diff 'state)
	(let ((state     (prime-context-state context))
	      (last-word (prime-context-last-word context)))
	  (cond
	   ((eq? state 'prime-state-preedit)
	    (prime-convert-get-prediction context))
	   ((eq? state 'prime-state-converting)
	    ;; Do nothing.  (prime-convert-get-conversion context) had been
	    ;; already executed at prime-convert-start-internal
	    )
	   ((eq? state 'prime-state-fund)
	    (prime-context-set-candidates! context '()))
	    )))
       ((eq? diff 'preedit)
	(prime-convert-get-prediction context))
       ))))

(define prime-update-candidate-window
  (lambda (context)
    (let ((diff (prime-context-history-compare context)))
      (cond
       ((eq? diff 'state)
	(let ((state (prime-context-state context)))
	  (cond
	   ((eq? state 'prime-state-fund)
	    (im-deactivate-candidate-selector context))

	   ((eq? state 'prime-state-preedit)
	    (if (> (prime-get-nr-candidates context) 0)
		(im-activate-candidate-selector
		 context
		 (prime-get-nr-candidates context)
		 3)))
;		 prime-nr-candidate-max)))

	   ((eq? state 'prime-state-converting)
 	    (im-activate-candidate-selector
 	     context (prime-get-nr-candidates context) prime-nr-candidate-max)
	    (im-select-candidate context (prime-context-nth context)))

	   ((eq? state 'prime-state-modifying)
	    (im-deactivate-candidate-selector context))

	   ((eq? state 'prime-state-segment)
 	    (im-activate-candidate-selector
	     context
	     (prime-segment-get-candidates-length context)
	     prime-nr-candidate-max)
	    (im-select-candidate context (prime-context-segment-nth context)))
	    )))

       ((eq? diff 'nth)
	(if (eq? (prime-context-state context) 'prime-state-segment)
	    (im-select-candidate context (prime-context-segment-nth context))
	    (im-select-candidate context (prime-context-nth context))))

       ((eq? diff 'preedit)
	(if (> (prime-get-nr-candidates context) 0)
	    (im-activate-candidate-selector
	     context (prime-get-nr-candidates context) prime-nr-candidate-max)
	    (im-deactivate-candidate-selector context)))
       ))))

(define prime-update-preedit
  (lambda (context)
    (if (prime-context-history-compare context)
	(begin
	  (im-clear-preedit context)
	  (prime-display-preedit context
				 (prime-update-preedit-internal context))
	  (im-update-preedit context)
	  ))))

(define prime-update-preedit-internal
  (lambda (context)
    (let* ((line       (prime-context-fund-line context))
	   (line-left  (prime-editor-get-left-string  line))
	   (line-right (prime-editor-get-right-string line)))
      (append
       (prime-context-display-head context)
       (if line-left  (list (cons 'committed line-left)))
       (prime-preedit-state-update-preedit context)
       (if line-right (list (cons 'committed line-right)))
       (prime-context-display-tail context)))))

(define prime-preedit-state-update-preedit
  (lambda (context)
    (let* ((state (prime-context-state context)))
      (cond
       ((eq? state 'prime-state-converting)
	(list (cons 'converting (prime-get-current-candidate context))
	      (cons 'cursor     "")))

       ((or (eq? state 'prime-state-modifying)
	    (eq? state 'prime-state-segment))
	(let* ((line (prime-context-modification context)))
	  (list (cons 'segment           (nth 0 line))
		(cons 'segment-highlight (nth 1 line))
		(cons 'cursor            "")
		(cons 'segment           (nth 2 line)))))

       ((prime-preedit-exist? context)
	(let* ((line  (prime-context-get-preedit-line context))
	       (left  (car line))
	       (right (apply string-append (cdr line))))
	  (list (cons 'preedit left)
		(cons 'cursor "")
		(cons 'preedit right))))

       (else
	(list (cons 'cursor "")))))))

(define prime-display-preedit-format
  (list (cons 'committed         preedit-none)
	(cons 'cursor            preedit-cursor)
	(cons 'pseudo-cursor     preedit-reverse)
	(cons 'preedit           preedit-underline)
	(cons 'converting        preedit-underline)
	(cons 'segment           preedit-underline)
	(cons 'segment-highlight preedit-reverse)
	(cons 'register-border   preedit-reverse)
	(cons 'register-label    preedit-reverse)
	(cons 'register-word     preedit-reverse)
	))

(define prime-display-preedit
  (lambda (context preedit-list)
    (if (not (null? preedit-list))
	(let ((type   (car (car preedit-list)))
	      (string (cdr (car preedit-list))))
	  (cond
	   ((eq? type 'cursor)
	    (prime-display-preedit-cursor context))
	   ((not (string=? string ""))
	    (im-pushback-preedit
	     context (cdr (assoc type prime-display-preedit-format)) string))
	   )
	  (prime-display-preedit context (cdr preedit-list))))))

(define prime-display-preedit-cursor
  (lambda (context)
    (im-pushback-preedit
     context (cdr (assoc 'cursor prime-display-preedit-format)) "")
    (if (and prime-pseudo-mode-cursor?
	     (= (prime-context-mode context) prime-mode-hiragana)
	     (eq? (prime-context-state context) 'prime-state-fund))
	(im-pushback-preedit context
			     (cdr (assoc 'pseudo-cursor
					 prime-display-preedit-format))
			     " "))
    ))

;;;; ------------------------------------------------------------

(define prime-register-mode-on
  (lambda (context)
    ;(print "prime-register-mode-on")
    (let* ((reading (prime-preedit-get-string-label context))
	   ;; Header and footer strings for a preedition line.
	   (current-display-head (prime-context-display-head context))
	   (current-display-tail (prime-context-display-tail context))
	   ;; Committed line in the current session.
	   (current-line         (prime-context-fund-line context))
	   (current-line-left    (prime-editor-get-left-string  current-line))
	   (current-line-right   (prime-editor-get-right-string current-line))
	   (new-context (prime-context-push context)))
      (prime-context-set-display-head!
       new-context
       (append current-display-head
	       (if current-line-left
		   (list (cons 'committed current-line-left)))
	       (list (cons 'register-label  "単語登録")
		     (cons 'register-border "[")
		     (cons 'register-word   reading)
		     (cons 'register-border "|"))))
      (prime-context-set-display-tail!
       new-context
       (append (list (cons 'register-border "]"))
	       (if current-line-right
		   (list (cons 'committed current-line-right)))
	       current-display-tail))
      ;; Go to Japanese mode immediately.
      (prime-mode-set context prime-mode-hiragana)
      )))

;; This just returns the empty context between this client and a prime
;; server.  However the prime server may not be initialized yet.  The
;; server will be initialized in prime-context-initialize! after a
;; user turn on the prime mode.
(define prime-init-handler
  (lambda (id im arg)
    (prime-context-new id im)))

(define prime-release-handler
  (lambda (context)
    ;(print "prime-release-handler")
    (let ((session (prime-context-session context)))
      (if session
	  (prime-engine-session-end (prime-context-connection context) session)))
    (let ((connection (prime-context-connection context)))
      (if (pair? connection)
          (begin
            (prime-engine-close connection)
            (prime-context-set-connection! context #f))))
    ))

(define prime-press-key-handler
  (lambda (context key key-state)
    (if (ichar-control? key)
	(im-commit-raw context)
	(let ((keymap (prime-keymap-get-keymap context key key-state)))
	  (prime-proc-call-command keymap context key key-state)
	  (prime-update-key-press context)
	  ))))

(define prime-release-key-handler
  (lambda (context key key-state)
    ;(print "prime-release-key-handler")
    (if (or (ichar-control? key)
	    (= (prime-context-mode context) prime-mode-latin)
	    (= (prime-context-mode context) prime-mode-application))
	(im-commit-raw context)
	;; else
	;; FIXME: update candidate words.
	(prime-update-key-release context)
	)))

(define prime-reset-handler
  (lambda (context)
    ;(print "prime-reset-handler")
    #f))

(define prime-mode-set
  (lambda (context mode)
    (prime-context-set-mode! context mode)
    ;; FIXME: I don't wanna use prime-context-session here.
    ;; FIXME: (2005-02-25) <Hiroyuki Komatsu>
    ;; If the session is #f, the PRIME mode has never been turned on.
    (if (not (prime-context-session context))
        (prime-context-initialize! context))

    (if (prime-context-session context)
	(begin
	  (prime-preedit-reset! context)
	  (prime-update context)
	  ))
    ))

(define prime-mode-language-set
  (lambda (context language)
    (let* ((lang-session-list (prime-context-lang-session-list context))
	   (session (safe-cdr (assoc language lang-session-list))))
      (if (not session)
	  (begin
	    (set! session (prime-engine-session-language-set (prime-context-connection context)
                                                             language))
	    (prime-context-set-lang-session-list!
	     context
	     (cons (cons language session) lang-session-list))))
      (prime-context-set-language! context language)
      (prime-context-set-session!  context session))))

(define prime-get-candidate-handler
  (lambda (context index-no accel-enum-hint)
    (let ((candidate
	   (if (eq? (prime-context-state context) 'prime-state-segment)
	       (nth index-no (prime-context-segment-candidates context))
	       (nth index-no (prime-context-candidates context)))))
      ;; The return value is a list with a candidate string and the next index.
      (list (prime-candidate-get-literal candidate)
	    (digit->string (+ index-no 1))
	    (prime-candidate-combine-annotation context candidate)))))

(define prime-candidate-combine-annotation
  (lambda (context candidate)
    (let ((string     "")
	  (usage      (prime-candidate-get-data    candidate "usage"))
	  (comment    (prime-candidate-get-data    candidate "comment"))
	  (form       (prime-candidate-get-data    candidate "form"))
	  (state      (prime-context-state context)))
      (if (and enable-annotation?
	       prime-custom-display-form?
	       form
	       (or (eq? state 'prime-state-converting)
		   (eq? state 'prime-state-segment)))
	  (set! string (string-append string "(" form ")")))
      (if (and enable-annotation?
	       prime-custom-display-usage?
	       usage
	       (or (eq? state 'prime-state-converting)
		   (eq? state 'prime-state-segment)))
	  (set! string (string-append string "\t▽" usage)))
      (if (and enable-annotation?
	       prime-custom-display-comment?
	       comment
	       (or (eq? state 'prime-state-converting)
		   (eq? state 'prime-state-segment)))
	  (set! string (string-append string "\t<" comment ">")))
      string)))

(define prime-candidate-get-literal
  (lambda (candidate)
    (car candidate)))

(define prime-candidate-get-data
  (lambda (candidate key)
    (safe-car (safe-cdr (assoc key (nth 1 candidate))))))

(define prime-set-candidate-index-handler
  (lambda (context selection-index)
    ;(print "prime-set-candidate-index-handler")
    (if (prime-context-session context)
	(begin
	  (if (eq? (prime-context-state context) 'prime-state-segment)
              (let*
                ((prev (prime-context-segment-nth context))
                 (prev-page (quotient prev prime-nr-candidate-max))
                 (new-page (quotient selection-index prime-nr-candidate-max)))
                (if (= new-page prev-page)
                  (prime-commit-segment-nth context selection-index)
                  (prime-context-set-segment-nth! context selection-index)))
              (let*
                ((prev (prime-context-nth context))
                 (page-limit
                  (if (and (eq? (prime-context-history-compare context) 'state)
                           (eq? (prime-context-state context) 'prime-state-preedit))
                    3
                    prime-nr-candidate-max))
                 (prev-page (quotient prev page-limit))
                 (new-page (quotient selection-index page-limit)))
                (if (= new-page prev-page)
                  (prime-commit-candidate context selection-index)
                  (prime-context-set-nth! context selection-index))))
	  (prime-update context)
	  ))))

(prime-configure-widgets)

(register-im
 'prime                             ;; name
 "ja"                               ;; lang
 "EUC-JP"                           ;; encoding 
 prime-im-name-label                ;; name-label
 prime-im-short-desc                ;; short-dest
 #f                                 ;; init-arg
 prime-init-handler                 ;; init
 prime-release-handler              ;; release
 context-mode-handler               ;; mode
 prime-press-key-handler            ;; key-press
 prime-release-key-handler          ;; key-release
 prime-reset-handler                ;; reset
 prime-get-candidate-handler        ;; get-candidate
 prime-set-candidate-index-handler  ;; set-candidate-index
 context-prop-activate-handler      ;; prop
 #f
 #f
 #f
 #f
 #f
) 
