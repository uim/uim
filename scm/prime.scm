;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

(require "util.scm")
(require "japanese.scm")
(require-custom "generic-key-custom.scm")
(require-custom "prime-custom.scm")
;;(require-custom "prime-key-custom.scm")

;; configs

(define prime-engine-command-lookup     "lookup_compact")
;(define prime-engine-command-lookup-all "lookup_compact")
(define prime-engine-command-lookup-all "lookup_compact_all")
;(define prime-engine-command-lookup     "lookup_prefix")
;(define prime-engine-command-lookup-all "lookup_prefix")

;; config function
;; should be replaced with boolean custom variable  -- YamaKen 2005-01-15
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
(define-key prime-typing-mode-hiragana-key?   "F6")
(define-key prime-typing-mode-katakana-key?   "F7")
(define-key prime-typing-mode-hankana-key?    "F8")
(define-key prime-typing-mode-wideascii-key?  "F9")
(define-key prime-typing-mode-ascii-key?      "F10")

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
   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)

   (prime-space-key?        . prime-command-fund-space)
   (prime-altspace-key?     . prime-command-fund-altspace)
   (prime-with-control-key? . prime-command-commit-raw)
   (prime-ja-direct-key?    . prime-command-fund-commit-ja-direct)
   (prime-symbol-key?       . prime-command-commit-raw)
   (prime-any-key?          . prime-command-fund-input)
   ))

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

   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)

;   (prime-with-control-key? . prime-command-commit-raw)
   (prime-with-control-key? . prime-command-pass)
   (prime-symbol-key?       . prime-command-pass)
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

(define prime-keymap-register-conv-state
  '(
   (prime-next-candidate-key? . prime-command-register-conv-next)
   (prime-prev-candidate-key? . prime-command-conv-prev)
   (prime-cancel-key?         . prime-command-conv-cancel)
   (prime-backspace-key?      . prime-command-conv-cancel)
   (prime-commit-key?         . prime-command-register-conv-commit)
   (prime-cand-select-key?    . prime-command-register-conv-select)
   ;; Typing mode key bindings
   (prime-typing-mode-hiragana-key?  . prime-command-mode-hiragana)
   (prime-typing-mode-katakana-key?  . prime-command-mode-katakana)
   (prime-typing-mode-hankana-key?   . prime-command-mode-hankana)
   (prime-typing-mode-wideascii-key? . prime-command-mode-wideascii)
   (prime-typing-mode-ascii-key?     . prime-command-mode-ascii)
   (prime-symbol-key?         . prime-command-pass)
   (prime-with-control-key?   . prime-command-pass)
   (prime-any-key?            . prime-command-register-conv-input)
   ))

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
		     (actions-new prime-input-mode-actions))
    (context-list-replace-widgets! 'prime prime-widgets)))

(define prime-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'state         'prime-state-no-preedit)
    (list 'learning-word #f)
    (list 'nth           0)
    (list 'candidates    ())
    (list 'mode          prime-mode-latin)
    (list 'last-word     "") ;;PRIMEやPOBoxの用語でいうContext
    (list 'session          "")  ; the actual value is -default or -register.
    (list 'session-default  "")
    (list 'session-register "")
    (list 'register-line '(() . ()))
    ;; history = (prev-status, prev-preedition, prev-register-preedtion 
    ;;            index-of-candidate)
    (list 'history       '(prime-state-no-preedit ("" "" "") (() . ()) 0)))))
(define-record 'prime-context prime-context-rec-spec)
(define prime-context-new-internal prime-context-new)

(define prime-context-new
  (lambda (id im)
    (print "prime-context-new")
    (let ((context (prime-context-new-internal id im))
	  (session1 (prime-engine-session-start))
	  (session2 (prime-engine-session-start)))
      (prime-context-set-session!          context session1)
      (prime-context-set-session-default!  context session1)
      (prime-context-set-session-register! context session2)
      context)))

(define prime-context-history-set!
  (lambda (context)
    (prime-context-set-history! context (list
				    (prime-context-state context)
				    (prime-context-copy-preedit-line  context)
				    (prime-context-copy-register-line context)
				    (prime-context-nth context)))))
(define prime-context-history-get prime-context-history)
(define prime-context-history-compare
  (lambda (context)
    (print "prime-context-history-compare")
    (let ((prev-data (prime-context-history-get context)))
      (cond
       ((not (equal? (prime-context-state context) (nth 0 prev-data)))
	'state)
       ((not (equal? (prime-context-get-preedit-line context)  (nth 1 prev-data)))
	'preedit)
       ((not (equal? (prime-context-get-register-line context) (nth 2 prev-data)))
	'cursor)
       ((not (equal? (prime-context-nth context) (nth 3 prev-data)))
	'nth)
       ))))


(define prime-context-reset-register-line!
 (lambda (context)
   (prime-editor-set-left  (prime-context-get-register-line context) '())
   (prime-editor-set-right (prime-context-get-register-line context) '())
   ))
(define prime-context-copy-register-line
  (lambda (context)
    (let ((line (prime-context-get-register-line context)))
      (cons (copy-list (car line)) (copy-list (cdr line)))
      )))
(define prime-context-get-register-line prime-context-register-line)
;; prime-context-set-register-line! is implicitly defined by define-record


(define prime-context-reset-preedit-line!
 (lambda (context)
   (prime-engine-edit-erase (prime-context-session context))))

;; This returns a duplicated list of the current preedition.
(define prime-context-copy-preedit-line
  (lambda (context)
    (copy-list (prime-context-get-preedit-line context))))

;; This returns a list of the current preedition.
;; The structure of the list is [left, cursor, right]. ex. ["ab", "c", "de"].
(define prime-context-get-preedit-line
  (lambda (context)
    (prime-engine-edit-get-preedition (prime-context-session context))))


(define prime-send-command
  (lambda (command)
    (let ((result (prime-lib-send-command command)))
      (let loop ((res result))
 	(if (string=? res "")
 	    (loop (prime-lib-send-command ""))
	    res
 	    )))))

(define prime-preedit-reset!
  (lambda (context)
    (print "prime-preedit-reset!")

    (prime-context-set-state! context 'prime-state-no-preedit)
    (prime-context-reset-preedit-line!  context)
    (prime-context-set-nth! context 0)
    ))

(define prime-get-nth-candidate
  (lambda (context n)
    (print "prime-get-nth-candidate")
    (if (>= n (prime-get-nr-candidates context))
	#f
	(nth 1 (nth n (prime-context-candidates context))))))

;; This returns the data sepecified by key of the N th word.
;; This is called by prime-get-nth-usage and prime-get-nth-annotation.
(define prime-get-nth-word-data
  (lambda (context n key)
    (if (> n (prime-get-nr-candidates context))
	#f
	(cadr (assoc key
		     (nth 2 (nth n (prime-context-candidates context))))))))

(define prime-get-nth-usage
  (lambda (context n)
    (print "prime-get-nth-usage")
    (prime-get-nth-word-data context n "usage")))

(define prime-get-nth-annotation
  (lambda (context n)
    (print "prime-get-nth-annotation")
    (prime-get-nth-word-data context n "annotation")))

(define prime-get-nr-candidates
  (lambda (context)
    (length (prime-context-candidates context))))

(define prime-get-current-candidate
  (lambda (context)
    (print "prime-get-current-candidate")
    (prime-get-nth-candidate context (prime-context-nth context))))

(define prime-get-candidates! ;;もうちょっと関数名をどうにかしたい
  (lambda (context preedit prime-context)
    (print "prime-get-candidates!")
    (prime-engine-set-context prime-context)
    (prime-context-set-candidates!
     context
     (prime-engine-lookup prime-engine-command-lookup preedit))
    ))

(define prime-get-all-candidates! ;;これももうちょっと関数名をどうにかしたい
  (lambda (context preedit prime-context)
    (prime-engine-set-context prime-context)
    (prime-context-set-candidates!
     context
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

;; This splits the string by the separator.  The difference from string-split
;; is the result of spliting "\t\t" by "\t".
;; (string-split "\t\t" "\t") => ().
;; (prime-util-string-split "\t\t" "\t") => ("" "" "").
;; The second argument separator must be a single character string.
(define prime-util-string-split
  (lambda (string separator)
    (if (string? string)
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
	  (reverse result))
	"")))

;;;; ------------------------------------------------------------
;;;; prime-uim:
;;;; ------------------------------------------------------------

(define prime-uim-candwin-get-range
  (lambda (context)
    (let* ((beginning (* (/ (prime-context-nth context) prime-nr-candidate-max)
			 prime-nr-candidate-max))
	   (end       (min (+ beginning prime-nr-candidate-max)
			   (prime-get-nr-candidates context))))
      (cons beginning end))))

;;;; ------------------------------------------------------------
;;;; prime-engine: Functions to connect with a prime server.
;;;; ------------------------------------------------------------

;; Don't append "\n" to arg-list in this function. That will cause a
;; problem with unix domain socket.
(define prime-engine-send-command
  (lambda (arg-list)
    (cdr 
     (string-split
      (prime-send-command
       (prime-util-string-concat arg-list "\t"))
      "\n"))))

(define prime-engine-lookup
  (lambda (command string)
    (print "prime-engine-lookup")
    (mapcar
     (lambda (string-line)
       (let ((word-data (prime-util-string-split string-line "\t")))
	 (list (nth 0 word-data)  ; reading
	       (nth 1 word-data)  ; literal
	       (prime-util-assoc-list (nthcdr 2 word-data)))))
     (prime-engine-send-command (list command string)))))

(define prime-engine-set-context
  (lambda (prime-context)
    (if (string=? prime-context "")
	(prime-engine-reset-context)
	(prime-engine-send-command (list "set_context" prime-context)))))

(define prime-engine-reset-context
  (lambda ()
    (prime-engine-send-command (list "reset_context"))))

;; session operations
(define prime-engine-session-start
  (lambda ()
    (car (prime-engine-send-command (list "session_start")))))

(define prime-engine-session-end
  (lambda (prime-session)
    (prime-engine-send-command (list "session_end" prime-session))))

;; composing operations
(define prime-engine-edit-insert
  (lambda (prime-session string)
    (prime-engine-send-command (list "edit_insert"    prime-session string))))
(define prime-engine-edit-delete
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_delete"    prime-session))))
(define prime-engine-edit-backspace
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_backspace" prime-session))))
(define prime-engine-edit-erase
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_erase"     prime-session))))

;; cursor operations
(define prime-engine-edit-cursor-left
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_cursor_left" prime-session))))
(define prime-engine-edit-cursor-right
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_cursor_right" prime-session))))
(define prime-engine-edit-cursor-left-edge
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_cursor_left_edge" prime-session))))
(define prime-engine-edit-cursor-right-edge
  (lambda (prime-session)
    (prime-engine-send-command (list "edit_cursor_right_edge" prime-session))))

;; preedition-getting operations
(define prime-engine-edit-get-preedition
  (lambda (prime-session)
    (prime-util-string-split (car (prime-engine-send-command
				   (list "edit_get_preedition" prime-session)))
			     "\t")))
(define prime-engine-edit-get-query-string
  (lambda (prime-session)
    (car (prime-engine-send-command
	  (list "edit_get_query_string" prime-session)))))

;; mode operations
(define prime-engine-edit-set-mode
  (lambda (prime-session mode)
    (prime-engine-send-command (list "edit_set_mode" prime-session mode))))

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
	   (else
 	    (prime-util-string-split conversion "\t")))))))

(define prime-engine-learn-word
  (lambda (pron literal pos context suffix rest)
    (prime-engine-send-command (list "learn_word"
				     pron literal pos context suffix rest))))

;; This returns a version string of the PRIME server.
(define prime-engine-get-version
  (lambda ()
    (car (prime-engine-send-command '("get_version")))))

(define prime-engine-get-env
  (lambda (env-name)
;    (print "prime-engine-get-env")
    (let* ((result (prime-util-string-split
		    (car (prime-engine-send-command (list "get_env" env-name)))
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

;; This changes the typing mode specified by mode-string.
(define prime-mode-set-mode
  (lambda (context mode-string)
    (print "prime-mode-set-mode")
    (if (= (prime-context-state context) 'prime-state-converting)
	(prime-convert-cancel context))
    (prime-engine-edit-set-mode (prime-context-session context) mode-string)))
    
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
    (prime-register-mode-on context)))


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
	(prime-context-set-nth! context
				(- (prime-context-nth context) 1))
	(prime-context-set-nth! context
				(- (prime-get-nr-candidates context) 1)))
    ))

(define prime-command-conv-cancel
  (lambda (context key key-state)
    (prime-convert-cancel context)))

(define prime-command-conv-commit
  (lambda (context key key-state)
    (print "prime-command-conv-commit")
    (prime-commit-candidate context (prime-context-nth context))
    ))

(define prime-command-register-conv-commit
  (lambda (context key key-state)
    (print "prime-command-register-conv-commit")
    (prime-commit-to-register-buffer context
				     (prime-get-current-candidate context))
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
    (prime-engine-edit-erase (prime-context-session context))))

(define prime-command-preedit-backspace
  (lambda (context key key-state)
    (prime-engine-edit-backspace (prime-context-session context))))

(define prime-command-preedit-delete
  (lambda (context key key-state)
    (prime-engine-edit-delete (prime-context-session context))))

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
    (prime-engine-edit-cursor-left-edge (prime-context-session context))))

(define prime-command-preedit-cursor-right-edge
  (lambda (context key key-state)
    (prime-engine-edit-cursor-right-edge (prime-context-session context))))

(define prime-command-preedit-cursor-left
  (lambda (context key key-state)
    (prime-engine-edit-cursor-left (prime-context-session context))))

(define prime-command-preedit-cursor-right
  (lambda (context key key-state)
    (prime-engine-edit-cursor-right (prime-context-session context))))

(define prime-command-preedit-input
  (lambda (context key key-state)
    (print "prime-command-preedit-input")
    (prime-engine-edit-insert (prime-context-session context)
			      (charcode->string key))))

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
    (prime-register-mode-off context)

    (prime-context-set-nth! context 0)
    (prime-context-set-state! context 'prime-state-preedit)
    ))

;; This registers the specified word to the PRIME dictionary and
;; reset the status to the normal fund mode.
(define prime-command-register-fund-commit
  (lambda (context key key-state)
    (print "prime-command-register-fund-commit")
    (let* ((learning-word (prime-context-learning-word context))
	   (registered    (prime-register-get-string-label context)))
      (if (not (string=? registered ""))
	  (let ((word-data (list (list "basekey" learning-word)
				 (list "base"    registered))))
	    (prime-commit-word-data context word-data)
	    (prime-register-mode-off context)
	    (prime-command-preedit-cancel context key key-state))))
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
    (print "prime-proc-call-command")
    (let ((command (prime-keymap-get-command keymap key key-state)))
      (if command
	  (begin
	    ((symbol-value command) context key key-state)
	    #t)
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


;; This returns a preediting string.
(define prime-preedit-get-string-label
  (lambda (context)
    (apply string-append (prime-engine-edit-get-preedition
			  (prime-context-session context)))))

;; This returns #t if the preediting string is not empty.  Or #f.
(define prime-preedit-exist?
  (lambda (context)
    (> (length (prime-preedit-get-string-label context)) 0)))

;; This returns a query string for PRIME server.
(define prime-preedit-get-string-raw
  (lambda (context)
    (prime-engine-edit-get-query-string (prime-context-session context))))

;; This returns a commited string of register mode.
(define prime-register-get-string-label
  (lambda (context)
    (let ((line (prime-context-get-register-line context)))
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
  (lambda (context)
    (print "prime-commit-raw")
    (im-commit-raw context)
    (prime-context-set-last-word! context "")
    (prime-preedit-reset! context)
    ))

(define prime-commit-without-learning
  (lambda (context word)
    (im-commit context word)
    (prime-context-set-last-word! context "")
    ))

(define prime-commit-word-data
  (lambda (context word-data)
    (print "prime-commit-word-data")
    (im-commit context
	       (string-append (or (cadr (assoc "base"        word-data)) "")
			      (or (cadr (assoc "conjugation" word-data)) "")
			      (or (cadr (assoc "suffix"      word-data)) "")))
    (prime-learn-word context word-data)
    (prime-preedit-reset! context)))

(define prime-commit-candidate
  (lambda (context n)
    (print "prime-commit-candidate")
    (let ((word-data
	   (nth 2 (nth n (prime-context-candidates context)))))
      (prime-commit-word-data context word-data))))

(define prime-commit-to-register-buffer
  (lambda (context word)
    (let ((line (prime-context-get-register-line context)))
      (prime-editor-set-left line (append (string-to-list word)
					  (prime-editor-get-left line)))
      (prime-preedit-reset! context)
      )))

;;;; ------------------------------------------------------------

(define prime-learn-word
  (lambda (context assoc-list)
    (print "prime-learn-word")
    (let ((key     (or (cadr (assoc "basekey"     assoc-list)) ""))
	  (value   (or (cadr (assoc "base"        assoc-list)) ""))
	  (part    (or (cadr (assoc "part"        assoc-list)) ""))
	  (prime-context (or (prime-context-last-word context) ""))
	  (suffix  (or (cadr (assoc "conjugation" assoc-list)) ""))
	  (rest    (or (cadr (assoc "suffix"      assoc-list)) "")))
      
      (prime-engine-learn-word key value part prime-context suffix rest)
      (prime-context-set-last-word! context
				    (string-append value suffix rest))
      )))

;; This function moves the cursor of candidate words.  If the cursor is out of
;; the range and the variable prime-auto-register-mode? is #t, the mode is
;; changed to register-mode.
(define prime-convert-selection-move
  (lambda (context selection-index)
    (prime-context-set-nth! context selection-index)
    (if (prime-get-current-candidate context)
	#f
	(if prime-auto-register-mode?
	    (prime-register-mode-on context)
	    (prime-context-set-nth! context 0)))
    ))

;; This resets the converting mode and goes to the preediting mode.
(define prime-convert-cancel
  (lambda (context)
    (print "prime-convert-cancel")
    (prime-context-set-state! context 'prime-state-preedit)
    (prime-context-set-nth! context 0)))


(define prime-begin-conversion-internal
  (lambda (context init-idx)
    (print "prime-begin-conversion-internal")
    (let ((res))
      (prime-get-all-candidates! context
				 (prime-preedit-get-string-raw context)
				 (prime-context-last-word context))
      (set! res (prime-get-nth-candidate context init-idx))
      (print res)
      (if res
	  (begin
	    (prime-context-set-nth!   context init-idx)
	    (prime-context-set-state! context 'prime-state-converting))
	  )
      (prime-convert-selection-move context init-idx)
      )))

(define prime-begin-conversion-reversely
  (lambda (context)
    (let ((last-idx (- (prime-get-nr-candidates context)
		       1)))
      (prime-begin-conversion-internal context last-idx))))

(define prime-begin-conversion
  (lambda (context)
    (prime-begin-conversion-internal context 0)))


;;;; ------------------------------------------------------------
;;;; prime-commit
;;;; ------------------------------------------------------------

(define prime-update
  (lambda (context)
    (print "prime-update")
    (print (prime-context-state context))

    (prime-update-state context)
    (prime-update-prediction context)
    
    (prime-update-candidate-window context)
    (prime-update-preedit context)

    (prime-update-history context)
    ))

(define prime-update-state
  (lambda (context)
    (if (not (prime-preedit-exist? context))
	(begin
	  (print "  prime-update-state: set-state no-preedit")
	  (prime-context-set-state! context 'prime-state-no-preedit)))
    ))

(define prime-update-history
  (lambda (context)
    (print "prime-update-history")
    (prime-context-history-set! context)))

(define prime-update-preedit
  (lambda (context)
    (print "prime-update-preedit")

    (if (prime-context-history-compare context)
	(let ((learning-word (prime-context-learning-word context)))
	  (im-clear-preedit context)
	  (prime-display-preedit
	   context
	   (if learning-word
	       (prime-register-state-update-preedit context)
	       (prime-preedit-state-update-preedit  context)))
	  (im-update-preedit context)
	  ))
    ))

(define prime-register-state-update-preedit
  (lambda (context)
    (print "prime-register-state-update-preedit")
    (let* ((learning-word  (prime-context-learning-word     context))
	   (line           (prime-context-get-register-line context))
	   (register-left  (prime-editor-get-left           line))
	   (register-right (reverse (prime-editor-get-right line))))
      (append
       (list
	(cons 'register-label  "単語登録")
	(cons 'register-border "[")
	(cons 'register-word   learning-word)
	(cons 'register-border "|")
	(cons 'committed (string-list-concat register-left)))

       (prime-preedit-state-update-preedit context)
       (list
	(cons 'committed (string-list-concat register-right))
	(cons 'register-border "]"))))))

(define prime-preedit-state-update-preedit
  (lambda (context)
    (print "prime-preedit-state-update-preedit")
    (let* ((state (prime-context-state            context))
	   (line  (prime-context-get-preedit-line context))
	   (left  (car line))
	   (right (apply string-append (cdr line)))
	   )
      (cond
       ((= state 'prime-state-converting)
	(list (cons 'converting (prime-get-current-candidate context))))

       ((prime-preedit-exist? context)
	(list (cons 'preedit left)
	      (cons 'cursor "")
	      (cons 'preedit right)))
       (else
	(list (cons 'cursor "")))))))

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
  (lambda (context preedit-list)
    (if preedit-list
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
	     (eq? (prime-context-state context) 'prime-state-no-preedit))
	(im-pushback-preedit context
			     (cdr (assoc 'pseudo-cursor
					 prime-display-preedit-format))
			     " "))
    ))

(define prime-update-prediction
  (lambda (context)
    (print "prime-update-prediction")
    (let ((diff (prime-context-history-compare context)))
      (cond
       ((= diff 'state)
	(let ((state     (prime-context-state context))
	      (last-word (prime-context-last-word context)))
	  (cond
	   ((= state 'prime-state-preedit)
	    (prime-get-candidates! context
				   (prime-preedit-get-string-raw context)
				   (prime-context-last-word context)))
	   ((= state 'prime-state-converting)
	    (prime-get-all-candidates! context
				       (prime-preedit-get-string-raw context)
				       (prime-context-last-word context)))
	   ((= state 'prime-state-no-preedit)
	    (prime-context-set-candidates! context '()))
	    )))
       ((= diff 'preedit)
	(prime-get-candidates! context
			       (prime-preedit-get-string-raw context)
			       (prime-context-last-word context)))
       ))))

(define prime-update-candidate-window
  (lambda (context)
    (print "prime-update-candidate-window")
    (let ((diff (prime-context-history-compare context)))
      (cond
       ((= diff 'state)
	(let ((state (prime-context-state context)))
	  (cond
	   ((= state 'prime-state-no-preedit)
	    (im-deactivate-candidate-selector context))
	   ((= state 'prime-state-preedit)
	    (if (> (prime-get-nr-candidates context) 0)
		(im-activate-candidate-selector
		 context
		 (prime-get-nr-candidates context)
		 3)))
;		 prime-nr-candidate-max)))
	   ((= state 'prime-state-converting)
 	    (im-activate-candidate-selector
 	     context (prime-get-nr-candidates context) prime-nr-candidate-max)
	    (im-select-candidate context (prime-context-nth context)))
	    )))
       ((= diff 'nth)
	(im-select-candidate context (prime-context-nth context)))
       ((= diff 'preedit)
	(if (> (prime-get-nr-candidates context) 0)
	    (im-activate-candidate-selector
	     context (prime-get-nr-candidates context) prime-nr-candidate-max)
	    (im-deactivate-candidate-selector context)))
       ))))

;;;; ------------------------------------------------------------

(define prime-register-mode-on
  (lambda (context)
    (print "prime-register-mode-on")
    (prime-context-set-learning-word! context
				      (prime-preedit-get-string-label context))
    (prime-context-set-session! context
				(prime-context-session-register context))
    ))

(define prime-register-mode-off
  (lambda (context)
    (prime-context-reset-register-line! context)
    (prime-context-set-learning-word!   context #f)
    (prime-context-set-session! context
				(prime-context-session-default context))
    ))

(define prime-init-handler
  (lambda (id im arg)
    (if (prime-lib-init prime-use-unixdomain?)
	(let ((context (prime-context-new id im)))
	  (prime-custom-init)
	  context)
	#f)))

(define prime-release-handler
  (lambda (context)
    (prime-engine-session-end (prime-context-session-default  context))
    (prime-engine-session-end (prime-context-session-register context))
    ))

(define prime-press-key-handler
  (lambda (context key state)
    (if (control-char? key)
	(im-commit-raw context)
	(prime-push-key context key state))))

(define prime-release-key-handler
  (lambda (context key state)
    (if (or (control-char? key)
	    (= (prime-context-mode context)
	       prime-mode-latin))
	(im-commit-raw context))))

(define prime-reset-handler
  (lambda (context)
    (print "prime-reset-handler")
    ))

(define prime-mode-set
  (lambda (context mode)
    (prime-context-set-mode! context mode)
    (prime-preedit-reset! context)
    (prime-update context)
    ))

(define prime-get-candidate-handler
  (lambda (context idx accel-enum-hint)
    (let* ((cand       (prime-get-nth-candidate  context idx))
	   (usage      (prime-get-nth-usage      context idx))
	   (annotation (prime-get-nth-annotation context idx)))
      (if (and prime-char-annotation?
	       annotation
	       (= (prime-context-state context) 'prime-state-converting))
	  (set! cand (string-append cand "  (" annotation ")")))
      (if (and prime-custom-display-usage?
	       usage
	       (= (prime-context-state context) 'prime-state-converting))
	  (set! cand (string-append cand "\t▽" usage)))

      ;; The return value is a list with a candidate string and the next index.
      (list cand (digit->string (+ idx 1))))))

(define prime-set-candidate-index-handler
  (lambda (context selection-index)
    (print "prime-set-candidate-index-handler")
    (prime-convert-selection-move context selection-index)
    (prime-update context)
    ))

(prime-configure-widgets)

(register-im
 'prime
 "ja"
 "EUC-JP"
 prime-im-name-label
 prime-im-short-desc
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
