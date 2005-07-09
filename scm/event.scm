;;; event.scm: Event definitions
;;;
;;; Copyright (c) 2004-2005 uim Project http://uim.freedesktop.org/
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

;; These events will cooperate with the composer framework which will
;; be appeared as composer.scm to enable flexible input method
;; component organization such as nested composer (input method) based
;; on loose relationships.  -- YamaKen 2005-02-18

(require "util.scm")
(require "utext.scm")
(require "ng-key.scm")

;;
;; event definitions
;;

;; TODO: register by define-event
(define valid-event-types
  '(unknown
    client-info
    reset
    focus-in
    focus-out
    key
    insert

    commit
    preedit-updated

    timer
    timer-set

    action
    chooser
    chooser-update-req
    chooser-update
    ))

(define event-rec-spec
  '((type unknown)))

(define upward-event-rec-spec event-rec-spec)
(define-record 'upward-event upward-event-rec-spec)

(define downward-event-rec-spec
  (append
   event-rec-spec
  '(;;(context-id -1)
    (consumed  #f)
    (loopback  #f)    ;; instructs re-injection into local composer
    (timestamp -1)    ;; placeholder
    (ext-state #f))))
;; use 'event' instead of 'downward-event' as record name for convenient use
(define-record 'event downward-event-rec-spec)

;; TODO
;;(define define-event
;;  (lambda (name spec)
;;    ))

(define event-external-state
  (lambda (ev state-id)
    (let ((state-reader (event-ext-state ev)))
      (and (procedure? state-reader)
	   (state-reader state-id)))))

(define-record 'timer-event
  downward-event-rec-spec)

(define-record 'reset-event
  downward-event-rec-spec)

(define-record 'focus-in-event
  downward-event-rec-spec)

(define-record 'focus-out-event
  downward-event-rec-spec)

(define-record 'client-info-event
  (append
   downward-event-rec-spec
   '((locale      #f)
     (bridge      "")     ;; "gtk", "uim-xim", "macuim", "scim-uim", ...
     (application "")     ;; acquire via bridge-dependent methods such as basename `echo $0`
     (expected    ""))))  ;; "direct", "number", "upper-alphabet", "ja-hiragana", ...

;; inserts a text into active IM context
;; For example, this can be used to insert a kanji word via clipboard
;; to register new dictionary entry
(define-record 'insert-event
  (append
   downward-event-rec-spec
   '((utext ()))))  ;; can include cursor position info

(define-record 'commit-event
  (append
   upward-event-rec-spec
   '((utext           ())    ;; can include cursor position info
     (preedit-updated #t)    ;; can also update preedit as atomic event
     (former-del-len  0)     ;; for surrounding text operation
     (latter-del-len  0))))  ;; for surrounding text operation

(define-record 'preedit-updated-event
  (append
   upward-event-rec-spec))

(define-record 'action-event
  (append
   downward-event-rec-spec
   '((action-id #f))))  ;; 'action_input_mode_direct

(define-record 'chooser-event
  (append
   downward-event-rec-spec
   '((chooser-id #f)  ;; 'chooser_candidate_selector
     (chosen     -1)  ;; negative value means that nothing is chosen
     (confirm    #t)  ;; finish current choice transaction
     (scope-top  -1))))

(define-record 'chooser-update-req-event
  (append
   downward-event-rec-spec
   '((chooser-id   #f)  ;; 'chooser_candidate_selector 'chooser_all etc.
     (initialize   #f)
     (items-top    -1)
     (nr-items     -1))))

(define-record 'chooser-update-event
  (append
   upward-event-rec-spec
   '((chooser-id          #f)
     (initialize          #f)  ;; invalidate all cached info about the chooser
     (transition          #f)  ;; 'activate 'deactivate #f
     (chooser-size        -1)  ;; number of items including hidden ones
     (chosen              -1)  ;; item index currently chosen
     (scope-top           -1)  ;; 
     (scope-size-hint     -1)  ;; number of items displayable at a time
     (title               #f)  ;; indication
     (status              #f)  ;; indication
     (updated-items-top   -1)
     (updated-items       ()))))  ;; list of indications

;; #f means "don't care" for lkey, pkey, str, press and autorepeat
;; when comparing with other key-event. But modifiers require exact
;; match.
(define-record 'key-event
  (append
   downward-event-rec-spec
   (list
    ;;(list text       #f)        ;; replace raw string with utext in future
    (list 'str        #f)        ;; precomposed string
    (list 'lkey       #f)        ;; logical keysym
    (list 'pkey       #f)        ;; physical keysym
    (list 'modifier   mod_None)  ;; set of modifiers
    (list 'press      #t)        ;; indicates press/release
    (list 'autorepeat #f))))     ;; whether generated by autorepeat or not
(define key-event-new-internal key-event-new)

(define key-event-new
  (lambda args
    (apply key-event-new-internal
	   (append (event-new 'key) args))))

(define key-release-event-new
  (lambda args
    (let ((ev (apply key-event-new args)))
      (key-event-set-press! ev #f)
      ev)))

;; TODO: make encoding sensitive
(define key-event-char
  (lambda (ev)
    (let ((str (key-event-str ev)))
      (and (string? str)
	   (string->char str)))))

(define key-event-extract-press-str
  (lambda (ev)
    (and (key-event-press ev)
	 (key-event-str ev))))

(define key-event-char-upcase!
  (lambda (ev)
    (let ((str ((compose charcode->string
			 char-upcase
			 key-event-char)
		ev)))
      (key-event-set-str! ev str))))

(define key-event-char-downcase!
  (lambda (ev)
    (let ((str ((compose charcode->string
			 char-downcase
			 key-event-char)
		ev)))
      (key-event-set-str! ev str))))

;; TODO: write test
(define key-event-covers?
  (lambda (self other)
    (and (every (lambda (getter)
		  (let ((self-val (getter self))
			(other-val (getter other)))
		    (and self-val ;; #f means "don't care"
			 (equal? self-val other-val))))
		(list key-event-lkey
		      key-event-pkey
		      key-event-str))
	 (modifier-match? (key-event-modifier self)
			  (key-event-modifier other))
	 ;; exact matches
	 (every (lambda (getter)
		  (equal? (getter self)
			  (getter other)))
		(list key-event-press
		      key-event-autorepeat)))))

;; TODO: write test
(define key-event-inspect
  (lambda (ev)
    (string-append
     (if (key-event-str ev)
	 (string-append "\"" (key-event-str ev) "\"")
	 "-")
     " "
     (symbol->string (or (key-event-lkey ev)
			 '-))
     " "
     (symbol->string (or (key-event-pkey ev)
			 '-))
     " ("
     (string-join
      " "
      (filter-map (lambda (mod-sym)
		    (and (not (= (bitwise-and (symbol-value mod-sym)
					      (key-event-modifier ev))
				 0))
			 (symbol->string mod-sym)))
		  valid-modifiers))
     ") "
     (if (key-event-press ev)
	 "press"
	 "release")
     " "
     (if (key-event-autorepeat ev)
	 "autorepeat"
	 "nonrepeat")
     " "
     (if (event-consumed ev)
	 "consumed"
	 "not-consumed")
     "\n")))

(define key-event-print-inspected
  (lambda (msg ev)
    (if inspect-key-event-translation?
	(puts (string-append msg
			     (key-event-inspect ev))))))
