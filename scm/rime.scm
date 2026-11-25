;;; rime.scm: Chinese input methods of Rime
;;;
;;; Copyright (c) 2026 uim Project https://github.com/uim/uim
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

;; Rime (librime) processes keys and has the composition and the
;; candidates. This only passes keys to it and shows what it has.

(require-extension (srfi 1))

(require "util.scm")
(require-dynlib "rime")
(require-custom "generic-key-custom.scm")
(require-custom "rime-custom.scm")
(require-custom "rime-key-custom.scm")

;;; ------------------------------------------------------------------
;;; context

(define rime-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on          #f)
    (list 'session     #f)
    (list 'schema-id   #f)   ; the last schema that is known
    (list 'preedit     '())  ; segments of the preedit shown now
    ;; ((text comment label) ...) of the page shown now, or ()
    (list 'candidates  '())
    (list 'cand-cursor 0))))
(define-record 'rime-context rime-context-rec-spec)
(define rime-context-new-internal rime-context-new)

;; Whether librime could be started.
(define rime-initialized? #f)

;; ((id . name) ...) of the schemas that librime has. They are known
;; after librime is started.
(define rime-schemas '())

(define (rime-ensure-initialized)
  (if (not rime-initialized?)
      (begin
        (set! rime-initialized?
              (rime-lib-init rime-shared-data-dir rime-user-data-dir))
        (if rime-initialized?
            (begin
              (set! rime-schemas (rime-lib-get-schema-list))
              (rime-refresh-schema-widget)))))
  rime-initialized?)

;; Returns the session of RC, or #f. It's created when it's needed
;; because starting librime may take time.
(define (rime-session rc)
  (or (rime-context-session rc)
      (and (rime-ensure-initialized)
           (let ((session (rime-lib-create-session)))
             (if session
                 (begin
                   (rime-context-set-session! rc session)
                   (set! rime-applied-schema rime-schema)
                   (if (not (string=? rime-schema ""))
                       (rime-lib-select-schema session rime-schema))))
             session))))

;;; ------------------------------------------------------------------
;;; what librime has

;; Returns ((attr . str) ...) for context-update-preedit.
(define (rime-compose-preedit before selected after)
  (filter
   (lambda (segment)
     (or (not (string=? (cdr segment) ""))
         (not (= (bitwise-and (car segment) preedit-cursor) 0))))
   (list (cons preedit-underline before)
         (cons preedit-reverse selected)
         (cons preedit-cursor "")
         (cons preedit-underline after))))

(define (rime-update-preedit rc before selected after)
  (let ((segments (if (and (string=? before "")
                           (string=? selected "")
                           (string=? after ""))
                      '()
                      (rime-compose-preedit before selected after))))
    (if (not (equal? segments (rime-context-preedit rc)))
        (begin
          (rime-context-set-preedit! rc segments)
          (context-update-preedit rc segments)))))

(define (rime-hide-candidates rc)
  (if (not (null? (rime-context-candidates rc)))
      (im-deactivate-candidate-selector rc))
  (rime-context-set-candidates! rc '())
  (rime-context-set-cand-cursor! rc 0))

(define (rime-update-candidates rc candidates cursor)
  (if (null? candidates)
      (rime-hide-candidates rc)
      (let ((n (length candidates))
            ;; librime has only the page shown now. The candidates
            ;; change with the page and as the input grows.
            (changed? (not (equal? (rime-context-candidates rc)
                                   candidates))))
        ;; The frontend gets the candidates when it's activated.
        (rime-context-set-candidates! rc candidates)
        (rime-context-set-cand-cursor! rc cursor)
        (if changed?
            (im-activate-candidate-selector rc n n))
        (im-select-candidate rc cursor))))

;; Commits what librime committed and shows what it composes now.
(define (rime-update rc)
  (let ((session (rime-context-session rc)))
    (if session
        (let ((commit (rime-lib-get-commit session))
              (context (rime-lib-get-context session)))
          (if commit
              (im-commit rc commit))
          (if context
              (begin
                (rime-update-preedit rc
                                     (list-ref context 0)
                                     (list-ref context 1)
                                     (list-ref context 2))
                (rime-update-candidates rc
                                        (list-ref context 4)
                                        (list-ref context 3)))
              (begin
                (rime-update-preedit rc "" "" "")
                (rime-hide-candidates rc)))))))

(define (rime-context-composing? rc)
  (or (not (null? (rime-context-preedit rc)))
      (not (null? (rime-context-candidates rc)))))

;; librime reports the schema ID of the engine of its menu while the
;; menu is shown.
(define rime-menu-schema-id ".default")

(define (rime-menu-shown? session)
  (let ((status (rime-lib-get-status session)))
    (and status
         (string=? (list-ref status 0) rime-menu-schema-id))))

;; Closes the menu of Rime with Escape. librime keeps it even if what
;; is composed is committed or discarded. What was composed before it
;; was shown comes back.
(define (rime-close-menu session)
  (if (rime-menu-shown? session)
      (rime-lib-process-key session
                            (assq-cdr 'escape rime-keysym-alist)
                            0)))

;; Commits what is composed as is.
(define (rime-flush rc)
  (let ((session (rime-context-session rc)))
    (if session
        (begin
          (rime-close-menu session)
          (rime-lib-commit-composition session)
          (rime-update rc)))))

;; Discards what is composed.
(define (rime-reset rc)
  (let ((session (rime-context-session rc)))
    (if session
        (begin
          (rime-close-menu session)
          (rime-lib-clear-composition session)
          (rime-update rc)))))

;;; ------------------------------------------------------------------
;;; keys

;; uim key symbol -> XKB keysym
(define rime-keysym-alist
  '((backspace         . #xff08)
    (tab               . #xff09)
    (return            . #xff0d)
    (escape            . #xff1b)
    (delete            . #xffff)
    (home              . #xff50)
    (left              . #xff51)
    (up                . #xff52)
    (right             . #xff53)
    (down              . #xff54)
    (prior             . #xff55)
    (next              . #xff56)
    (end               . #xff57)
    (insert            . #xff63)
    ;; librime switches to the ASCII mode when one of them is pressed
    ;; and released without another key. A key without a side, which
    ;; is from a bridge that doesn't tell the side, is passed as the
    ;; left one.
    (Shift_key         . #xffe1)
    (Shift_L_key       . #xffe1)
    (Shift_R_key       . #xffe2)
    (Control_key       . #xffe3)
    (Control_L_key     . #xffe3)
    (Control_R_key     . #xffe4)))

;; F1 is #xffbe and F35 is #xffe0.
(define (rime-function-keysym key)
  (let ((name (symbol->string key)))
    (and (> (string-length name) 1)
         (char=? (string-ref name 0) #\F)
         (let ((n (string->number (substring name 1 (string-length name)))))
           (and (integer? n)
                (<= 1 n 35)
                (+ #xffbe (- n 1)))))))

;; Returns the XKB keysym of KEY, or #f.
(define (rime-keysym key key-state)
  (cond
   ((symbol? key)
    (or (assq-cdr key rime-keysym-alist)
        (rime-function-keysym key)))
   ;; Some bridges such as uim-fep pass Ctrl-a as 1.
   ((and (<= 1 key 26) (control-key-mask key-state))
    (+ key 96))
   ((ichar-control? key)
    #f)
   ((< key 256)
    key)
   (else
    (+ #x1000000 key))))

;; The modifier masks of librime.
(define rime-shift-mask   #x1)
(define rime-control-mask #x4)
(define rime-alt-mask     #x8)
(define rime-super-mask   #x4000000)
(define rime-release-mask #x40000000)

(define (rime-mask key-state)
  (bitwise-ior (if (shift-key-mask key-state) rime-shift-mask 0)
               (if (control-key-mask key-state) rime-control-mask 0)
               (if (alt-key-mask key-state) rime-alt-mask 0)
               (if (super-key-mask key-state) rime-super-mask 0)))

;; Sends a key to librime. Returns whether it consumed the key.
(define (rime-send-key rc key key-state release?)
  (let ((session (rime-session rc))
        (keysym (rime-keysym key key-state)))
    (and session
         keysym
         (let ((consumed? (rime-lib-process-key
                           session
                           keysym
                           (bitwise-ior (rime-mask key-state)
                                        (if release? rime-release-mask 0)))))
           (rime-update rc)
           consumed?))))

;;; ------------------------------------------------------------------
;;; modes

(define (rime-set-on rc)
  (rime-context-set-on! rc #t)
  ;; librime starts now.
  (rime-session rc))

(define (rime-set-off rc)
  (rime-flush rc)
  (rime-context-set-on! rc #f))

(register-action 'action_rime_off
                 (lambda (rc)
                   (list
                    'off
                    "-"
                    (N_ "off")
                    (N_ "Direct Input Mode")))
                 (lambda (rc)
                   (not (rime-context-on rc)))
                 (lambda (rc)
                   (rime-set-off rc)))

;; Whether librime is in its ASCII mode. Shift switches it by default.
(define (rime-ascii-mode? rc)
  (let* ((session (rime-context-session rc))
         (status (and session
                      (rime-lib-get-status session))))
    (and status
         (list-ref status 2))))

;; Turns RC on in the ASCII mode or the mode of the schema.
(define (rime-set-ascii-mode rc ascii-mode?)
  (rime-set-on rc)
  (let ((session (rime-context-session rc)))
    (if session
        (begin
          (rime-lib-set-option session "ascii_mode" ascii-mode?)
          (rime-update rc)))))

(register-action 'action_rime_on
                 (lambda (rc)
                   (list
                    'on
                    "中"
                    (N_ "on")
                    (N_ "Rime Input Mode")))
                 (lambda (rc)
                   (and (rime-context-on rc)
                        (not (rime-ascii-mode? rc))))
                 (lambda (rc)
                   (rime-set-ascii-mode rc #f)))

(register-action 'action_rime_ascii
                 (lambda (rc)
                   (list
                    'ascii
                    "A"
                    (N_ "ASCII")
                    (N_ "Rime ASCII Mode")))
                 (lambda (rc)
                   (and (rime-context-on rc)
                        (rime-ascii-mode? rc)))
                 (lambda (rc)
                   (rime-set-ascii-mode rc #t)))

;;; ------------------------------------------------------------------
;;; schemas

;; Returns the ID of the schema that RC uses, or #f. librime doesn't
;; report one of the schemas while it shows its menu, so the last one
;; is kept.
(define (rime-current-schema-id rc)
  (let* ((session (rime-context-session rc))
         (status (and session
                      (rime-lib-get-status session)))
         (id (and status
                  (list-ref status 0))))
    (if (assoc id rime-schemas)
        (rime-context-set-schema-id! rc id))
    (rime-context-schema-id rc)))

;; Switches the session of RC to the schema. What is composed is
;; committed as it's converted, as turning RC off does: librime
;; discards it.
(define (rime-switch-schema rc schema-id)
  (let ((session (rime-context-session rc)))
    (if (and session
             (not (equal? (rime-current-schema-id rc) schema-id)))
        (begin
          (rime-flush rc)
          (rime-lib-select-schema session schema-id)
          (rime-update rc)))))

;; Turns RC on with the schema.
(define (rime-select-schema rc schema-id)
  (rime-set-on rc)
  (rime-switch-schema rc schema-id))

;; rime-schema that the existing sessions follow. It's needed because
;; uim-pref makes the hooks of all customs called even if they aren't
;; changed.
(define rime-applied-schema #f)

;; Switches the existing sessions to rime-schema when it's changed.
;; They keep the schema chosen in a toolbar while it isn't changed.
(define (rime-apply-schema-custom)
  (if (not (equal? rime-schema rime-applied-schema))
      (begin
        (set! rime-applied-schema rime-schema)
        (if (assoc rime-schema rime-schemas)
            (for-each
             (lambda (context)
               (if (eq? (im-name (context-im context)) 'rime)
                   (begin
                     (rime-switch-schema context rime-schema)
                     (context-update-widgets context))))
             context-list)))))

(define (rime-schema-action-id schema-id)
  (string->symbol (string-append "action_rime_schema_" schema-id)))

;; The first character of the name is shown in a toolbar.
(define (rime-schema-indication schema)
  (let ((id (car schema))
        (name (cdr schema)))
    (list (string->symbol (string-append "rime_schema_" id))
          (if (string=? name "")
              "?"
              (substring name 0 1))
          name
          (string-append name " (" id ")"))))

;; librime isn't started yet.
(define rime-schema-unknown-indication
  (list 'rime_schema_unknown
        "-"
        (N_ "Schema")
        (N_ "The schemas of Rime are shown after Rime is turned on")))

;; Registers an action for each schema and returns their IDs.
(define (rime-register-schema-actions)
  (map (lambda (schema)
         (let ((id (car schema))
               (action-id (rime-schema-action-id (car schema)))
               (indication (rime-schema-indication schema)))
           (register-action action-id
                            (lambda (rc)
                              indication)
                            (lambda (rc)
                              (equal? (rime-current-schema-id rc) id))
                            (lambda (rc)
                              (rime-select-schema rc id)))
           action-id))
       rime-schemas))

(define (rime-register-schema-widget)
  (let ((action-ids (rime-register-schema-actions)))
    (register-widget 'widget_rime_schema
                     (indicator-new
                      (lambda (rc)
                        (let ((schema (assoc (rime-current-schema-id rc)
                                             rime-schemas)))
                          (if schema
                              (rime-schema-indication schema)
                              rime-schema-unknown-indication))))
                     (actions-new action-ids))))

;; Shows the schemas that are known now in the existing contexts. Only
;; the widget of the schemas is replaced: creating the other widgets
;; again resets what they show, such as the input mode.
(define (rime-refresh-schema-widget)
  (define (schema-widget? widget)
    (eq? (widget-id widget) 'widget_rime_schema))
  (rime-register-schema-widget)
  (for-each
   (lambda (context)
     (if (and (eq? (im-name (context-im context)) 'rime)
              (find schema-widget? (context-widgets context)))
         (begin
           (context-set-widgets!
            context
            (map (lambda (widget)
                   (if (schema-widget? widget)
                       (widget-new 'widget_rime_schema context)
                       widget))
                 (context-widgets context)))
           (context-propagate-widget-configuration context))))
   context-list))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define (rime-configure-widgets)
  (register-widget 'widget_rime_input_mode
                   (activity-indicator-new rime-input-mode-actions)
                   (actions-new rime-input-mode-actions))
  (rime-register-schema-widget)
  (context-list-replace-widgets! 'rime rime-widgets))

;;; ------------------------------------------------------------------
;;; handlers

(define (rime-init-handler id im arg)
  (let ((rc (rime-context-new-internal id im)))
    (rime-context-set-widgets! rc rime-widgets)
    rc))

(define (rime-release-handler rc)
  (let ((session (rime-context-session rc)))
    (if session
        (begin
          (rime-lib-destroy-session session)
          (rime-context-set-session! rc #f))))
  #f)

(define (rime-press-key-handler rc key key-state)
  (cond
   ((not (rime-context-on rc))
    (if (rime-on-key? key key-state)
        (rime-set-on rc)
        (im-commit-raw rc)))
   ((and (rime-off-key? key key-state)
         (not (rime-context-composing? rc)))
    (rime-set-off rc))
   ((not (rime-send-key rc key key-state #f))
    (im-commit-raw rc))))

(define (rime-release-key-handler rc key key-state)
  (if (not (and (rime-context-on rc)
                (rime-send-key rc key key-state #t)))
      (im-commit-raw rc)))

(define (rime-reset-handler rc)
  (rime-reset rc))

(define (rime-focus-out-handler rc)
  (rime-flush rc))

(define (rime-displace-handler rc)
  (rime-flush rc))

(define (rime-get-candidate-handler rc idx accel-enum-hint)
  (let ((candidates (rime-context-candidates rc)))
    (if (< -1 idx (length candidates))
        (let ((candidate (list-ref candidates idx)))
          (list (list-ref candidate 0)
                (list-ref candidate 2)
                (list-ref candidate 1)))
        (list "" "" ""))))

;; Frontends report a candidate that is chosen with a mouse here.
;; Selecting a candidate in librime commits it or goes to the next
;; part of the input.
(define (rime-set-candidate-index-handler rc idx)
  (let ((session (rime-context-session rc)))
    (if (and session
             (< -1 idx (length (rime-context-candidates rc)))
             (not (= idx (rime-context-cand-cursor rc))))
        (begin
          (rime-lib-select-candidate session idx)
          (rime-update rc)))))

(rime-configure-widgets)

(im-receive-modifier-keys! 'rime)

(register-im
 'rime
 "zh"
 "UTF-8"
 rime-im-name-label
 rime-im-short-desc
 #f
 rime-init-handler
 rime-release-handler
 context-mode-handler
 rime-press-key-handler
 rime-release-key-handler
 rime-reset-handler
 rime-get-candidate-handler
 rime-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 rime-focus-out-handler
 #f
 rime-displace-handler)
