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

;;; Karukan (https://github.com/togatoga/karukan) is a Japanese input
;;; method with neural kana-kanji conversion. This module drives
;;; karukan-imserver, which speaks JSON-RPC 2.0 a line at a time over
;;; its standard input and output, so it needs no native plugin.
;;;
;;; karukan-imserver holds one input state. It's shared by all
;;; contexts of a process: the context that has the state commits what
;;; it composes when it loses the focus or another context takes the
;;; state over.

(require-extension (srfi 1 95 180))

(require "util.scm")
(require "process.scm")
(require "fileio.scm")
(require "ustr.scm")
(require-custom "generic-key-custom.scm")
(require-custom "karukan-custom.scm")
(require-custom "karukan-key-custom.scm")

;;; ------------------------------------------------------------------
;;; server

(define karukan-server-rec-spec
  '((in-port #f)    ; file port to read responses
    (out-fd  #f)    ; fd to write requests
    (next-id 1)
    (owner   #f)))  ; the context that has the input state
(define-record 'karukan-server karukan-server-rec-spec)

;; The server process is shared by all contexts.
(define karukan-server #f)
;; Not to notify a failure at every key.
(define karukan-server-failed? #f)

;; The protocol version that this module speaks.
(define karukan-protocol-version 1)

(define (karukan-json-ref key object)
  (assq-cdr key object))

(define (karukan-json-array-ref key object)
  (let ((array (karukan-json-ref key object)))
    (if (vector? array)
        (vector->list array)
        '())))

(define (karukan-json-string-ref key object)
  (let ((value (karukan-json-ref key object)))
    (if (string? value)
        value
        "")))

(define (karukan-to-json object)
  (call-with-output-string
   (lambda (port)
     (json-write object port))))

(define (karukan-from-json line)
  (guard (err (#t #f))
    (call-with-input-string line json-read)))

(define (karukan-server-close server)
  (guard (err (#t #f))
    (close-file-port (karukan-server-in-port server)))
  (guard (err (#t #f))
    (file-close (karukan-server-out-fd server)))
  (if (eq? karukan-server server)
      (set! karukan-server #f)))

(define (karukan-server-lost server)
  ;; karukan-server-ensure notifies a failure to start.
  (if (eq? karukan-server server)
      (uim-notify-fatal (_ "uim-karukan: karukan-imserver is gone")))
  (karukan-server-close server))

(define (karukan-server-write server message)
  (if (< (file-write-string (karukan-server-out-fd server)
                            (string-append (karukan-to-json message) "\n"))
         0)
      (begin
        (karukan-server-lost server)
        #f)
      #t))

;; Sends a request and returns its result, or #f on failure.
(define (karukan-server-request server method params)
  (let ((id (karukan-server-next-id server)))
    (karukan-server-set-next-id! server (+ id 1))
    (and (karukan-server-write server
                               (list (cons 'jsonrpc "2.0")
                                     (cons 'id id)
                                     (cons 'method method)
                                     (cons 'params params)))
         (let loop ()
           (let* ((line (file-read-line (karukan-server-in-port server)))
                  (response (and (string? line)
                                 (karukan-from-json line))))
             (cond
              ((not (list? response))
               (karukan-server-lost server)
               #f)
              ;; The response to a request that failed before.
              ((not (eqv? (karukan-json-ref 'id response) id))
               (loop))
              ((karukan-json-ref 'error response)
               => (lambda (error)
                    (uim-notify-info
                     (format "uim-karukan: ~a"
                             (karukan-json-string-ref 'message error)))
                    #f))
              (else
               (karukan-json-ref 'result response))))))))

;; Sends a notification, which has no response.
(define (karukan-server-notify server method params)
  (karukan-server-write server
                        (list (cons 'jsonrpc "2.0")
                              (cons 'method method)
                              (cons 'params params))))

(define (karukan-server-start)
  ;; karukan-imserver logs to the standard error, which is the
  ;; application's one.
  (let* ((argv (if (getenv "RUST_LOG")
                   (list karukan-server-command)
                   (list "env" "RUST_LOG=warn" karukan-server-command)))
         (fds (process-io (car argv) argv)))
    (and fds
         (let ((server (karukan-server-new)))
           (karukan-server-set-in-port! server (open-file-port (car fds)))
           (karukan-server-set-out-fd! server (cdr fds))
           ;; The models are loaded in the background.
           (let ((result (karukan-server-request server "init" '())))
             (cond
              ((not result)
               (karukan-server-close server)
               #f)
              ((not (eqv? (karukan-json-ref 'protocol_version result)
                          karukan-protocol-version))
               (uim-notify-fatal
                (format (_ "uim-karukan: unsupported protocol version ~a of ~a")
                        (karukan-json-ref 'protocol_version result)
                        karukan-server-command))
               (karukan-server-close server)
               #f)
              (else
               server)))))))

;; Returns the server, starting it if needed, or #f.
(define (karukan-server-ensure)
  (or karukan-server
      (and (not karukan-server-failed?)
           (let ((server (karukan-server-start)))
             (if server
                 (set! karukan-server server)
                 (begin
                   (set! karukan-server-failed? #t)
                   (uim-notify-fatal
                    (format (_ "uim-karukan: cannot start ~a")
                            karukan-server-command))))
             server))))

;;; ------------------------------------------------------------------
;;; context

(define karukan-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on          #f)
    (list 'preedit     '())  ; segments of the preedit shown now
    ;; ((text . description) ...) of the page shown now, or ()
    (list 'candidates  '())
    (list 'cand-cursor 0))))
(define-record 'karukan-context karukan-context-rec-spec)
(define karukan-context-new-internal karukan-context-new)

(define (karukan-context-composing? kc)
  (or (not (null? (karukan-context-preedit kc)))
      (not (null? (karukan-context-candidates kc)))))

(define (karukan-context-owner? kc)
  (and karukan-server
       (eq? (karukan-server-owner karukan-server) kc)))

;;; ------------------------------------------------------------------
;;; actions from the server

(define (karukan-style-attr style)
  (cond
   ((member style '("underline" "underline_double"))
    preedit-underline)
   ((member style '("highlight" "reverse"))
    (bitwise-ior preedit-reverse preedit-cursor))
   (else
    preedit-none)))

;; Returns ((attr . str) ...) for context-update-preedit. Positions are
;; in characters.
(define (karukan-compose-preedit text caret attributes)
  (let* ((length (string-length text))
         (caret (max 0 (min caret length)))
         (ranges (filter-map
                  (lambda (attribute)
                    (let ((start (karukan-json-ref 'start attribute))
                          (end (karukan-json-ref 'end attribute)))
                      (and (integer? start)
                           (integer? end)
                           (list (max 0 (min start length))
                                 (max 0 (min end length))
                                 (karukan-style-attr
                                  (karukan-json-ref 'style attribute))))))
                  attributes))
         (highlighted? (any (lambda (range)
                              (= (bitwise-and (car (cddr range))
                                              preedit-reverse)
                                 preedit-reverse))
                            ranges))
         (boundaries (delete-duplicates
                      (sort (append (list 0 length)
                                    (if highlighted? '() (list caret))
                                    (append-map (lambda (range)
                                                  (list (car range)
                                                        (cadr range)))
                                                ranges))
                            <)
                      =)))
    (define (attr-at position)
      (fold (lambda (range attr)
              (if (and (<= (car range) position)
                       (< position (cadr range)))
                  (bitwise-ior attr (car (cddr range)))
                  attr))
            preedit-none
            ranges))
    (let loop ((boundaries boundaries)
               (segments '()))
      (if (or (null? boundaries)
              (null? (cdr boundaries)))
          (reverse (if (and (not highlighted?) (= caret length))
                       (cons (cons preedit-cursor "") segments)
                       segments))
          (let* ((start (car boundaries))
                 (end (cadr boundaries))
                 (segments (if (and (not highlighted?) (= start caret))
                               (cons (cons preedit-cursor "") segments)
                               segments)))
            (loop (cdr boundaries)
                  (cons (cons (attr-at start) (substring text start end))
                        segments)))))))

(define (karukan-update-preedit kc action)
  (let ((text (karukan-json-string-ref 'text action))
        (caret (karukan-json-ref 'caret action)))
    (if (string=? text "")
        (karukan-clear-preedit kc)
        (let ((segments (karukan-compose-preedit
                         text
                         (if (integer? caret) caret (string-length text))
                         (karukan-json-array-ref 'attributes action))))
          (karukan-context-set-preedit! kc segments)
          (context-update-preedit kc segments)))))

(define (karukan-show-candidates kc action)
  (let ((candidates (map (lambda (candidate)
                           (cons (karukan-json-string-ref 'text candidate)
                                 (karukan-json-string-ref 'description
                                                          candidate)))
                         (karukan-json-array-ref 'candidates action))))
    (if (null? candidates)
        (karukan-hide-candidates kc)
        (let ((n (length candidates))
              (cursor (or (karukan-json-ref 'cursor action) 0))
              ;; The server sends only the page shown now. The
              ;; candidates change with the page and as the reading
              ;; grows.
              (changed? (not (equal? (karukan-context-candidates kc)
                                     candidates))))
          ;; The frontend gets the candidates when it's activated.
          (karukan-context-set-candidates! kc candidates)
          (karukan-context-set-cand-cursor! kc cursor)
          (if changed?
              (im-activate-candidate-selector kc n n))
          (im-select-candidate kc cursor)))))

(define (karukan-hide-candidates kc)
  (if (not (null? (karukan-context-candidates kc)))
      (im-deactivate-candidate-selector kc))
  (karukan-context-set-candidates! kc '())
  (karukan-context-set-cand-cursor! kc 0))

(define (karukan-apply-action kc action)
  (let ((type (karukan-json-string-ref 'type action)))
    (cond
     ((string=? type "update_preedit")
      (karukan-update-preedit kc action))
     ((string=? type "show_candidates")
      (karukan-show-candidates kc action))
     ((string=? type "hide_candidates")
      (karukan-hide-candidates kc))
     ((string=? type "commit")
      ;; karukan-imserver doesn't clear the preedit with a commit.
      (karukan-clear-preedit kc)
      (let ((text (karukan-json-string-ref 'text action)))
        (if (not (string=? text ""))
            (im-commit kc text))))
     ;; The auxiliary text such as the conversion time has no place in
     ;; uim.
     (else
      #f))))

;; Applies the actions of RESULT. Returns whether the server consumed
;; the key.
(define (karukan-apply kc result)
  (for-each (lambda (action)
              (karukan-apply-action kc action))
            (karukan-json-array-ref 'actions result))
  (eq? (karukan-json-ref 'consumed result) #t))

;;; ------------------------------------------------------------------
;;; input state

;; Commits what KC composes.
(define (karukan-flush kc)
  (if (and (karukan-context-owner? kc)
           (karukan-context-composing? kc))
      (let ((result (karukan-server-request karukan-server "commit" '())))
        (if result
            (karukan-apply kc result)
            (karukan-clear kc)))))

(define (karukan-clear-preedit kc)
  (if (not (null? (karukan-context-preedit kc)))
      (begin
        (karukan-context-set-preedit! kc '())
        (context-update-preedit kc '()))))

;; Forgets what KC composes.
(define (karukan-clear kc)
  (karukan-clear-preedit kc)
  (karukan-hide-candidates kc))

(define (karukan-reset kc)
  (if (karukan-context-owner? kc)
      (karukan-server-request karukan-server "reset" '()))
  (karukan-clear kc))

;; Gives the input state to KC. Returns the server or #f.
(define (karukan-take-over kc)
  (let ((server (karukan-server-ensure)))
    (and server
         (let ((owner (karukan-server-owner server)))
           (if (not (eq? owner kc))
               (begin
                 (if owner
                     (karukan-flush owner))
                 (karukan-server-set-owner! server kc)))
           ;; karukan-flush may lose the server.
           karukan-server))))

;; The text before the cursor helps the conversion. It's sent when a
;; composition starts.
(define (karukan-send-surrounding-text kc)
  (let* ((ustr (and karukan-use-surrounding-text?
                    (im-acquire-text kc 'primary 'cursor 'line 0)))
         (former (and ustr (ustr-former-seq ustr)))
         (text (if (and (pair? former) (string? (car former)))
                   (car former)
                   "")))
    (karukan-server-notify karukan-server
                           "set_surrounding_text"
                           (list (cons 'text text)
                                 (cons 'cursor_pos (string-length text))))))

;;; ------------------------------------------------------------------
;;; keys

;; uim key symbol -> XKB keysym
(define karukan-keysym-alist
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
    (Kanji             . #xff21)
    (Muhenkan          . #xff22)
    (Henkan_Mode       . #xff23)
    (romaji            . #xff24)
    (hiragana          . #xff25)
    (katakana          . #xff26)
    (hiragana-katakana . #xff27)
    (zenkaku           . #xff28)
    (hankaku           . #xff29)
    (zenkaku-hankaku   . #xff2a)
    (eisu-toggle       . #xff30)))

;; F1 is #xffbe and F35 is #xffe0.
(define (karukan-function-keysym key)
  (let ((name (symbol->string key)))
    (and (> (string-length name) 1)
         (char=? (string-ref name 0) #\F)
         (let ((n (string->number (substring name 1 (string-length name)))))
           (and (integer? n)
                (<= 1 n 35)
                (+ #xffbe (- n 1)))))))

;; Returns the XKB keysym of KEY, or #f.
(define (karukan-keysym key key-state)
  (cond
   ((symbol? key)
    (or (assq-cdr key karukan-keysym-alist)
        (karukan-function-keysym key)))
   ;; Some bridges such as uim-fep pass Ctrl-a as 1.
   ((and (<= 1 key 26) (control-key-mask key-state))
    (+ key 96))
   ((ichar-control? key)
    #f)
   ((< key 256)
    key)
   (else
    (+ #x1000000 key))))

(define (karukan-modifiers key-state)
  (list (cons 'shift (and (shift-key-mask key-state) #t))
        (cons 'control (and (control-key-mask key-state) #t))
        (cons 'alt (and (alt-key-mask key-state) #t))
        (cons 'super (and (super-key-mask key-state) #t))))

;; Returns the result of the key, or #f.
(define (karukan-process-key kc keysym key-state)
  (and (karukan-take-over kc)
       (begin
         (if (not (karukan-context-composing? kc))
             (karukan-send-surrounding-text kc))
         ;; Sending the text may lose the server.
         (and karukan-server
              (karukan-server-request
               karukan-server
               "process_key"
               (list (cons 'keysym keysym)
                     (cons 'modifiers (karukan-modifiers key-state))))))))

;; Sends a key to the server. Returns whether it consumed the key.
(define (karukan-send-key kc keysym key-state)
  (let ((result
         (or (karukan-process-key kc keysym key-state)
             ;; A server that has exited is found by writing to it.
             ;; What KC composes is gone with it.
             (and (not karukan-server)
                  (begin
                    (karukan-clear kc)
                    (karukan-process-key kc keysym key-state))))))
    (if result
        (karukan-apply kc result)
        (begin
          (karukan-clear kc)
          #f))))

;;; ------------------------------------------------------------------
;;; modes

;; The input mode of Karukan goes back to hiragana. The toolbar doesn't
;; show the katakana mode of Ctrl+K and uim can't send the keys that
;; leave it, such as Right Alt.
(define (karukan-set-on kc)
  (karukan-context-set-on! kc #t)
  ;; The models start loading now.
  (if (karukan-take-over kc)
      (begin
        (karukan-flush kc)
        (karukan-server-request karukan-server "reset" '()))))

(define (karukan-set-off kc)
  (karukan-flush kc)
  (karukan-context-set-on! kc #f))

(register-action 'action_karukan_direct
                 (lambda (kc)
                   '(ja_direct
                     "-"
                     "直接入力"
                     "直接(無変換)入力モード"))
                 (lambda (kc)
                   (not (karukan-context-on kc)))
                 (lambda (kc)
                   (karukan-set-off kc)))

(register-action 'action_karukan_hiragana
                 (lambda (kc)
                   '(ja_hiragana
                     "あ"
                     "ひらがな"
                     "ひらがな入力モード"))
                 (lambda (kc)
                   (karukan-context-on kc))
                 (lambda (kc)
                   (karukan-set-on kc)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define (karukan-configure-widgets)
  (register-widget 'widget_karukan_input_mode
                   (activity-indicator-new karukan-input-mode-actions)
                   (actions-new karukan-input-mode-actions))
  (context-list-replace-widgets! 'karukan karukan-widgets))

;;; ------------------------------------------------------------------
;;; handlers

(define (karukan-init-handler id im arg)
  (let ((kc (karukan-context-new-internal id im)))
    (karukan-context-set-widgets! kc karukan-widgets)
    kc))

(define (karukan-release-handler kc)
  (if (karukan-context-owner? kc)
      (begin
        (karukan-server-request karukan-server "reset" '())
        (if karukan-server
            (karukan-server-set-owner! karukan-server #f))))
  #f)

(define (karukan-proc-on kc key key-state)
  (cond
   ;; karukan-imserver uses Shift+Space while composing.
   ((and (karukan-off-key? key key-state)
         (not (karukan-context-composing? kc)))
    (karukan-set-off kc))
   (else
    (let ((keysym (karukan-keysym key key-state)))
      (if (not (and keysym
                    (karukan-send-key kc keysym key-state)))
          (im-commit-raw kc))))))

(define (karukan-press-key-handler kc key key-state)
  (cond
   ((karukan-context-on kc)
    (karukan-proc-on kc key key-state))
   ((karukan-on-key? key key-state)
    (karukan-set-on kc))
   (else
    (im-commit-raw kc))))

(define (karukan-release-key-handler kc key key-state)
  (if (or (ichar-control? key)
          (not (karukan-context-on kc)))
      (im-commit-raw kc)))

(define (karukan-reset-handler kc)
  (karukan-reset kc))

(define (karukan-focus-out-handler kc)
  (if (karukan-context-owner? kc)
      (begin
        (karukan-flush kc)
        (if karukan-server
            (karukan-server-notify karukan-server "save_learning" '())))))

(define (karukan-displace-handler kc)
  (karukan-flush kc))

(define (karukan-get-candidate-handler kc idx accel-enum-hint)
  (let ((candidates (karukan-context-candidates kc)))
    (if (< -1 idx (length candidates))
        (let ((candidate (list-ref candidates idx)))
          (list (car candidate)
                (number->string (+ idx 1))
                (cdr candidate)))
        (list "" "" ""))))

;; Frontends report every move of the highlight here. Selecting a
;; candidate commits it in karukan-imserver, so this moves the cursor
;; with the keys instead.
(define (karukan-set-candidate-index-handler kc idx)
  (let loop ()
    (let ((cursor (karukan-context-cand-cursor kc)))
      (if (and (< -1 idx (length (karukan-context-candidates kc)))
               (not (= idx cursor))
               (karukan-send-key kc
                                 (if (< cursor idx)
                                     (assq-cdr 'down karukan-keysym-alist)
                                     (assq-cdr 'up karukan-keysym-alist))
                                 0)
               ;; Stop if the cursor doesn't move.
               (not (= cursor (karukan-context-cand-cursor kc))))
          (loop)))))

(karukan-configure-widgets)

(register-im
 'karukan
 "ja"
 "UTF-8"
 karukan-im-name-label
 karukan-im-short-desc
 #f
 karukan-init-handler
 karukan-release-handler
 context-mode-handler
 karukan-press-key-handler
 karukan-release-key-handler
 karukan-reset-handler
 karukan-get-candidate-handler
 karukan-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 karukan-focus-out-handler
 #f
 karukan-displace-handler)
