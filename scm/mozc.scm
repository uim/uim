;;; mozc.scm: Mozc for uim.
;;;
;;; Copyright (c) 2010-2026 uim Project https://github.com/uim/uim
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

;;; This is a rewrite of the mozc.scm from the MacUIM project by
;;; Etsushi Kato (https://github.com/e-kato/macuim, Mozc/scm/mozc.scm),
;;; which carries the BSD-3-Clause license above. Unlike the
;;; original, this version needs no native plugin: it drives
;;; mozc_server through uim-mozc-helper (see mozc/README.md) using
;;; line-oriented S-expressions.

;;; This module talks to mozc_server through uim-mozc-helper (see
;;; mozc/README.md). uim-mozc-helper speaks line oriented
;;; S-expressions, so everything here is plain Scheme: no native
;;; plugin is needed.

(require-extension (srfi 1 2))

(require "util.scm")
(require "process.scm")
(require "fileio.scm")
(require "japanese.scm")
(require "ustr.scm")
(require-custom "generic-key-custom.scm")
(require-custom "mozc-custom.scm")
(require-custom "mozc-key-custom.scm")

;;; ------------------------------------------------------------------
;;; helper process

(define mozc-helper-rec-spec
  '((in-port        #f)   ; file port to read responses
    (out-fd         #f)   ; fd to write requests
    (event-id       0)
    (version        "")   ; Mozc version
    (preedit-method roman)))
(define-record 'mozc-helper mozc-helper-rec-spec)

;; The helper process is shared by all contexts.
(define mozc-helper #f)
(define mozc-helper-failed? #f)

(define (mozc-alist-ref key alist)
  (assq-cdr key alist))

(define (mozc-read-sexp str)
  (guard (err (#t #f))
    (read (open-input-string str))))

(define (mozc-helper-read-line helper)
  (let ((line (file-read-line (mozc-helper-in-port helper))))
    (if (or (eof-object? line)
            (not line)
            (null? line))
        #f
        line)))

(define (mozc-helper-close helper)
  (guard (err (#t #f))
    (close-file-port (mozc-helper-in-port helper)))
  (guard (err (#t #f))
    (file-close (mozc-helper-out-fd helper)))
  (if (eq? mozc-helper helper)
      (set! mozc-helper #f)))

(define (mozc-helper-start)
  (let ((fds (process-io mozc-helper-command
                         (list mozc-helper-command
                               (string-append "--emacs-helper="
                                              mozc-emacs-helper-command)))))
    (and fds
         (let* ((helper (mozc-helper-new))
                (in-port (open-file-port (car fds))))
           (mozc-helper-set-in-port! helper in-port)
           (mozc-helper-set-out-fd! helper (cdr fds))
           ;; The helper stays silent until we greet it, so that uim's
           ;; process-io doesn't misread an unsolicited banner.
           (let* ((greeting (mozc-helper-request helper '(Hello))))
             (cond
              ((not (list? greeting))
               (uim-notify-fatal
                (format (N_ "uim-mozc: broken greeting from ~a")
                        mozc-helper-command))
               (mozc-helper-close helper)
               #f)
              ((mozc-alist-ref 'error greeting)
               (uim-notify-fatal
                (format "uim-mozc: ~a"
                        (or (mozc-alist-ref 'message greeting)
                            (mozc-alist-ref 'error greeting))))
               (mozc-helper-close helper)
               #f)
              ((not (mozc-alist-ref 'uim-mozc-helper greeting))
               (uim-notify-fatal
                (format (N_ "uim-mozc: ~a isn't uim-mozc-helper")
                        mozc-helper-command))
               (mozc-helper-close helper)
               #f)
              (else
               (mozc-helper-set-version!
                helper (or (mozc-alist-ref 'version greeting) ""))
               (let ((method (mozc-alist-ref
                              'preedit-method
                              (or (mozc-alist-ref 'config greeting) '()))))
                 (if (symbol? method)
                     (mozc-helper-set-preedit-method! helper method)))
               helper)))))))

(define (mozc-helper-ensure)
  (or mozc-helper
      (and (not mozc-helper-failed?)
           (let ((helper (mozc-helper-start)))
             (if helper
                 (set! mozc-helper helper)
                 (begin
                   (set! mozc-helper-failed? #t)
                   (uim-notify-fatal
                    (format (N_ "uim-mozc: cannot start ~a")
                            mozc-helper-command))))
             helper))))

;; Sends REQUEST-BODY (a list without the event ID) and returns the
;; response alist, or #f on failure.
(define (mozc-helper-request helper request-body)
  (let* ((event-id (mozc-helper-event-id helper))
         (request (cons event-id request-body)))
    (mozc-helper-set-event-id! helper (+ event-id 1))
    ;; write-to-string escapes newlines in string fields as \n, so the
    ;; whole request stays on one line for the helper's line reader; the
    ;; helper decodes \n back to a newline.
    (if (< (file-write-string (mozc-helper-out-fd helper)
                              (string-append (write-to-string request) "\n"))
           0)
        (begin
          (uim-notify-fatal (N_ "uim-mozc: uim-mozc-helper is gone"))
          (mozc-helper-close helper)
          #f)
        (let* ((line (mozc-helper-read-line helper))
               (response (and line (mozc-read-sexp line))))
          (cond
           ((not (list? response))
            (uim-notify-fatal (N_ "uim-mozc: uim-mozc-helper is gone"))
            (mozc-helper-close helper)
            #f)
           ((mozc-alist-ref 'error response)
            (uim-notify-info
             (format "uim-mozc: ~a"
                     (or (mozc-alist-ref 'message response)
                         (mozc-alist-ref 'error response))))
            #f)
           (else
            response))))))

;;; ------------------------------------------------------------------
;;; context

(define mozc-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'session-id      #f)  ; session ID in uim-mozc-helper
    (list 'on              #f)
    (list 'mode            'hiragana) ; composition mode symbol
    (list 'has-preedit     #f)
    (list 'candidates      '()) ; candidates of the current page
    (list 'cand-nr         0)
    (list 'cand-reactivate #f)
    (list 'cand-page       0)
    (list 'preedit-method  'roman)
    (list 'consumed        #f))))
(define-record 'mozc-context mozc-context-rec-spec)
(define mozc-context-new-internal mozc-context-new)

(define mozc-type-direct          'direct)
(define mozc-type-hiragana        'hiragana)
(define mozc-type-katakana        'full-katakana)
(define mozc-type-halfkana        'half-katakana)
(define mozc-type-halfwidth-alnum 'half-ascii)
(define mozc-type-fullwidth-alnum 'full-ascii)

(define mozc-input-rule-roma 'roman)
(define mozc-input-rule-kana 'kana)

(define (mozc-context-session-ready? mc)
  (and (mozc-context-session-id mc)
       mozc-helper
       #t))

(define (mozc-context-send mc request-body)
  (and (mozc-context-session-ready? mc)
       (let ((response (mozc-helper-request mozc-helper request-body)))
         (and response
              (mozc-alist-ref 'output response)))))

(define (mozc-context-send-input mc input)
  (mozc-context-send mc (list 'SendInput (mozc-context-session-id mc) input)))

(define (mozc-context-send-command mc command)
  (mozc-context-send mc
                     (list 'SendCommand (mozc-context-session-id mc) command)))

(define (mozc-context-create-session! mc)
  (let ((helper (mozc-helper-ensure)))
    (and helper
         (let ((response (mozc-helper-request helper '(CreateSession))))
           (and response
                (let ((session-id (mozc-alist-ref 'session-id response)))
                  (mozc-context-set-session-id! mc session-id)
                  (mozc-context-set-preedit-method!
                   mc (mozc-helper-preedit-method helper))
                  session-id))))))

(define (mozc-context-delete-session! mc)
  (if (mozc-context-session-ready? mc)
      (begin
        (mozc-helper-request mozc-helper
                             (list 'DeleteSession
                                   (mozc-context-session-id mc)))
        (mozc-context-set-session-id! mc #f))))

;;; ------------------------------------------------------------------
;;; output handling

;; Splits STR before the N-th character, returning (former . latter),
;; with a single string->list traversal.
(define (mozc-string-split-at str n)
  (let ((chars (string->list str)))
    (cons (list->string (take chars n))
          (list->string (drop chars n)))))

(define mozc-separator
  (lambda ()
    (let ((attr (bitwise-ior preedit-separator
                             preedit-underline)))
      (if mozc-show-segment-separator?
          (cons attr mozc-segment-separator)
          #f))))

(define (mozc-segment-attr segment)
  (case (mozc-alist-ref 'annotation segment)
    ((underline) preedit-underline)
    ((highlight) (bitwise-ior preedit-reverse preedit-cursor))
    (else preedit-none)))

;; Returns a list of (attr . str) for context-update-preedit.
(define (mozc-compose-preedit preedit)
  (let ((cursor (or (mozc-alist-ref 'cursor preedit) 0))
        (highlighted? (and (mozc-alist-ref 'highlighted-position preedit) #t))
        (separator (mozc-separator))
        (cursor-seg (cons preedit-cursor "")))
    (let loop ((segments (or (mozc-alist-ref 'segment preedit) '()))
               (count 0)
               (i 0)
               (segs '()))
      (if (null? segments)
          (reverse segs)
          (let* ((segment (car segments))
                 (str (or (mozc-alist-ref 'value segment) ""))
                 (len (or (mozc-alist-ref 'value-length segment)
                          (string-length str)))
                 (attr (mozc-segment-attr segment))
                 (prev-count count)
                 (count (+ count len))
                 (segs (if (and separator (> i 0))
                           (cons separator segs)
                           segs)))
            (cond
             ;; cursor is inside this segment
             ((or (and (< prev-count cursor) (> count cursor))
                  (and (= cursor 0) (= i 0)))
              (let ((pos (- cursor prev-count)))
                (loop (cdr segments)
                      count
                      (+ i 1)
                      (if (= pos 0)
                          (cons (cons attr str) (cons cursor-seg segs))
                          (let ((split (mozc-string-split-at str pos)))
                            (cons (cons attr (cdr split))
                                  (cons cursor-seg
                                        (cons (cons attr (car split))
                                              segs))))))))
             (else
              (loop (cdr segments)
                    count
                    (+ i 1)
                    (if (and (= count cursor) (not highlighted?))
                        (cons cursor-seg (cons (cons attr str) segs))
                        (cons (cons attr str) segs))))))))))

(define (mozc-update-preedit mc output)
  (let ((preedit (mozc-alist-ref 'preedit output)))
    (cond
     (preedit
      (context-update-preedit mc (mozc-compose-preedit preedit))
      (mozc-context-set-has-preedit! mc #t))
     ((mozc-context-has-preedit mc)
      (context-update-preedit mc '())
      (mozc-context-set-has-preedit! mc #f)))))

(define (mozc-update-result mc output)
  (let ((result (mozc-alist-ref 'result output)))
    (if result
        (let ((value (mozc-alist-ref 'value result)))
          (if (and (string? value) (not (string=? value "")))
              (im-commit mc value))))))

(define (mozc-update-deletion-range mc output)
  (let ((range (mozc-alist-ref 'deletion-range output)))
    (if range
        (let ((offset (or (mozc-alist-ref 'offset range) 0))
              (length (or (mozc-alist-ref 'length range) 0)))
          (if (>= (+ offset length) 0)
              (im-delete-text mc 'primary 'cursor (- offset) (+ offset length)))))))

;; mozc's output holds only the page it shows now, while uim asks for
;; any candidate by its index in the whole list.

(define (mozc-find-candidate mc idx)
  (find (lambda (candidate)
          (eqv? (mozc-alist-ref 'index candidate) idx))
        (mozc-context-candidates mc)))

(define (mozc-first-index mc)
  (let ((candidates (mozc-context-candidates mc)))
    (and (pair? candidates)
         (mozc-alist-ref 'index (car candidates)))))

;; Turns mozc's page one step toward IDX. Returns #f when it did not
;; move. No mozc-update here: a frontend calls this from inside the
;; selector callbacks.
(define (mozc-turn-page mc idx)
  (let* ((first (mozc-first-index mc))
         (output (and first
                      (mozc-context-send-command
                       mc `((type . ,(if (< idx first)
                                         'convert-prev-page
                                         'convert-next-page))))))
         (cw (and output (mozc-alist-ref 'candidate-window output))))
    (and cw
         (begin
           (mozc-context-set-candidates! mc (or (mozc-alist-ref 'candidate cw)
                                                '()))
           ;; mozc wraps around at both ends
           (not (eqv? first (mozc-first-index mc)))))))

(define (mozc-candidate-at mc idx)
  (and (< idx (mozc-context-cand-nr mc))
       (let loop ((rest (mozc-context-cand-nr mc))) ; loop cap
         (or (mozc-find-candidate mc idx)
             (and (> rest 0)
                  (mozc-turn-page mc idx)
                  (loop (- rest 1)))))))

;; uim reports every move of the highlight here, so SELECT_CANDIDATE
;; is wrong: it closes the candidate window.
(define (mozc-highlight-candidate mc idx)
  (let* ((candidate (mozc-candidate-at mc idx))
         (id (and candidate (mozc-alist-ref 'id candidate))))
    (and id
         (mozc-context-send-command mc `((type . highlight-candidate)
                                         (id . ,id))))))

(define (mozc-update-candidates mc output)
  (let ((cw (mozc-alist-ref 'candidate-window output)))
    (cond
     ((not cw)
      (if (> (mozc-context-cand-nr mc) 0)
          (im-deactivate-candidate-selector mc))
      (mozc-context-set-candidates! mc '())
      (mozc-context-set-cand-nr! mc 0))
     (else
      (let* ((size (or (mozc-alist-ref 'size cw) 0))
             (page-size (or (mozc-alist-ref 'page-size cw) 9))
             (focused (mozc-alist-ref 'focused-index cw))
             (page (if focused (quotient focused page-size) 0))
             (first-time? (or (not (= (mozc-context-cand-nr mc) size))
                              (not focused))))
        (mozc-context-set-candidates! mc (or (mozc-alist-ref 'candidate cw)
                                             '()))
        ;; the callbacks below reach mozc-candidate-at
        (mozc-context-set-cand-nr! mc size)
        (if (or first-time?
                (and (mozc-context-cand-reactivate mc)
                     (not (= page (mozc-context-cand-page mc)))))
            (begin
              (im-activate-candidate-selector mc size page-size)
              (mozc-context-set-cand-reactivate! mc (not (= page 0)))))
        (mozc-context-set-cand-page! mc page)
        (if focused
            (im-select-candidate mc focused))
        ;; a frontend draws page 0 first, which can move mozc off it
        (if (and focused (not (mozc-find-candidate mc focused)))
            (mozc-highlight-candidate mc focused)))))))

(define (mozc-update-mode mc output)
  ;; mozc-context-mode holds the composition mode to restore when the IME
  ;; is turned back on, so 'direct is deliberately not stored here; the
  ;; on/off state is tracked separately by mozc-context-on.
  (let ((mode (mozc-alist-ref 'mode output)))
    (if (and (symbol? mode) (not (eq? mode 'direct)))
        (mozc-context-set-mode! mc mode))))

(define (mozc-launch-tool mc output)
  (let ((mode (mozc-alist-ref 'launch-tool-mode output)))
    (case mode
      ((config-dialog) (mozc-tool-activate mc 'mozc-tool-config-dialog))
      ((dictionary-tool) (mozc-tool-activate mc 'mozc-tool-dictionary-tool))
      ((word-register-dialog)
       (mozc-tool-activate mc 'mozc-tool-word-register-dialog))
      (else #f))))

;; Returns the text to reconvert, or #f.  Selected text is preferred.
(define (mozc-acquire-text mc use-primary?)
  (let* ((ustr (im-acquire-text mc 'selection 'beginning 0 'full))
         (latter (and ustr (ustr-latter-seq ustr))))
    (cond
     ((and latter (not (null? latter)))
      (cons 'selection (car latter)))
     (use-primary?
      (let* ((ustr (im-acquire-text mc 'primary 'cursor 'line 0))
             (former (and ustr (ustr-former-seq ustr))))
        (and former
             (not (null? former))
             (cons 'primary (car former)))))
     (else
      #f))))

(define (mozc-delete-acquired-text mc source)
  (if (eq? source 'primary)
      (im-delete-text mc 'primary 'cursor 'line 0)
      (im-delete-text mc 'selection 'beginning 0 'full)))

(define (mozc-execute-callback mc output)
  (let* ((callback (mozc-alist-ref 'callback output))
         (command (and callback (mozc-alist-ref 'session-command callback)))
         (type (and command (mozc-alist-ref 'type command))))
    (case type
      ((undo)
       (let ((new-output (mozc-context-send-command mc '((type . undo)))))
         (if new-output
             (mozc-update mc new-output))))
      ((convert-reverse)
       (let ((text (mozc-acquire-text mc #f)))
         (let ((new-output
                (if text
                    (mozc-context-send-command
                     mc `((type . convert-reverse) (text . ,(cdr text))))
                    ;; UNDO if no selection
                    (mozc-context-send-command mc '((type . undo))))))
           (if new-output
               (begin
                 (if text
                     (mozc-delete-acquired-text mc (car text)))
                 (mozc-update mc new-output))))))
      (else
       #f))))

(define (mozc-update mc output)
  (mozc-context-set-consumed! mc (mozc-alist-ref 'consumed output))
  (mozc-update-deletion-range mc output)
  (mozc-update-result mc output)
  (mozc-update-preedit mc output)
  (mozc-update-candidates mc output)
  (mozc-update-mode mc output)
  (mozc-launch-tool mc output)
  (mozc-execute-callback mc output))

;;; ------------------------------------------------------------------
;;; key translation

;; uim key symbol -> Mozc special key symbol
(define mozc-special-key-alist
  '((return            . enter)
    (backspace         . backspace)
    (delete            . del)
    (escape            . escape)
    (tab               . tab)
    (left              . left)
    (up                . up)
    (right             . right)
    (down              . down)
    (prior             . page-up)
    (next              . page-down)
    (home              . home)
    (end               . end)
    (insert            . insert)
    (Kanji             . kanji)
    (Muhenkan          . muhenkan)
    (Henkan_Mode       . henkan)
    (hiragana          . kana)
    (katakana          . katakana)
    (hiragana-katakana . kana)
    (zenkaku-hankaku   . hankaku)
    (zenkaku           . hankaku)
    (hankaku           . hankaku)
    (eisu-toggle       . eisu)
    (eisu-shift        . eisu)
    (caps-lock         . caps-lock)
    (F1  . f1)  (F2  . f2)  (F3  . f3)  (F4  . f4)  (F5  . f5)
    (F6  . f6)  (F7  . f7)  (F8  . f8)  (F9  . f9)  (F10 . f10)
    (F11 . f11) (F12 . f12) (F13 . f13) (F14 . f14) (F15 . f15)
    (F16 . f16) (F17 . f17) (F18 . f18) (F19 . f19) (F20 . f20)
    (F21 . f21) (F22 . f22) (F23 . f23) (F24 . f24)))

;; Kana input: ASCII character -> kana for a Japanese keyboard
(define mozc-kana-map-jp
  '((#\1 . "ぬ") (#\! . "ぬ") (#\2 . "ふ") (#\" . "ふ")
    (#\3 . "あ") (#\# . "ぁ") (#\4 . "う") (#\$ . "ぅ")
    (#\5 . "え") (#\% . "ぇ") (#\6 . "お") (#\& . "ぉ")
    (#\7 . "や") (#\' . "ゃ") (#\8 . "ゆ") (#\( . "ゅ")
    (#\9 . "よ") (#\) . "ょ") (#\0 . "わ") (#\~ . "を")
    (#\- . "ほ") (#\= . "ほ") (#\^ . "へ") (#\| . "ー")
    (#\q . "た") (#\Q . "た") (#\w . "て") (#\W . "て")
    (#\e . "い") (#\E . "ぃ") (#\r . "す") (#\R . "す")
    (#\t . "か") (#\T . "か") (#\y . "ん") (#\Y . "ん")
    (#\u . "な") (#\U . "な") (#\i . "に") (#\I . "に")
    (#\o . "ら") (#\O . "ら") (#\p . "せ") (#\P . "せ")
    (#\@ . "゛") (#\` . "゛") (#\[ . "゜") (#\{ . "「")
    (#\a . "ち") (#\A . "ち") (#\s . "と") (#\S . "と")
    (#\d . "し") (#\D . "し") (#\f . "は") (#\F . "は")
    (#\g . "き") (#\G . "き") (#\h . "く") (#\H . "く")
    (#\j . "ま") (#\J . "ま") (#\k . "の") (#\K . "の")
    (#\l . "り") (#\L . "り") (#\; . "れ") (#\+ . "れ")
    (#\: . "け") (#\* . "け") (#\] . "む") (#\} . "」")
    (#\z . "つ") (#\Z . "っ") (#\x . "さ") (#\X . "さ")
    (#\c . "そ") (#\C . "そ") (#\v . "ひ") (#\V . "ひ")
    (#\b . "こ") (#\B . "こ") (#\n . "み") (#\N . "み")
    (#\m . "も") (#\M . "も") (#\, . "ね") (#\< . "、")
    (#\. . "る") (#\> . "。") (#\/ . "め") (#\? . "・")
    (#\_ . "ろ") (#\\ . "ろ")))

;; Kana input: ASCII character -> kana for a US keyboard
(define mozc-kana-map-us
  '((#\` . "ろ") (#\~ . "ろ") (#\1 . "ぬ") (#\! . "ぬ")
    (#\2 . "ふ") (#\@ . "ふ") (#\3 . "あ") (#\# . "ぁ")
    (#\4 . "う") (#\$ . "ぅ") (#\5 . "え") (#\% . "ぇ")
    (#\6 . "お") (#\^ . "ぉ") (#\7 . "や") (#\& . "ゃ")
    (#\8 . "ゆ") (#\* . "ゅ") (#\9 . "よ") (#\( . "ょ")
    (#\0 . "わ") (#\) . "を") (#\- . "ほ") (#\_ . "ー")
    (#\= . "へ") (#\+ . "へ")
    (#\q . "た") (#\Q . "た") (#\w . "て") (#\W . "て")
    (#\e . "い") (#\E . "ぃ") (#\r . "す") (#\R . "す")
    (#\t . "か") (#\T . "か") (#\y . "ん") (#\Y . "ん")
    (#\u . "な") (#\U . "な") (#\i . "に") (#\I . "に")
    (#\o . "ら") (#\O . "ら") (#\p . "せ") (#\P . "せ")
    (#\[ . "゛") (#\{ . "「") (#\] . "゜") (#\} . "」")
    (#\\ . "む") (#\| . "ー")
    (#\a . "ち") (#\A . "ち") (#\s . "と") (#\S . "と")
    (#\d . "し") (#\D . "し") (#\f . "は") (#\F . "は")
    (#\g . "き") (#\G . "き") (#\h . "く") (#\H . "く")
    (#\j . "ま") (#\J . "ま") (#\k . "の") (#\K . "の")
    (#\l . "り") (#\L . "り") (#\; . "れ") (#\: . "れ")
    (#\' . "け") (#\" . "け")
    (#\z . "つ") (#\Z . "っ") (#\x . "さ") (#\X . "さ")
    (#\c . "そ") (#\C . "そ") (#\v . "ひ") (#\V . "ひ")
    (#\b . "こ") (#\B . "こ") (#\n . "み") (#\N . "み")
    (#\m . "も") (#\M . "も") (#\, . "ね") (#\< . "、")
    (#\. . "る") (#\> . "。") (#\/ . "め") (#\? . "・")))

(define (mozc-kana-string key)
  (let ((map (if (eq? mozc-keyboard-type-for-kana-input-method 'us-keyboard)
                 mozc-kana-map-us
                 mozc-kana-map-jp)))
    (and (< key 128)
         (assv-cdr (integer->char key) map))))

(define (assv-cdr key alist)
  (safe-cdr (assv key alist)))

;; Returns a KeyEvent alist for uim-mozc-helper, or #f if the key
;; can't be sent to Mozc.
(define (mozc-translate-key mc key key-state)
  (let ((modifiers (append
                    (if (control-key-mask key-state) '(ctrl) '())
                    (if (alt-key-mask key-state) '(alt) '()))))
    (cond
     ((symbol? key)
      (let ((special (assq-cdr key mozc-special-key-alist)))
        (and special
             (let ((modifiers (if (shift-key-mask key-state)
                                  (cons 'shift modifiers)
                                  modifiers)))
               `((special-key . ,special)
                 (modifier-keys ,@modifiers))))))
     ((= key 32)
      `((special-key . space)
        (modifier-keys ,@(if (shift-key-mask key-state)
                             (cons 'shift modifiers)
                             modifiers))))
     ((ichar-control? key)
      #f)
     (else
      (let ((kana (and (eq? (mozc-context-preedit-method mc) 'kana)
                       (null? modifiers)
                       (mozc-kana-string key))))
        (if kana
            `((key-code . ,key)
              (key-string . ,kana)
              (modifier-keys ,@modifiers))
            `((key-code . ,key)
              (modifier-keys ,@modifiers))))))))

(define (mozc-context-context-alist mc)
  (if mozc-use-context-aware-conversion?
      (let* ((ustr (im-acquire-text mc 'primary 'cursor 'line 'line))
             (former (and ustr (ustr-former-seq ustr)))
             (latter (and ustr (ustr-latter-seq ustr))))
        (append
         (if (and former (not (null? former)))
             (list (cons 'preceding-text (car former)))
             '())
         (if (and latter (not (null? latter)))
             (list (cons 'following-text (car latter)))
             '())))
      '()))

;; Sends the key to Mozc.  Returns #t if the key was consumed.
(define (mozc-press-key mc key key-state)
  (let ((key-event (mozc-translate-key mc key key-state)))
    (and key-event
         (let* ((context (mozc-context-context-alist mc))
                (input `((type . send-key)
                         (key . ,key-event)
                         ,@(if (null? context)
                               '()
                               (list (cons 'context context)))))
                (output (mozc-context-send-input mc input)))
           (and output
                (begin
                  (mozc-update mc output)
                  (mozc-context-consumed mc)))))))

;;; ------------------------------------------------------------------
;;; commands

(define (mozc-submit mc)
  (let ((output (mozc-context-send-command mc '((type . submit)))))
    (if output
        (mozc-update mc output))))

(define (mozc-set-mode mc mode)
  (cond
   ((not (mozc-context-session-ready? mc))
    #f)
   ((eq? mode mozc-type-direct)
    (mozc-submit mc)
    (mozc-context-set-on! mc #f))
   (else
    (let ((output (mozc-context-send-command
                   mc `((type . switch-composition-mode)
                        (composition-mode . ,mode)))))
      (if output
          (mozc-update mc output))
      (mozc-context-set-mode! mc mode)
      (mozc-context-set-on! mc #t)))))

(define (mozc-set-on mc)
  (if (mozc-context-session-ready? mc)
      (let ((output (mozc-context-send-command
                     mc `((type . switch-composition-mode)
                          (composition-mode . ,(mozc-context-mode mc))))))
        (if output
            (mozc-update mc output))))
  (mozc-context-set-on! mc #t))

(define (mozc-set-input-rule mc rule)
  (let* ((output (mozc-context-send-input mc '((type . get-config))))
         (config (and output (mozc-alist-ref 'config output))))
    (and config
         (let* ((new-config (cons (cons 'preedit-method rule)
                                  (alist-delete 'preedit-method config)))
                (output (mozc-context-send-input
                         mc `((type . set-config)
                              (config . ,new-config)))))
           (and output
                (begin
                  (mozc-context-set-preedit-method! mc rule)
                  (if mozc-helper
                      (mozc-helper-set-preedit-method! mozc-helper rule))
                  #t))))))

(define (mozc-select-candidate mc idx)
  (let ((output (mozc-highlight-candidate mc idx)))
    (and output
         (begin
           (mozc-update mc output)
           #t))))

(define (mozc-reconvert mc)
  (and (mozc-context-session-ready? mc)
       (let ((text (mozc-acquire-text mc #t)))
         (and text
              (begin
                (if (not (mozc-context-on mc))
                    (mozc-set-on mc))
                (let ((output (mozc-context-send-command
                               mc `((type . convert-reverse)
                                    (text . ,(cdr text))))))
                  (and output
                       (begin
                         (mozc-delete-acquired-text mc (car text))
                         (mozc-update mc output)
                         #t))))))))

;;; ------------------------------------------------------------------
;;; mozc_tool

(define (mozc-run-process file . args)
  (let-optionals* args ((argv (list file)))
    (let ((pid (process-fork)))
      (cond ((< pid 0)
             (begin
               (uim-notify-fatal (N_ "cannot fork"))
               #f))
            ((= 0 pid) ;; child
             (let ((pid2 (process-fork)))
               (cond ((< pid2 0)
                      (begin
                        (uim-notify-fatal (N_ "cannot fork"))
                        #f))
                     ((= 0 pid2)
                      (setenv "MALLOC_CHECK_" "0" 0)
                      (setenv "GTK_IM_MODULE" "gtk-im-context-simple" 0)
                      (if (= (process-execute file argv) -1)
                        (uim-notify-fatal (format (N_ "cannot execute ~a") file)))
                      (_exit 0))
                     (else
                       (_exit 0)))))
            (else
              (process-waitpid pid 0)
              pid)))))

(define mozc-tool-activate
  (lambda (mc option)
    (case option
      ((mozc-tool-about-dialog)
       (mozc-run-process mozc-tool-about-dialog-cmd (list mozc-tool-about-dialog-cmd mozc-tool-about-dialog-cmd-option)))
      ((mozc-tool-config-dialog)
       (mozc-run-process mozc-tool-config-dialog-cmd (list mozc-tool-config-dialog-cmd mozc-tool-config-dialog-cmd-option)))
      ((mozc-tool-dictionary-tool)
       (mozc-run-process mozc-tool-dictionary-tool-cmd (list mozc-tool-dictionary-tool-cmd mozc-tool-dictionary-tool-cmd-option)))
      ((mozc-tool-word-register-dialog)
       (mozc-run-process mozc-tool-word-register-dialog-cmd (list mozc-tool-word-register-dialog-cmd mozc-tool-word-register-dialog-cmd-option)))
      (else
        #f))))

;;; ------------------------------------------------------------------
;;; actions and widgets

(define (mozc-register-input-mode-action name indication mode)
  (register-action name
                   (lambda (mc) indication)
                   (lambda (mc)
                     (and (mozc-context-session-id mc)
                          (mozc-context-on mc)
                          (eq? (mozc-context-mode mc) mode)))
                   (lambda (mc)
                     (mozc-set-mode mc mode))))

(mozc-register-input-mode-action 'action_mozc_hiragana
                                 '(ja_hiragana
                                   "あ"
                                   "ひらがな"
                                   "ひらがな入力モード")
                                 mozc-type-hiragana)

(mozc-register-input-mode-action 'action_mozc_katakana
                                 '(ja_katakana
                                   "ア"
                                   "カタカナ"
                                   "カタカナ入力モード")
                                 mozc-type-katakana)

(mozc-register-input-mode-action 'action_mozc_halfkana
                                 '(ja_halfkana
                                   "ｱ"
                                   "半角カタカナ"
                                   "半角カタカナ入力モード")
                                 mozc-type-halfkana)

(mozc-register-input-mode-action 'action_mozc_halfwidth_alnum
                                 '(ja_halfwidth_alnum
                                   "a"
                                   "半角英数"
                                   "半角英数入力モード")
                                 mozc-type-halfwidth-alnum)

(mozc-register-input-mode-action 'action_mozc_fullwidth_alnum
                                 '(ja_fullwidth_alnum
                                   "Ａ"
                                   "全角英数"
                                   "全角英数入力モード")
                                 mozc-type-fullwidth-alnum)

(register-action 'action_mozc_direct
		 (lambda (mc)
                   '(ja_direct
                      "-"
                      "直接入力"
                      "直接(無変換)入力モード"))
		 (lambda (mc)
		   (not (mozc-context-on mc)))
		 (lambda (mc)
                   (mozc-set-mode mc mozc-type-direct)))

(define (mozc-register-input-rule-action name indication rule)
  (register-action name
                   (lambda (mc) indication)
                   (lambda (mc)
                     (and (mozc-context-session-id mc)
                          (eq? (mozc-context-preedit-method mc) rule)))
                   (lambda (mc)
                     (mozc-set-input-rule mc rule))))

(mozc-register-input-rule-action 'action_mozc_roma
                                 '(ja_romaji
                                   "Ｒ"
                                   "ローマ字"
                                   "ローマ字入力モード")
                                 mozc-input-rule-roma)

(mozc-register-input-rule-action 'action_mozc_kana
                                 '(ja_kana
                                   "か"
                                   "かな"
                                   "かな入力モード")
                                 mozc-input-rule-kana)

(register-action 'action_mozc_tool_selector
                 (lambda (mc)
                   '(mozc_tool_selector
                     "M"
                     "MozcTool selector"
                     "MozcTool selector"))
                 (lambda (mc)
                   #t)
                 (lambda (mc)
                   #f))

(define (mozc-register-tool-action name indication tool)
  (register-action name
                   (lambda (mc) indication)
                   (lambda (mc) #f)
                   (lambda (mc) (mozc-tool-activate mc tool))))

(mozc-register-tool-action 'action_mozc_tool_about_dialog
                           '(mozc_tool_about_dialog
                             "A"
                             "About"
                             "About Mozc")
                           'mozc-tool-about-dialog)

(mozc-register-tool-action 'action_mozc_tool_config_dialog
                           '(mozc_tool_config_dialog
                             "C"
                             "Config dialog"
                             "Config dialog")
                           'mozc-tool-config-dialog)

(mozc-register-tool-action 'action_mozc_tool_dictionary_tool
                           '(mozc_tool_dictionary_tool
                             "D"
                             "Dictionary tool"
                             "Dictionary tool")
                           'mozc-tool-dictionary-tool)

(mozc-register-tool-action 'action_mozc_tool_word_register_dialog
                           '(mozc_tool_word_register_dialog
                             "W"
                             "Word register dialog"
                             "Word register dialog")
                           'mozc-tool-word-register-dialog)

(register-action 'action_mozc_reconvert
                 (lambda (mc)
                   '(mozc_reconvert
                     "R"
                     "Reconvert"
                     "Reconvert"))
                 (lambda (mc)
                   #f)
                 (lambda (mc)
                   (mozc-reconvert mc)))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define mozc-configure-widgets
  (lambda ()
    (register-widget 'widget_mozc_input_mode
		     (activity-indicator-new mozc-input-mode-actions)
		     (actions-new mozc-input-mode-actions))
    (register-widget 'widget_mozc_kana_input_method
		     (activity-indicator-new mozc-kana-input-method-actions)
		     (actions-new mozc-kana-input-method-actions))
    (register-widget 'widget_mozc_tool
		     (activity-indicator-new mozc-tool-actions)
		     (actions-new (remove (lambda (x) (eq? x 'action_mozc_tool_selector)) mozc-tool-actions)))
    (context-list-replace-widgets! 'mozc mozc-widgets)))

;;; ------------------------------------------------------------------
;;; handlers

(define mozc-context-new
  (lambda (id im)
    (let ((mc (mozc-context-new-internal id im)))
      (mozc-context-set-widgets! mc mozc-widgets)
      (if (not (= (getuid) 0))
          (mozc-context-create-session! mc))
      mc)))

(define mozc-init-handler
  (lambda (id im arg)
    (mozc-context-new id im)))

(define mozc-release-handler
  (lambda (mc)
    (mozc-context-delete-session! mc)
    #f))

(define mozc-proc-direct-state
  (lambda (mc key key-state)
    (if (mozc-on-key? key key-state)
        (mozc-set-on mc)
        (im-commit-raw mc))))

(define mozc-kana-toggle
  (lambda (mc)
    (let ((mode (mozc-context-mode mc)))
      (cond
       ((eq? mode mozc-type-hiragana)
        (mozc-set-mode mc mozc-type-katakana)
        #t)
       ((eq? mode mozc-type-katakana)
        (mozc-set-mode mc mozc-type-hiragana)
        #t)
       (else
        #f)))))

(define mozc-proc-input-state
  (lambda (mc key key-state)
    (cond
     ((not (mozc-context-session-ready? mc))
      (im-commit-raw mc))
     ((and (mozc-off-key? key key-state)
           (not (mozc-context-has-preedit mc)))
      (mozc-set-mode mc mozc-type-direct))
     ;; non available modifiers on Mozc
     ((or (meta-key-mask key-state)
          (super-key-mask key-state)
          (hyper-key-mask key-state))
      (if (not (mozc-context-has-preedit mc))
          (im-commit-raw mc)))
     ((and (mozc-kana-toggle-key? key key-state)
           (mozc-kana-toggle mc))
      #f)
     ((mozc-press-key mc key key-state)
      #f) ; consumed
     (else
      (and mozc-use-with-vi?
           (mozc-vi-escape-key? key key-state)
           (mozc-set-mode mc mozc-type-direct))
      (im-commit-raw mc)))))

(define mozc-press-key-handler
  (lambda (mc key key-state)
    (if (mozc-context-on mc)
        (mozc-proc-input-state mc key key-state)
        (mozc-proc-direct-state mc key key-state))))

(define mozc-release-key-handler
  (lambda (mc key key-state)
    (if (or (ichar-control? key)
            (not (mozc-context-on mc)))
        (im-commit-raw mc))))

(define mozc-reset-handler
  (lambda (mc)
    #f))

(define mozc-displace-handler
  (lambda (mc)
    (if (mozc-context-session-ready? mc)
        (mozc-submit mc))))

(define mozc-get-candidate-handler
  (lambda (mc idx accel-enum-hint)
    (let ((candidate (mozc-candidate-at mc idx)))
      (if candidate
          (let ((annotation (or (mozc-alist-ref 'annotation candidate) '())))
            (list (string-append (or (mozc-alist-ref 'prefix annotation) "")
                                 (or (mozc-alist-ref 'value candidate) "")
                                 (or (mozc-alist-ref 'suffix annotation) ""))
                  (or (mozc-alist-ref 'shortcut annotation) "")
                  (or (mozc-alist-ref 'description annotation) "")))
          (list "" "" "")))))

(define mozc-set-candidate-index-handler
  (lambda (mc idx)
    (mozc-select-candidate mc idx)))

(mozc-configure-widgets)

(register-im
  'mozc
  "ja"
  "UTF-8"
  mozc-im-name-label
  mozc-im-short-desc
  #f
  mozc-init-handler
  mozc-release-handler
  context-mode-handler
  mozc-press-key-handler
  mozc-release-key-handler
  mozc-reset-handler
  mozc-get-candidate-handler
  mozc-set-candidate-index-handler
  context-prop-activate-handler
  #f
  #f
  #f
  #f
  mozc-displace-handler
)
