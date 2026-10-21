;;; anthy-utf8.scm: Anthy (UTF-8) for uim.
;;; charset: UTF-8
;;;
;;; Copyright (c) 2003-2026 uim Project https://github.com/uim/uim
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

;; "anthy-utf8" is the same input method as "anthy" except its name,
;; actions and toolbar settings. It's kept for compatibility. "anthy"
;; used EUC-JP and "anthy-utf8" used UTF-8 before.

(require-dynlib "anthy")
(require "anthy.scm")
(require-custom "anthy-utf8-custom.scm")

(for-each
 (lambda (name)
   (apply register-action
          (symbol-append 'action_anthy_utf8_ name)
          (cdr (fetch-action (symbol-append 'action_anthy_ name)))))
 '(hiragana katakana halfkana halfwidth_alnum fullwidth_alnum direct
   roma kana azik kzik act))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define (anthy-utf8-configure-widgets)
  (register-widget 'widget_anthy_utf8_input_mode
                   (activity-indicator-new anthy-utf8-input-mode-actions)
                   (actions-new anthy-utf8-input-mode-actions))
  (register-widget 'widget_anthy_utf8_kana_input_method
                   (activity-indicator-new anthy-utf8-kana-input-method-actions)
                   (actions-new anthy-utf8-kana-input-method-actions))
  (context-list-replace-widgets! 'anthy-utf8 anthy-utf8-widgets))

(define (anthy-utf8-init-handler id im arg)
  (let ((ac (anthy-init-handler id im arg)))
    (anthy-context-set-widgets! ac anthy-utf8-widgets)
    ac))

(anthy-utf8-configure-widgets)

(register-im
 'anthy-utf8
 "ja"
 "UTF-8"
 anthy-utf8-im-name-label
 anthy-utf8-im-short-desc
 #f
 anthy-utf8-init-handler
 anthy-release-handler
 context-mode-handler
 anthy-press-key-handler
 anthy-release-key-handler
 anthy-reset-handler
 anthy-get-candidate-handler
 anthy-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
