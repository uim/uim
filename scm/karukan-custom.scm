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

(require "i18n.scm")

(define karukan-im-name-label (N_ "Karukan"))
(define karukan-im-short-desc (N_ "Japanese input method with neural kana-kanji conversion"))

(define-custom-group 'karukan
                     karukan-im-name-label
                     karukan-im-short-desc)

(define-custom-group 'karukan-server
                     (N_ "Karukan server")
                     (N_ "long description will be here."))

(define-custom 'karukan-server-command "karukan-imserver"
               '(karukan karukan-server)
               '(string ".*")
               (N_ "karukan-imserver command")
               (N_ "Command name or path of karukan-imserver that converts"))

(define-custom 'karukan-use-surrounding-text? #t
               '(karukan karukan-server)
               '(boolean)
               (N_ "Use the text before the cursor for conversion")
               (N_ "long description will be here."))

;;
;; toolbar
;;

(define karukan-input-mode-indication-alist
  (list
   (list 'action_karukan_direct
         'ja_direct
         "-"
         (N_ "Direct input")
         (N_ "Direct input mode"))
   (list 'action_karukan_hiragana
         'ja_hiragana
         "あ"
         (N_ "Hiragana")
         (N_ "Hiragana input mode"))))

(define-custom 'karukan-widgets '(widget_karukan_input_mode)
  '(karukan toolbar)
  (list 'ordered-list
        (list 'widget_karukan_input_mode
              (N_ "Input mode")
              (N_ "Input mode")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; karukan-configure-widgets is defined in karukan.scm.
(custom-add-hook 'karukan-widgets
                 'custom-set-hooks
                 (lambda ()
                   (karukan-configure-widgets)))

(define-custom 'default-widget_karukan_input_mode 'action_karukan_direct
  '(karukan toolbar)
  (cons 'choice
        (map indication-alist-entry-extract-choice
             karukan-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'karukan-input-mode-actions
               (map car karukan-input-mode-indication-alist)
  '(karukan toolbar)
  (cons 'ordered-list
        (map indication-alist-entry-extract-choice
             karukan-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'karukan-input-mode-actions
                     'custom-set-hooks
                     (lambda ()
                       (custom-choice-range-reflect-olist-val
                        'default-widget_karukan_input_mode
                        'karukan-input-mode-actions
                        karukan-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_karukan_input_mode
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_karukan_input_mode karukan-widgets)))

(custom-add-hook 'karukan-input-mode-actions
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_karukan_input_mode karukan-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_karukan_input_mode
                 'custom-set-hooks
                 (lambda ()
                   (karukan-configure-widgets)))

(custom-add-hook 'karukan-input-mode-actions
                 'custom-set-hooks
                 (lambda ()
                   (karukan-configure-widgets)))
