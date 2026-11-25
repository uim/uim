;;; rime-custom.scm: Customization variables for rime.scm
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

(define rime-im-name-label (N_ "Rime"))
(define rime-im-short-desc (N_ "Chinese input methods of Rime"))

(define-custom-group 'rime
                     rime-im-name-label
                     rime-im-short-desc)

(define-custom-group 'rime-data
                     (N_ "Rime data")
                     (N_ "long description will be here."))

;; rime-lib-default-shared-data-dir is defined by the rime plugin. The
;; preference tools read this file without it.
(define-custom 'rime-shared-data-dir
               (if (symbol-bound? 'rime-lib-default-shared-data-dir)
                   (rime-lib-default-shared-data-dir)
                   "/usr/share/rime-data")
               '(rime rime-data)
               '(pathname directory)
               (N_ "Shared data directory")
               (N_ "The directory that has the schemas of Rime"))

(define-custom 'rime-user-data-dir
               (string-append (or (home-directory (user-name)) "")
                              "/.uim.d/rime")
               '(rime rime-data)
               '(pathname directory)
               (N_ "User data directory")
               (N_ "The directory for the user settings and the learned data of Rime"))

(define-custom 'rime-schema ""
               '(rime rime-data)
               '(string ".*")
               (N_ "Schema")
               (N_ "The ID of the schema to use such as luna_pinyin. The default one of Rime is used if this is empty."))

;;
;; toolbar
;;

(define rime-input-mode-indication-alist
  (list
   (list 'action_rime_off
         'off
         "-"
         (N_ "off")
         (N_ "Direct Input Mode"))
   (list 'action_rime_on
         'on
         "中"
         (N_ "on")
         (N_ "Rime Input Mode"))
   (list 'action_rime_ascii
         'ascii
         "A"
         (N_ "ASCII")
         (N_ "Rime ASCII Mode"))))

(define-custom 'rime-widgets '(widget_rime_input_mode widget_rime_schema)
  '(rime toolbar)
  (list 'ordered-list
        (list 'widget_rime_input_mode
              (N_ "Input mode")
              (N_ "Input mode"))
        (list 'widget_rime_schema
              (N_ "Schema")
              (N_ "Schema")))
  (N_ "Enabled toolbar buttons")
  (N_ "long description will be here."))

;; rime-configure-widgets is defined in rime.scm.
(custom-add-hook 'rime-widgets
                 'custom-set-hooks
                 (lambda ()
                   (rime-configure-widgets)))

(define-custom 'default-widget_rime_input_mode 'action_rime_off
  '(rime toolbar)
  (cons 'choice
        (map indication-alist-entry-extract-choice
             rime-input-mode-indication-alist))
  (N_ "Default input mode")
  (N_ "long description will be here."))

(define-custom 'rime-input-mode-actions
               (map car rime-input-mode-indication-alist)
  '(rime toolbar)
  (cons 'ordered-list
        (map indication-alist-entry-extract-choice
             rime-input-mode-indication-alist))
  (N_ "Input mode menu items")
  (N_ "long description will be here."))

;; value dependency
(if custom-full-featured?
    (custom-add-hook 'rime-input-mode-actions
                     'custom-set-hooks
                     (lambda ()
                       (custom-choice-range-reflect-olist-val
                        'default-widget_rime_input_mode
                        'rime-input-mode-actions
                        rime-input-mode-indication-alist))))

;; activity dependency
(custom-add-hook 'default-widget_rime_input_mode
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_rime_input_mode rime-widgets)))

(custom-add-hook 'rime-input-mode-actions
                 'custom-activity-hooks
                 (lambda ()
                   (memq 'widget_rime_input_mode rime-widgets)))

;; dynamic reconfiguration
(custom-add-hook 'default-widget_rime_input_mode
                 'custom-set-hooks
                 (lambda ()
                   (rime-configure-widgets)))

(custom-add-hook 'rime-input-mode-actions
                 'custom-set-hooks
                 (lambda ()
                   (rime-configure-widgets)))
