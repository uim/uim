;;; predict-custom.scm: Customization variables for predict-*.scm
;;;
;;; Copyright (c) 2009-2013 uim Project https://github.com/uim/uim
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

(require-extension (srfi 1))

(require "i18n.scm")
(require "sqlite3.scm")

(define-custom-group 'predict
                     (N_ "Ancillary Prediction")
                     (N_ "long description will be here."))


(define-custom 'predict-custom-enable? #f
               '(predict)
               '(boolean)
               (N_ "Enable ancillary prediction (for Ajax-IME, Canna, SJ3, TUT-Code, Wnn)")
               (N_ "long description will be here."))

(define-custom 'predict-custom-methods '()
               '(predict)
               (list 'ordered-list
                     (list 'look
                           (N_ "Look")
                           (N_ "Look prediction"))
                     (list 'look-skk
                           (N_ "Look-SKK")
                           (N_ "Look-SKK prediction"))
                     (list 'sqlite3
                           (N_ "Sqlite3")
                           (N_ "Sqlite3 prediction"))
                     (list 'google-suggest
                           (N_ "Google Suggest")
                           (N_ "Google Suggest prediction")))
               (N_ "Prediction methods")
               (N_ "long description will be here."))

(custom-add-hook 'predict-custom-methods
                 'custom-activity-hooks
                 (lambda ()
                   predict-custom-enable?))

;;
;; predict-look
;;
(define-custom-group 'predict-look
                     (N_ "Look prediction")
                     (N_ "long description will be here."))

(define-custom 'predict-custom-look-dict "/usr/share/dict/words"
               '(predict predict-look)
               '(pathname regular-file)
               (N_ "UNIX look dictionary file")
               (N_ "long description will be here"))

(define-custom 'predict-custom-look-candidates-max 10
               '(predict predict-look)
               '(integer 1 99)
               (N_ "Max words of candidates for look")
               (N_ "long description will be here"))

(custom-add-hook 'predict-custom-look-dict
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'look item))
                              predict-custom-methods))))

(custom-add-hook 'predict-custom-look-candidates-max
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'look item))
                              predict-custom-methods))))

;;
;; predict-look-skk
;;
(define-custom-group 'predict-look-skk
                     (N_ "Look-SKK prediction")
                     (N_ "long description will be here."))

(define-custom 'predict-custom-look-skk-jisyo "/usr/share/skk/SKK-JISYO.L"
               '(predict predict-look-skk)
               '(pathname regular-file)
               (N_ "Sorted SKK-JISYO dictionary file")
               (N_ "long description will be here"))

(define-custom 'predict-custom-look-skk-candidates-max 10
               '(predict predict-look-skk)
               '(integer 1 99)
               (N_ "Max words of candidates for look-skk")
               (N_ "long description will be here"))

(custom-add-hook 'predict-custom-look-skk-jisyo
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'look-skk item))
                              predict-custom-methods))))

(custom-add-hook 'predict-custom-look-skk-candidates-max
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'look-skk item))
                              predict-custom-methods))))

;;
;; predict-sqlite3
;;
(define-custom-group 'predict-sqlite3
                     (N_ "Sqlite3 prediction")
                     (N_ "long description will be here."))

(define-custom 'predict-custom-sqlite3-candidates-max 5
               '(predict predict-sqlite3)
               '(integer 1 99)
               (N_ "Max words of candidates for sqlite3")
               (N_ "long description will be here"))

(custom-add-hook 'predict-custom-sqlite3-candidates-max
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'sqlite3 item))
                              predict-custom-methods))))

;;
;; predict-google-suggest
;;
(define-custom-group 'predict-google-suggest
                     (N_ "Google suggest prediction")
                     (N_ "long description will be here."))

(define-custom 'predict-custom-google-suggest-candidates-max 5
               '(predict predict-google-suggest)
               '(integer 1 99)
               (N_ "Max words of candidates for google suggest")
               (N_ "long description will be here"))

(custom-add-hook 'predict-custom-google-suggest-candidates-max
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'google-suggest item))
                              predict-custom-methods))))

(define-custom 'predict-custom-google-suggest-language 'en
               '(predict predict-google-suggest)
               (list 'choice
                     (list 'en (N_ "English") (N_ "English"))
                     (list 'ja (N_ "Japanese")  (N_ "Japanese")))
               (N_ "Language")
               (N_ "long description will be here."))

(custom-add-hook 'predict-custom-google-suggest-language
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'google-suggest item))
                              predict-custom-methods))))

(define-custom 'predict-custom-google-suggest-use-ssl #t
               '(predict predict-google-suggest)
               '(boolean)
               (N_ "Enable SSL with Google Suggest")
               (N_ "long description will be here."))

(custom-add-hook 'predict-custom-google-suggest-use-ssl
                 'custom-activity-hooks
                 (lambda ()
                   (and predict-custom-enable?
                        (find (lambda (item)
                                (eq? 'google-suggest item))
                              predict-custom-methods))))
