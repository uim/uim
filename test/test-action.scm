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

(require-extension (unittest))

(require "load-action.scm")
(require "rk.scm")
(require "japanese.scm")

(define test-orig-context-propagate-widget-configuration
  context-propagate-widget-configuration)
(define test-orig-context-propagate-widget-states
  context-propagate-widget-states)

(define (setup)
  ;; Some tests define the defaults of the widgets and replace the
  ;; propagation procedures. Reset them for the following tests.
  (for-each (lambda (sym)
              (if (symbol-bound? sym)
                  (%%undefine sym)))
            '(default-widget_test_input_mode
              default-widget_test_kana_input_method))
  (set! context-propagate-widget-configuration
        test-orig-context-propagate-widget-configuration)
  (set! context-propagate-widget-states
        test-orig-context-propagate-widget-states)
  (eval
   '(begin
      (custom-set-value! 'toolbar-show-action-based-switcher-button? #f)

      (set! widget-proto-list ())
      (set! action-list ())

      (define test-type-hiragana 0)
      (define test-type-katakana 1)
      (define test-type-hankana 2)

      (define test-input-rule-roma 0)
      (define test-input-rule-kana 1)
      (define test-input-rule-azik 2)

      (define context-rec-spec
        '((id      #f) ;; must be first member
          (im      #f)
          (widgets ())))
      (define-record 'context context-rec-spec)

      (define-record 'test-context
        (append
         context-rec-spec
         (list
          (list 'on                 #f)
          (list 'wide-latin         #f)
          (list 'kana-mode          test-type-hiragana)
          (list 'rkc                ())
          (list 'input-rule         test-input-rule-roma))))

      (register-action 'action_test_hiragana
                       (lambda (tc)
                         '(figure_ja_hiragana
                           "あ"
                           "ひらがな"
                           "ひらがな入力モード"))
                       (lambda (tc)
                         (and (test-context-on tc)
                              (= (test-context-kana-mode tc)
                                 test-type-hiragana)))
                       (lambda (tc)
                         (test-context-set-on! tc #t)
                         (test-context-set-kana-mode! tc test-type-hiragana)
                         (set! test-activated 'action_test_hiragana)))

      (register-action 'action_test_katakana
                       (lambda (tc)
                         '(figure_ja_katakana
                           "ア"
                           "カタカナ"
                           "カタカナ入力モード"))
                       (lambda (tc)
                         (and (test-context-on tc)
                              (= (test-context-kana-mode tc)
                                 test-type-katakana)))
                       (lambda (tc)
                         (test-context-set-on! tc #t)
                         (test-context-set-kana-mode! tc test-type-katakana)
                         (set! test-activated 'action_test_katakana)))

      (register-action 'action_test_hankana
                       (lambda (tc)
                         '(figure_ja_hankana
                           "ｱ"
                           "半角カタカナ"
                           "半角カタカナ入力モード"))
                       (lambda (tc)
                         (and (test-context-on tc)
                              (= (test-context-kana-mode tc)
                                 test-type-hankana)))
                       (lambda (tc)
                         (test-context-set-on! tc #t)
                         (test-context-set-kana-mode! tc test-type-hankana)
                         (set! test-activated 'action_test_hankana)))

      (register-action 'action_test_direct
                       (lambda (tc)
                         '(figure_ja_direct
                           "a"
                           "直接入力"
                           "直接(無変換)入力モード"))
                       (lambda (tc)
                         (and (not (test-context-on tc))
                              (not (test-context-wide-latin tc))))
                       (lambda (tc)
                         (test-context-set-on! tc #f)
                         (test-context-set-wide-latin! tc #f)
                         (set! test-activated 'action_test_direct)))

      (register-action 'action_test_zenkaku
                       (lambda (tc)
                         '(figure_ja_zenkaku
                           "Ａ"
                           "全角英数"
                           "全角英数入力モード"))
                       (lambda (tc)
                         (and (not (test-context-on tc))
                              (test-context-wide-latin tc)))
                       (lambda (tc)
                         (test-context-set-on! tc #f)
                         (test-context-set-wide-latin! tc #t)
                         (set! test-activated 'action_test_zenkaku)))

      (register-action 'action_test_alt_direct
                       (lambda (tc)
                         '(figure_ja_direct
                           "aa"
                           "直接入力"
                           "直接(無変換)入力モード"))
                       (lambda (tc)
                         (and (not (test-context-on tc))
                              (not (test-context-wide-latin tc))))
                       (lambda (tc)
                         (test-context-set-on! tc #f)
                         (test-context-set-wide-latin! tc #f)
                         (set! test-activated 'action_test_alt_direct)))

      (register-action 'action_test_roma
                       (lambda (tc)
                         '(figure_ja_roma
                           "Ｒ"
                           "ローマ字"
                           "ローマ字入力モード"))
                       (lambda (tc)
                         (= (test-context-input-rule tc)
                            test-input-rule-roma))
                       (lambda (tc)
                         (rk-context-set-rule! (test-context-rkc tc)
                                               ja-rk-rule)
                         (test-context-set-input-rule! tc test-input-rule-roma)
                         (set! test-activated 'action_test_roma)))

      (register-action 'action_test_kana
                       (lambda (tc)
                         '(figure_ja_kana
                           "か"
                           "かな"
                           "かな入力モード"))
                       (lambda (tc)
                         (= (test-context-input-rule tc)
                            test-input-rule-kana))
                       (lambda (tc)
                         (require "japanese-kana.scm")
                         (rk-context-set-rule! (test-context-rkc tc)
                                               ja-kana-hiragana-rule)
                         (test-context-set-input-rule! tc test-input-rule-kana)
                         (set! test-activated 'action_test_kana)))

      (register-widget
       'widget_test_input_mode
       (activity-indicator-new '(action_test_hiragana
                                 action_test_katakana
                                 action_test_hankana
                                 action_test_direct
                                 action_test_zenkaku))
       (actions-new '(action_test_hiragana
                      action_test_katakana
                      action_test_hankana
                      action_test_direct
                      action_test_zenkaku)))

      (register-widget
       'widget_test_kana_input_method
       (activity-indicator-new '(action_test_roma
                                 action_test_kana))
       (actions-new '(action_test_roma
                      action_test_kana)))

      (register-widget
       'widget_test_null
       #f
       #f)

      (register-widget
       'widget_fallback
       (indicator-new (lambda (owner)
                        fallback-indication))
       #f) ;; has no actions

      (register-widget
       'widget_test_kana_input_method_without_act_indicator
       (indicator-new (lambda (owner)
                        fallback-indication))
       (actions-new '(action_test_roma
                      action_test_kana)))

      (define tc (test-context-new 0 (retrieve-im 'direct)))
      (test-context-set-rkc! tc (rk-context-new ja-rk-rule #t #f))

      (define test-prop-label #f)
      (define im-update-prop-label
        (lambda (context message)
          (set! test-prop-label message)))
      (define test-prop-list #f)
      (define im-update-prop-list
        (lambda (context message)
          (set! test-prop-list message)))

      (define test-mode-list ())
      (define test-updated-mode-list ())
      (define im-clear-mode-list
        (lambda (context)
          (set! test-mode-list ())))
      (define im-update-mode-list
        (lambda (context)
          (set! test-updated-mode-list test-mode-list)))
      (define im-pushback-mode-list
        (lambda (context label)
          (set! test-mode-list (append test-mode-list
                                       (list label)))))
      (define test-updated-mode #f)
      (define im-update-mode
        (lambda (context mode)
          (set! test-updated-mode mode)))

      (define test-widget-conf #f)
      (define test-widget-state #f)
      (define test-activated #f))
   (interaction-environment)))

(test-begin "indicator-new")
(setup)
(define test-indicator (indicator-new (lambda ()
                                        '(unknown
                                          "?"
                                          "unknown"
                                          "Unknown"))))

(test-false (indicator-id test-indicator))
(test-false (indicator-activity-pred test-indicator))
(test-false (indicator-handler test-indicator))
(test-end)

(test-begin "register-action")
(setup)
(set! action-list ())
(test-equal 0
            (length action-list))
(register-action
 'action_test_hiragana
 (lambda (tc)
   '(figure_ja_hiragana
     "あ"
     "ひらがな"
     "ひらがな入力モード"))
 (lambda (tc)
   (and (test-context-on tc)
        (= (test-context-kana-mode tc)
           test-type-hiragana)))
 (lambda (tc)
   (test-context-set-on! tc #t)
   (test-context-set-kana-mode! tc test-type-hiragana)))
(test-equal 1
            (length action-list))
(test-equal 'action_test_hiragana
            (caar action-list))

(register-action
 'action_test_katakana
 (lambda (tc)
   '(figure_ja_katakana
     "ア"
     "カタカナ"
     "カタカナ入力モード"))
 (lambda (tc)
   (and (test-context-on tc)
        (= (test-context-kana-mode tc)
           test-type-katakana)))
 (lambda (tc)
   (test-context-set-on! tc #t)
   (test-context-set-kana-mode! tc test-type-katakana)))
(test-equal 2
            (length action-list))
(test-equal 'action_test_katakana
            (caar action-list))

(register-action
 'action_test_hankana
 (lambda (tc)
   '(figure_ja_hankana
     "ｱ"
     "半角カタカナ"
     "半角カタカナ入力モード"))
 (lambda (tc)
   (and (test-context-on tc)
        (= (test-context-kana-mode tc)
           test-type-hankana)))
 (lambda (tc)
   (test-context-set-on! tc #t)
   (test-context-set-kana-mode! tc test-type-hankana)))
(test-equal 3
            (length action-list))
(test-equal 'action_test_hankana
            (caar action-list))
(test-end)

(test-begin "fetch-action")
(setup)
(test-equal 'action_test_hiragana
            (action-id (fetch-action 'action_test_hiragana)))
(test-equal 'action_test_katakana
            (action-id (fetch-action 'action_test_katakana)))
(test-equal 'action_test_hankana
            (action-id (fetch-action 'action_test_hankana)))
(test-end)

(test-begin "action-active?")
(setup)
(test-false (action-active? (fetch-action 'action_test_hiragana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_katakana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_hankana)
                            tc))
(test-eq #t (action-active? (fetch-action 'action_test_direct)
                            tc))
(test-false (action-active? (fetch-action 'action_test_zenkaku)
                            tc))

(test-context-set-wide-latin! tc #t)
(test-false (action-active? (fetch-action 'action_test_hiragana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_katakana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_hankana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_direct)
                            tc))
(test-eq #t (action-active? (fetch-action 'action_test_zenkaku)
                            tc))

(test-context-set-on! tc #t)
(test-eq #t (action-active? (fetch-action 'action_test_hiragana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_katakana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_hankana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_direct)
                            tc))
(test-false (action-active? (fetch-action 'action_test_zenkaku)
                            tc))

(test-context-set-kana-mode! tc test-type-katakana)
(test-false (action-active? (fetch-action 'action_test_hiragana)
                            tc))
(test-eq #t (action-active? (fetch-action 'action_test_katakana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_hankana)
                            tc))
(test-false (action-active? (fetch-action 'action_test_direct)
                            tc))
(test-false (action-active? (fetch-action 'action_test_zenkaku)
                            tc))
(test-end)

(test-begin "action-indicate")
(setup)
(test-equal '(figure_ja_hiragana
              "あ"
              "ひらがな"
              "ひらがな入力モード")
            (action-indicate (fetch-action 'action_test_hiragana)
                             tc))
(test-equal '(figure_ja_katakana
              "ア"
              "カタカナ"
              "カタカナ入力モード")
            (action-indicate (fetch-action 'action_test_katakana)
                             tc))
(test-equal '(figure_ja_kana
              "か"
              "かな"
              "かな入力モード")
            (action-indicate (fetch-action 'action_test_kana)
                             tc))
;; no action
(test-equal fallback-indication
            (action-indicate #f tc))
;; no indication handler
(test-equal fallback-indication
            (action-indicate (action-new) tc))
(test-end)

(test-begin "actions-new")
(setup)
(test-equal '(action_test_katakana
              action_test_kana
              action_test_hiragana)
            (map action-id (actions-new '(action_test_katakana
                                          action_test_kana
                                          action_test_hiragana))))
(test-equal '(action_test_katakana
              action_test_kana
              action_test_hiragana)
            (map action-id (actions-new '(action_test_katakana
                                          action_test_kana
                                          action_nonexistent
                                          action_test_hiragana))))
(test-equal '()
            (map action-id (actions-new ())))
(test-end)

(test-begin "activity-indicator-new")
(setup)
(define indicator (activity-indicator-new '(action_test_hiragana
                                            action_test_katakana
                                            action_test_hankana
                                            action_test_direct
                                            action_test_zenkaku)))
(test-equal '(figure_ja_direct
              "a"
              "直接入力"
              "直接(無変換)入力モード")
            (action-indicate indicator tc))

(test-context-set-wide-latin! tc #t)
(test-equal '(figure_ja_zenkaku
              "Ａ"
              "全角英数"
              "全角英数入力モード")
            (action-indicate indicator tc))

(test-context-set-on! tc #t)
(test-equal '(figure_ja_hiragana
              "あ"
              "ひらがな"
              "ひらがな入力モード")
            (action-indicate indicator tc))

(test-context-set-kana-mode! tc test-type-katakana)
(test-equal '(figure_ja_katakana
              "ア"
              "カタカナ"
              "カタカナ入力モード")
            (action-indicate indicator tc))
;; no activity case
(define test-type-invalid 100)
(test-context-set-kana-mode! tc test-type-invalid)
(test-equal '(unknown
              "?"
              "unknown"
              "unknown")
            (action-indicate indicator tc))
(test-end)

(test-begin "register-widget")
(setup)
(set! widget-proto-list ())
(test-equal 0
            (length widget-proto-list))

(begin
  (register-widget
   'widget_test_input_mode
   (indicator-new (lambda (tc)
                    fallback-indication))
   (actions-new '(action_test_hiragana
                  action_test_katakana
                  action_test_hankana
                  action_test_direct
                  action_test_zenkaku)))
  #t)
(test-equal 1
            (length widget-proto-list))
(test-equal 'widget_test_input_mode
            (caar widget-proto-list))

(begin
  (register-widget
   'widget_test_input_mode
   (indicator-new (lambda (tc)
                    fallback-indication))
   (actions-new '(action_test_direct)))
  #t)
(test-equal 1
            (length widget-proto-list))
(test-equal 'widget_test_input_mode
            (caar widget-proto-list))

(begin
  (register-widget
   'widget_test_kana_input_method
   (indicator-new (lambda (tc)
                    fallback-indication))
   (actions-new '(action_test_roma
                  action_test_kana)))
  #t)
(test-equal 2
            (length widget-proto-list))
(test-equal 'widget_test_kana_input_method
            (caar widget-proto-list))
(test-equal 'widget_test_input_mode
            (car (cadr widget-proto-list)))
(test-end)

(test-begin "widget-new")
(setup)
(test-false (widget-new 'widget_test_nonexistent tc))
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
(test-equal 'widget_test_input_mode
            (widget-id test-input-mode))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))

;; widget_test_input_mode with default value
(begin
  (define default-widget_test_input_mode 'action_test_hiragana)
  (define test-input-mode
    (widget-new 'widget_test_input_mode tc)))
(test-equal 'action_test_hiragana
            (action-id (widget-activity test-input-mode)))

;; widget_test_input_mode with default value #2
(begin
  (define default-widget_test_input_mode 'action_test_katakana)
  (define test-input-mode
    (widget-new 'widget_test_input_mode tc)))
(test-equal 'action_test_katakana
            (action-id (widget-activity test-input-mode)))

;; widget_test_input_mode with default value #3
(begin
  (define default-widget_test_input_mode 'action_test_zenkaku)
  (define test-input-mode
    (widget-new 'widget_test_input_mode tc)))
(test-equal 'action_test_zenkaku
            (action-id (widget-activity test-input-mode)))

;; widget_test_input_mode with invalid default value
(begin
  (define default-widget_test_input_mode 'action_nonexistent)
  (define test-input-mode
    (widget-new 'widget_test_input_mode tc)))
(test-equal 'action_test_zenkaku
            (action-id (widget-activity test-input-mode)))

;; widget_test_kana_input_method
(define test-kana-input-method
  (widget-new 'widget_test_kana_input_method tc))
(test-equal 'action_test_roma
            (action-id (widget-activity test-kana-input-method)))

;; widget_test_kana_input_method with default value
(begin
  (define default-widget_test_kana_input_method 'action_test_kana)
  (define test-kana-input-method
    (widget-new 'widget_test_kana_input_method tc)))
(test-equal 'action_test_kana
            (action-id (widget-activity test-kana-input-method)))

;; widget_test_kana_input_method with invalid default value
(begin
  (define default-widget_test_kana_input_method 'action_nonexistent)
  (define test-kana-input-method
    (widget-new 'widget_test_kana_input_method tc)))
(test-equal 'action_test_kana
            (action-id (widget-activity test-kana-input-method)))
(test-end)

(test-begin "widget-activity")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
;; action_test_direct (initial activity)
(test-false (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hiragana
            (test-context-kana-mode tc))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))

;; action_test_direct -> action_test_hiragana
(test-context-set-wide-latin! tc #t)
(test-context-set-on! tc #t)
(test-equal test-type-hiragana
            (test-context-kana-mode tc))
(test-equal 'action_test_hiragana
            (action-id (widget-activity test-input-mode)))

;; action_test_hiragana -> action_test_katakana
(test-context-set-wide-latin! tc #f)
(test-context-set-kana-mode! tc test-type-katakana)
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-katakana
            (test-context-kana-mode tc))
(test-equal 'action_test_katakana
            (action-id (widget-activity test-input-mode)))

;; action_test_katakana -> action_test_hankana
(test-context-set-kana-mode! tc test-type-hankana)
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_hankana
            (action-id (widget-activity test-input-mode)))

;; action_test_hankana -> action_test_direct
(test-context-set-on! tc #f)
(test-false (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))

;; action_test_direct -> invalid
(define test-type-invalid 100)
(test-context-set-on! tc #t)
(test-context-set-kana-mode! tc test-type-invalid)
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-invalid
            (test-context-kana-mode tc))
(test-false (widget-activity test-input-mode))

;; duplicate activity
(begin
  (register-widget
   'widget_test_invalid_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   (actions-new '(action_test_hiragana
                  action_test_katakana
                  action_test_hankana
                  action_test_direct
                  action_test_alt_direct
                  action_test_zenkaku)))
  (context-init-widgets! tc '(widget_test_invalid_input_mode
                              widget_test_kana_input_method))
  (define test-invalid-input-mode
    (widget-new 'widget_test_invalid_input_mode tc)))

;; action_test_direct and action_test_alt_direct are conflicted
(test-false (widget-activity test-invalid-input-mode))
;; conflicted -> action_test_hiragana
(test-eq #t (widget-activate! test-invalid-input-mode
                              'action_test_hiragana))
(test-equal 'action_test_hiragana
            (action-id (widget-activity test-invalid-input-mode)))
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-invalid-input-mode
                              'action_test_katakana))
(test-equal 'action_test_katakana
            (action-id (widget-activity test-invalid-input-mode)))
(test-end)

(test-begin "widget-activate!")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
;; action_test_direct (initial activity)
(test-false (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hiragana
            (test-context-kana-mode tc))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))
;; action_test_direct -> action_test_hiragana
(test-eq #t (widget-activate! test-input-mode
                              'action_test_hiragana))
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hiragana
            (test-context-kana-mode tc))
(test-equal 'action_test_hiragana
            (action-id (widget-activity test-input-mode)))
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode
                              'action_test_katakana))
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-katakana
            (test-context-kana-mode tc))
(test-equal 'action_test_katakana
            (action-id (widget-activity test-input-mode)))
;; action_test_katakana -> action_test_hankana
(test-eq #t (widget-activate! test-input-mode
                              'action_test_hankana))
(test-eq #t (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_hankana
            (action-id (widget-activity test-input-mode)))
;; action_test_hankana -> action_test_zenkaku
(test-eq #t (widget-activate! test-input-mode
                              'action_test_zenkaku))
(test-false (test-context-on tc))
(test-eq #t (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_zenkaku
            (action-id (widget-activity test-input-mode)))
;; action_test_zenkaku -> action_test_direct
(test-eq #t (widget-activate! test-input-mode
                              'action_test_direct))
(test-false (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))
;; action_test_direct -> invalid
(test-false (widget-activate! test-input-mode
                              'action_nonexistent))
(test-false (test-context-on tc))
(test-false (test-context-wide-latin tc))
(test-equal test-type-hankana
            (test-context-kana-mode tc))
(test-equal 'action_test_direct
            (action-id (widget-activity test-input-mode)))
(test-end)

(test-begin "widget-configuration")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
(test-equal '(action_unknown
              (figure_ja_hiragana
               "あ"
               "ひらがな"
               "ひらがな入力モード")
              (figure_ja_katakana
               "ア"
               "カタカナ"
               "カタカナ入力モード")
              (figure_ja_hankana
               "ｱ"
               "半角カタカナ"
               "半角カタカナ入力モード")
              (figure_ja_direct
               "a"
               "直接入力"
               "直接(無変換)入力モード")
              (figure_ja_zenkaku
               "Ａ"
               "全角英数"
               "全角英数入力モード"))
            (widget-configuration test-input-mode))

;; widget_test_kana_input_method
(define test-kana-input-method
  (widget-new 'widget_test_kana_input_method tc))
(test-equal '(action_unknown
              (figure_ja_roma
               "Ｒ"
               "ローマ字"
               "ローマ字入力モード")
              (figure_ja_kana
               "か"
               "かな"
               "かな入力モード"))
            (widget-configuration test-kana-input-method))

;; widget_test_null
(define test-null
  (widget-new 'widget_test_null tc))
(test-equal '(action_unknown)
            (widget-configuration test-null))
(test-end)

(test-begin "widget-state")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
(test-eq #t (equal? (list (fetch-action 'action_test_direct)
                          '(figure_ja_direct
                            "a"
                            "直接入力"
                            "直接(無変換)入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (widget-activate! test-input-mode
                              'action_test_katakana))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))
(test-false (widget-activate! test-input-mode
                              'action_nonexistent))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))

;; widget_test_kana_input_method
(define test-kana-input-method
  (widget-new 'widget_test_kana_input_method tc))
(test-eq #t (equal? (list (fetch-action 'action_test_roma)
                          '(figure_ja_roma
                            "Ｒ"
                            "ローマ字"
                            "ローマ字入力モード"))
                    (widget-state test-kana-input-method)))

;; widget_test_null
(define test-null
  (widget-new 'widget_test_null tc))
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-state test-null)))
(test-end)

(test-begin "widget-update-configuration!")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
(test-equal '(action_unknown
              (figure_ja_hiragana
               "あ"
               "ひらがな"
               "ひらがな入力モード")
              (figure_ja_katakana
               "ア"
               "カタカナ"
               "カタカナ入力モード")
              (figure_ja_hankana
               "ｱ"
               "半角カタカナ"
               "半角カタカナ入力モード")
              (figure_ja_direct
               "a"
               "直接入力"
               "直接(無変換)入力モード")
              (figure_ja_zenkaku
               "Ａ"
               "全角英数"
               "全角英数入力モード"))
            (widget-configuration test-input-mode))
(test-false (widget-prev-config test-input-mode))
(test-eq #t (widget-update-configuration! test-input-mode))
(test-equal '(action_unknown
              (figure_ja_hiragana
               "あ"
               "ひらがな"
               "ひらがな入力モード")
              (figure_ja_katakana
               "ア"
               "カタカナ"
               "カタカナ入力モード")
              (figure_ja_hankana
               "ｱ"
               "半角カタカナ"
               "半角カタカナ入力モード")
              (figure_ja_direct
               "a"
               "直接入力"
               "直接(無変換)入力モード")
              (figure_ja_zenkaku
               "Ａ"
               "全角英数"
               "全角英数入力モード"))
            (widget-configuration test-input-mode))
(test-equal '(action_unknown
              (figure_ja_hiragana
               "あ"
               "ひらがな"
               "ひらがな入力モード")
              (figure_ja_katakana
               "ア"
               "カタカナ"
               "カタカナ入力モード")
              (figure_ja_hankana
               "ｱ"
               "半角カタカナ"
               "半角カタカナ入力モード")
              (figure_ja_direct
               "a"
               "直接入力"
               "直接(無変換)入力モード")
              (figure_ja_zenkaku
               "Ａ"
               "全角英数"
               "全角英数入力モード"))
            (widget-prev-config test-input-mode))
(test-false (widget-update-configuration! test-input-mode))
(test-equal '(action_unknown
              (figure_ja_hiragana
               "あ"
               "ひらがな"
               "ひらがな入力モード")
              (figure_ja_katakana
               "ア"
               "カタカナ"
               "カタカナ入力モード")
              (figure_ja_hankana
               "ｱ"
               "半角カタカナ"
               "半角カタカナ入力モード")
              (figure_ja_direct
               "a"
               "直接入力"
               "直接(無変換)入力モード")
              (figure_ja_zenkaku
               "Ａ"
               "全角英数"
               "全角英数入力モード"))
            (widget-prev-config test-input-mode))

;; widget_test_null
(define test-null
  (widget-new 'widget_test_null tc))
(test-equal '(action_unknown)
            (widget-configuration test-null))
(test-false (widget-prev-config test-null))
;; initial update (widget_test_null with fallback-indication)
(test-eq #t (widget-update-configuration! test-null))
;; subsequent update
(test-false (widget-update-configuration! test-null))
(test-end)

(test-begin "widget-update-state!")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))

;; initial state
(test-eq #t (equal? (list (fetch-action 'action_test_direct)
                          '(figure_ja_direct
                            "a"
                            "直接入力"
                            "直接(無変換)入力モード"))
                    (widget-state test-input-mode)))
(test-false (widget-prev-state test-input-mode))
;; initial update
(test-eq #t (widget-update-state! test-input-mode))
(test-eq #t (equal? (list (fetch-action 'action_test_direct)
                          '(figure_ja_direct
                            "a"
                            "直接入力"
                            "直接(無変換)入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (equal? (list (fetch-action 'action_test_direct)
                          '(figure_ja_direct
                            "a"
                            "直接入力"
                            "直接(無変換)入力モード"))
                    (widget-prev-state test-input-mode)))
;; action_test_direct -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode 'action_test_katakana))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (equal? (list (fetch-action 'action_test_direct)
                          '(figure_ja_direct
                            "a"
                            "直接入力"
                            "直接(無変換)入力モード"))
                    (widget-prev-state test-input-mode)))
(test-eq #t (widget-update-state! test-input-mode))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-prev-state test-input-mode)))
;; action_test_katakana -> action_test_katakana
(test-false (widget-update-state! test-input-mode))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-prev-state test-input-mode)))
;; invalid activation
(test-false (widget-activate! test-input-mode 'action_nonexistent))
(test-false (widget-update-state! test-input-mode))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-state test-input-mode)))
(test-eq #t (equal? (list (fetch-action 'action_test_katakana)
                          '(figure_ja_katakana
                            "ア"
                            "カタカナ"
                            "カタカナ入力モード"))
                    (widget-prev-state test-input-mode)))

;; widget_test_null
(define test-null
  (widget-new 'widget_test_null tc))

;; initial state
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-state test-null)))
(test-false (widget-prev-state test-null))
;; initial update
(test-eq #t (widget-update-state! test-null))
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-state test-null)))
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-prev-state test-null)))
;; subsequent update
(test-false (widget-update-state! test-null))
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-state test-null)))
(test-eq #t (equal? (list #f
                          '(unknown
                            "?"
                            "unknown"
                            "unknown"))
                    (widget-prev-state test-null)))
(test-end)

(test-begin "widget-debug-message")
(setup)
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))
(test-equal "something in somewhere. debug widget_test_input_mode."
            (widget-debug-message test-input-mode
                                  "somewhere"
                                  "something"))
(test-end)

(test-begin "indication-compose-label")
(setup)
(test-equal "figure_ja_hiragana\tあ\tひらがな\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_hiragana)
                              tc)))
(test-equal "figure_ja_katakana\tア\tカタカナ\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_katakana)
                              tc)))
(test-equal "figure_ja_hankana\tｱ\t半角カタカナ\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_hankana)
                              tc)))
(test-equal "figure_ja_direct\ta\t直接入力\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_direct)
                              tc)))
(test-equal "figure_ja_zenkaku\tＡ\t全角英数\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_zenkaku)
                              tc)))
(test-equal "figure_ja_roma\tＲ\tローマ字\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_roma)
                              tc)))
(test-equal "figure_ja_kana\tか\tかな\n"
            (indication-compose-label
             (action-indicate (fetch-action 'action_test_kana)
                              tc)))
(test-end)

(test-begin "indication-compose-branch")
(setup)
(test-equal "branch\tfigure_ja_hiragana\tあ\tひらがな\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_hiragana)
                              tc)))
(test-equal "branch\tfigure_ja_katakana\tア\tカタカナ\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_katakana)
                              tc)))
(test-equal "branch\tfigure_ja_hankana\tｱ\t半角カタカナ\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_hankana)
                              tc)))
(test-equal "branch\tfigure_ja_direct\ta\t直接入力\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_direct)
                              tc)))
(test-equal "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_zenkaku)
                              tc)))
(test-equal "branch\tfigure_ja_roma\tＲ\tローマ字\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_roma)
                              tc)))
(test-equal "branch\tfigure_ja_kana\tか\tかな\n"
            (indication-compose-branch
             (action-indicate (fetch-action 'action_test_kana)
                              tc)))
(test-end)

(test-begin "indication-compose-leaf")
(setup)
;; inactive leaves
(test-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_hiragana)
                              tc)
             'action_test_hiragana
             #f))
(test-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_katakana)
                              tc)
             'action_test_katakana
             #f))
(test-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_hankana)
                              tc)
             'action_test_hankana
             #f))
(test-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_direct)
                              tc)
             'action_test_direct
             #f))
(test-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_zenkaku)
                              tc)
             'action_test_zenkaku
             #f))
(test-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_roma)
                              tc)
             'action_test_roma
             #f))
(test-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_kana)
                              tc)
             'action_test_kana
             #f))
;; active leaves
(test-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_hiragana)
                              tc)
             'action_test_hiragana
             #t))
(test-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_katakana)
                              tc)
             'action_test_katakana
             #t))
(test-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_hankana)
                              tc)
             'action_test_hankana
             #t))
(test-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_direct)
                              tc)
             'action_test_direct
             #t))
(test-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_zenkaku)
                              tc)
             'action_test_zenkaku
             #t))
(test-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_roma)
                              tc)
             'action_test_roma
             #t))
(test-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n"
            (indication-compose-leaf
             (action-indicate (fetch-action 'action_test_kana)
                              tc)
             'action_test_kana
             #t))
(test-end)

(test-begin "widget-compose-live-branch")
(setup)
;; widget_test_input_mode
(define test-input-mode
  (widget-new 'widget_test_input_mode tc))

(test-equal (string-append
             "branch\tfigure_ja_direct\ta\t直接入力\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
            (widget-compose-live-branch test-input-mode))
(test-eq #t (widget-activate! test-input-mode
                              'action_test_zenkaku))
(test-equal (string-append
             "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n")
            (widget-compose-live-branch test-input-mode))

;; prop_test_kana_input_method
(define test-kana-input-method
  (widget-new 'widget_test_kana_input_method tc))
(test-equal (string-append
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            (widget-compose-live-branch test-kana-input-method))
(test-eq #t (widget-activate! test-kana-input-method
                              'action_test_kana))
(test-equal (string-append
             "branch\tfigure_ja_kana\tか\tかな\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n")
            (widget-compose-live-branch test-kana-input-method))
(test-end)

(test-begin "context-init-widgets!")
(setup)
(begin
  (define context-propagate-widget-configuration
    (lambda (context)
      (set! test-widget-conf (context-widgets context))))
  ;; 2 widgets
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  #f)

(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-conf))
;; contains a non-existent widget
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_nonexistent
                              widget_test_kana_input_method))
  #f)
(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-conf))
;; no widgets
(begin
  (context-init-widgets! tc ())
  #f)
(test-equal '(widget_fallback)
            (map widget-id test-widget-conf))
;; null widget
(begin
  (context-init-widgets! tc '(widget_test_null))
  #f)
(test-equal '(widget_test_null)
            (map widget-id test-widget-conf))
(test-end)

(test-begin "context-update-widgets")
(setup)
(begin
  (define context-propagate-widget-configuration
    (lambda (context)
      (set! test-widget-conf (context-widgets context))))
  (define context-propagate-widget-states
    (lambda (context)
      (set! test-widget-state (context-widgets context))))
  ;; 2 widgets + non-existent widget
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_nonexistent
                              widget_test_kana_input_method))
  ;; initial update
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)

(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-conf))
(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-state))
;; duplicate update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))

;; duplicate update #2
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))

;; state update
(begin
  (define test-widget-conf '())
  (define test-widget-state '()))
(test-eq #t (widget-activate! (assq 'widget_test_input_mode
                                    (context-widgets tc))
                              'action_test_katakana))
(begin
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-state))
;; duplicate state update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))
;; configuration update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (register-action 'action_test_alt_hiragana
                   (lambda (tc)
                     '(figure_ja_hiragana
                       "ひ" ;; differs from action_test_hiragana
                       "ひらがな"
                       "ひらがな入力モード"))
                   (lambda (tc)
                     (and (test-context-on tc)
                          (= (test-context-kana-mode tc)
                             test-type-hiragana)))
                   (lambda (tc)
                     (test-context-set-on! tc #t)
                     (test-context-set-kana-mode! tc test-type-hiragana)))
  (for-each (lambda (widget)
              (if (eq? (widget-id widget)
                       'widget_test_input_mode)
                (widget-set-actions!
                 widget
                 (actions-new '(action_test_alt_hiragana
                                action_test_katakana
                                action_test_hankana
                                action_test_direct
                                action_test_zenkaku)))))
            (context-widgets tc))
  (context-update-widgets tc)
  #f)

(test-equal '(widget_test_input_mode
              widget_test_kana_input_method)
            (map widget-id test-widget-conf))
(test-eq #t (null? (map widget-id test-widget-state)))
;; duplicate configuration update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))
;; configuration & state update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-init-widgets! tc '(widget_test_input_mode))
  (context-update-widgets tc)
  #f)
(test-equal '(widget_test_input_mode)
            (map widget-id test-widget-conf))
(test-equal '(widget_test_input_mode)
            (map widget-id test-widget-state))
;; duplicate configuration & state update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))
;; The framework can't detect the configuration information
;; invalidation when violently reconfigured by
;; context-set-widgets!.
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-set-widgets!
   tc
   (filter (lambda (widget)
             (not (eq? (widget-id widget)
                       'widget_test_kana_input_method)))
           (context-widgets tc)))
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))

(begin
  ;; no widgets
  (context-init-widgets! tc ())
  ;; initial update (widget_fallback)
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-equal '(widget_fallback)
            (map widget-id test-widget-conf))
(test-equal '(widget_fallback)
            (map widget-id test-widget-state))

;; subsequent update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))

(begin
  ;; null widget
  (context-init-widgets! tc '(widget_test_null))
  ;; initial update (widget_test_null with fallback-indication)
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-equal '(widget_test_null)
            (map widget-id test-widget-conf))
(test-equal '(widget_test_null)
            (map widget-id test-widget-state))
;; subsequent update
(begin
  (define test-widget-conf '())
  (define test-widget-state '())
  (context-update-widgets tc)
  #f)
(test-eq #t (null? (map widget-id test-widget-conf)))
(test-eq #t (null? (map widget-id test-widget-state)))
(test-end)

(test-begin "context-propagate-prop-list-update")
(setup)
(begin
  (define test-prop-list #f)
  (define im-update-prop-list
    (lambda (context message)
      (set! test-prop-list message)))
  ;; 2 widgets
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (context-propagate-prop-list-update tc))
(test-equal (string-append
             "branch\tfigure_ja_direct\ta\t直接入力\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)

;; 2 widgets (updated state)
(test-eq #t (widget-activate! (assq 'widget_test_input_mode
                                    (context-widgets tc))
                              'action_test_katakana))
(context-propagate-prop-list-update tc)
(test-equal (string-append
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)

;; 2 widgets with non-existent
(begin
  (context-init-widgets! tc '(widget_test_kana_input_method
                              widget_test_nonexistent
                              widget_test_input_mode))
  (context-propagate-prop-list-update tc))
(test-equal (string-append
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
            test-prop-list)

;; no widgets
(begin
  (context-init-widgets! tc ())
  (context-propagate-prop-list-update tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
;; widget_test_null
(begin
  (context-init-widgets! tc '(widget_test_null))
  (context-propagate-prop-list-update tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
(test-end)

;; TODO: context-update-mode
(test-begin "context-propagate-widget-states")
(setup)
;; 2 widgets
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  ;; initial state
  (context-propagate-widget-states tc))
(test-equal (string-append
             "branch\tfigure_ja_direct\ta\t直接入力\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)
(test-false test-prop-label)
(test-equal 3
            test-updated-mode)
;; 2 widgets (updated state)
(test-eq #t (widget-activate! (assq 'widget_test_input_mode
                                    (context-widgets tc))
                              'action_test_katakana))
(context-propagate-widget-states tc)
(test-equal (string-append
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)
(test-false test-prop-label)
(test-equal 1
            test-updated-mode)
;; 2 widgets with non-existent
(begin
  (context-init-widgets! tc '(widget_test_kana_input_method
                              widget_test_nonexistent
                              widget_test_input_mode))
  (context-propagate-widget-states tc))
(test-equal (string-append
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
            test-prop-list)
(test-false test-prop-label)
(test-equal 1
            test-updated-mode)
;; no widgets
(begin
  (context-init-widgets! tc ())
  (context-propagate-widget-states tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
(test-false test-prop-label)
(test-equal 0
            test-updated-mode)
;; widget_test_null
(begin
  (context-init-widgets! tc '(widget_test_null))
  (context-propagate-widget-states tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
(test-false test-prop-label)
(test-equal 0
            test-updated-mode)
(test-end)

(test-begin "context-propagate-widget-configuration")
(setup)
(begin
  ;; 2 widgets
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  ;; initial state
  (context-propagate-widget-configuration tc))
(test-equal (string-append
             "branch\tfigure_ja_direct\ta\t直接入力\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
(test-equal 3
            test-updated-mode)
;; 2 widgets (updated state)
(test-eq #t (widget-activate! (assq 'widget_test_input_mode
                                    (context-widgets tc))
                              'action_test_katakana))
(context-propagate-widget-configuration tc)
(test-equal (string-append
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
            test-prop-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
(test-equal 1
            test-updated-mode)
;; 2 widgets with non-existent
(begin
  (context-init-widgets! tc '(widget_test_kana_input_method
                              widget_test_nonexistent
                              widget_test_input_mode))
  (context-propagate-widget-configuration tc))
(test-equal (string-append
             "branch\tfigure_ja_roma\tＲ\tローマ字\n"
             "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
             "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
             "branch\tfigure_ja_katakana\tア\tカタカナ\n"
             "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
             "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
             "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
             "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
             "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
            test-prop-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
(test-equal 1
            test-updated-mode)
;; no widgets
(begin
  (context-init-widgets! tc ())
  (context-propagate-widget-configuration tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
(test-equal '("unknown")
            test-mode-list)
(test-equal '("unknown")
            test-updated-mode-list)
(test-equal 0
            test-updated-mode)
;; widget_test_null
(begin
  (context-init-widgets! tc '(widget_test_null))
  (context-propagate-widget-configuration tc))
(test-equal "branch\tunknown\t?\tunknown\n"
            test-prop-list)
(test-equal '("unknown")
            test-mode-list)
(test-equal '("unknown")
            test-updated-mode-list)
(test-equal 0
            test-updated-mode)
(test-end)

(test-begin "context-prop-activate-handler")
(setup)
;; 2 widgets
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (set! test-activated #f))
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_hiragana")
                 #t))
(test-equal 'action_test_hiragana
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_zenkaku")
                 #t))
(test-equal 'action_test_zenkaku
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-equal 'action_test_kana
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-equal 'action_test_direct
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-equal 'action_test_direct
            test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_nonexistent")
                 #t))
(test-false test-activated)
;; 1 widget
(begin
  (context-init-widgets! tc '(widget_test_kana_input_method))
  (set! test-activated #f))
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_hiragana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_zenkaku")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-equal 'action_test_kana
            test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_nonexistent")
                 #t))
(test-false test-activated)
;; no widgets
(begin
  (context-init-widgets! tc ())
  (set! test-activated #f))
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_hiragana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_nonexistent")
                 #t))
(test-false test-activated)
;; widget_test_null (no action handlers)
(begin
  (context-init-widgets! tc '(widget_test_null))
  (set! test-activated #f))
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_hiragana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_direct")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_nonexistent")
                 #t))
(test-false test-activated)
(test-end)

(test-begin "context-find-mode-widget")
(setup)
(context-init-widgets! tc '(widget_test_input_mode
                            widget_test_kana_input_method))
(test-equal 'widget_test_input_mode
            (widget-id (context-find-mode-widget tc)))
(context-init-widgets! tc '(widget_test_kana_input_method
                            widget_test_input_mode))
(test-equal 'widget_test_input_mode
            (widget-id (context-find-mode-widget tc)))
(context-init-widgets! tc '(widget_test_kana_input_method
                            widget_test_input_mode
                            widget_test_null))
(test-equal 'widget_test_input_mode
            (widget-id (context-find-mode-widget tc)))
(context-init-widgets! tc '(widget_test_kana_input_method
                            widget_test_null))
(test-false (context-find-mode-widget tc))
(context-init-widgets! tc ())
(test-false (context-find-mode-widget tc))
(test-end)

(test-begin "widget-action-id->mode-value")
(setup)
(define mw (widget-new 'widget_test_input_mode tc))
(test-equal 0
            (widget-action-id->mode-value mw
                                          'action_test_hiragana))
(test-equal 1
            (widget-action-id->mode-value mw
                                          'action_test_katakana))
(test-equal 2
            (widget-action-id->mode-value mw
                                          'action_test_hankana))
(test-equal 3
            (widget-action-id->mode-value mw
                                          'action_test_direct))
(test-equal 4
            (widget-action-id->mode-value mw
                                          'action_test_zenkaku))
(test-error (widget-action-id->mode-value mw 'action_test_nonexistent))
(test-end)

(test-begin "widget-mode-value->action-id")
(setup)
(define mw (widget-new 'widget_test_input_mode tc))

(test-equal 'action_test_hiragana
            (widget-mode-value->action-id mw 0))
(test-equal 'action_test_katakana
            (widget-mode-value->action-id mw 1))
(test-equal 'action_test_hankana
            (widget-mode-value->action-id mw 2))
(test-equal 'action_test_direct
            (widget-mode-value->action-id mw 3))
(test-equal 'action_test_zenkaku
            (widget-mode-value->action-id mw 4))
(test-false (widget-mode-value->action-id mw 5))
(test-false (widget-mode-value->action-id mw -1))
(test-end)

(test-begin "context-current-mode")
(setup)
;; widget_test_input_mode
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (define test-input-mode (context-find-mode-widget tc)))

;; action_test_direct (initial activity)
(test-equal 3
            (context-current-mode tc))
;; action_test_direct -> action_test_hiragana
(test-eq #t (widget-activate! test-input-mode 'action_test_hiragana))
(test-equal 0
            (context-current-mode tc))
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode 'action_test_katakana))
(test-equal 1
            (context-current-mode tc))
;; action_test_katakana -> action_test_hankana
(test-eq #t (widget-activate! test-input-mode 'action_test_hankana))
(test-equal 2
            (context-current-mode tc))
;; action_test_hankana -> action_test_zenkaku
(test-eq #t (widget-activate! test-input-mode 'action_test_zenkaku))
(test-equal 4
            (context-current-mode tc))
;; action_test_zenkaku -> action_test_direct
(test-eq #t (widget-activate! test-input-mode 'action_test_direct))
(test-equal 3
            (context-current-mode tc))
;; action_test_direct -> invalid
(test-false (widget-activate! test-input-mode 'action_nonexistent))
(test-equal 3
            (context-current-mode tc))
(test-error (context-current-mode #f))

;; no mode-widget
(context-init-widgets! tc '(widget_test_null
                            widget_test_kana_input_method))
(test-equal 0
            (context-current-mode tc))
(test-error (context-current-mode #f))

;; no activity
(begin
  (register-widget
   'widget_test_dummy_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   #f) ;; has no actions
  (context-init-widgets! tc '(widget_test_dummy_input_mode
                              widget_test_kana_input_method)))
(test-equal 0
            (context-current-mode tc))
(test-error (context-current-mode #f))

;; duplicate activity
(begin
  (register-widget
   'widget_test_invalid_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   (actions-new '(action_test_hiragana
                  action_test_katakana
                  action_test_hankana
                  action_test_direct
                  action_test_alt_direct
                  action_test_zenkaku)))
  (context-init-widgets! tc '(widget_test_invalid_input_mode
                              widget_test_kana_input_method)))
;; context-current-mode returns 0 rather than 3 when
;; action_test_direct and action_test_alt_direct are conflicted.
(test-equal 0
            (context-current-mode tc))
;; action_test_direct -> action_test_hiragana
(test-eq #t (widget-activate! test-input-mode 'action_test_hiragana))
(test-equal 0
            (context-current-mode tc))
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode 'action_test_katakana))
(test-equal 1
            (context-current-mode tc))
(test-error (context-current-mode #f))
(test-end)

(test-begin "context-update-mode")
(setup)
;; widget_test_input_mode
(begin
  (define test-updated-mode #f)
  (define im-update-mode
    (lambda (context mode)
      (set! test-updated-mode mode)))
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (define test-input-mode (context-find-mode-widget tc)))
;; action_test_direct (initial activity)
(context-update-mode tc)
(test-equal 3
            test-updated-mode)
;; action_test_direct -> action_test_hiragana
(test-eq #t (widget-activate! test-input-mode 'action_test_hiragana))
(context-update-mode tc)
(test-equal 0
            test-updated-mode)
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode 'action_test_katakana))
(context-update-mode tc)
(test-equal 1
            test-updated-mode)
;; action_test_katakana -> action_test_hankana
(test-eq #t (widget-activate! test-input-mode 'action_test_hankana))
(context-update-mode tc)
(test-equal 2
            test-updated-mode)
;; action_test_hankana -> action_test_zenkaku
(test-eq #t (widget-activate! test-input-mode 'action_test_zenkaku))
(context-update-mode tc)
(test-equal 4
            test-updated-mode)
;; action_test_zenkaku -> action_test_direct
(test-eq #t (widget-activate! test-input-mode 'action_test_direct))
(context-update-mode tc)
(test-equal 3
            test-updated-mode)
;; action_test_direct -> invalid
(test-false (widget-activate! test-input-mode 'action_nonexistent))
(context-update-mode tc)
(test-equal 3
            test-updated-mode)
(test-error (context-current-mode #f))

;; no mode-widget
(begin
  (context-init-widgets! tc '(widget_test_null
                              widget_test_kana_input_method))
  (context-update-mode tc))
(test-equal 0
            test-updated-mode)

;; no activity
(begin
  (register-widget
   'widget_test_dummy_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   #f) ;; has no actions
  (context-init-widgets! tc '(widget_test_dummy_input_mode
                              widget_test_kana_input_method))
  (context-update-mode tc))
(test-equal 0
            test-updated-mode)

;; duplicate activity
(begin
  (register-widget
   'widget_test_invalid_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   (actions-new '(action_test_hiragana
                  action_test_katakana
                  action_test_hankana
                  action_test_direct
                  action_test_alt_direct
                  action_test_zenkaku)))
  (context-init-widgets! tc '(widget_test_invalid_input_mode
                              widget_test_kana_input_method))
  ;; context-current-mode returns 0 rather than 3 when
  ;; action_test_direct and action_test_alt_direct are conflicted.
  (context-update-mode tc))
(test-equal 0
            test-updated-mode)
;; action_test_direct -> action_test_hiragana
(test-eq #t (widget-activate! test-input-mode 'action_test_hiragana))
(context-update-mode tc)
(test-equal 0
            test-updated-mode)
;; action_test_hiragana -> action_test_katakana
(test-eq #t (widget-activate! test-input-mode 'action_test_katakana))
(context-update-mode tc)
(test-equal 1
            test-updated-mode)
(test-end)

(test-begin "context-update-mode-list")
(setup)
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (define test-input-mode (context-find-mode-widget tc))
  ;; initial state
  (context-update-mode-list tc))

(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
(test-equal 3
            test-updated-mode)
;; action_test_direct -> action_test_hankana
(test-eq #t (widget-activate! test-input-mode 'action_test_hankana))
(context-update-mode-list tc)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
(test-equal 2
            test-updated-mode)
;; duplicate activity
(begin
  (register-widget
   'widget_test_invalid_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   (actions-new '(action_test_hiragana
                  action_test_katakana
                  action_test_hankana
                  action_test_direct
                  action_test_alt_direct
                  action_test_zenkaku)))
  (context-init-widgets! tc '(widget_test_invalid_input_mode
                              widget_test_kana_input_method))
  (test-context-set-on! tc #f)
  (context-update-mode-list tc))
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "直接入力"
              "全角英数")
            test-mode-list)
(test-equal '("ひらがな"
              "カタカナ"
              "半角カタカナ"
              "直接入力"
              "直接入力"
              "全角英数")
            test-updated-mode-list)
;; context-current-mode returns 0 rather than 3 when
;; action_test_direct and action_test_alt_direct are conflicted.
(test-equal 0
            test-updated-mode)

;; no activity
(begin
  (register-widget
   'widget_test_dummy_input_mode
   (indicator-new (lambda (owner)
                    fallback-indication))
   #f) ;; has no actions
  (context-init-widgets! tc '(widget_test_dummy_input_mode
                              widget_test_kana_input_method)))
(test-eq #t (widget-activate! test-input-mode
                              'action_test_hankana))
(context-update-mode-list tc)
(test-equal '()
            test-mode-list)
(test-equal '()
            test-updated-mode-list)
(test-equal 0
            test-updated-mode)
(test-end)

(test-begin "context-mode-handler")
(setup)
;; 2 widgets
(begin
  (context-init-widgets! tc '(widget_test_input_mode
                              widget_test_kana_input_method))
  (set! test-activated #f))
(test-eq #t (and (context-mode-handler tc 0)
                 #t))
(test-equal 'action_test_hiragana
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-mode-handler tc 4)
                 #t))
(test-equal 'action_test_zenkaku
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-equal 'action_test_kana
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-mode-handler tc 3)
                 #t))
(test-equal 'action_test_direct
            test-activated)
(set! test-activated #f)
(test-eq #t (and (context-mode-handler tc 3)
                 #t))
(test-equal 'action_test_direct
            test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc -1)
                 #t))
(test-false test-activated)
;; 1 widget
(begin
  (context-init-widgets! tc '(widget_test_kana_input_method))
  (set! test-activated #f))
(test-false (and (context-mode-handler tc 0)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc 4)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-eq #t (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-equal 'action_test_kana
            test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc 3)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc 3)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc -1)
                 #t))
(test-false test-activated)
;; no widgets
(begin
  (context-init-widgets! tc ())
  (set! test-activated #f))
(test-false (and (context-mode-handler tc 0)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc 3)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc -1)
                 #t))
(test-false test-activated)
;; widget_test_null (no action handlers)
(begin
  (context-init-widgets! tc '(widget_test_null))
  (set! test-activated #f))
(test-false (and (context-mode-handler tc 0)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-prop-activate-handler
                  tc
                  "action_test_kana")
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc 3)
                 #t))
(test-false test-activated)
(set! test-activated #f)
(test-false (and (context-mode-handler tc -1)
                 #t))
(test-false test-activated)
(test-end)

(test-report-result)
