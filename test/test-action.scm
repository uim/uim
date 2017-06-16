;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

;; These tests are passed at revision 6605 (new repository)

(define-module test.test-action
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-action)

(define (setup)
  (uim-test-setup)
  (uim-eval
   '(begin
      (custom-set-value! 'toolbar-show-action-based-switcher-button? #f)

      (require "load-action.scm")
      (require "rk.scm")
      (require "japanese.scm")
      (require-module "anthy")

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
      (define test-activated #f))))

(define (teardown)
  (uim-test-teardown))

(define (test-indicator-new)
  (uim-eval
   '(define test-indicator (indicator-new (lambda ()
                                            '(unknown
                                              "?"
                                              "unknown"
                                              "Unknown")))))

  (assert-uim-false '(indicator-id test-indicator))
  (assert-uim-false '(indicator-activity-pred test-indicator))
  (assert-uim-false '(indicator-handler test-indicator))
  #f)

(define (test-register-action)
  (uim-eval '(set! action-list ()))
  (assert-uim-equal 0
                    '(length action-list))
  (uim-eval '(register-action
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
                (test-context-set-kana-mode! tc test-type-hiragana))))
  (assert-uim-equal 1
                    '(length action-list))
  (assert-uim-equal 'action_test_hiragana
                    '(caar action-list))

  (uim-eval '(register-action
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
                (test-context-set-kana-mode! tc test-type-katakana))))
  (assert-uim-equal 2
                    '(length action-list))
  (assert-uim-equal 'action_test_katakana
                    '(caar action-list))

  (uim-eval '(register-action
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
                (test-context-set-kana-mode! tc test-type-hankana))))
  (assert-uim-equal 3
                    '(length action-list))
  (assert-uim-equal 'action_test_hankana
                    '(caar action-list)))

(define (test-fetch-action)
  (assert-uim-equal 'action_test_hiragana
                    '(action-id (fetch-action 'action_test_hiragana)))
  (assert-uim-equal 'action_test_katakana
                    '(action-id (fetch-action 'action_test_katakana)))
  (assert-uim-equal 'action_test_hankana
                    '(action-id (fetch-action 'action_test_hankana)))
  #f)

(define (test-action-active?)
  (assert-uim-false '(action-active? (fetch-action 'action_test_hiragana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_katakana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hankana)
                                     tc))
  (assert-uim-true  '(action-active? (fetch-action 'action_test_direct)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_zenkaku)
                                     tc))

  (uim-eval '(test-context-set-wide-latin! tc #t))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hiragana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_katakana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hankana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_direct)
                                     tc))
  (assert-uim-true  '(action-active? (fetch-action 'action_test_zenkaku)
                                     tc))

  (uim-eval '(test-context-set-on! tc #t))
  (assert-uim-true  '(action-active? (fetch-action 'action_test_hiragana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_katakana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hankana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_direct)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_zenkaku)
                                     tc))

  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hiragana)
                                     tc))
  (assert-uim-true  '(action-active? (fetch-action 'action_test_katakana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_hankana)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_direct)
                                     tc))
  (assert-uim-false '(action-active? (fetch-action 'action_test_zenkaku)
                                     tc))
  #f)

(define (test-action-indicate)
  (assert-uim-equal '(figure_ja_hiragana
                      "あ"
                      "ひらがな"
                      "ひらがな入力モード")
                    '(action-indicate (fetch-action 'action_test_hiragana)
                                      tc))
  (assert-uim-equal '(figure_ja_katakana
                      "ア"
                      "カタカナ"
                      "カタカナ入力モード")
                    '(action-indicate (fetch-action 'action_test_katakana)
                                      tc))
  (assert-uim-equal '(figure_ja_kana
                      "か"
                      "かな"
                      "かな入力モード")
                    '(action-indicate (fetch-action 'action_test_kana)
                                      tc))
  ;; no action
  (assert-uim-equal (uim 'fallback-indication)
                    '(action-indicate #f tc))
  ;; no indication handler
  (assert-uim-equal (uim 'fallback-indication)
                    '(action-indicate (action-new) tc))
  #f)

(define (test-actions-new)
  (assert-uim-equal '(action_test_katakana
                      action_test_kana
                      action_test_hiragana)
                    '(map action-id (actions-new '(action_test_katakana
                                                   action_test_kana
                                                   action_test_hiragana))))
  (assert-uim-equal (uim ''(action_test_katakana
                            action_test_kana
                            action_test_hiragana))
                    '(map action-id (actions-new '(action_test_katakana
                                                   action_test_kana
                                                   action_nonexistent
                                                   action_test_hiragana))))
  (assert-uim-equal ()
                    '(map action-id (actions-new ())))
  #f)

(define (test-activity-indicator-new)
  (uim-eval
   '(define indicator (activity-indicator-new '(action_test_hiragana
                                                action_test_katakana
                                                action_test_hankana
                                                action_test_direct
                                                action_test_zenkaku))))
  (assert-uim-equal '(figure_ja_direct
                      "a"
                      "直接入力"
                      "直接(無変換)入力モード")
                    '(action-indicate indicator tc))

  (uim-eval '(test-context-set-wide-latin! tc #t))
  (assert-uim-equal '(figure_ja_zenkaku
                      "Ａ"
                      "全角英数"
                      "全角英数入力モード")
                    '(action-indicate indicator tc))

  (uim-eval '(test-context-set-on! tc #t))
  (assert-uim-equal '(figure_ja_hiragana
                      "あ"
                      "ひらがな"
                      "ひらがな入力モード")
                    '(action-indicate indicator tc))

  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-uim-equal '(figure_ja_katakana
                      "ア"
                      "カタカナ"
                      "カタカナ入力モード")
                    '(action-indicate indicator tc))
  ;; no activity case
  (uim-eval '(define test-type-invalid 100))
  (uim-eval '(test-context-set-kana-mode! tc test-type-invalid))
  (assert-uim-equal '(unknown
                      "?"
                      "unknown"
                      "unknown")
                    '(action-indicate indicator tc))
  #f)

(define (test-register-widget)
  (uim-eval '(set! widget-proto-list ()))
  (assert-uim-equal 0
                    '(length widget-proto-list))

  (uim-eval
   '(begin
      (register-widget
       'widget_test_input_mode
       (indicator-new (lambda (tc)
                        fallback-indication))
       (actions-new '(action_test_hiragana
                      action_test_katakana
                      action_test_hankana
                      action_test_direct
                      action_test_zenkaku)))
      #t))
  (assert-uim-equal 1
                    '(length widget-proto-list))
  (assert-uim-equal 'widget_test_input_mode
                    '(caar widget-proto-list))

  (uim-eval
   '(begin
      (register-widget
       'widget_test_input_mode
       (indicator-new (lambda (tc)
                        fallback-indication))
       (actions-new '(action_test_direct)))
      #t))
  (assert-uim-equal 1
                    '(length widget-proto-list))
  (assert-uim-equal 'widget_test_input_mode
                    '(caar widget-proto-list))

  (uim-eval
   '(begin
      (register-widget
       'widget_test_kana_input_method
       (indicator-new (lambda (tc)
                        fallback-indication))
       (actions-new '(action_test_roma
                      action_test_kana)))
      #t))
  (assert-uim-equal 2
                    '(length widget-proto-list))
  (assert-uim-equal 'widget_test_kana_input_method
                    '(caar widget-proto-list))
  (assert-uim-equal 'widget_test_input_mode
                    '(car (cadr widget-proto-list)))
  #f)

(define (test-widget-new)
  (assert-uim-false '(widget-new 'widget_test_nonexistent tc))
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-uim-equal 'widget_test_input_mode
                    '(widget-id test-input-mode))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))

  ;; widget_test_input_mode with default value
  (uim-eval
   '(begin
      (define default-widget_test_input_mode 'action_test_hiragana)
      (define test-input-mode
        (widget-new 'widget_test_input_mode tc))))
  (assert-uim-equal 'action_test_hiragana
                    '(action-id (widget-activity test-input-mode)))

  ;; widget_test_input_mode with default value #2
  (uim-eval
   '(begin
      (define default-widget_test_input_mode 'action_test_katakana)
      (define test-input-mode
        (widget-new 'widget_test_input_mode tc))))
  (assert-uim-equal 'action_test_katakana
                    '(action-id (widget-activity test-input-mode)))

  ;; widget_test_input_mode with default value #3
  (uim-eval
   '(begin
      (define default-widget_test_input_mode 'action_test_zenkaku)
      (define test-input-mode
        (widget-new 'widget_test_input_mode tc))))
  (assert-uim-equal 'action_test_zenkaku
                    '(action-id (widget-activity test-input-mode)))

  ;; widget_test_input_mode with invalid default value
  (uim-eval
   '(begin
      (define default-widget_test_input_mode 'action_nonexistent)
      (define test-input-mode
        (widget-new 'widget_test_input_mode tc))))
  (assert-uim-equal 'action_test_zenkaku
                    '(action-id (widget-activity test-input-mode)))

  ;; widget_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-uim-equal 'action_test_roma
                    '(action-id (widget-activity test-kana-input-method)))

  ;; widget_test_kana_input_method with default value
  (uim-eval
   '(begin
      (define default-widget_test_kana_input_method 'action_test_kana)
      (define test-kana-input-method
        (widget-new 'widget_test_kana_input_method tc))))
  (assert-uim-equal 'action_test_kana
                    '(action-id (widget-activity test-kana-input-method)))

  ;; widget_test_kana_input_method with invalid default value
  (uim-eval
   '(begin
      (define default-widget_test_kana_input_method 'action_nonexistent)
      (define test-kana-input-method
        (widget-new 'widget_test_kana_input_method tc))))
  (assert-uim-equal 'action_test_kana
                    '(action-id (widget-activity test-kana-input-method)))
  #f)

(define (test-widget-activity)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  ;; action_test_direct (initial activity)
  (assert-uim-false '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hiragana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))

  ;; action_test_direct -> action_test_hiragana
  (uim-eval '(test-context-set-wide-latin! tc #t))
  (uim-eval '(test-context-set-on! tc #t))
  (assert-uim-equal (uim 'test-type-hiragana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_hiragana
                    '(action-id (widget-activity test-input-mode)))

  ;; action_test_hiragana -> action_test_katakana
  (uim-eval '(test-context-set-wide-latin! tc #f))
  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-katakana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_katakana
                    '(action-id (widget-activity test-input-mode)))

  ;; action_test_katakana -> action_test_hankana
  (uim-eval '(test-context-set-kana-mode! tc test-type-hankana))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_hankana
                    '(action-id (widget-activity test-input-mode)))

  ;; action_test_hankana -> action_test_direct
  (uim-eval '(test-context-set-on! tc #f))
  (assert-uim-false '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))

  ;; action_test_direct -> invalid
  (uim-eval '(define test-type-invalid 100))
  (uim-eval '(test-context-set-on! tc #t))
  (uim-eval '(test-context-set-kana-mode! tc test-type-invalid))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-invalid)
                    '(test-context-kana-mode tc))
  (assert-uim-false '(widget-activity test-input-mode))

  ;; duplicate activity
  (uim-eval
   '(begin
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
        (widget-new 'widget_test_invalid_input_mode tc))))

  ;; action_test_direct and action_test_alt_direct are conflicted
  (assert-uim-false '(widget-activity test-invalid-input-mode))
  ;; conflicted -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-invalid-input-mode
                                       'action_test_hiragana))
  (assert-uim-equal 'action_test_hiragana
                    '(action-id (widget-activity test-invalid-input-mode)))
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-invalid-input-mode
                                       'action_test_katakana))
  (assert-uim-equal 'action_test_katakana
                    '(action-id (widget-activity test-invalid-input-mode)))
  #f)

(define (test-widget-activate!)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  ;; action_test_direct (initial activity)
  (assert-uim-false '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hiragana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_direct -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_hiragana))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hiragana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_hiragana
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_katakana))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-katakana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_katakana
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_katakana -> action_test_hankana
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_hankana))
  (assert-uim-true  '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_hankana
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_hankana -> action_test_zenkaku
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_zenkaku))
  (assert-uim-false '(test-context-on tc))
  (assert-uim-true  '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_zenkaku
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_zenkaku -> action_test_direct
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_direct))
  (assert-uim-false '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))
  ;; action_test_direct -> invalid
  (assert-uim-false '(widget-activate! test-input-mode
                                       'action_nonexistent))
  (assert-uim-false '(test-context-on tc))
  (assert-uim-false '(test-context-wide-latin tc))
  (assert-uim-equal (uim 'test-type-hankana)
                    '(test-context-kana-mode tc))
  (assert-uim-equal 'action_test_direct
                    '(action-id (widget-activity test-input-mode)))
  #f)

(define (test-widget-configuration)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-uim-equal '(action_unknown
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
                    '(widget-configuration test-input-mode))

  ;; widget_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-uim-equal '(action_unknown
                      (figure_ja_roma
                       "Ｒ"
                       "ローマ字"
                       "ローマ字入力モード")
                      (figure_ja_kana
                       "か"
                       "かな"
                       "かな入力モード"))
                    '(widget-configuration test-kana-input-method))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-uim-equal '(action_unknown)
                    '(widget-configuration test-null))
  #f)

(define (test-widget-state)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_direct)
                                   '(figure_ja_direct
                                     "a"
                                     "直接入力"
                                     "直接(無変換)入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_katakana))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-false '(widget-activate! test-input-mode
                                       'action_nonexistent))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))

  ;; widget_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_roma)
                                   '(figure_ja_roma
                                     "Ｒ"
                                     "ローマ字"
                                     "ローマ字入力モード"))
                             (widget-state test-kana-input-method)))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-state test-null)))
  #f)

(define (test-widget-update-configuration!)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-uim-equal '(action_unknown
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
                    '(widget-configuration test-input-mode))
  (assert-uim-false '(widget-prev-config test-input-mode))
  (assert-uim-true  '(widget-update-configuration! test-input-mode))
  (assert-uim-equal '(action_unknown
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
                    '(widget-configuration test-input-mode))
  (assert-uim-equal '(action_unknown
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
                    '(widget-prev-config test-input-mode))
  (assert-uim-false '(widget-update-configuration! test-input-mode))
  (assert-uim-equal '(action_unknown
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
                    '(widget-prev-config test-input-mode))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-uim-equal '(action_unknown)
                    '(widget-configuration test-null))
  (assert-uim-false '(widget-prev-config test-null))
  ;; initial update (widget_test_null with fallback-indication)
  (assert-uim-true  '(widget-update-configuration! test-null))
  ;; subsequent update
  (assert-uim-false '(widget-update-configuration! test-null))
  #f)

(define (test-widget-update-state!)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))

  ;; initial state
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_direct)
                                   '(figure_ja_direct
                                     "a"
                                     "直接入力"
                                     "直接(無変換)入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-false '(widget-prev-state test-input-mode))
  ;; initial update
  (assert-uim-true  '(widget-update-state! test-input-mode))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_direct)
                                   '(figure_ja_direct
                                     "a"
                                     "直接入力"
                                     "直接(無変換)入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_direct)
                                   '(figure_ja_direct
                                     "a"
                                     "直接入力"
                                     "直接(無変換)入力モード"))
                             (widget-prev-state test-input-mode)))
  ;; action_test_direct -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_katakana))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_direct)
                                   '(figure_ja_direct
                                     "a"
                                     "直接入力"
                                     "直接(無変換)入力モード"))
                             (widget-prev-state test-input-mode)))
  (assert-uim-true  '(widget-update-state! test-input-mode))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-prev-state test-input-mode)))
  ;; action_test_katakana -> action_test_katakana
  (assert-uim-false '(widget-update-state! test-input-mode))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-prev-state test-input-mode)))
  ;; invalid activation
  (assert-uim-false '(widget-activate! test-input-mode 'action_nonexistent))
  (assert-uim-false '(widget-update-state! test-input-mode))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-state test-input-mode)))
  (assert-uim-true  '(equal? (list (fetch-action 'action_test_katakana)
                                   '(figure_ja_katakana
                                     "ア"
                                     "カタカナ"
                                     "カタカナ入力モード"))
                             (widget-prev-state test-input-mode)))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))

  ;; initial state
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-state test-null)))
  (assert-uim-false '(widget-prev-state test-null))
  ;; initial update
  (assert-uim-true  '(widget-update-state! test-null))
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-state test-null)))
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-prev-state test-null)))
  ;; subsequent update
  (assert-uim-false '(widget-update-state! test-null))
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-state test-null)))
  (assert-uim-true  '(equal? (list #f
                                   '(unknown
                                     "?"
                                     "unknown"
                                     "unknown"))
                             (widget-prev-state test-null)))
  #f)

(define (test-widget-debug-message)
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-uim-equal "something in somewhere. debug widget_test_input_mode."
                    '(widget-debug-message test-input-mode
                                           "somewhere"
                                           "something"))
  #f)

(define (test-indication-compose-label)
  (assert-uim-equal "figure_ja_hiragana\tあ\tひらがな\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_hiragana)
                                       tc)))
  (assert-uim-equal "figure_ja_katakana\tア\tカタカナ\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_katakana)
                                       tc)))
  (assert-uim-equal "figure_ja_hankana\tｱ\t半角カタカナ\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_hankana)
                                       tc)))
  (assert-uim-equal "figure_ja_direct\ta\t直接入力\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_direct)
                                       tc)))
  (assert-uim-equal "figure_ja_zenkaku\tＡ\t全角英数\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_zenkaku)
                                       tc)))
  (assert-uim-equal "figure_ja_roma\tＲ\tローマ字\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_roma)
                                       tc)))
  (assert-uim-equal "figure_ja_kana\tか\tかな\n"
                    '(indication-compose-label
                      (action-indicate (fetch-action 'action_test_kana)
                                       tc)))
  #f)

(define (test-indication-compose-branch)
  (assert-uim-equal "branch\tfigure_ja_hiragana\tあ\tひらがな\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_hiragana)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_katakana)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_hankana\tｱ\t半角カタカナ\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_hankana)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_direct\ta\t直接入力\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_direct)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_zenkaku)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_roma)
                                       tc)))
  (assert-uim-equal "branch\tfigure_ja_kana\tか\tかな\n"
                    '(indication-compose-branch
                      (action-indicate (fetch-action 'action_test_kana)
                                       tc)))
  #f)

(define (test-indication-compose-leaf)
  ;; inactive leaves
  (assert-uim-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_hiragana)
                                       tc)
                      'action_test_hiragana
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_katakana)
                                       tc)
                      'action_test_katakana
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_hankana)
                                       tc)
                      'action_test_hankana
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_direct)
                                       tc)
                      'action_test_direct
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_zenkaku)
                                       tc)
                      'action_test_zenkaku
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_roma)
                                       tc)
                      'action_test_roma
                      #f))
  (assert-uim-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_kana)
                                       tc)
                      'action_test_kana
                      #f))
  ;; active leaves
  (assert-uim-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_hiragana)
                                       tc)
                      'action_test_hiragana
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_katakana)
                                       tc)
                      'action_test_katakana
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_hankana)
                                       tc)
                      'action_test_hankana
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_direct)
                                       tc)
                      'action_test_direct
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_zenkaku)
                                       tc)
                      'action_test_zenkaku
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_roma)
                                       tc)
                      'action_test_roma
                      #t))
  (assert-uim-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n"
                    '(indication-compose-leaf
                      (action-indicate (fetch-action 'action_test_kana)
                                       tc)
                      'action_test_kana
                      #t))
  #f)

(define (test-widget-compose-live-branch)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))

  (assert-uim-equal (string-append
                     "branch\tfigure_ja_direct\ta\t直接入力\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                    '(widget-compose-live-branch test-input-mode))
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_zenkaku))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n")
                    '(widget-compose-live-branch test-input-mode))

  ;; prop_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    '(widget-compose-live-branch test-kana-input-method))
  (assert-uim-true  '(widget-activate! test-kana-input-method
                                       'action_test_kana))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_kana\tか\tかな\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n")
                    '(widget-compose-live-branch test-kana-input-method))
  #f)

(define (test-context-init-widgets!)
  (uim-eval
   '(begin
      (define context-propagate-widget-configuration
        (lambda (context)
          (set! test-widget-conf (context-widgets context))))
      ;; 2 widgets
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      #f))

  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-conf))
  ;; contains a non-existent widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_nonexistent
                                  widget_test_kana_input_method))
      #f))
  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-conf))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      #f))
  (assert-uim-equal '(widget_fallback)
                    '(map widget-id test-widget-conf))
  ;; null widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      #f))
  (assert-uim-equal '(widget_test_null)
                    '(map widget-id test-widget-conf))
  #f)

(define (test-context-update-widgets)
  (uim-eval
   '(begin
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
      #f))

  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-conf))
  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-state))
  ;; duplicate update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))

  ;; duplicate update #2
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))

  ;; state update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())))
  (assert-uim-true '(widget-activate! (assq 'widget_test_input_mode
                                            (context-widgets tc))
                                      'action_test_katakana))
  (uim-eval
   '(begin
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-state))
  ;; duplicate state update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))
  ;; configuration update
  (uim-eval
   '(begin
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
      #f))

  (assert-uim-equal '(widget_test_input_mode
                      widget_test_kana_input_method)
                    '(map widget-id test-widget-conf))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))
  ;; duplicate configuration update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))
  ;; configuration & state update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-init-widgets! tc '(widget_test_input_mode))
      (context-update-widgets tc)
      #f))
  (assert-uim-equal '(widget_test_input_mode)
                    '(map widget-id test-widget-conf))
  (assert-uim-equal '(widget_test_input_mode)
                    '(map widget-id test-widget-state))
  ;; duplicate configuration & state update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))
  ;; The framework can't detect the configuration information
  ;; invalidation when violently reconfigured by
  ;; context-set-widgets!.
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-set-widgets!
       tc
       (filter (lambda (widget)
                 (not (eq? (widget-id widget)
                           'widget_test_kana_input_method)))
               (context-widgets tc)))
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))

  (uim-eval
   '(begin
      ;; no widgets
      (context-init-widgets! tc ())
      ;; initial update (widget_fallback)
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-uim-equal '(widget_fallback)
                    '(map widget-id test-widget-conf))
  (assert-uim-equal '(widget_fallback)
                    '(map widget-id test-widget-state))

  ;; subsequent update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))

  (uim-eval
   '(begin
      ;; null widget
      (context-init-widgets! tc '(widget_test_null))
      ;; initial update (widget_test_null with fallback-indication)
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-uim-equal '(widget_test_null)
                    '(map widget-id test-widget-conf))
  (assert-uim-equal '(widget_test_null)
                    '(map widget-id test-widget-state))
  ;; subsequent update
  (uim-eval
   '(begin
      (define test-widget-conf '())
      (define test-widget-state '())
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-true  (null? (uim '(map widget-id test-widget-state))))
  #f)

(define (test-context-propagate-prop-list-update)
  (uim-eval
   '(begin
      (define test-prop-list #f)
      (define im-update-prop-list
        (lambda (context message)
          (set! test-prop-list message)))
      ;; 2 widgets
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (context-propagate-prop-list-update tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_direct\ta\t直接入力\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)

  ;; 2 widgets (updated state)
  (assert-uim-true '(widget-activate! (assq 'widget_test_input_mode
                                            (context-widgets tc))
                                      'action_test_katakana))
  (uim-eval '(context-propagate-prop-list-update tc))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)

  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-prop-list-update tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                    'test-prop-list)

  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-prop-list-update tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-prop-list-update tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  #f)

;; TODO: context-update-mode
(define (test-context-propagate-widget-states)
  ;; 2 widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      ;; initial state
      (context-propagate-widget-states tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_direct\ta\t直接入力\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)
  (assert-uim-false 'test-prop-label)
  (assert-uim-equal 3
                    'test-updated-mode)
  ;; 2 widgets (updated state)
  (assert-uim-true '(widget-activate! (assq 'widget_test_input_mode
                                            (context-widgets tc))
                                      'action_test_katakana))
  (uim-eval '(context-propagate-widget-states tc))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)
  (assert-uim-false 'test-prop-label)
  (assert-uim-equal 1
                    'test-updated-mode)
  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-widget-states tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                    'test-prop-list)
  (assert-uim-false 'test-prop-label)
  (assert-uim-equal 1
                    'test-updated-mode)
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-widget-states tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  (assert-uim-false 'test-prop-label)
  (assert-uim-equal 0
                    'test-updated-mode)
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-widget-states tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  (assert-uim-false 'test-prop-label)
  (assert-uim-equal 0
                    'test-updated-mode)
  #f)

(define (test-context-propagate-widget-configuration)
  (uim-eval
   '(begin
      ;; 2 widgets
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      ;; initial state
      (context-propagate-widget-configuration tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_direct\ta\t直接入力\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  (assert-uim-equal 3
                    'test-updated-mode)
  ;; 2 widgets (updated state)
  (assert-uim-true '(widget-activate! (assq 'widget_test_input_mode
                                            (context-widgets tc))
                                      'action_test_katakana))
  (uim-eval '(context-propagate-widget-configuration tc))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                    'test-prop-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  (assert-uim-equal 1
                    'test-updated-mode)
  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-widget-configuration tc)))
  (assert-uim-equal (string-append
                     "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                     "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                     "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                     "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                     "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                     "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                     "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                     "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                     "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                    'test-prop-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  (assert-uim-equal 1
                    'test-updated-mode)
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-widget-configuration tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  (assert-uim-equal '("unknown")
                    'test-mode-list)
  (assert-uim-equal '("unknown")
                    'test-updated-mode-list)
  (assert-uim-equal 0
                    'test-updated-mode)
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-widget-configuration tc)))
  (assert-uim-equal "branch\tunknown\t?\tunknown\n"
                    'test-prop-list)
  (assert-uim-equal '("unknown")
                    'test-mode-list)
  (assert-uim-equal '("unknown")
                    'test-updated-mode-list)
  (assert-uim-equal 0
                    'test-updated-mode)
  #f)

(define (test-context-prop-activate-handler)
  ;; 2 widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_hiragana")
                          #t))
  (assert-uim-equal 'action_test_hiragana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_zenkaku")
                          #t))
  (assert-uim-equal 'action_test_zenkaku
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-equal 'action_test_kana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-equal 'action_test_direct
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-equal 'action_test_direct
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_nonexistent")
                          #t))
  (assert-uim-false 'test-activated)
  ;; 1 widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_hiragana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_zenkaku")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-equal 'action_test_kana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_nonexistent")
                          #t))
  (assert-uim-false 'test-activated)
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (set! test-activated #f)))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_hiragana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_nonexistent")
                          #t))
  (assert-uim-false 'test-activated)
  ;; widget_test_null (no action handlers)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (set! test-activated #f)))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_hiragana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_direct")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_nonexistent")
                          #t))
  (assert-uim-false 'test-activated)
  #f)

(define (test-context-find-mode-widget)
  (uim-eval
   '(context-init-widgets! tc '(widget_test_input_mode
                                widget_test_kana_input_method)))
  (assert-uim-equal 'widget_test_input_mode
                    '(widget-id (context-find-mode-widget tc)))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_input_mode)))
  (assert-uim-equal 'widget_test_input_mode
                    '(widget-id (context-find-mode-widget tc)))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_input_mode
                                widget_test_null)))
  (assert-uim-equal 'widget_test_input_mode
                    '(widget-id (context-find-mode-widget tc)))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_null)))
  (assert-uim-false '(context-find-mode-widget tc))
  (uim-eval
   '(context-init-widgets! tc ()))
  (assert-uim-false '(context-find-mode-widget tc))
  #f)

(define (test-widget-action-id->mode-value)
  (uim-eval
   '(define mw (widget-new 'widget_test_input_mode tc)))
  (assert-uim-equal 0
                    '(widget-action-id->mode-value mw
                                                   'action_test_hiragana))
  (assert-uim-equal 1
                    '(widget-action-id->mode-value mw
                                                   'action_test_katakana))
  (assert-uim-equal 2
                    '(widget-action-id->mode-value mw
                                                   'action_test_hankana))
  (assert-uim-equal 3
                    '(widget-action-id->mode-value mw
                                                   'action_test_direct))
  (assert-uim-equal 4
                    '(widget-action-id->mode-value mw
                                                   'action_test_zenkaku))
  (assert-uim-error '(widget-action-id->mode-value mw 'action_test_nonexistent))
  #f)

(define (test-widget-mode-value->action-id)
  (uim-eval
   '(define mw (widget-new 'widget_test_input_mode tc)))

  (assert-uim-equal 'action_test_hiragana
                    '(widget-mode-value->action-id mw 0))
  (assert-uim-equal 'action_test_katakana
                    '(widget-mode-value->action-id mw 1))
  (assert-uim-equal 'action_test_hankana
                    '(widget-mode-value->action-id mw 2))
  (assert-uim-equal 'action_test_direct
                    '(widget-mode-value->action-id mw 3))
  (assert-uim-equal 'action_test_zenkaku
                    '(widget-mode-value->action-id mw 4))
  (assert-uim-false '(widget-mode-value->action-id mw 5))
  (assert-uim-false '(widget-mode-value->action-id mw -1))
  #f)

(define (test-context-current-mode)
  ;; widget_test_input_mode
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (define test-input-mode (context-find-mode-widget tc))))

  ;; action_test_direct (initial activity)
  (assert-uim-equal 3
                    '(context-current-mode tc))
  ;; action_test_direct -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hiragana))
  (assert-uim-equal 0
                    '(context-current-mode tc))
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_katakana))
  (assert-uim-equal 1
                    '(context-current-mode tc))
  ;; action_test_katakana -> action_test_hankana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hankana))
  (assert-uim-equal 2
                    '(context-current-mode tc))
  ;; action_test_hankana -> action_test_zenkaku
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_zenkaku))
  (assert-uim-equal 4
                    '(context-current-mode tc))
  ;; action_test_zenkaku -> action_test_direct
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_direct))
  (assert-uim-equal 3
                    '(context-current-mode tc))
  ;; action_test_direct -> invalid
  (assert-uim-false '(widget-activate! test-input-mode 'action_nonexistent))
  (assert-uim-equal 3
                    '(context-current-mode tc))
  (assert-uim-error '(context-current-mode #f))

  ;; no mode-widget
  (uim-eval
   '(context-init-widgets! tc '(widget_test_null
                                widget_test_kana_input_method)))
  (assert-uim-equal 0
                    '(context-current-mode tc))
  (assert-uim-error '(context-current-mode #f))

  ;; no activity
  (uim-eval
   '(begin
      (register-widget
       'widget_test_dummy_input_mode
       (indicator-new (lambda (owner)
                        fallback-indication))
       #f) ;; has no actions
      (context-init-widgets! tc '(widget_test_dummy_input_mode
                                  widget_test_kana_input_method))))
  (assert-uim-equal 0
                    '(context-current-mode tc))
  (assert-uim-error '(context-current-mode #f))

  ;; duplicate activity
  (uim-eval
   '(begin
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
                                  widget_test_kana_input_method))))
  ;; context-current-mode returns 0 rather than 3 when
  ;; action_test_direct and action_test_alt_direct are conflicted.
  (assert-uim-equal 0
                    '(context-current-mode tc))
  ;; action_test_direct -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hiragana))
  (assert-uim-equal 0
                    '(context-current-mode tc))
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_katakana))
  (assert-uim-equal 1
                    '(context-current-mode tc))
  (assert-uim-error '(context-current-mode #f))
  #f)

(define (test-context-update-mode)
  ;; widget_test_input_mode
  (uim-eval
   '(begin
      (define test-updated-mode #f)
      (define im-update-mode
        (lambda (context mode)
          (set! test-updated-mode mode)))
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (define test-input-mode (context-find-mode-widget tc))))
  ;; action_test_direct (initial activity)
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 3
                    'test-updated-mode)
  ;; action_test_direct -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hiragana))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 0
                    'test-updated-mode)
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_katakana))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 1
                    'test-updated-mode)
  ;; action_test_katakana -> action_test_hankana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hankana))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 2
                    'test-updated-mode)
  ;; action_test_hankana -> action_test_zenkaku
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_zenkaku))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 4
                    'test-updated-mode)
  ;; action_test_zenkaku -> action_test_direct
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_direct))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 3
                    'test-updated-mode)
  ;; action_test_direct -> invalid
  (assert-uim-false '(widget-activate! test-input-mode 'action_nonexistent))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 3
                    'test-updated-mode)
  (assert-uim-error '(context-current-mode #f))

  ;; no mode-widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null
                                  widget_test_kana_input_method))
      (context-update-mode tc)))
  (assert-uim-equal 0
                    'test-updated-mode)

  ;; no activity
  (uim-eval
   '(begin
      (register-widget
       'widget_test_dummy_input_mode
       (indicator-new (lambda (owner)
                        fallback-indication))
       #f) ;; has no actions
      (context-init-widgets! tc '(widget_test_dummy_input_mode
                                  widget_test_kana_input_method))
      (context-update-mode tc)))
  (assert-uim-equal 0
                    'test-updated-mode)

  ;; duplicate activity
  (uim-eval
   '(begin
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
      (context-update-mode tc)))
  (assert-uim-equal 0
                    'test-updated-mode)
  ;; action_test_direct -> action_test_hiragana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hiragana))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 0
                    'test-updated-mode)
  ;; action_test_hiragana -> action_test_katakana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_katakana))
  (uim-eval '(context-update-mode tc))
  (assert-uim-equal 1
                    'test-updated-mode)
  #f)

(define (test-context-update-mode-list)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (define test-input-mode (context-find-mode-widget tc))
      ;; initial state
      (context-update-mode-list tc)))

  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  (assert-uim-equal 3
                    'test-updated-mode)
  ;; action_test_direct -> action_test_hankana
  (assert-uim-true  '(widget-activate! test-input-mode 'action_test_hankana))
  (uim-eval '(context-update-mode-list tc))
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  (assert-uim-equal 2
                    'test-updated-mode)
  ;; duplicate activity
  (uim-eval
   '(begin
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
      (context-update-mode-list tc)))
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "直接入力"
                      "全角英数")
                    'test-mode-list)
  (assert-uim-equal '("ひらがな"
                      "カタカナ"
                      "半角カタカナ"
                      "直接入力"
                      "直接入力"
                      "全角英数")
                    'test-updated-mode-list)
  ;; context-current-mode returns 0 rather than 3 when
  ;; action_test_direct and action_test_alt_direct are conflicted.
  (assert-uim-equal 0
                    'test-updated-mode)

  ;; no activity
  (uim-eval
   '(begin
      (register-widget
       'widget_test_dummy_input_mode
       (indicator-new (lambda (owner)
                        fallback-indication))
       #f) ;; has no actions
      (context-init-widgets! tc '(widget_test_dummy_input_mode
                                  widget_test_kana_input_method))))
  (assert-uim-true  '(widget-activate! test-input-mode
                                       'action_test_hankana))
  (uim-eval '(context-update-mode-list tc))
  (assert-uim-equal ()
                    'test-mode-list)
  (assert-uim-equal ()
                    'test-updated-mode-list)
  (assert-uim-equal 0
                    'test-updated-mode)
  #f)

(define (test-context-mode-handler)
  ;; 2 widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-uim-true  '(and (context-mode-handler tc 0)
                          #t))
  (assert-uim-equal 'action_test_hiragana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-mode-handler tc 4)
                          #t))
  (assert-uim-equal 'action_test_zenkaku
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-equal 'action_test_kana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-equal 'action_test_direct
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-equal 'action_test_direct
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc -1)
                          #t))
  (assert-uim-false 'test-activated)
  ;; 1 widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-uim-false '(and (context-mode-handler tc 0)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc 4)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-true  '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-equal 'action_test_kana
                    'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc -1)
                          #t))
  (assert-uim-false 'test-activated)
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (set! test-activated #f)))
  (assert-uim-false '(and (context-mode-handler tc 0)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc -1)
                          #t))
  (assert-uim-false 'test-activated)
  ;; widget_test_null (no action handlers)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (set! test-activated #f)))
  (assert-uim-false '(and (context-mode-handler tc 0)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-prop-activate-handler
                           tc
                           "action_test_kana")
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc 3)
                          #t))
  (assert-uim-false 'test-activated)
  (uim-eval '(set! test-activated #f))
  (assert-uim-false '(and (context-mode-handler tc -1)
                          #t))
  (assert-uim-false 'test-activated)
  #f)

(provide "test/test-action")
