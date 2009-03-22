;;; Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/
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

  (assert-false (uim-bool '(indicator-id test-indicator)))
  (assert-false (uim-bool '(indicator-activity-pred test-indicator)))
  (assert-false (uim-bool '(indicator-handler test-indicator)))
  #f)

(define (test-register-action)
  (uim-eval '(set! action-list ()))
  (assert-equal 0
                (uim '(length action-list)))
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
  (assert-equal 1
                (uim '(length action-list)))
  (assert-equal 'action_test_hiragana
                (uim '(caar action-list)))

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
  (assert-equal 2
                (uim '(length action-list)))
  (assert-equal 'action_test_katakana
                (uim '(caar action-list)))

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
  (assert-equal 3
                (uim '(length action-list)))
  (assert-equal 'action_test_hankana
                (uim '(caar action-list))))

(define (test-fetch-action)
  (assert-equal 'action_test_hiragana
                (uim '(action-id (fetch-action 'action_test_hiragana))))
  (assert-equal 'action_test_katakana
                (uim '(action-id (fetch-action 'action_test_katakana))))
  (assert-equal 'action_test_hankana
                (uim '(action-id (fetch-action 'action_test_hankana))))
  #f)

(define (test-action-active?)
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hiragana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_katakana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hankana)
                                           tc)))
  (assert-true  (uim-bool '(action-active? (fetch-action 'action_test_direct)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_zenkaku)
                                           tc)))

  (uim-eval '(test-context-set-wide-latin! tc #t))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hiragana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_katakana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hankana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_direct)
                                           tc)))
  (assert-true  (uim-bool '(action-active? (fetch-action 'action_test_zenkaku)
                                           tc)))

  (uim-eval '(test-context-set-on! tc #t))
  (assert-true  (uim-bool '(action-active? (fetch-action 'action_test_hiragana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_katakana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hankana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_direct)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_zenkaku)
                                           tc)))

  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hiragana)
                                           tc)))
  (assert-true  (uim-bool '(action-active? (fetch-action 'action_test_katakana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_hankana)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_direct)
                                           tc)))
  (assert-false (uim-bool '(action-active? (fetch-action 'action_test_zenkaku)
                                           tc)))
  #f)

(define (test-action-indicate)
  (assert-equal '(figure_ja_hiragana
                  "あ"
                  "ひらがな"
                  "ひらがな入力モード")
                (uim '(action-indicate (fetch-action 'action_test_hiragana)
                                       tc)))
  (assert-equal '(figure_ja_katakana
                  "ア"
                  "カタカナ"
                  "カタカナ入力モード")
                (uim '(action-indicate (fetch-action 'action_test_katakana)
                                       tc)))
  (assert-equal '(figure_ja_kana
                  "か"
                  "かな"
                  "かな入力モード")
                (uim '(action-indicate (fetch-action 'action_test_kana)
                                       tc)))
  ;; no action
  (assert-equal (uim 'fallback-indication)
                (uim '(action-indicate #f tc)))
  ;; no indication handler
  (assert-equal (uim 'fallback-indication)
                (uim '(action-indicate (action-new) tc)))
  #f)

(define (test-actions-new)
  (assert-equal '(action_test_katakana
                  action_test_kana
                  action_test_hiragana)
                (uim '(map action-id (actions-new '(action_test_katakana
                                                    action_test_kana
                                                    action_test_hiragana)))))
  (assert-equal (uim ''(action_test_katakana
                        action_test_kana
                        action_test_hiragana))
                (uim '(map action-id (actions-new '(action_test_katakana
                                                    action_test_kana
                                                    action_nonexistent
                                                    action_test_hiragana)))))
  (assert-equal ()
                (uim '(map action-id (actions-new ()))))
  #f)

(define (test-activity-indicator-new)
  (uim-eval
   '(define indicator (activity-indicator-new '(action_test_hiragana
                                                action_test_katakana
                                                action_test_hankana
                                                action_test_direct
                                                action_test_zenkaku))))
  (assert-equal '(figure_ja_direct
                  "a"
                  "直接入力"
                  "直接(無変換)入力モード")
                (uim '(action-indicate indicator tc)))

  (uim-eval '(test-context-set-wide-latin! tc #t))
  (assert-equal '(figure_ja_zenkaku
                  "Ａ"
                  "全角英数"
                  "全角英数入力モード")
                (uim '(action-indicate indicator tc)))

  (uim-eval '(test-context-set-on! tc #t))
  (assert-equal '(figure_ja_hiragana
                  "あ"
                  "ひらがな"
                  "ひらがな入力モード")
                (uim '(action-indicate indicator tc)))

  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-equal '(figure_ja_katakana
                  "ア"
                  "カタカナ"
                  "カタカナ入力モード")
                (uim '(action-indicate indicator tc)))
   ;; no activity case
  (uim-eval '(define test-type-invalid 100))
  (uim-eval '(test-context-set-kana-mode! tc test-type-invalid))
  (assert-equal '(unknown
                  "?"
                  "unknown"
                  "unknown")
                (uim '(action-indicate indicator tc)))
  #f)

(define (test-register-widget)
  (uim-eval '(set! widget-proto-list ()))
  (assert-equal 0
                (uim '(length widget-proto-list)))

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
  (assert-equal 1
                (uim '(length widget-proto-list)))
  (assert-equal 'widget_test_input_mode
                (uim '(caar widget-proto-list)))

  (uim-eval
   '(begin
      (register-widget
       'widget_test_input_mode
       (indicator-new (lambda (tc)
                        fallback-indication))
       (actions-new '(action_test_direct)))
      #t))
  (assert-equal 1
                (uim '(length widget-proto-list)))
  (assert-equal 'widget_test_input_mode
                (uim '(caar widget-proto-list)))

  (uim-eval
   '(begin
      (register-widget
       'widget_test_kana_input_method
       (indicator-new (lambda (tc)
                        fallback-indication))
       (actions-new '(action_test_roma
                      action_test_kana)))
      #t))
  (assert-equal 2
                (uim '(length widget-proto-list)))
  (assert-equal 'widget_test_kana_input_method
                (uim '(caar widget-proto-list)))
  (assert-equal 'widget_test_input_mode
                (uim '(car (cadr widget-proto-list))))
  #f)

(define (test-widget-new)
  (assert-false (uim-bool '(widget-new 'widget_test_nonexistent tc)))
  ;; widget_test_input_mode
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal 'widget_test_input_mode
                (uim '(widget-id test-input-mode)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))

  ;; widget_test_input_mode with default value
  (uim-eval '(define default-widget_test_input_mode 'action_test_hiragana))
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal 'action_test_hiragana
                (uim '(action-id (widget-activity test-input-mode))))

  ;; widget_test_input_mode with default value #2
  (uim-eval '(define default-widget_test_input_mode 'action_test_katakana))
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal 'action_test_katakana
                (uim '(action-id (widget-activity test-input-mode))))

  ;; widget_test_input_mode with default value #3
  (uim-eval '(define default-widget_test_input_mode 'action_test_zenkaku))
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal 'action_test_zenkaku
                (uim '(action-id (widget-activity test-input-mode))))

  ;; widget_test_input_mode with invalid default value
  (uim-eval '(define default-widget_test_input_mode 'action_nonexistent))
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal 'action_test_zenkaku
                (uim '(action-id (widget-activity test-input-mode))))

  ;; widget_test_kana_input_method
  (assert-true  (uim-bool '(begin (define test-kana-input-method
                                    (widget-new 'widget_test_kana_input_method tc))
                                  #t)))
  (assert-equal 'action_test_roma
                (uim '(action-id (widget-activity test-kana-input-method))))

  ;; widget_test_kana_input_method with default value
  (uim-eval '(define default-widget_test_kana_input_method 'action_test_kana))
  (assert-true  (uim-bool '(begin (define test-kana-input-method
                                    (widget-new 'widget_test_kana_input_method tc))
                                  #t)))
  (assert-equal 'action_test_kana
                (uim '(action-id (widget-activity test-kana-input-method))))

  ;; widget_test_kana_input_method with invalid default value
  (uim-eval '(define default-widget_test_kana_input_method 'action_nonexistent))
  (assert-true  (uim-bool '(begin (define test-kana-input-method
                                    (widget-new 'widget_test_kana_input_method tc))
                                  #t)))
  (assert-equal 'action_test_kana
                (uim '(action-id (widget-activity test-kana-input-method))))
  #f)

(define (test-widget-activity)
   ;;; widget_test_input_mode
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  ;; action_test_direct (initial activity)
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hiragana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))

  ;; action_test_direct -> action_test_hiragana
  (uim-eval '(test-context-set-wide-latin! tc #t))
  (uim-eval '(test-context-set-on! tc #t))
  (assert-equal (uim 'test-type-hiragana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_hiragana
                (uim '(action-id (widget-activity test-input-mode))))

  ;; action_test_hiragana -> action_test_katakana
  (uim-eval '(test-context-set-wide-latin! tc #f))
  (uim-eval '(test-context-set-kana-mode! tc test-type-katakana))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-katakana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_katakana
                (uim '(action-id (widget-activity test-input-mode))))

  ;; action_test_katakana -> action_test_hankana
  (uim-eval '(test-context-set-kana-mode! tc test-type-hankana))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_hankana
                (uim '(action-id (widget-activity test-input-mode))))

  ;; action_test_hankana -> action_test_direct
  (uim-eval '(test-context-set-on! tc #f))
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))

  ;; action_test_direct -> invalid
  (uim-eval '(define test-type-invalid 100))
  (uim-eval '(test-context-set-on! tc #t))
  (uim-eval '(test-context-set-kana-mode! tc test-type-invalid))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-invalid)
                (uim '(test-context-kana-mode tc)))
  (assert-false (uim-bool '(widget-activity test-input-mode)))

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
  (assert-false (uim-bool '(widget-activity test-invalid-input-mode)))
  ;; conflicted -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-invalid-input-mode
                                             'action_test_hiragana)))
  (assert-equal 'action_test_hiragana
                (uim '(action-id (widget-activity test-invalid-input-mode))))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-invalid-input-mode
                                             'action_test_katakana)))
  (assert-equal 'action_test_katakana
                (uim '(action-id (widget-activity test-invalid-input-mode))))
  #f)

(define (test-widget-activate!)
   ;;; widget_test_input_mode
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  ;; action_test_direct (initial activity)
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hiragana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_direct -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hiragana)))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hiragana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_hiragana
                (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-katakana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_katakana
                (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_katakana -> action_test_hankana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hankana)))
  (assert-true  (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_hankana
		 (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_hankana -> action_test_zenkaku
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_zenkaku)))
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-true  (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_zenkaku
                (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_zenkaku -> action_test_direct
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_direct)))
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))
  ;; action_test_direct -> invalid
  (assert-false (uim-bool '(widget-activate! test-input-mode
                                             'action_nonexistent)))
  (assert-false (uim-bool '(test-context-on tc)))
  (assert-false (uim-bool '(test-context-wide-latin tc)))
  (assert-equal (uim 'test-type-hankana)
                (uim '(test-context-kana-mode tc)))
  (assert-equal 'action_test_direct
                (uim '(action-id (widget-activity test-input-mode))))
  #f)

(define (test-widget-configuration)
  ;; widget_test_input_mode
  (assert-true  (uim-bool '(begin (define test-input-mode
                                    (widget-new 'widget_test_input_mode tc))
                                  #t)))
  (assert-equal '(action_unknown
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
                (uim '(widget-configuration test-input-mode)))

  ;; widget_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-equal '(action_unknown
                  (figure_ja_roma
                   "Ｒ"
                   "ローマ字"
                   "ローマ字入力モード")
                  (figure_ja_kana
                   "か"
                   "かな"
                   "かな入力モード"))
                (uim '(widget-configuration test-kana-input-method)))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-equal '(action_unknown)
                (uim '(widget-configuration test-null)))
  #f)

(define (test-widget-state)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_direct)
                                         '(figure_ja_direct
                                           "a"
                                           "直接入力"
                                           "直接(無変換)入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))
  (assert-false (uim-bool '(widget-activate! test-input-mode
                                             'action_nonexistent)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))

  ;; widget_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_roma)
                                         '(figure_ja_roma
                                           "Ｒ"
                                           "ローマ字"
                                           "ローマ字入力モード"))
                                   (widget-state test-kana-input-method))))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-state test-null))))
  #f)

(define (test-widget-update-configuration!)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-equal '(action_unknown
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
                (uim '(widget-configuration test-input-mode)))
  (assert-false (uim-bool '(widget-prev-config test-input-mode)))
  (assert-true  (uim-bool '(widget-update-configuration! test-input-mode)))
  (assert-equal '(action_unknown
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
                (uim '(widget-configuration test-input-mode)))
  (assert-equal '(action_unknown
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
                (uim '(widget-prev-config test-input-mode)))
  (assert-false (uim-bool '(widget-update-configuration! test-input-mode)))
  (assert-equal '(action_unknown
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
                (uim '(widget-prev-config test-input-mode)))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))
  (assert-equal '(action_unknown)
                (uim '(widget-configuration test-null)))
  (assert-false (uim-bool '(widget-prev-config test-null)))
  ;; initial update (widget_test_null with fallback-indication)
  (assert-true  (uim-bool '(widget-update-configuration! test-null)))
  ;; subsequent update
  (assert-false (uim-bool '(widget-update-configuration! test-null)))
  #f)

(define (test-widget-update-state!)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))

  ;; initial state
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_direct)
                                         '(figure_ja_direct
                                           "a"
                                           "直接入力"
                                           "直接(無変換)入力モード"))
                                   (widget-state test-input-mode))))
  (assert-false (uim-bool '(widget-prev-state test-input-mode)))
  ;; initial update
  (assert-true  (uim-bool '(widget-update-state! test-input-mode)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_direct)
                                         '(figure_ja_direct
                                           "a"
                                           "直接入力"
                                           "直接(無変換)入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_direct)
                                         '(figure_ja_direct
                                           "a"
                                           "直接入力"
                                           "直接(無変換)入力モード"))
                                   (widget-prev-state test-input-mode))))
  ;; action_test_direct -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_direct)
                                         '(figure_ja_direct
                                           "a"
                                           "直接入力"
                                           "直接(無変換)入力モード"))
                                   (widget-prev-state test-input-mode))))
  (assert-true  (uim-bool '(widget-update-state! test-input-mode)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-prev-state test-input-mode))))
  ;; action_test_katakana -> action_test_katakana
  (assert-false (uim-bool '(widget-update-state! test-input-mode)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-prev-state test-input-mode))))
  ;; invalid activation
  (assert-false (uim-bool '(widget-activate! test-input-mode
                                             'action_nonexistent)))
  (assert-false (uim-bool '(widget-update-state! test-input-mode)))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-state test-input-mode))))
  (assert-true  (uim-bool '(equal? (list (fetch-action 'action_test_katakana)
                                         '(figure_ja_katakana
                                           "ア"
                                           "カタカナ"
                                           "カタカナ入力モード"))
                                   (widget-prev-state test-input-mode))))

  ;; widget_test_null
  (uim-eval
   '(define test-null
      (widget-new 'widget_test_null tc)))

  ;; initial state
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-state test-null))))
  (assert-false (uim-bool '(widget-prev-state test-null)))
  ;; initial update
  (assert-true  (uim-bool '(widget-update-state! test-null)))
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-state test-null))))
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-prev-state test-null))))
  ;; subsequent update
  (assert-false (uim-bool '(widget-update-state! test-null)))
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-state test-null))))
  (assert-true  (uim-bool '(equal? (list #f
                                         '(unknown
                                           "?"
                                           "unknown"
                                           "unknown"))
                                   (widget-prev-state test-null))))
  #f)

(define (test-widget-debug-message)
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))
  (assert-equal "something in somewhere. debug widget_test_input_mode."
                (uim '(widget-debug-message test-input-mode
                                            "somewhere"
                                            "something")))
  #f)

(define (test-indication-compose-label)
  (assert-equal "figure_ja_hiragana\tあ\tひらがな\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_hiragana)
                                        tc))))
  (assert-equal "figure_ja_katakana\tア\tカタカナ\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_katakana)
                                        tc))))
  (assert-equal "figure_ja_hankana\tｱ\t半角カタカナ\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_hankana)
                                        tc))))
  (assert-equal "figure_ja_direct\ta\t直接入力\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_direct)
                                        tc))))
  (assert-equal "figure_ja_zenkaku\tＡ\t全角英数\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_zenkaku)
                                        tc))))
  (assert-equal "figure_ja_roma\tＲ\tローマ字\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_roma)
                                        tc))))
  (assert-equal "figure_ja_kana\tか\tかな\n"
                (uim '(indication-compose-label
                       (action-indicate (fetch-action 'action_test_kana)
                                        tc))))
  #f)

(define (test-indication-compose-branch)
  (assert-equal "branch\tfigure_ja_hiragana\tあ\tひらがな\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_hiragana)
                                        tc))))
  (assert-equal "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_katakana)
                                        tc))))
  (assert-equal "branch\tfigure_ja_hankana\tｱ\t半角カタカナ\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_hankana)
                                        tc))))
  (assert-equal "branch\tfigure_ja_direct\ta\t直接入力\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_direct)
                                        tc))))
  (assert-equal "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_zenkaku)
                                        tc))))
  (assert-equal "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_roma)
                                        tc))))
  (assert-equal "branch\tfigure_ja_kana\tか\tかな\n"
                (uim '(indication-compose-branch
                       (action-indicate (fetch-action 'action_test_kana)
                                        tc))))
  #f)

(define (test-indication-compose-leaf)
  ;; inactive leaves
  (assert-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_hiragana)
                                        tc)
                       'action_test_hiragana
                       #f)))
  (assert-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_katakana)
                                        tc)
                       'action_test_katakana
                       #f)))
  (assert-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_hankana)
                                        tc)
                       'action_test_hankana
                       #f)))
  (assert-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_direct)
                                        tc)
                       'action_test_direct
                       #f)))
  (assert-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_zenkaku)
                                        tc)
                       'action_test_zenkaku
                       #f)))
  (assert-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_roma)
                                        tc)
                       'action_test_roma
                       #f)))
  (assert-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_kana)
                                        tc)
                       'action_test_kana
                       #f)))
  ;; active leaves
  (assert-equal "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_hiragana)
                                        tc)
                       'action_test_hiragana
                       #t)))
  (assert-equal "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_katakana)
                                        tc)
                       'action_test_katakana
                       #t)))
  (assert-equal "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_hankana)
                                        tc)
                       'action_test_hankana
                       #t)))
  (assert-equal "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_direct)
                                        tc)
                       'action_test_direct
                       #t)))
  (assert-equal "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_zenkaku)
                                        tc)
                       'action_test_zenkaku
                       #t)))
  (assert-equal "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_roma)
                                        tc)
                       'action_test_roma
                       #t)))
  (assert-equal "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n"
                (uim '(indication-compose-leaf
                       (action-indicate (fetch-action 'action_test_kana)
                                        tc)
                       'action_test_kana
                       #t)))
  #f)

(define (test-widget-compose-live-branch)
  ;; widget_test_input_mode
  (uim-eval
   '(define test-input-mode
      (widget-new 'widget_test_input_mode tc)))

  (assert-equal (string-append
                 "branch\tfigure_ja_direct\ta\t直接入力\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                (uim '(widget-compose-live-branch test-input-mode)))
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_zenkaku)))
  (assert-equal (string-append
                 "branch\tfigure_ja_zenkaku\tＡ\t全角英数\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t*\n")
                (uim '(widget-compose-live-branch test-input-mode)))

  ;; prop_test_kana_input_method
  (uim-eval
   '(define test-kana-input-method
      (widget-new 'widget_test_kana_input_method tc)))
  (assert-equal (string-append
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim '(widget-compose-live-branch test-kana-input-method)))
  (assert-true  (uim-bool '(widget-activate! test-kana-input-method
                                             'action_test_kana)))
  (assert-equal (string-append
                 "branch\tfigure_ja_kana\tか\tかな\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t*\n")
                (uim '(widget-compose-live-branch test-kana-input-method)))
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

  (assert-equal '(widget_test_input_mode
                  widget_test_kana_input_method)
                (uim '(map widget-id test-widget-conf)))
  ;; contains a non-existent widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_nonexistent
                                  widget_test_kana_input_method))
      #f))
  (assert-equal '(widget_test_input_mode
                  widget_test_kana_input_method)
                (uim '(map widget-id test-widget-conf)))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      #f))
  (assert-equal '(widget_fallback)
                (uim '(map widget-id test-widget-conf)))
  ;; null widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      #f))
  (assert-equal '(widget_test_null)
                (uim '(map widget-id test-widget-conf)))
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

  (assert-equal '(widget_test_input_mode
                  widget_test_kana_input_method)
                (uim '(map widget-id test-widget-conf)))
  (assert-equal '(widget_test_input_mode
                  widget_test_kana_input_method)
                (uim '(map widget-id test-widget-state)))
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
  (assert-true (uim-bool '(widget-activate! (assq 'widget_test_input_mode
                                                  (context-widgets tc))
                                            'action_test_katakana)))
  (uim-eval
   '(begin
      (context-update-widgets tc)
      #f))
  (assert-true  (null? (uim '(map widget-id test-widget-conf))))
  (assert-equal '(widget_test_input_mode
                  widget_test_kana_input_method)
                (uim '(map widget-id test-widget-state)))
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

   (assert-equal '(widget_test_input_mode
		   widget_test_kana_input_method)
		 (uim '(map widget-id test-widget-conf)))
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
   (assert-equal '(widget_test_input_mode)
		 (uim '(map widget-id test-widget-conf)))
   (assert-equal '(widget_test_input_mode)
		 (uim '(map widget-id test-widget-state)))
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
   (assert-equal '(widget_fallback)
		 (uim '(map widget-id test-widget-conf)))
   (assert-equal '(widget_fallback)
		 (uim '(map widget-id test-widget-state)))

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
   (assert-equal '(widget_test_null)
		 (uim '(map widget-id test-widget-conf)))
   (assert-equal '(widget_test_null)
		 (uim '(map widget-id test-widget-state)))
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
  (assert-equal (string-append
                 "branch\tfigure_ja_direct\ta\t直接入力\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))

  ;; 2 widgets (updated state)
  (assert-true (uim-bool '(widget-activate! (assq 'widget_test_input_mode
                                                  (context-widgets tc))
                                            'action_test_katakana)))
  (uim-eval '(context-propagate-prop-list-update tc))
  (assert-equal (string-append
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))

  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-prop-list-update tc)))
  (assert-equal (string-append
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                (uim 'test-prop-list))

  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-prop-list-update tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-prop-list-update tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
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
  (assert-equal (string-append
                 "branch\tfigure_ja_direct\ta\t直接入力\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))
  (assert-false (uim-bool 'test-prop-label))
  (assert-equal 3
                (uim 'test-updated-mode))
  ;; 2 widgets (updated state)
  (assert-true (uim-bool '(widget-activate! (assq 'widget_test_input_mode
                                                  (context-widgets tc))
                                            'action_test_katakana)))
  (uim-eval '(context-propagate-widget-states tc))
  (assert-equal (string-append
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))
  (assert-false (uim-bool 'test-prop-label))
  (assert-equal 1
                (uim 'test-updated-mode))
  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-widget-states tc)))
  (assert-equal (string-append
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                (uim 'test-prop-list))
  (assert-false (uim-bool 'test-prop-label))
  (assert-equal 1
                (uim 'test-updated-mode))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-widget-states tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
  (assert-false (uim-bool 'test-prop-label))
  (assert-equal 0
                (uim 'test-updated-mode))
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-widget-states tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
  (assert-false (uim-bool 'test-prop-label))
  (assert-equal 0
                (uim 'test-updated-mode))
  #f)

(define (test-context-propagate-widget-configuration)
  (uim-eval
   '(begin
      ;; 2 widgets
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      ;; initial state
      (context-propagate-widget-configuration tc)))
  (assert-equal (string-append
                 "branch\tfigure_ja_direct\ta\t直接入力\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t*\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  (assert-equal 3
                (uim 'test-updated-mode))
  ;; 2 widgets (updated state)
  (assert-true (uim-bool '(widget-activate! (assq 'widget_test_input_mode
                                                  (context-widgets tc))
                                            'action_test_katakana)))
  (uim-eval '(context-propagate-widget-configuration tc))
  (assert-equal (string-append
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n"
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n")
                (uim 'test-prop-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  (assert-equal 1
                (uim 'test-updated-mode))
  ;; 2 widgets with non-existent
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method
                                  widget_test_nonexistent
                                  widget_test_input_mode))
      (context-propagate-widget-configuration tc)))
  (assert-equal (string-append
                 "branch\tfigure_ja_roma\tＲ\tローマ字\n"
                 "leaf\tfigure_ja_roma\tＲ\tローマ字\tローマ字入力モード\taction_test_roma\t*\n"
                 "leaf\tfigure_ja_kana\tか\tかな\tかな入力モード\taction_test_kana\t\n"
                 "branch\tfigure_ja_katakana\tア\tカタカナ\n"
                 "leaf\tfigure_ja_hiragana\tあ\tひらがな\tひらがな入力モード\taction_test_hiragana\t\n"
                 "leaf\tfigure_ja_katakana\tア\tカタカナ\tカタカナ入力モード\taction_test_katakana\t*\n"
                 "leaf\tfigure_ja_hankana\tｱ\t半角カタカナ\t半角カタカナ入力モード\taction_test_hankana\t\n"
                 "leaf\tfigure_ja_direct\ta\t直接入力\t直接(無変換)入力モード\taction_test_direct\t\n"
                 "leaf\tfigure_ja_zenkaku\tＡ\t全角英数\t全角英数入力モード\taction_test_zenkaku\t\n")
                (uim 'test-prop-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  (assert-equal 1
                (uim 'test-updated-mode))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (context-propagate-widget-configuration tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
  (assert-equal '("unknown")
                (uim 'test-mode-list))
  (assert-equal '("unknown")
                (uim 'test-updated-mode-list))
  (assert-equal 0
                (uim 'test-updated-mode))
  ;; widget_test_null
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (context-propagate-widget-configuration tc)))
  (assert-equal "branch\tunknown\t?\tunknown\n"
                (uim 'test-prop-list))
  (assert-equal '("unknown")
                (uim 'test-mode-list))
  (assert-equal '("unknown")
                (uim 'test-updated-mode-list))
  (assert-equal 0
                (uim 'test-updated-mode))
  #f)

(define (test-context-prop-activate-handler)
  ;; 2 widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_hiragana")
                                #t)))
  (assert-equal 'action_test_hiragana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_zenkaku")
                                #t)))
  (assert-equal 'action_test_zenkaku
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-equal 'action_test_kana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-equal 'action_test_direct
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-equal 'action_test_direct
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_nonexistent")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; 1 widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_hiragana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_zenkaku")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-equal 'action_test_kana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_nonexistent")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_hiragana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_nonexistent")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; widget_test_null (no action handlers)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_hiragana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_direct")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_nonexistent")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  #f)

(define (test-context-find-mode-widget)
  (uim-eval
   '(context-init-widgets! tc '(widget_test_input_mode
                                widget_test_kana_input_method)))
  (assert-equal 'widget_test_input_mode
                (uim '(widget-id (context-find-mode-widget tc))))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_input_mode)))
  (assert-equal 'widget_test_input_mode
                (uim '(widget-id (context-find-mode-widget tc))))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_input_mode
                                widget_test_null)))
  (assert-equal 'widget_test_input_mode
                (uim '(widget-id (context-find-mode-widget tc))))
  (uim-eval
   '(context-init-widgets! tc '(widget_test_kana_input_method
                                widget_test_null)))
  (assert-false (uim-bool '(context-find-mode-widget tc)))
  (uim-eval
   '(context-init-widgets! tc ()))
  (assert-false (uim-bool '(context-find-mode-widget tc)))
  #f)

(define (test-widget-action-id->mode-value)
  (uim-eval
   '(define mw (widget-new 'widget_test_input_mode tc)))
  (assert-equal 0
                (uim '(widget-action-id->mode-value mw
                                                    'action_test_hiragana)))
  (assert-equal 1
                (uim '(widget-action-id->mode-value mw
                                                    'action_test_katakana)))
  (assert-equal 2
                (uim '(widget-action-id->mode-value mw
                                                    'action_test_hankana)))
  (assert-equal 3
                (uim '(widget-action-id->mode-value mw
                                                    'action_test_direct)))
  (assert-equal 4
                (uim '(widget-action-id->mode-value mw
                                                    'action_test_zenkaku)))
  (assert-error (lambda ()
                  (uim '(widget-action-id->mode-value mw 'action_test_nonexistent))))
  #f)

(define (test-widget-mode-value->action-id)
  (uim-eval
   '(define mw (widget-new 'widget_test_input_mode tc)))

  (assert-equal 'action_test_hiragana
                (uim '(widget-mode-value->action-id mw 0)))
  (assert-equal 'action_test_katakana
                (uim '(widget-mode-value->action-id mw 1)))
  (assert-equal 'action_test_hankana
                (uim '(widget-mode-value->action-id mw 2)))
  (assert-equal 'action_test_direct
                (uim '(widget-mode-value->action-id mw 3)))
  (assert-equal 'action_test_zenkaku
                (uim '(widget-mode-value->action-id mw 4)))
  (assert-false (uim-bool '(widget-mode-value->action-id mw 5)))
  (assert-false (uim-bool '(widget-mode-value->action-id mw -1)))
  #f)

(define (test-context-current-mode)
  ;; widget_test_input_mode
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (define test-input-mode (context-find-mode-widget tc))))

  ;; action_test_direct (initial activity)
  (assert-equal 3
                (uim '(context-current-mode tc)))
  ;; action_test_direct -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hiragana)))
  (assert-equal 0
                (uim '(context-current-mode tc)))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (assert-equal 1
                (uim '(context-current-mode tc)))
  ;; action_test_katakana -> action_test_hankana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hankana)))
  (assert-equal 2
                (uim '(context-current-mode tc)))
  ;; action_test_hankana -> action_test_zenkaku
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_zenkaku)))
  (assert-equal 4
                (uim '(context-current-mode tc)))
  ;; action_test_zenkaku -> action_test_direct
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_direct)))
  (assert-equal 3
                (uim '(context-current-mode tc)))
  ;; action_test_direct -> invalid
  (assert-false (uim-bool '(widget-activate! test-input-mode
                                             'action_nonexistent)))
  (assert-equal 3
                (uim '(context-current-mode tc)))
  (assert-error (lambda ()
                  (uim '(context-current-mode #f))))

  ;; no mode-widget
  (uim-eval
   '(context-init-widgets! tc '(widget_test_null
                                widget_test_kana_input_method)))
  (assert-equal 0
                (uim '(context-current-mode tc)))
  (assert-error (lambda ()
                  (uim '(context-current-mode #f))))

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
  (assert-equal 0
                (uim '(context-current-mode tc)))
  (assert-error (lambda ()
                  (uim '(context-current-mode #f))))

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
  (assert-equal 0
                (uim '(context-current-mode tc)))
  ;; action_test_direct -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hiragana)))
  (assert-equal 0
                (uim '(context-current-mode tc)))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (assert-equal 1
                (uim '(context-current-mode tc)))
  (assert-error (lambda ()
                  (uim '(context-current-mode #f))))
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
  (assert-equal 3
                (uim 'test-updated-mode))
  ;; action_test_direct -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hiragana)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 0
                (uim 'test-updated-mode))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 1
                (uim 'test-updated-mode))
  ;; action_test_katakana -> action_test_hankana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hankana)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 2
                (uim 'test-updated-mode))
  ;; action_test_hankana -> action_test_zenkaku
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_zenkaku)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 4
                (uim 'test-updated-mode))
  ;; action_test_zenkaku -> action_test_direct
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_direct)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 3
                (uim 'test-updated-mode))
  ;; action_test_direct -> invalid
  (assert-false (uim-bool '(widget-activate! test-input-mode
                                             'action_nonexistent)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 3
                (uim 'test-updated-mode))
  (assert-error (lambda ()
                  (uim '(context-current-mode #f))))

  ;; no mode-widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null
                                  widget_test_kana_input_method))
      (context-update-mode tc)))
  (assert-equal 0
                (uim 'test-updated-mode))

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
  (assert-equal 0
                (uim 'test-updated-mode))

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
  (assert-equal 0
                (uim 'test-updated-mode))
  ;; action_test_direct -> action_test_hiragana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hiragana)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 0
                (uim 'test-updated-mode))
  ;; action_test_hiragana -> action_test_katakana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_katakana)))
  (uim-eval '(context-update-mode tc))
  (assert-equal 1
                (uim 'test-updated-mode))
  #f)

(define (test-context-update-mode-list)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (define test-input-mode (context-find-mode-widget tc))
      ;; initial state
      (context-update-mode-list tc)))

  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  (assert-equal 3
                (uim 'test-updated-mode))
  ;; action_test_direct -> action_test_hankana
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hankana)))
  (uim-eval '(context-update-mode-list tc))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  (assert-equal 2
                (uim 'test-updated-mode))
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
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "直接入力"
                  "全角英数")
                (uim 'test-mode-list))
  (assert-equal '("ひらがな"
                  "カタカナ"
                  "半角カタカナ"
                  "直接入力"
                  "直接入力"
                  "全角英数")
                (uim 'test-updated-mode-list))
  ;; context-current-mode returns 0 rather than 3 when
  ;; action_test_direct and action_test_alt_direct are conflicted.
  (assert-equal 0
                (uim 'test-updated-mode))

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
  (assert-true  (uim-bool '(widget-activate! test-input-mode
                                             'action_test_hankana)))
  (uim-eval '(context-update-mode-list tc))
  (assert-equal ()
                (uim 'test-mode-list))
  (assert-equal ()
                (uim 'test-updated-mode-list))
  (assert-equal 0
                (uim 'test-updated-mode))
  #f)

(define (test-context-mode-handler)
  ;; 2 widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_input_mode
                                  widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-true  (uim-bool '(and (context-mode-handler tc 0)
                                #t)))
  (assert-equal 'action_test_hiragana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-mode-handler tc 4)
                                #t)))
  (assert-equal 'action_test_zenkaku
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-equal 'action_test_kana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-equal 'action_test_direct
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-equal 'action_test_direct
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc -1)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; 1 widget
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_kana_input_method))
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-mode-handler tc 0)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc 4)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-true  (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-equal 'action_test_kana
                (uim 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc -1)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; no widgets
  (uim-eval
   '(begin
      (context-init-widgets! tc ())
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-mode-handler tc 0)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc -1)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  ;; widget_test_null (no action handlers)
  (uim-eval
   '(begin
      (context-init-widgets! tc '(widget_test_null))
      (set! test-activated #f)))
  (assert-false (uim-bool '(and (context-mode-handler tc 0)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-prop-activate-handler
                                 tc
                                 "action_test_kana")
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc 3)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  (uim-eval '(set! test-activated #f))
  (assert-false (uim-bool '(and (context-mode-handler tc -1)
                                #t)))
  (assert-false (uim-bool 'test-activated))
  #f)

(provide "test/test-action")
