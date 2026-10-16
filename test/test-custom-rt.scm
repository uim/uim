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

;; TODO:
;;
;; custom-reload-customs

(require-extension (unittest))

;; define-custom doesn't change the value of an existing variable. So
;; the variables defined by the tests are removed before each test case.
(define setup
  (lambda ()
    (for-each %%undefine
              '(test-style test-available-ims
                test-cancel-key test-cancel-key?
                test-foo-key test-foo-key?
                test-bar-key test-bar-key?
                test-baz-key test-baz-key?
                test-use-candidate-window? test-nr-candidate-max
                test-string test-dic-file-name))))

(test-begin "define-custom (choice)")
(setup)
(test-false (symbol-bound? 'test-style))

(define-custom 'test-style 'test-style-ddskk
  '(global)
  '(choice
    (test-style-uim "uim" "uim native")
    (test-style-ddskk "ddskk like" "Similar to ddskk")
    (test-style-canna "canna like" "Similar to canna"))
  "Test style"
  "long description will be here.")

(test-true (symbol-bound? 'test-style))
(test-equal 'test-style-ddskk
            test-style)
(test-end)

(test-begin "define-custom (choice) #2")
(setup)
(define test-style 'test-style-uim)

(define-custom 'test-style 'test-style-ddskk
  '(global)
  '(choice
    (test-style-uim "uim" "uim native")
    (test-style-ddskk "ddskk like" "Similar to ddskk")
    (test-style-canna "canna like" "Similar to canna"))
  "Test style"
  "long description will be here.")

(test-true  (symbol-bound? 'test-style))
;; preexisting value is not overridden
(test-equal 'test-style-uim
            test-style)
(test-end)

(test-begin "define-custom (key)")
(setup)
;; single key str
(test-false (symbol-bound? 'test-foo-key))
(test-false (symbol-bound? 'test-foo-key?))

(define-custom 'test-foo-key '("a")
  '(global)
  '(key)
  "test foo key"
  "long description will be here")

(test-true  (symbol-bound? 'test-foo-key))
(test-equal '("a")
            test-foo-key)
(test-true  (symbol-bound? 'test-foo-key?))
(test-true  (test-foo-key? (string->charcode "a") 0))

;; key reference + key str
(test-false (symbol-bound? 'test-bar-key))
(test-false (symbol-bound? 'test-bar-key?))

(define-custom 'test-bar-key '(test-foo-key "b")
  '(global)
  '(key)
  "test bar key"
  "long description will be here")

(test-true  (symbol-bound? 'test-bar-key))
(test-equal '(test-foo-key "b")
            test-bar-key)
(test-true  (symbol-bound? 'test-bar-key?))
(test-true  (test-bar-key? (string->charcode "a") 0))
(test-true  (test-bar-key? (string->charcode "b") 0))
(test-end)

(test-begin "define-custom (key) #2")
(setup)
(define test-foo-key '("b"))
(test-false (symbol-bound? 'test-foo-key?))

(define-custom 'test-foo-key '("a")
  '(global)
  '(key)
  "test foo key"
  "long description will be here")

;; preexisting value is not overridden
(test-equal '("b")
            test-foo-key)
;; key predicate is not defined since custom-set-value! is not
;; invoked
(test-false (symbol-bound? 'test-foo-key?))
(test-end)

(define setup-methods
  (lambda ()
    (setup)

    (define-custom 'test-style 'test-style-ddskk
      '(global)
      '(choice
        (test-style-uim "uim" "uim native")
        (test-style-ddskk "ddskk like" "Similar to ddskk")
        (test-style-canna "canna like" "Similar to canna"))
      "Test style"
      "long description will be here.")
    (define-custom 'test-available-ims '(anthy canna skk)
      '(global)
      '(ordered-list
        (anthy "Anthy" "Anthy")
        (canna "Cannd" "Canna")
        (skk "SKK" "SKK"))
      "Test avalilable IMs"
      "long description will be here.")
    (define-custom 'test-cancel-key '("<Control>g" "escape")
      '(global)
      '(key)
      "test cancel key"
      "long description will be here.")
    (define-custom 'test-foo-key '("a" test-cancel-key)
      '(global)
      '(key)
      "test foo key"
      "long description will be here.")
    (define-custom 'test-bar-key '("b")
      '(global)
      '(key)
      "test bar key"
      "long description will be here.")
    (define-custom 'test-use-candidate-window? #t
      '(test ui)
      '(boolean)
      "Use candidate window"
      "long description will be here.")
    (define-custom 'test-nr-candidate-max 10
      '(test advanced ui)
      '(integer 1 20)
      "Number of candidates in candidate window at a time"
      "long description will be here.")
    (define-custom 'test-string "a string"
      '(test)
      '(string ".+")
      "A string for testing purpose"
      "long description will be here.")
    (define-custom 'test-dic-file-name "/usr/share/skk/SKK-JISYO.L"
      '(test)
      '(pathname)
      "Dictionary file"
      "long description will be here.")))

(test-begin "custom-key-exist?")
(setup-methods)
(test-true  (custom-key-exist? 'test-cancel-key))
(test-false (custom-key-exist? 'test-baz-key))
(define-key test-baz-key? '("z"))
(test-false (custom-key-exist? 'test-baz-key))
(define-custom 'test-baz-key '("z")
  '(global)
  '(key)
  "test foo key"
  "long description will be here.")
(test-true  (custom-key-exist? 'test-baz-key))
(test-end)

(test-begin "custom-value")
(setup-methods)
(test-equal 'test-style-ddskk
            (custom-value 'test-style))
(test-equal '(anthy canna skk)
            (custom-value 'test-available-ims))
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
(test-true  (custom-value 'test-use-candidate-window?))
(test-equal 10
            (custom-value 'test-nr-candidate-max))
(test-equal "a string"
            (custom-value 'test-string))
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-value 'test-dic-file-name))
(test-end)

(test-begin "custom-set-value!")
(setup-methods)
;;; choice
;; default value
(test-equal 'test-style-ddskk
            (custom-value 'test-style))
;; valid value
(test-true  (custom-set-value! 'test-style 'test-style-uim))
(test-equal 'test-style-uim
            (custom-value 'test-style))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-style 'test-style-invalid))
(test-equal 'test-style-invalid
            (custom-value 'test-style))

;;; ordered-list
;; default value
(test-equal '(anthy canna skk)
            (custom-value 'test-available-ims))
;; valid value
(test-true  (custom-set-value! 'test-available-ims
                               '()))
(test-equal '()
            (custom-value 'test-available-ims))
(test-true  (custom-set-value! 'test-available-ims
                               '(anthy)))
(test-equal '(anthy)
            (custom-value 'test-available-ims))
(test-true  (custom-set-value! 'test-available-ims
                               '(anthy skk)))
(test-equal '(anthy skk)
            (custom-value 'test-available-ims))
(test-true  (custom-set-value! 'test-available-ims
                               '(skk anthy)))
(test-equal '(skk anthy)
            (custom-value 'test-available-ims))
(test-true  (custom-set-value! 'test-available-ims
                               '(skk anthy canna)))
(test-equal '(skk anthy canna)
            (custom-value 'test-available-ims))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-available-ims
                               '(nonexistent)))
(test-equal '(nonexistent)
            (custom-value 'test-available-ims))

;;; key
;; default value
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
;; valid value
(test-true  (custom-set-value! 'test-cancel-key '("a")))
(test-equal '("a")
            (custom-value 'test-cancel-key))
(test-true  (procedure? test-cancel-key?))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-cancel-key
                               '(test-nonexistent "a")))
(test-equal '(test-nonexistent "a")
            (custom-value 'test-cancel-key))
(test-true  (procedure? test-cancel-key?))

;;; boolean
;; default value
(test-true  (custom-value 'test-use-candidate-window?))
;; valid value
(test-true  (custom-set-value! 'test-use-candidate-window? #f))
(test-false (custom-value 'test-use-candidate-window?))
;; boolean regards all non-#f value as true
(test-true (custom-set-value! 'test-use-candidate-window? 10))
(test-true (custom-value 'test-use-candidate-window?))

;;; integer
;; default value
(test-equal 10
            (custom-value 'test-nr-candidate-max))
;; valid value
(test-true  (custom-set-value! 'test-nr-candidate-max 5))
(test-equal 5
            (custom-value 'test-nr-candidate-max))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-nr-candidate-max 25))
(test-equal 25
            (custom-value 'test-nr-candidate-max))

;;; string
;; default value
(test-equal "a string"
            (custom-value 'test-string))
;; valid value
(test-true  (custom-set-value! 'test-string "a altered string"))
(test-equal "a altered string"
            (custom-value 'test-string))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-string #f))
(test-false (custom-value 'test-string))

;;; pathname
;; default value
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-value 'test-dic-file-name))
;; valid value
(test-true  (custom-set-value! 'test-dic-file-name
                               "/usr/local/share/skk/SKK-JISYO.ML"))
(test-equal "/usr/local/share/skk/SKK-JISYO.ML"
            (custom-value 'test-dic-file-name))
;; invalid value is also accepted
(test-true  (custom-set-value! 'test-dic-file-name #f))
(test-false (custom-value 'test-dic-file-name))
(test-end)

(test-report-result)
