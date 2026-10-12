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
;; custom-broadcast-customs

(require-extension (unittest) (srfi 95))

;; im-custom.scm requires stub-im-list defined in loader.scm but
;; loader.scm may not be available because it's generated at build time.
(define stub-im-list '())
(require "custom.scm")

(define sort-symbol
  (lambda (symbols)
    (map string->symbol
         (sort (map symbol->string
                    symbols)
               string<?))))

(define test-undefine!
  (lambda (syms)
    (for-each (lambda (sym)
                (eval (list 'undefine sym) (interaction-environment)))
              syms)))

;; Each test case expects the state just after loading custom.scm.
;; define-custom doesn't change the value of an existing variable. So
;; the fixture variables are also removed.
(define test-custom-copy-alist
  (lambda (alist)
    (map (lambda (x)
           (if (pair? x)
               (cons (car x) (cdr x))
               x))
         alist)))

(define test-custom-initial-state
  (map (lambda (sym)
         (cons sym (test-custom-copy-alist (symbol-value sym))))
       '(custom-rec-alist
         custom-group-rec-alist
         custom-subgroup-alist
         custom-activity-hooks
         custom-get-hooks
         custom-set-hooks
         custom-literalize-hooks
         custom-update-hooks
         custom-group-update-hooks
         custom-group-list-update-hooks)))

(define test-custom-fixture-symbols
  '(test-cancel-key test-cancel-key?
                    test-foo-key test-foo-key?
                    test-bar-key test-bar-key?
                    test-baz-key test-baz-key?
                    test-null-key test-null-key?
                    test-style test-available-ims test-null-ims
                    test-use-candidate-window? test-use-with-vi?
                    test-nr-candidate-max test-string test-dic-file-name
                    test-modelist test-case-table
                    test-custom test-custom1 test-custom2 test-custom3 test-custom4
                    test-custom5
                    test-bool test-bool2 test-bool3 test-bool4 test-bool5
                    skk-dic-file-name eb-dic-path))

(define test-custom-reset!
  (lambda ()
    (for-each (lambda (pair)
                (set-symbol-value! (car pair)
                                   (test-custom-copy-alist (cdr pair))))
              test-custom-initial-state)
    (test-undefine! test-custom-fixture-symbols)))

(define setup-validators
  (lambda ()
    (test-custom-reset!)
    (define-custom 'test-cancel-key '("<Control>g" "escape")
      '(global)
      '(key)
      "test cancel key"
      "long description will be here")
    (define-custom 'test-foo-key '("a" test-cancel-key)
      '(global)
      '(key)
      "test foo key"
      "long description will be here")
    (define-custom 'test-bar-key '("b")
      '(global)
      '(key)
      "test bar key"
      "long description will be here")
    (define-custom 'test-baz-key '(test-foo-key "c" test-bar-key "d")
      '(global)
      '(key)
      "test bar key"
      "long description will be here")))

(test-begin "anything?")
(setup-validators)
(test-true (anything? #f))
(test-true (anything? "foo"))
(test-true (anything? 'foo))
(test-true (anything? -1))
(test-true (anything? 0))
(test-true (anything? 1))
(test-true (anything? 10))
(test-true (anything? ()))
(test-true (anything? '(1 "2" 'three)))
(test-end)

(test-begin "custom-boolean?")
(setup-validators)
(test-true (custom-boolean? #f))
(test-true (custom-boolean? "foo"))
(test-true (custom-boolean? 'foo))
(test-true (custom-boolean? -1))
(test-true (custom-boolean? 0))
(test-true (custom-boolean? 1))
(test-true (custom-boolean? 10))
(test-true (custom-boolean? ()))
(test-true (custom-boolean? '(1 "2" 'three)))
(test-end)

(test-begin "custom-integer?")
(setup-validators)
(test-false (custom-integer? #f 2 10))
(test-false (custom-integer? "foo" 2 10))
(test-false (custom-integer? 'foo 2 10))
(test-false (custom-integer? -1 2 10))
(test-false (custom-integer? 0 2 10))
(test-false (custom-integer? 1 2 10))
(test-true  (custom-integer? 2 2 10))
(test-true  (custom-integer? 3 2 10))
(test-true  (custom-integer? 9 2 10))
(test-true  (custom-integer? 10 2 10))
(test-false (custom-integer? 11 2 10))
(test-false (custom-integer? () 2 10))
(test-false (custom-integer? '(1 "2" 'three) 2 10))
(test-end)

(test-begin "custom-string?")
(setup-validators)
(test-false (custom-string? #f ".*"))
(test-true  (custom-string? "" ".*"))
(test-true  (custom-string? "foo" ".*"))
(test-false (custom-string? 'foo ".*"))
(test-false (custom-string? -1 ".*"))
(test-false (custom-string? 0 ".*"))
(test-false (custom-string? 1 ".*"))
(test-false (custom-string? 10 ".*"))
(test-false (custom-string? () ".*"))
(test-false (custom-string? '(1 "2" 'three) ".*"))
(test-end)

(test-begin "custom-pathname?")
(setup-validators)
(test-error (custom-pathname?))
(test-error (custom-pathname? #f))
(test-error (custom-pathname? 'foo))
(test-error (custom-pathname? -1))
(test-error (custom-pathname? 0))
(test-error (custom-pathname? 1))
(test-error (custom-pathname? 10))
(test-error (custom-pathname? ()))
(test-error (custom-pathname? '(1 "2" 'three)))
(test-error (custom-pathname? "/usr/share/uim/foo.scm"))
(test-error (custom-pathname? "~/.uim"))
(test-error (custom-pathname? "share/uim/bar.scm"))
(test-error (custom-pathname? "baz.scm"))
(test-false (custom-pathname? #f 'regular-file))
(test-false (custom-pathname? 'foo 'regular-file))
(test-false (custom-pathname? -1 'regular-file))
(test-false (custom-pathname? 0 'regular-file))
(test-false (custom-pathname? 1 'regular-file))
(test-false (custom-pathname? 10 'regular-file))
(test-false (custom-pathname? () 'regular-file))
(test-false (custom-pathname? '(1 "2" 'three) 'regular-file))
;; regular file
(test-true  (custom-pathname? "/usr/share/uim/foo.scm"
                              'regular-file))
(test-true  (custom-pathname? "~/.uim" 'regular-file))
(test-true  (custom-pathname? "share/uim/bar.scm"
                              'regular-file))
(test-true  (custom-pathname? "baz.scm" 'regular-file))
;; directory
(test-true  (custom-pathname? "/usr/share/uim/" 'directory))
(test-true  (custom-pathname? "~/" 'directory))
(test-true  (custom-pathname? "/" 'directory))
;; current implementation does not validate the string form
(test-true  (custom-pathname? "/usr/share/uim/foo.scm"
                              'directory))
(test-true  (custom-pathname? "~/.uim" 'directory))
(test-true  (custom-pathname? "share/uim/bar.scm"
                              'directory))
(test-true  (custom-pathname? "baz.scm" 'directory))
(test-end)

(test-begin "custom-valid-choice?")
(setup-validators)
(test-false (custom-valid-choice?
             #f
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             "foo"
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             -1
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             0
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             1
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             10
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             ()
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             '(1 "2" 'three)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-true  (custom-valid-choice?
             'uim-color-uim
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-valid-choice?
             'uim-color-nonexistent
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-end)

(test-begin "custom-ordered-list?")
(setup-validators)
(test-false (custom-ordered-list?
             #f
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             "foo"
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             -1
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             0
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             1
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             10
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-true  (custom-ordered-list?
             ()
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             '(1 "2" 'three)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             'uim-color-uim
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))

(test-true  (custom-ordered-list?
             '(uim-color-uim)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-true  (custom-ordered-list?
             '(uim-color-uim uim-color-atok)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-true  (custom-ordered-list?
             '(uim-color-atok uim-color-uim)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-true  (custom-ordered-list?
             '(uim-color-atok uim-color-uim uim-color-user)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")
             '(uim-color-user "user defined" "user defined")))
(test-true  (custom-ordered-list?
             '(uim-color-atok uim-color-user)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")
             '(uim-color-user "user defined" "user defined")))
(test-false (custom-ordered-list?
             '(uim-color-nonexistent)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             '(uim-color-uim uim-color-nonexistent)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-false (custom-ordered-list?
             '(uim-color-uim uim-color-atok uim-color-nonexistent)
             '(uim-color-uim "uim" "uim native")
             '(uim-color-atok "ATOK like" "Similar to ATOK")))
(test-end)

(test-begin "custom-key?")
(setup-validators)
;; no keys
(test-true  (custom-key? ()))
;; single key
(test-true  (custom-key? '("a")))
;; single symbolic key
(test-true  (custom-key? '("return")))
;; single key with single modifier
(test-true  (custom-key? '("<Control>a")))
;; single key with multiple modifier
(test-true  (custom-key? '("<Control><Alt>a")))
;; multiple keys
(test-true  (custom-key?
             '("a" "return" "<Control>a" "<Control><Alt>a")))
;; single key reference
(test-true  (custom-key? '(test-cancel-key)))
;; multiple key reference
(test-true  (custom-key?
             '(test-cancel-key test-foo-key test-bar-key)))
;; key and key reference
(test-true  (custom-key?
             '(test-cancel-key "a" test-bar-key "<Alt>a")))

;; custom-key must be a list
(test-false (custom-key? "a"))
(test-false (custom-key? 'test-cancel-key))
(test-false (custom-key? test-cancel-key?))
(test-false (custom-key? 32))
(test-false (custom-key? #f))
;; null key is invalid
(test-false (custom-key? '("")))
;; custom-key cannot contain key with translator
(test-true  (custom-key? '("<IgnoreShift>0")))
(test-true  (custom-key?
             '("<IgnoreShift><IgnoreCase>return")))
;; custom-key cannot contain raw closure
(test-false (custom-key? (list test-cancel-key)))
;; key reference must exist
(test-false (custom-key? '(test-nonexistent-key)))
;; symbolic key must be valid
(test-false (custom-key? '("nonexistent")))
(test-false (custom-key? '("<Control>nonexistent")))
;; symbolic key must be expressed as string
(test-false (custom-key? '(return)))
;; custom-key cannot contain invalid key elements
(test-false (custom-key?
             '(test-nonexistent-key "<Alt>a")))
(test-end)

(test-begin "custom-table?")
(setup-validators)
(test-true  (custom-table?
             '()))
(test-true  (custom-table?
             '((""))))
(test-true  (custom-table?
             '(("Alice"))))
(test-true  (custom-table?
             '(("Alice" "Bob"))))
(test-true  (custom-table?
             '(("Alice" "Bob") ("Carol" "Dave"))))
(test-true  (custom-table?
             '(("Alice" "Bob") ("Carol" "Dave" "Eve"))))

(test-false (custom-table?
             #t))
(test-false (custom-table?
             "Alice"))
(test-false (custom-table?
             'Alice))
(test-false (custom-table?
             1))

(test-false (custom-table?
             '(("Alice" "Bob") #t)))
(test-false (custom-table?
             '(("Alice" "Bob") "Carol")))
(test-false (custom-table?
             '(("Alice" "Bob") 'Carol)))
(test-false (custom-table?
             '(("Alice" "Bob") 1)))

(test-false (custom-table?
             '(("Alice" "Bob") ("Carol" "Dave" #t))))
(test-false (custom-table?
             '(("Alice" "Bob") ("Carol" "Dave" 'Eve))))
(test-false (custom-table?
             '(("Alice" "Bob") ("Carol" "Dave" 1))))
(test-end)

(test-begin "custom-expand-key-references")
(setup-validators)
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
;; no expansion
(test-equal '("<Control>g" "escape")
            (custom-expand-key-references
             (custom-value 'test-cancel-key)))
;; single expansion
(test-equal '("a" "<Control>g" "escape")
            (custom-expand-key-references
             (custom-value 'test-foo-key)))
(test-equal '(test-foo-key "c" test-bar-key "d")
            (custom-value 'test-baz-key))
;; recursive expansion
(test-equal '("a" "<Control>g" "escape" "c" "b" "d")
            (custom-expand-key-references
             (custom-value 'test-baz-key)))
(test-end)

(define setup-pathname
  (lambda ()
    (test-custom-reset!)
    (define-custom 'skk-dic-file-name (string-append (sys-datadir)
                                                     "/skk/SKK-JISYO.L")
      '(global)
      '(pathname regular-file)
      (_ "Dictionary file")
      (_ "long description will be here."))
    (define-custom 'eb-dic-path
      (string-append (getenv "HOME") "/dict")
      '(global)
      '(pathname directory)
      (_ "The directory which contains EB dictionary file")
      (_ "long description will be here."))))

(test-begin "custom-pathname-type")
(setup-pathname)
(test-equal 'regular-file
            (custom-pathname-type 'skk-dic-file-name))
(test-equal 'directory
            (custom-pathname-type 'eb-dic-path))
(test-end)

(test-begin "custom-choice-rec-new")
(test-custom-reset!)
(test-equal (list #f "" "")
            (custom-choice-rec-new))
(test-end)

(test-begin "custom-choice-label")
(test-custom-reset!)
(test-equal "uim"
            (custom-choice-label 'uim-color 'uim-color-uim))
(test-equal "ATOK like"
            (custom-choice-label 'uim-color 'uim-color-atok))
(test-equal "uim-color-nonexistent"
            (custom-choice-label 'uim-color
                                 'uim-color-nonexistent))
(test-error (custom-choice-label 'uim-nonexistent
                                 'uim-nonexistent))
(test-end)

(test-begin "custom-choice-desc")
(test-custom-reset!)
(test-equal "uim native"
            (custom-choice-desc 'uim-color 'uim-color-uim))
(test-equal "Similar to ATOK"
            (custom-choice-desc 'uim-color 'uim-color-atok))
(test-equal "uim-color-nonexistent"
            (custom-choice-desc 'uim-color
                                'uim-color-nonexistent))
(test-error (custom-choice-desc 'uim-nonexistent
                                'uim-nonexistent))
(test-end)

(define test-group-recs-length 0)
(define setup-group
  (lambda ()
    (test-custom-reset!)
    ;; to reset previously defined groups
    (set! custom-rec-alist '())
    (set! custom-group-rec-alist '())
    (set! custom-subgroup-alist '())

    ;; resurrect the predefined subgroups defined in custom.scm
    (define-custom-group 'main
      (_ "-")
      (_ "Main settings of this group"))

    (define-custom-group 'hidden
      (_ "Hidden settings")
      (_ "Hidden settings of this group. This group is invisible from uim_custom clients. Exists for internal variable management."))

    (set! test-group-recs-length 0)
    (define-custom-group 'global
      (_ "Global settings")
      (_ "long description will be here."))

    (define-custom-group 'advanced
      (_ "Advanced settings")
      (_ "long description will be here."))

    (define-custom 'uim-color 'uim-color-uim
      '(global)
      '(choice
        (uim-color-uim "uim" "uim native")
        (uim-color-atok "ATOK like" "Similar to ATOK"))
      (_ "Preedit color")
      (_ "long description will be here."))
    (define-custom-group 'im-switching
      (_ "Input method switching")
      (_ "long description will be here."))

    (define-custom 'enable-im-switch #f
      '(global im-switching advanced)
      '(boolean)
      (_ "Enable IM switching by hotkey")
      (_ "long description will be here."))

    (define-custom 'candidate-window-position "caret"
      '(global)
      '(string "^(caret|left|right)$")
      (_ "Candidate window position")
      (_ "long description will be here."))
    (define-custom-group 'anthy
      "Anthy"
      "Japanese Kana Kanji Conversion Engine, Anthy")

    (define-custom 'anthy-use-candidate-window? #t
      '(anthy)
      '(boolean)
      (_ "Use candidate window")
      (_ "long description will be here."))

    (define-custom 'anthy-candidate-op-count 1
      '(anthy)
      '(integer 0 99)
      (_ "Conversion key press count to show candidate window")
      (_ "long description will be here."))

    (define-custom 'anthy-nr-candidate-max 10
      '(anthy)
      '(integer 1 20)
      (_ "Number of candidates in candidate window at a time")
      (_ "long description will be here."))

    (define-custom 'anthy-select-candidate-by-numeral-key? #f
      '(anthy)
      '(boolean)
      (_ "Select candidate by numeral keys")
      (_ "long description will be here."))

    (define-custom 'anthy-show-segment-separator? #f
      '(anthy advanced)
      '(boolean)
      (_ "Show segment separator")
      (_ "long description will be here."))

    (define-custom 'anthy-segment-separator "|"
      '(anthy advanced)
      '(string ".*")
      (_ "Segment separator")
      (_ "long description will be here."))
    (define-custom-group 'canna
      "Canna"
      "Canna")

    (define-custom 'canna-use-candidate-window? #t
      '(canna)
      '(boolean)
      (_ "Use candidate window")
      (_ "long description will be here."))

    (define-custom 'canna-candidate-op-count 1
      '(canna)
      '(integer 0 99)
      (_ "Conversion key press count to show candidate window")
      (_ "long description will be here."))

    (define-custom 'canna-nr-candidate-max 10
      '(canna)
      '(integer 1 20)
      (_ "Number of candidates in candidate window at a time")
      (_ "long description will be here."))

    (define-custom 'canna-show-segment-separator? #f
      '(canna advanced)
      '(boolean)
      (_ "Show segment separator")
      (_ "long description will be here."))

    (define-custom 'canna-segment-separator "|"
      '(canna advanced)
      '(string ".*")
      (_ "Segment separator")
      (_ "long description will be here."))
    (define-custom-group 'skk
      "SKK"
      "Uim's SKK like input method")

    (define-custom 'skk-dic-file-name (string-append (sys-datadir)
                                                     "/skk/SKK-JISYO.L")
      '(skk)
      '(pathname regular-file)
      (_ "Dictionary file")
      (_ "long description will be here."))

    (define-custom 'skk-personal-dic-filename
      (string-append (getenv "HOME") "/.skk-jisyo")
      '(skk)
      '(pathname regular-file)
      (_ "Personal dictionary file")
      (_ "long description will be here."))

    (define-custom 'skk-uim-personal-dic-filename
      (string-append (getenv "HOME") "/.skk-uim-jisyo")
      '(skk)
      '(pathname regular-file)
      (_ "Personal dictionary file (dedicated to uim)")
      (_ "long description will be here."))

    (define-custom 'skk-use-candidate-window? #t
      '(skk)
      '(boolean)
      (_ "Use candidate window")
      (_ "long description will be here."))

    (define-custom 'skk-candidate-op-count 0
      '(skk)
      '(integer 0 99)
      (_ "Conversion key press count to show candidate window")
      (_ "long description will be here."))

    (define-custom 'skk-nr-candidate-max 10
      '(skk)
      '(integer 1 20)
      (_ "Number of candidates in candidate window at a time")
      (_ "long description will be here."))

    (define-custom 'skk-use-recursive-learning? #t
      '(skk advanced)
      '(boolean)
      (_ "Use recursive learning")
      (_ "long description will be here."))

    (define-custom 'skk-egg-like-newline? #f
      '(skk advanced)
      '(boolean)
      (_ "Use Enter key as just committing (egg-like operation)")
      (_ "long description will be here."))

    (define-custom 'skk-commit-newline-explicitly? #f
      '(skk advanced)
      '(boolean)
      (_ "Commit newline as ASCII string instead of native key-event")
      (_ "long description will be here."))

    (define-custom 'skk-style 'skk-style-ddskk-like
      '(skk advanced)
      '(choice
        (skk-style-ddskk-like "ddskk" "Similar to ddskk")
        (skk-style-uim "uim" "uim native"))
      (_ "Visual style")
      (_ "long description will be here."))
    (define-custom-group 'prime
      "PRIME"
      "Japanese predictable input method")
    (define-custom 'prime-nr-candidate-max 10
      '(prime)
      '(integer 1 20)
      (_ "Number of candidates in candidate window at a time")
      (_ "long description will be here."))

    (define-custom 'prime-always-show-window? #t
      '(prime)
      '(boolean)
      (_ "Always showing candidate window")
      (_ "long description will be here."))

    (define-custom 'prime-auto-register-mode? #t
      '(prime)
      '(boolean)
      (_ "Enable auto register mode")
      (_ "long description will be here."))

    (define-custom 'prime-pseudo-mode-cursor? #f
      '(prime)
      '(boolean)
      (_ "Enable pseudo mode cursor")
      (_ "long description will be here."))

    (define-custom 'prime-char-annotation? #t
      '(prime)
      '(boolean)
      (_ "Show candidate annotations")
      (_ "long description will be here."))

    (define-custom 'prime-mask-pending-preedit? #f
      '(prime)
      '(boolean)
      (_ "Mask preedit strings (For tcode users)")
      (_ "long description will be here."))
    (define-custom-group 'other-ims
      (_ "Other input methods")
      (_ "long description will be here."))

    (define-custom 'generic-use-candidate-window? #t
      '(other-ims)
      '(boolean)
      (_ "Use candidate window")
      (_ "long description will be here."))

    (define-custom 'generic-candidate-op-count 1
      '(other-ims)
      '(integer 0 99)
      (_ "Conversion key press count to show candidate window")
      (_ "long description will be here."))

    (define-custom 'generic-nr-candidate-max 10
      '(other-ims)
      '(integer 1 20)
      (_ "Number of candidates in candidate window at a time")
      (_ "long description will be here."))
    (define-custom-group 'spellcheck
      "Spellcheck"
      "Spellcheck")

    (define-custom 'spellcheck-use-candidate-window? #t
      '(spellcheck)
      '(boolean)
      (_ "Use candidate window")
      (_ "long description will be here."))

    (define-custom 'spellcheck-candidate-op-count 1
      '(spellcheck)
      '(integer 0 99)
      (_ "Conversion key press count to show candidate window")
      (_ "long description will be here."))

    (define-custom 'spellcheck-preedit-immediate-commit? #f
      '(spellcheck)
      '(boolean)
      (_ "spellcheck-preedit-immediate-commit?")
      (_ "long description will be here."))

    (define-custom 'spellcheck-always-show-window? #t
      '(spellcheck)
      '(boolean)
      (_ "Always showing candidate window")
      (_ "long description will be here."))))

(test-begin "custom-group-rec-new")
(setup-group)
(test-equal (list #f "" "")
            (custom-group-rec-new))
(test-end)

(test-begin "define-custom-group, custom-group-rec")
(setup-group)
(set! test-group-recs-length
      (length custom-group-rec-alist))

(test-false (custom-group-rec 'test-group))
(define-custom-group
  'test-group
  "test group"
  "long description of test group")
(test-true  (custom-group-rec 'test-group))
(test-equal (list 'test-group
                  "test group"
                  "long description of test group")
            (custom-group-rec 'test-group))
(test-equal (+ test-group-recs-length 1)
            (length custom-group-rec-alist))

(test-false (custom-group-rec 'test-group2))
(define-custom-group
  'test-group2
  "test group 2"
  "long description of test group 2")
(test-true  (custom-group-rec 'test-group2))
(test-equal (list 'test-group2
                  "test group 2"
                  "long description of test group 2")
            (custom-group-rec 'test-group2))
(test-equal (+ test-group-recs-length 2)
            (length custom-group-rec-alist))

(test-false (custom-group-rec 'test-group3))
(define-custom-group
  'test-group3
  "test group 3"
  "long description of test group 3")
(test-true  (custom-group-rec 'test-group3))
(test-equal (list 'test-group3
                  "test group 3"
                  "long description of test group 3")
            (custom-group-rec 'test-group3))
(test-equal (+ test-group-recs-length 3)
            (length custom-group-rec-alist))

;; verify again after other groups added
(test-equal (list 'test-group
                  "test group"
                  "long description of test group")
            (custom-group-rec 'test-group))
(test-equal (list 'test-group2
                  "test group 2"
                  "long description of test group 2")
            (custom-group-rec 'test-group2))
(test-equal (list 'test-group3
                  "test group 3"
                  "long description of test group 3")
            (custom-group-rec 'test-group3))
(test-end)

(test-begin "custom-list-groups")
(setup-group)
(test-equal '(advanced anthy canna global hidden im-switching main other-ims prime skk spellcheck)
            (sort-symbol (custom-list-groups)))
(test-end)

(test-begin "custom-list-primary-groups")
(setup-group)
;; defined order have to be kept
(test-equal '(global anthy canna skk prime other-ims spellcheck)
            (custom-list-primary-groups))
(test-end)

(test-begin "custom-collect-by-group")
(setup-group)
;; defined order have to be kept
(test-equal '(uim-color enable-im-switch candidate-window-position anthy-use-candidate-window? anthy-candidate-op-count anthy-nr-candidate-max anthy-select-candidate-by-numeral-key? anthy-show-segment-separator? anthy-segment-separator canna-use-candidate-window? canna-candidate-op-count canna-nr-candidate-max canna-show-segment-separator? canna-segment-separator skk-dic-file-name skk-personal-dic-filename skk-uim-personal-dic-filename skk-use-candidate-window? skk-candidate-op-count skk-nr-candidate-max skk-use-recursive-learning? skk-egg-like-newline? skk-commit-newline-explicitly? skk-style prime-nr-candidate-max prime-always-show-window? prime-auto-register-mode? prime-pseudo-mode-cursor? prime-char-annotation? prime-mask-pending-preedit? generic-use-candidate-window? generic-candidate-op-count generic-nr-candidate-max spellcheck-use-candidate-window? spellcheck-candidate-op-count spellcheck-preedit-immediate-commit? spellcheck-always-show-window?)
            (custom-collect-by-group #f))  ;; any group
(test-equal '(uim-color enable-im-switch candidate-window-position)
            (custom-collect-by-group 'global))
(test-equal '(anthy-use-candidate-window? anthy-candidate-op-count anthy-nr-candidate-max anthy-select-candidate-by-numeral-key? anthy-show-segment-separator? anthy-segment-separator)
            (custom-collect-by-group 'anthy))
(test-equal '(canna-use-candidate-window? canna-candidate-op-count canna-nr-candidate-max canna-show-segment-separator? canna-segment-separator)
            (custom-collect-by-group 'canna))
(test-equal '(skk-dic-file-name skk-personal-dic-filename skk-uim-personal-dic-filename skk-use-candidate-window? skk-candidate-op-count skk-nr-candidate-max skk-use-recursive-learning? skk-egg-like-newline? skk-commit-newline-explicitly? skk-style)
            (custom-collect-by-group 'skk))
(test-equal '(prime-nr-candidate-max prime-always-show-window? prime-auto-register-mode? prime-pseudo-mode-cursor? prime-char-annotation? prime-mask-pending-preedit?)
            (custom-collect-by-group 'prime))
(test-equal '(generic-use-candidate-window? generic-candidate-op-count generic-nr-candidate-max)
            (custom-collect-by-group 'other-ims))
(test-equal '(spellcheck-use-candidate-window? spellcheck-candidate-op-count spellcheck-preedit-immediate-commit? spellcheck-always-show-window?)
            (custom-collect-by-group 'spellcheck))
(test-end)

(define setup-group-methods
  (lambda ()
    (test-custom-reset!)
    (define-custom-group
      'test-group
      "test group"
      "long description of test group")
    (define-custom-group
      'test-group2
      "test group 2"
      "long description of test group 2")
    (define-custom-group
      'test-group3
      "test group 3"
      "long description of test group 3")
    (define-custom-group
      'test-group4
      "test group 4"
      "long description of test group 4")

    (define-custom 'test-custom #t
      '(test-group)
      '(boolean)
      "test custom"
      "long description will be here.")
    (define-custom 'test-custom2 #t
      '(test-group test-group2)
      '(boolean)
      "test custom2"
      "long description will be here.")
    (define-custom 'test-custom3 #t
      '(test-group test-group3 test-group2)
      '(boolean)
      "test custom3"
      "long description will be here.")
    (define-custom 'test-custom4 #t
      '(test-group4)
      '(boolean)
      "test custom4"
      "long description will be here.")))

(test-begin "custom-group-label")
(setup-group-methods)
(test-equal "test group"
            (custom-group-label 'test-group))
(test-equal "test group 2"
            (custom-group-label 'test-group2))
(test-equal "test group 3"
            (custom-group-label 'test-group3))
(test-end)

(test-begin "custom-group-desc")
(setup-group-methods)
(test-equal "long description of test group"
            (custom-group-desc 'test-group))
(test-equal "long description of test group 2"
            (custom-group-desc 'test-group2))
(test-equal "long description of test group 3"
            (custom-group-desc 'test-group3))
(test-end)

(test-begin "custom-group-subgroups")
(setup-group-methods)
(test-true  (string? (custom-group-label 'main)))
(test-true  (string? (custom-group-desc 'main)))
(test-equal '(main test-group2 test-group3)
            (custom-group-subgroups 'test-group))
(test-equal '()
            (custom-group-subgroups 'test-group2))
(test-equal '()
            (custom-group-subgroups 'test-group3))
(test-equal '(main)
            (custom-group-subgroups 'test-group4))
(test-end)

(define test-hook '())
(define test-group1-trace '())
(define test-custom1-trace '())
(define test-custom2-trace '())
(define test-custom3-trace '())
(define setup-hooks
  (lambda ()
    (test-custom-reset!)
    (set! test-hook '())
    (set! test-group1-trace '())
    (set! test-custom1-trace '())
    (set! test-custom2-trace '())
    (set! test-custom3-trace '())
    (define-custom-group 'test-group1
      (_ "Test group 1")
      (_ "long description will be here."))
    (define-custom 'test-custom1 'test-custom1-ddskk
      '(test-group1)
      '(choice
        (test-custom1-uim "uim" "uim native")
        (test-custom1-ddskk "ddskk like" "Similar to ddskk")
        (test-custom1-canna "canna like" "Similar to canna"))
      "Test custom1"
      "long description will be here.")
    (define-custom 'test-custom2 'test-custom2-ddskk
      '(global)
      '(choice
        (test-custom2-uim "uim" "uim native")
        (test-custom2-ddskk "ddskk like" "Similar to ddskk")
        (test-custom2-canna "canna like" "Similar to canna"))
      "Test custom2"
      "long description will be here.")
    (define-custom 'test-custom3 'test-custom3-ddskk
      '(global)
      '(choice
        (test-custom3-uim "uim" "uim native")
        (test-custom3-ddskk "ddskk like" "Similar to ddskk")
        (test-custom3-canna "canna like" "Similar to canna"))
      "Test custom3"
      "long description will be here.")))

(test-begin "custom-hook-procs (null)")
(setup-hooks)
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom3 test-hook))
(test-end)

(test-begin "custom-add-hook, custom-hook-procs, custom-call-hook-procs")
(setup-hooks)
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))

(custom-add-hook 'test-custom1
                 'test-hook
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'first
                               test-custom1-trace))))
(test-equal 1
            (length (custom-hook-procs 'test-custom1 test-hook)))
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(custom-call-hook-procs 'test-custom1 test-hook)
(test-equal '(first)
            test-custom1-trace)
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom3 test-hook))

(custom-add-hook 'test-custom1
                 'test-hook
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'second
                               test-custom1-trace))))
(test-equal 2
            (length (custom-hook-procs 'test-custom1 test-hook)))
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-true  (custom-call-hook-procs 'test-custom1 test-hook))
(test-equal '(first second first)
            test-custom1-trace)
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom3 test-hook))

(custom-add-hook 'test-custom1
                 'test-hook
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'third
                               test-custom1-trace))))
(test-equal 3
            (length (custom-hook-procs 'test-custom1 test-hook)))
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(custom-call-hook-procs 'test-custom1 test-hook)
(test-equal '(first second third first second first)
            test-custom1-trace)
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom3 test-hook))

;; test isolation of hooks
(test-equal '()
            test-custom3-trace)
(custom-add-hook 'test-custom3
                 'test-hook
                 (lambda ()
                   (set! test-custom3-trace
                         (cons 'fourth
                               test-custom3-trace))))

(test-equal 1
            (length (custom-hook-procs 'test-custom3 test-hook)))
(test-equal 3
            (length (custom-hook-procs 'test-custom1 test-hook)))
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal 'test-custom3-ddskk
            (custom-value 'test-custom3))
(custom-call-hook-procs 'test-custom3 test-hook)
(test-equal '(fourth)
            test-custom3-trace)
(test-equal '(first second third first second first)
            test-custom1-trace)
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-end)

(test-begin "custom-remove-hook")
(setup-hooks)
;; null
(test-equal '()
            test-hook)
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
(test-false (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; null as 'any'
(test-equal '()
            test-hook)
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
(test-false (custom-remove-hook #f 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; 1 proc
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 1))
(test-equal '(1)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; 2 procs
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 1))
(custom-add-hook 'test-custom1 'test-hook (lambda () 2))
(test-equal '(2 1)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; 3 procs
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 1))
(custom-add-hook 'test-custom1 'test-hook (lambda () 2))
(custom-add-hook 'test-custom1 'test-hook (lambda () 3))
(test-equal '(3 2 1)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; 3 procs as 'any'
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 1))
(custom-add-hook 'test-custom1 'test-hook (lambda () 2))
(custom-add-hook 'test-custom1 'test-hook (lambda () 3))
(test-equal '(3 2 1)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook #f 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
;; 3 procs * 3 customs (1)
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '(13 12 11)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
(test-equal '(33 32 31 23 22 21)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
;; 3 procs * 3 customs (2)
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '(23 22 21)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom2 test-hook)))
(test-true  (custom-remove-hook 'test-custom2 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom2 test-hook))
(test-equal '(33 32 31 13 12 11)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
;; 3 procs * 3 customs (3)
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '(33 32 31)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom3 test-hook)))
(test-true  (custom-remove-hook 'test-custom3 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom3 test-hook))
(test-equal '(23 22 21 13 12 11)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
;; 3 procs * 3 customs (mixed)
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '(13 12 11)
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom1 test-hook)))
(test-true  (custom-remove-hook 'test-custom1 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom1 test-hook))
(test-equal '(33 23 32 22 31 21)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
;; 3 procs * 3 customs (mixed) as 'any'
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '(33 23 13 32 22 12 31 21 11)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
(test-true  (custom-remove-hook #f 'test-hook))
(test-equal '()
            test-hook)
;; 3 procs * 3 customs (mixed) as non-existent custom
(set! test-hook ())
(test-equal '()
            test-hook)
(custom-add-hook 'test-custom1 'test-hook (lambda () 11))
(custom-add-hook 'test-custom2 'test-hook (lambda () 21))
(custom-add-hook 'test-custom3 'test-hook (lambda () 31))
(custom-add-hook 'test-custom1 'test-hook (lambda () 12))
(custom-add-hook 'test-custom2 'test-hook (lambda () 22))
(custom-add-hook 'test-custom3 'test-hook (lambda () 32))
(custom-add-hook 'test-custom1 'test-hook (lambda () 13))
(custom-add-hook 'test-custom2 'test-hook (lambda () 23))
(custom-add-hook 'test-custom3 'test-hook (lambda () 33))
(test-equal '()
            (map (lambda (f) (f))
                 (custom-hook-procs 'test-custom4 test-hook)))
(test-false (custom-remove-hook 'test-custom4 'test-hook))
(test-equal '()
            (custom-hook-procs 'test-custom4 test-hook))
(test-equal '(33 23 13 32 22 12 31 21 11)
            (map (lambda (pair) ((cdr pair)))
                 test-hook))
(test-end)

(test-begin "custom-active?")
(setup-hooks)
(custom-add-hook 'test-custom1
                 'custom-activity-hooks
                 (lambda ()
                   (symbol-bound? 'car)))
(test-true  (custom-active? 'test-custom1))

(custom-add-hook 'test-custom1
                 'custom-activity-hooks
                 (lambda ()
                   (symbol-bound? 'cdr)))
(test-true  (custom-active? 'test-custom1))

(begin
  (custom-add-hook 'test-custom1
                   'custom-activity-hooks
                   (lambda ()
                     (symbol-bound? 'test-nonexistent))))
(test-false (custom-active? 'test-custom1))

(custom-add-hook 'test-custom1
                 'custom-activity-hooks
                 (lambda ()
                   (symbol-bound? 'cons)))
(test-false (custom-active? 'test-custom1))
(test-end)

(test-begin "custom-update-hooks")
(setup-hooks)
(custom-add-hook 'test-custom1
                 'custom-activity-hooks
                 (lambda ()
                   (eq? test-custom3 'test-custom3-uim)))
(custom-add-hook 'test-custom2
                 'custom-activity-hooks
                 (lambda ()
                   (eq? test-custom3 'test-custom3-uim)))
(custom-add-hook 'test-custom1
                 'custom-update-hooks
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'updated test-custom1-trace))))
(custom-add-hook 'test-custom2
                 'custom-update-hooks
                 (lambda ()
                   (set! test-custom2-trace
                         (cons 'updated test-custom2-trace))))
(test-equal '()
            test-custom1-trace)
(test-equal '()
            test-custom2-trace)
(test-equal 'test-custom3-ddskk
            test-custom3)
(test-false (custom-active? 'test-custom1))
(test-false (custom-active? 'test-custom2))
;; update hook
(test-true  (custom-set-value! 'test-custom3 'test-custom3-uim))
(test-equal '(updated)
            test-custom1-trace)
(test-equal '(updated)
            test-custom2-trace)
(test-true  (custom-active? 'test-custom1))
(test-true  (custom-active? 'test-custom2))
(test-end)

(test-begin "custom-update-hooks (self update)")
(setup-hooks)
(custom-add-hook 'test-custom1
                 'custom-activity-hooks
                 (lambda ()
                   (eq? test-custom1 'test-custom1-uim)))
(custom-add-hook 'test-custom1
                 'custom-update-hooks
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'updated test-custom1-trace))))
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
(test-false (custom-active? 'test-custom1))
;; update hook
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal '(updated)
            test-custom1-trace)
(test-true  (custom-active? 'test-custom1))
(test-end)

(test-begin "custom-register-cb (custom update hook)")
(setup-hooks)
(define test-update-gate (lambda (func ptr custom-sym)
                           (set! test-custom1-trace
                                 (list func ptr custom-sym))))
(custom-register-cb 'custom-update-hooks
                    custom-rec
                    'test-custom1
                    'custom1-ptr
                    test-update-gate 'custom1-func)
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
;; custom update hook
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal '(custom1-func custom1-ptr test-custom1)
            test-custom1-trace)
(test-end)

(test-begin "custom-register-cb (custom update hook, 2 callbaks)")
(setup-hooks)
(define test-update-gate (lambda (func ptr custom-sym)
                           (set! test-custom1-trace
                                 (cons (list func ptr custom-sym)
                                       test-custom1-trace))))
(custom-register-cb 'custom-update-hooks
                    custom-rec
                    'test-custom1
                    'custom1-ptr
                    test-update-gate 'custom1-func)
(custom-register-cb 'custom-update-hooks
                    custom-rec
                    'test-custom1
                    'custom1-ptr2
                    test-update-gate 'custom1-func2)
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
;; update hook
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal '((custom1-func custom1-ptr test-custom1)
              (custom1-func2 custom1-ptr2 test-custom1))
            test-custom1-trace)
(test-end)

(test-begin "custom-register-cb (custom-group update hook)")
(setup-hooks)
(define test-update-gate (lambda (func ptr custom-sym)
                           (set! test-group1-trace
                                 (list func ptr custom-sym))))
(custom-register-cb 'custom-group-update-hooks
                    custom-group-rec
                    'test-group1
                    'group1-ptr
                    test-update-gate 'group1-func)
(test-equal '()
            test-group1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
;; custom update hook is not set
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
;; custom-group update hook
(define-custom 'test-custom4 #f
  '(global)
  '(boolean)
  "Test custom4"
  "long description will be here.")
(test-equal '()
            test-group1-trace)
(define-custom 'test-custom5 #f
  '(test-group1)
  '(boolean)
  "Test custom5"
  "long description will be here.")
(test-equal '(group1-func group1-ptr test-group1)
            test-group1-trace)
(test-end)

(test-begin "custom-register-cb (custom-group update hook, 2 callbaks)")
(setup-hooks)
(define test-update-gate (lambda (func ptr custom-sym)
                           (set! test-group1-trace
                                 (cons (list func ptr custom-sym)
                                       test-group1-trace))))
(custom-register-cb 'custom-group-update-hooks
                    custom-group-rec
                    'test-group1
                    'group1-ptr
                    test-update-gate 'group1-func)
(custom-register-cb 'custom-group-update-hooks
                    custom-group-rec
                    'test-group1
                    'group1-ptr
                    test-update-gate 'group1-func2)
(test-equal '()
            test-group1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
;; custom update hook is not set
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal '()
            test-custom1-trace)
;; custom-group update hook
(define-custom 'test-custom4 #f
  '(global)
  '(boolean)
  "Test custom4"
  "long description will be here.")
(test-equal '()
            test-group1-trace)
(define-custom 'test-custom5 #f
  '(test-group1)
  '(boolean)
  "Test custom5"
  "long description will be here.")
(test-equal '((group1-func group1-ptr test-group1)
              (group1-func2 group1-ptr test-group1))
            test-group1-trace)
(test-end)

(test-begin "custom-register-cb (group-list update hook)")
(setup-hooks)
(define test-update-gate (lambda (func ptr custom-sym)
                           (set! test-group1-trace
                                 (list func ptr custom-sym))))
(custom-register-cb 'custom-group-list-update-hooks
                    (lambda (dummy) #t)
                    'global
                    'group1-ptr
                    test-update-gate 'group1-func)
(test-equal '()
            test-group1-trace)
(test-equal 'test-custom1-ddskk
            test-custom1)
;; custom update hook is not set
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
;; custom-group update hook is not set
(define-custom 'test-custom5 #f
  '(test-group1)
  '(boolean)
  "Test custom5"
  "long description will be here.")
(test-equal '()
            test-group1-trace)
;; new group causes group-list-update-hook invocation
(define-custom-group 'test-group2
  (_ "Test group 2")
  (_ "long description will be here."))
(test-equal '(group1-func group1-ptr global)
            test-group1-trace)
(test-end)

(define setup-get-set-hooks
  (lambda ()
    (test-custom-reset!)
    (set! test-custom1-trace '())
    (set! test-custom3-trace '())
    (define-custom 'test-custom1 'test-custom1-ddskk
      '(global)
      '(choice
        (test-custom1-uim "uim" "uim native")
        (test-custom1-ddskk "ddskk like" "Similar to ddskk")
        (test-custom1-canna "canna like" "Similar to canna"))
      "Test custom1"
      "long description will be here.")))

(test-begin "custom-get-hooks")
(setup-get-set-hooks)
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(custom-add-hook 'test-custom1
                 'custom-get-hooks
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'first
                               test-custom1-trace))))
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal '(first)
            test-custom1-trace)
(test-end)

(test-begin "custom-get-hooks (self update)")
(setup-get-set-hooks)
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(custom-add-hook 'test-custom1
                 'custom-get-hooks
                 (lambda ()
                   (set! test-custom1
                         'test-custom1-uim)
                   (set! test-custom1-trace
                         (cons 'first
                               test-custom1-trace))))
(test-equal '()
            test-custom1-trace)
(test-equal 'test-custom1-uim
            (custom-value 'test-custom1))
(test-equal '(first)
            test-custom1-trace)
(test-end)

(test-begin "custom-set-hooks")
(setup-get-set-hooks)
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal 'test-custom1-uim
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(custom-add-hook 'test-custom1
                 'custom-set-hooks
                 (lambda ()
                   (set! test-custom1-trace
                         (cons 'second
                               test-custom1-trace))))
(test-equal '()
            test-custom1-trace)
(test-true  (custom-set-value! 'test-custom1 'test-custom1-canna))
(test-equal '(second)
            test-custom1-trace)
(test-equal 'test-custom1-canna
            (custom-value 'test-custom1))
(test-equal '(second)
            test-custom1-trace)
(test-end)

(test-begin "custom-set-hooks (self update)")
(setup-get-set-hooks)
(test-equal 'test-custom1-ddskk
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(test-true  (custom-set-value! 'test-custom1 'test-custom1-uim))
(test-equal 'test-custom1-uim
            (custom-value 'test-custom1))
(test-equal '()
            test-custom1-trace)
(custom-add-hook 'test-custom1
                 'custom-set-hooks
                 (lambda ()
                   (set! test-custom1
                         'test-custom1-canna)
                   (set! test-custom1-trace
                         (cons 'second
                               test-custom1-trace))))
(test-equal '()
            test-custom1-trace)
(test-true  (custom-set-value! 'test-custom1 'test-custom1-ddskk))
(test-equal '(second)
            test-custom1-trace)
(test-equal 'test-custom1-canna
            (custom-value 'test-custom1))
(test-equal '(second)
            test-custom1-trace)
(test-end)

(define setup-define-custom
  (lambda ()
    (test-custom-reset!)))

;; tests updated features committed in r559 and r1862 of new repository
(test-begin "define-custom (group)")
(setup-define-custom)
(define-custom 'test-bool #f
  '(global)
  '(boolean)
  "Test bool"
  "long description will be here.")

;; implicit subgroup 'main' is complemented
(test-equal '(global main)
            (custom-groups 'test-bool))

;; at least a primary group required
(test-error (define-custom 'test-bool2 #f
              '()
              '(boolean)
              "Test bool"
              "long description will be here."))

;; referring undefined group(s) causes error
(test-error (define-custom 'test-bool3 #f
              '(global nonexistent)
              '(boolean)
              "Test bool"
              "long description will be here."))
(test-error (define-custom 'test-bool4 #f
              '(nonexistent)
              '(boolean)
              "Test bool"
              "long description will be here."))
(test-error (define-custom 'test-bool5 #f
              '(nonexistent hidden)
              '(boolean)
              "Test bool"
              "long description will be here."))
(test-end)

(test-begin "define-custom (choice)")
(setup-define-custom)
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
(test-equal '(global main)
            (custom-groups 'test-style))
(test-equal '(test-style-uim test-style-ddskk test-style-canna)
            (custom-range 'test-style))
(test-equal "Test style"
            (custom-label 'test-style))

(define-custom-group 'global-keys
  "global-keys"
  "global-keys")
;; overwriting definition
(define-custom 'test-style 'test-style-uim
  '(global-keys)
  '(choice
    (test-style-canna "canna like" "Similar to canna")
    (test-style-uim "uim" "uim native"))
  "Test style (overwritten)"
  "long description will be here.")

(test-true (symbol-bound? 'test-style))
(test-equal 'test-style-ddskk
            test-style)
(test-equal '(global-keys main)
            (custom-groups 'test-style))
(test-equal '(test-style-canna test-style-uim)
            (custom-range 'test-style))
(test-equal "Test style (overwritten)"
            (custom-label 'test-style))
(test-end)

(test-begin "define-custom (choice) #2")
(setup-define-custom)
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
(setup-define-custom)
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
(test-equal '(global main)
            (custom-groups 'test-foo-key))
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
(setup-define-custom)
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
    (test-custom-reset!)

    (define-custom-group 'test
      "test"
      "test")
    (define-custom-group 'ui
      "ui"
      "ui")

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
    (define-custom 'test-null-ims ()
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
    (define-custom 'test-null-key ()
      '(global)
      '(key)
      "test null key"
      "long description will be here.")
    (define-custom 'test-use-candidate-window? #t
      '(test ui)
      '(boolean)
      "Use candidate window"
      "long description will be here.")
    (define-custom 'test-use-with-vi? #f
      '(test ui)
      '(boolean)
      "Use with vi"
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
      '(pathname regular-file)
      "Dictionary file"
      "long description will be here.")
    (define-custom 'test-modelist 'hiragana
      '(test)
      '(choice
        (hiragana "hiragana" "hiragana")
        (katakana "katakana" "katakana")
        (latin "latin" "latin")
        (wide-latin "wide-latin" "wide-latin"))
      "Mode list"
      "long description will be here.")
    (define-custom 'test-case-table '(("abc" "ABC") ("def" "DEF"))
      '(test)
      '(table
        (lower-case "lower-case" "lower-case")
        (upper-case "upper-case" "upper-case"))
      "alphabet table"
      "long description will be here.")))

(test-begin "custom-valid?")
(setup-methods)
;; choice
(test-true  (custom-valid? 'test-style 'test-style-uim))
(test-true  (custom-valid? 'test-style 'test-style-ddskk))
(test-true  (custom-valid? 'test-style 'test-style-canna))
(test-false (custom-valid? 'test-style 'test-style-invalid))
;; ordered-list
(test-false (custom-valid? 'test-available-ims 'anthy))
(test-false (custom-valid? 'test-available-ims 'canna))
(test-false (custom-valid? 'test-available-ims 'skk))
(test-false (custom-valid? 'test-available-ims 'nonexistent))
(test-true  (custom-valid? 'test-available-ims ()))
(test-true  (custom-valid? 'test-available-ims '(anthy)))
(test-true  (custom-valid? 'test-available-ims '(canna)))
(test-true  (custom-valid? 'test-available-ims '(skk)))
(test-true  (custom-valid? 'test-available-ims '(anthy skk)))
(test-true  (custom-valid? 'test-available-ims '(skk anthy)))
(test-true  (custom-valid? 'test-available-ims
                           '(skk anthy canna)))
(test-false (custom-valid? 'test-available-ims
                           '(nonexistent)))
(test-false (custom-valid? 'test-available-ims
                           '(anthy nonexistent)))
;; key
(test-true  (custom-valid? 'test-cancel-key ()))
(test-true  (custom-valid? 'test-cancel-key '("a")))
(test-true  (custom-valid? 'test-cancel-key '("return")))
(test-true  (custom-valid? 'test-cancel-key '("<Control>a")))
(test-true  (custom-valid? 'test-cancel-key '("<Control><Alt>a")))
(test-true  (custom-valid? 'test-cancel-key '("a" "return" "<Control>a" "<Control><Alt>a")))
(test-true  (custom-valid? 'test-cancel-key '(test-cancel-key)))
(test-true  (custom-valid? 'test-cancel-key '(test-cancel-key test-foo-key test-bar-key)))
(test-true  (custom-valid? 'test-cancel-key '(test-cancel-key "a" test-bar-key "<Alt>a")))
(test-false (custom-valid? 'test-cancel-key "a"))
(test-false (custom-valid? 'test-cancel-key 'test-cancel-key))
(test-false (custom-valid? 'test-cancel-key test-cancel-key?))
(test-false (custom-valid? 'test-cancel-key 32))
(test-false (custom-valid? 'test-cancel-key #f))
(test-false (custom-valid? 'test-cancel-key '("")))
(test-true  (custom-valid? 'test-cancel-key '("<IgnoreShift>0")))
(test-true  (custom-valid? 'test-cancel-key '("<IgnoreShift><IgnoreCase>return")))
(test-false (custom-valid? 'test-cancel-key (list test-cancel-key)))
(test-false (custom-valid? 'test-cancel-key '(test-nonexistent-key)))
(test-false (custom-valid? 'test-cancel-key '("nonexistent")))
(test-false (custom-valid? 'test-cancel-key '("<Control>nonexistent")))
(test-false (custom-valid? 'test-cancel-key '(return)))
(test-false (custom-valid? 'test-cancel-key '(test-nonexistent-key "<Alt>a")))

;; integer
(test-false (custom-valid? 'test-nr-candidate-max 0))
(test-true  (custom-valid? 'test-nr-candidate-max 1))
(test-true  (custom-valid? 'test-nr-candidate-max 10))
(test-true  (custom-valid? 'test-nr-candidate-max 20))
(test-false (custom-valid? 'test-nr-candidate-max 21))
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
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-value 'test-case-table))
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
;; invalid value is ignored
(test-false (custom-set-value! 'test-style 'test-style-invalid))
(test-equal 'test-style-uim
            (custom-value 'test-style))

;;; ordered-list
;; default value
(test-equal '(anthy canna skk)
            (custom-value 'test-available-ims))
;; valid value
(test-true  (custom-set-value! 'test-available-ims
                               ()))
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
;; invalid value is ignored
(test-false (custom-set-value! 'test-available-ims
                               '(nonexistent)))
(test-equal '(skk anthy canna)
            (custom-value 'test-available-ims))

;;; key
;; default value
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
;; valid value
(test-true  (custom-set-value! 'test-cancel-key '("a")))
(test-equal '("a")
            (custom-value 'test-cancel-key))
;; invalid value is ignored
(test-false (custom-set-value! 'test-cancel-key
                               '(test-nonexistent "a")))
(test-equal '("a")
            (custom-value 'test-cancel-key))

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
;; invalid value is ignored
(test-false (custom-set-value! 'test-nr-candidate-max 25))
(test-equal 5
            (custom-value 'test-nr-candidate-max))

;;; string
;; default value
(test-equal "a string"
            (custom-value 'test-string))
;; valid value
(test-true  (custom-set-value! 'test-string "a altered string"))
(test-equal "a altered string"
            (custom-value 'test-string))
;; invalid value is ignored
(test-false (custom-set-value! 'test-string #f))
(test-equal "a altered string"
            (custom-value 'test-string))

;;; pathname
;; default value
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-value 'test-dic-file-name))
;; valid value
(test-true  (custom-set-value!
             'test-dic-file-name
             "/usr/local/share/skk/SKK-JISYO.ML"))
(test-equal "/usr/local/share/skk/SKK-JISYO.ML"
            (custom-value 'test-dic-file-name))
;; invalid value is ignored
(test-false (custom-set-value! 'test-dic-file-name #f))
(test-equal "/usr/local/share/skk/SKK-JISYO.ML"
            (custom-value 'test-dic-file-name))

;;; choice (2)
;; default value
(test-equal 'hiragana
            (custom-value 'test-modelist))
;; valid value
(test-true  (custom-set-value! 'test-modelist 'latin))
(test-equal 'latin
            (custom-value 'test-modelist))
;; invalid value is ignored
(test-false (custom-set-value! 'test-modelist 'kanji))
(test-equal 'latin
            (custom-value 'test-modelist))
;;; table
;; default value
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-value 'test-case-table))
;; valid value
(test-true  (custom-set-value!
             'test-case-table
             '(("ghi" "GHI") ("jkl" "JKL"))))
(test-equal '(("ghi" "GHI") ("jkl" "JKL"))
            (custom-value 'test-case-table))
;; invalid value is ignored
(test-false (custom-set-value! 'test-case-table #f))
(test-equal '(("ghi" "GHI") ("jkl" "JKL"))
            (custom-value 'test-case-table))
(test-end)

(test-begin "custom-default?")
(setup-methods)
;;; choice
;; default value
(test-equal 'test-style-ddskk
            (custom-value 'test-style))
(test-true  (custom-default? 'test-style))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-style 'test-style-uim))
(test-false (custom-default? 'test-style))
;; come back to default
(test-true  (custom-set-value! 'test-style 'test-style-ddskk))
(test-true  (custom-default? 'test-style))

;;; ordered-list
;; default value
(test-equal '(anthy canna skk)
            (custom-value 'test-available-ims))
(test-true  (custom-default? 'test-available-ims))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-available-ims '(anthy)))
(test-false (custom-default? 'test-available-ims))
;; come back to default
(test-true  (custom-set-value! 'test-available-ims
                               '(anthy canna skk)))
(test-true  (custom-default? 'test-available-ims))

;;; key
;; default value
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
(test-true  (custom-default? 'test-cancel-key))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-cancel-key '("a")))
(test-false (custom-default? 'test-cancel-key))
;; come back to default
(test-true  (custom-set-value! 'test-cancel-key
                               '("<Control>g" "escape")))
(test-true  (custom-default? 'test-cancel-key))

;;; boolean
;; default value
(test-true  (custom-value 'test-use-candidate-window?))
(test-true  (custom-default? 'test-use-candidate-window?))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-use-candidate-window? #f))
(test-false (custom-default? 'test-use-candidate-window?))
;; come back to default
(test-true  (custom-set-value! 'test-use-candidate-window? #t))
(test-true  (custom-default? 'test-use-candidate-window?))

;;; integer
;; default value
(test-equal 10
            (custom-value 'test-nr-candidate-max))
(test-true  (custom-default? 'test-nr-candidate-max))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-nr-candidate-max 5))
(test-false (custom-default? 'test-nr-candidate-max))
;; come back to default
(test-true  (custom-set-value! 'test-nr-candidate-max 10))
(test-true  (custom-default? 'test-nr-candidate-max))

;;; string
;; default value
(test-equal "a string"
            (custom-value 'test-string))
(test-true  (custom-default? 'test-string))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-string "a altered string"))
(test-false (custom-default? 'test-string))
;; come back to default
(test-true  (custom-set-value! 'test-string "a string"))
(test-true  (custom-default? 'test-string))

;;; pathname
;; default value
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-value 'test-dic-file-name))
(test-true  (custom-default? 'test-dic-file-name))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-dic-file-name
                               "/usr/local/share/skk/SKK-JISYO.ML"))
(test-false (custom-default? 'test-dic-file-name))
;; come back to default
(test-true  (custom-set-value! 'test-dic-file-name
                               "/usr/share/skk/SKK-JISYO.L"))
(test-true  (custom-default? 'test-dic-file-name))

;;; table
;; default value
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-value 'test-case-table))
(test-true  (custom-default? 'test-case-table))
;; valid, but non-default value
(test-true  (custom-set-value! 'test-case-table
                               '(("ghi" "GHI") ("jkl" "JKL"))))
(test-false (custom-default? 'test-case-table))
;; come back to default
(test-true  (custom-set-value! 'test-case-table
                               '(("abc" "ABC") ("def" "DEF"))))
(test-true  (custom-default? 'test-case-table))
(test-end)

(test-begin "custom-default-value")
(setup-methods)
;;; choice
;; default value
(test-equal 'test-style-ddskk
            (custom-value 'test-style))
(test-equal 'test-style-ddskk
            (custom-default-value 'test-style))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-style 'test-style-uim))
(test-equal 'test-style-uim
            (custom-value 'test-style))
(test-equal 'test-style-ddskk
            (custom-default-value 'test-style))

;;; ordered-list
;; default value
(test-equal '(anthy canna skk)
            (custom-value 'test-available-ims))
(test-equal '(anthy canna skk)
            (custom-default-value 'test-available-ims))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-available-ims '(anthy)))
(test-equal '(anthy)
            (custom-value 'test-available-ims))
(test-equal '(anthy canna skk)
            (custom-default-value 'test-available-ims))

;;; key
;; default value
(test-equal '("<Control>g" "escape")
            (custom-value 'test-cancel-key))
(test-equal '("<Control>g" "escape")
            (custom-default-value 'test-cancel-key))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-cancel-key '("a")))
(test-equal '("a")
            (custom-value 'test-cancel-key))
(test-equal '("<Control>g" "escape")
            (custom-default-value 'test-cancel-key))

;;; boolean
;; default value
(test-true  (custom-value 'test-use-candidate-window?))
(test-true  (custom-default-value 'test-use-candidate-window?))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-use-candidate-window? #f))
(test-false (custom-value 'test-use-candidate-window?))
(test-true  (custom-default-value 'test-use-candidate-window?))

;;; integer
;; default value
(test-equal 10
            (custom-value 'test-nr-candidate-max))
(test-equal 10
            (custom-default-value 'test-nr-candidate-max))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-nr-candidate-max 5))
(test-equal 5
            (custom-value 'test-nr-candidate-max))
(test-equal 10
            (custom-default-value 'test-nr-candidate-max))

;;; string
;; default value
(test-equal "a string"
            (custom-value 'test-string))
(test-equal "a string"
            (custom-default-value 'test-string))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-string "a altered string"))
(test-equal "a altered string"
            (custom-value 'test-string))
(test-equal "a string"
            (custom-default-value 'test-string))

;;; pathname
;; default value
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-value 'test-dic-file-name))
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-default-value 'test-dic-file-name))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-dic-file-name
                               "/usr/local/share/skk/SKK-JISYO.ML"))
(test-equal "/usr/local/share/skk/SKK-JISYO.ML"
            (custom-value 'test-dic-file-name))
(test-equal "/usr/share/skk/SKK-JISYO.L"
            (custom-default-value 'test-dic-file-name))

;;; table
;; default value
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-value 'test-case-table))
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-default-value 'test-case-table))
;; default value is not affected by current value
(test-true  (custom-set-value! 'test-case-table
                               '(("ghi" "GHI") ("jkl" "JKL"))))
(test-equal '(("ghi" "GHI") ("jkl" "JKL"))
            (custom-value 'test-case-table))
(test-equal '(("abc" "ABC") ("def" "DEF"))
            (custom-default-value 'test-case-table))
(test-end)

(test-begin "custom-groups")
(setup-methods)
(test-equal '(global main)
            (custom-groups 'test-style))
(test-equal '(global main)
            (custom-groups 'test-available-ims))
(test-equal '(global main)
            (custom-groups 'test-cancel-key))
(test-equal '(test ui)
            (custom-groups 'test-use-candidate-window?))
(test-equal '(test advanced ui)
            (custom-groups 'test-nr-candidate-max))
(test-equal '(test main)
            (custom-groups 'test-string))
(test-equal '(test main)
            (custom-groups 'test-dic-file-name))
(test-equal '(test main)
            (custom-groups 'test-case-table))
(test-end)

(test-begin "custom-type")
(setup-methods)
(test-equal 'choice
            (custom-type 'test-style))
(test-equal 'ordered-list
            (custom-type 'test-available-ims))
(test-equal 'key
            (custom-type 'test-cancel-key))
(test-equal 'boolean
            (custom-type 'test-use-candidate-window?))
(test-equal 'integer
            (custom-type 'test-nr-candidate-max))
(test-equal 'string
            (custom-type 'test-string))
(test-equal 'pathname
            (custom-type 'test-dic-file-name))
(test-equal 'table
            (custom-type 'test-case-table))
(test-end)

(test-begin "custom-type-attrs")
(setup-methods)
(test-equal '((test-style-uim "uim" "uim native")
              (test-style-ddskk "ddskk like" "Similar to ddskk")
              (test-style-canna "canna like" "Similar to canna"))
            (custom-type-attrs 'test-style))
(test-equal '((anthy "Anthy" "Anthy")
              (canna "Cannd" "Canna")
              (skk "SKK" "SKK"))
            (custom-type-attrs 'test-available-ims))
(test-equal '()
            (custom-type-attrs 'test-cancel-key))
(test-equal '()
            (custom-type-attrs 'test-use-candidate-window?))
(test-equal '(1 20)
            (custom-type-attrs 'test-nr-candidate-max))
(test-equal '(".+")
            (custom-type-attrs 'test-string))
(test-equal '(regular-file)
            (custom-type-attrs 'test-dic-file-name))
(test-equal '((lower-case "lower-case" "lower-case")
              (upper-case "upper-case" "upper-case"))
            (custom-type-attrs 'test-case-table))
(test-end)

(test-begin "custom-range")
(setup-methods)
(test-equal '(test-style-uim test-style-ddskk test-style-canna)
            (custom-range 'test-style))
(test-equal '(anthy canna skk)
            (custom-range 'test-available-ims))
(test-equal '()
            (custom-range 'test-cancel-key))
(test-equal '()
            (custom-range 'test-use-candidate-window?))
(test-equal '(1 20)
            (custom-range 'test-nr-candidate-max))
(test-equal '(".+")
            (custom-range 'test-string))
(test-equal '()
            (custom-range 'test-dic-file-name))
(test-equal '(lower-case upper-case)
            (custom-range 'test-case-table))
(test-end)

(test-begin "custom-label")
(setup-methods)
(test-equal "Test style"
            (custom-label 'test-style))
(test-equal "Test avalilable IMs"
            (custom-label 'test-available-ims))
(test-equal "test cancel key"
            (custom-label 'test-cancel-key))
(test-equal "Use candidate window"
            (custom-label 'test-use-candidate-window?))
(test-equal "Number of candidates in candidate window at a time"
            (custom-label 'test-nr-candidate-max))
(test-equal "A string for testing purpose"
            (custom-label 'test-string))
(test-equal "Dictionary file"
            (custom-label 'test-dic-file-name))
(test-equal "alphabet table"
            (custom-label 'test-case-table))
(test-end)

(test-begin "custom-desc")
(setup-methods)
(test-equal "long description will be here."
            (custom-desc 'test-style))
(test-equal "long description will be here."
            (custom-desc 'test-available-ims))
(test-equal "long description will be here."
            (custom-desc 'test-cancel-key))
(test-equal "long description will be here."
            (custom-desc 'test-use-candidate-window?))
(test-equal "long description will be here."
            (custom-desc 'test-nr-candidate-max))
(test-equal "long description will be here."
            (custom-desc 'test-style))
(test-equal "long description will be here."
            (custom-desc 'test-dic-file-name))
(test-equal "long description will be here."
            (custom-desc 'test-case-table))
(test-end)

(test-begin "custom-value-as-literal")
(setup-methods)
(test-equal "'test-style-ddskk"
            (custom-value-as-literal 'test-style))
(test-equal "'(anthy canna skk)"
            (custom-value-as-literal 'test-available-ims))
(test-equal "'()"
            (custom-value-as-literal 'test-null-ims))
(test-equal "'(\"<Control>g\" \"escape\")"
            (custom-value-as-literal 'test-cancel-key))
(test-equal "'()"
            (custom-value-as-literal 'test-null-key))
(test-equal "#t"
            (custom-value-as-literal 'test-use-candidate-window?))
(test-equal "#f"
            (custom-value-as-literal 'test-use-with-vi?))
(test-equal "10"
            (custom-value-as-literal 'test-nr-candidate-max))
(test-equal "\"a string\""
            (custom-value-as-literal 'test-string))
(test-equal "\"/usr/share/skk/SKK-JISYO.L\""
            (custom-value-as-literal 'test-dic-file-name))
(test-equal "'((\"abc\" \"ABC\") (\"def\" \"DEF\"))"
            (custom-value-as-literal 'test-case-table))
(test-end)

(test-begin "custom-definition-as-literal")
(setup-methods)
(test-equal "(define test-style 'test-style-ddskk)"
            (custom-definition-as-literal 'test-style))
(test-equal "(define test-available-ims '(anthy canna skk))"
            (custom-definition-as-literal 'test-available-ims))
(test-equal "(define test-null-ims '())"
            (custom-definition-as-literal 'test-null-ims))
(test-equal "(define test-cancel-key '(\"<Control>g\" \"escape\"))\n(define test-cancel-key? (make-key-predicate '(\"<Control>g\" \"escape\")))"
            (custom-definition-as-literal 'test-cancel-key))
(test-equal "(define test-null-key '())\n(define test-null-key? (make-key-predicate '()))"
            (custom-definition-as-literal 'test-null-key))
(test-equal "(define test-use-candidate-window? #t)"
            (custom-definition-as-literal 'test-use-candidate-window?))
(test-equal "(define test-use-with-vi? #f)"
            (custom-definition-as-literal 'test-use-with-vi?))
(test-equal "(define test-nr-candidate-max 10)"
            (custom-definition-as-literal 'test-nr-candidate-max))
(test-equal "(define test-string \"a string\")"
            (custom-definition-as-literal 'test-string))
(test-equal "(define test-dic-file-name \"/usr/share/skk/SKK-JISYO.L\")"
            (custom-definition-as-literal 'test-dic-file-name))
(test-equal "(define test-case-table '((\"abc\" \"ABC\") (\"def\" \"DEF\")))"
            (custom-definition-as-literal 'test-case-table))
;; hooked
(test-equal "(define test-style 'test-style-ddskk)"
            (custom-definition-as-literal 'test-style))
(custom-add-hook 'test-style 'custom-literalize-hooks
                 (lambda () "(define test-style 'hooked)"))
(test-equal "(define test-style 'hooked)"
            (custom-definition-as-literal 'test-style))
(custom-add-hook 'test-style 'custom-literalize-hooks
                 (lambda () "(define test-style 'hooked2)"))
(test-equal "(define test-style 'hooked2)\n(define test-style 'hooked)"
            (custom-definition-as-literal 'test-style))
(test-end)

(define setup-interfaces
  (lambda ()
    (test-custom-reset!)

    (define-custom-group 'test
      "test"
      "test")
    (define-custom-group 'ui
      "ui"
      "ui")

    (define-custom 'test-nr-candidate-max 10
      '(test advanced ui)
      '(integer 1 20)
      "Number of candidates in candidate window at a time"
      "long description will be here.")))

(test-begin "custom-prop-update-custom-handler")
(setup-interfaces)
(define test-context (context-new 1 (find-im 'direct #f)))
;; default value
(test-equal 10
            (custom-value 'test-nr-candidate-max))
;; valid value
(test-true  (custom-prop-update-custom-handler
             test-context 'test-nr-candidate-max "5"))
(test-true  (custom-prop-update-custom-handler
             test-context 'test-nr-candidate-max "'5"))
(test-equal 5
            (custom-value 'test-nr-candidate-max))
;; invalid value is ignored
(test-false (custom-prop-update-custom-handler
             test-context 'test-nr-candidate-max "25"))
(test-false (custom-prop-update-custom-handler
             test-context 'test-nr-candidate-max "'25"))
(test-equal 5
            (custom-value 'test-nr-candidate-max))
(test-end)

(test-report-result)
