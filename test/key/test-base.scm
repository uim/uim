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

(define-module test.key.test-base
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.key.test-base)

(define (setup)
  (uim-test-setup)
  (uim-eval
   '(begin
      (define test-shift-state (cdr (assq 'Shift_key key-state-alist)))
      (define test-control-state (cdr (assq 'Control_key key-state-alist)))
      (define test-alt-state (cdr (assq 'Alt_key key-state-alist)))
      (define test-meta-state (cdr (assq 'Meta_key key-state-alist)))
      (define test-super-state (cdr (assq 'Super_key key-state-alist)))
      (define test-hyper-state (cdr (assq 'Hyper_key key-state-alist))))))

(define (teardown)
  (uim-test-teardown))

(define (test-intern-key-symbol)
  (assert-uim-equal 'backspace
                    '(intern-key-symbol "backspace"))
  (assert-uim-equal 'delete
                    '(intern-key-symbol "delete"))
  (assert-uim-equal 'zenkaku-hankaku
                    '(intern-key-symbol "zenkaku-hankaku"))
  (assert-uim-equal 'F10
                    '(intern-key-symbol "F10"))
  (assert-uim-equal 'Private9
                    '(intern-key-symbol "Private9"))
  (assert-uim-equal 'Hyper_key
                    '(intern-key-symbol "Hyper_key"))
  (assert-uim-false '(intern-key-symbol "nonexistent"))
  #f)

(define (test-modifier-key-mask-predicates)
  (assert-uim-true  '(shift-key-mask test-shift-state))
  (assert-uim-false '(shift-key-mask test-control-state))
  (assert-uim-false '(shift-key-mask test-alt-state))
  (assert-uim-false '(shift-key-mask test-meta-state))
  (assert-uim-false '(shift-key-mask test-super-state))
  (assert-uim-false '(shift-key-mask test-hyper-state))
  (assert-uim-false '(shift-key-mask 0))

  (assert-uim-false '(control-key-mask test-shift-state))
  (assert-uim-true  '(control-key-mask test-control-state))
  (assert-uim-false '(control-key-mask test-alt-state))
  (assert-uim-false '(control-key-mask test-meta-state))
  (assert-uim-false '(control-key-mask test-super-state))
  (assert-uim-false '(control-key-mask test-hyper-state))
  (assert-uim-false '(control-key-mask 0))

  (assert-uim-false '(alt-key-mask test-shift-state))
  (assert-uim-false '(alt-key-mask test-control-state))
  (assert-uim-true  '(alt-key-mask test-alt-state))
  (assert-uim-false '(alt-key-mask test-meta-state))
  (assert-uim-false '(alt-key-mask test-super-state))
  (assert-uim-false '(alt-key-mask test-hyper-state))
  (assert-uim-false '(alt-key-mask 0))

  (assert-uim-false '(meta-key-mask test-shift-state))
  (assert-uim-false '(meta-key-mask test-control-state))
  (assert-uim-false '(meta-key-mask test-alt-state))
  (assert-uim-true  '(meta-key-mask test-meta-state))
  (assert-uim-false '(meta-key-mask test-super-state))
  (assert-uim-false '(meta-key-mask test-hyper-state))
  (assert-uim-false '(meta-key-mask 0))

  (assert-uim-false '(super-key-mask test-shift-state))
  (assert-uim-false '(super-key-mask test-control-state))
  (assert-uim-false '(super-key-mask test-alt-state))
  (assert-uim-false '(super-key-mask test-meta-state))
  (assert-uim-true  '(super-key-mask test-super-state))
  (assert-uim-false '(super-key-mask test-hyper-state))
  (assert-uim-false '(super-key-mask 0))

  (assert-uim-false '(hyper-key-mask test-shift-state))
  (assert-uim-false '(hyper-key-mask test-control-state))
  (assert-uim-false '(hyper-key-mask test-alt-state))
  (assert-uim-false '(hyper-key-mask test-meta-state))
  (assert-uim-false '(hyper-key-mask test-super-state))
  (assert-uim-true  '(hyper-key-mask test-hyper-state))
  (assert-uim-false '(hyper-key-mask 0))

  (assert-uim-true  '(modifier-key-mask test-shift-state))
  (assert-uim-true  '(modifier-key-mask test-control-state))
  (assert-uim-true  '(modifier-key-mask test-alt-state))
  (assert-uim-true  '(modifier-key-mask test-meta-state))
  (assert-uim-true  '(modifier-key-mask test-super-state))
  (assert-uim-true  '(modifier-key-mask test-hyper-state))
  (assert-uim-false '(modifier-key-mask 0))
  #f)

(define (test-modifier-key?)
  (assert-uim-true  '(modifier-key? 'Shift_key 0))
  (assert-uim-true  '(modifier-key? 'Control_key 0))
  (assert-uim-true  '(modifier-key? 'Alt_key 0))
  (assert-uim-true  '(modifier-key? 'Meta_key 0))
  (assert-uim-true  '(modifier-key? 'Super_key 0))
  (assert-uim-true  '(modifier-key? 'Hyper_key 0))
  (assert-uim-false '(modifier-key? 'return 0))
  (assert-uim-false '(modifier-key? 'escape 0))
  (assert-uim-false '(modifier-key? 0 0))   ;; NUL
  (assert-uim-false '(modifier-key? 97 0))  ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-shift-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-shift-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-shift-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-shift-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-shift-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-shift-state))
  (assert-uim-false '(modifier-key? 'return test-shift-state))
  (assert-uim-false '(modifier-key? 'escape test-shift-state))
  (assert-uim-false '(modifier-key? 0 test-shift-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-shift-state)) ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-control-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-control-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-control-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-control-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-control-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-control-state))
  (assert-uim-false '(modifier-key? 'return test-control-state))
  (assert-uim-false '(modifier-key? 'escape test-control-state))
  (assert-uim-false '(modifier-key? 0 test-control-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-control-state)) ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-alt-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-alt-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-alt-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-alt-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-alt-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-alt-state))
  (assert-uim-false '(modifier-key? 'return test-alt-state))
  (assert-uim-false '(modifier-key? 'escape test-alt-state))
  (assert-uim-false '(modifier-key? 0 test-alt-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-alt-state)) ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-meta-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-meta-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-meta-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-meta-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-meta-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-meta-state))
  (assert-uim-false '(modifier-key? 'return test-meta-state))
  (assert-uim-false '(modifier-key? 'escape test-meta-state))
  (assert-uim-false '(modifier-key? 0 test-meta-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-meta-state)) ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-super-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-super-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-super-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-super-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-super-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-super-state))
  (assert-uim-false '(modifier-key? 'return test-super-state))
  (assert-uim-false '(modifier-key? 'escape test-super-state))
  (assert-uim-false '(modifier-key? 0 test-super-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-super-state)) ;; a

  (assert-uim-true  '(modifier-key? 'Shift_key test-hyper-state))
  (assert-uim-true  '(modifier-key? 'Control_key test-hyper-state))
  (assert-uim-true  '(modifier-key? 'Alt_key test-hyper-state))
  (assert-uim-true  '(modifier-key? 'Meta_key test-hyper-state))
  (assert-uim-true  '(modifier-key? 'Super_key test-hyper-state))
  (assert-uim-true  '(modifier-key? 'Hyper_key test-hyper-state))
  (assert-uim-false '(modifier-key? 'return test-hyper-state))
  (assert-uim-false '(modifier-key? 'escape test-hyper-state))
  (assert-uim-false '(modifier-key? 0 test-hyper-state)) ;; NUL
  (assert-uim-false '(modifier-key? 97 test-hyper-state)) ;; a
  #f)

(define (test-translator-prefix?)
  (assert-uim-true  '(translator-prefix? 'IgnoreCase))
  (assert-uim-true  '(translator-prefix? 'IgnoreShift))
  (assert-uim-true  '(translator-prefix? 'IgnoreRegularShift))
  (assert-uim-false '(translator-prefix? 'NonExistent))
  #f)

(define (test-intern-key-prefix)
  (assert-uim-equal 'Shift_key
                    '(intern-key-prefix "Shift" tag-prefix-alist))
  (assert-uim-equal 'Control_key
                    '(intern-key-prefix "Control" tag-prefix-alist))
  (assert-uim-equal 'Alt_key
                    '(intern-key-prefix "Alt" tag-prefix-alist))
  (assert-uim-equal 'Meta_key
                    '(intern-key-prefix "Meta" tag-prefix-alist))
  (assert-uim-equal 'Super_key
                    '(intern-key-prefix "Super" tag-prefix-alist))
  (assert-uim-equal 'Hyper_key
                    '(intern-key-prefix "Hyper" tag-prefix-alist))
  (assert-uim-equal 'IgnoreCase
                    '(intern-key-prefix "IgnoreCase" tag-prefix-alist))
  (assert-uim-equal 'IgnoreShift
                    '(intern-key-prefix "IgnoreShift" tag-prefix-alist))
  (assert-uim-equal 'IgnoreRegularShift
                    '(intern-key-prefix "IgnoreRegularShift" tag-prefix-alist))
  (assert-uim-false '(intern-key-prefix "NonExistent" tag-prefix-alist))

  (assert-uim-equal 'Shift_key
                    '(intern-key-prefix "S" emacs-like-prefix-alist))
  (assert-uim-equal 'Control_key
                    '(intern-key-prefix "C" emacs-like-prefix-alist))
  (assert-uim-equal 'Alt_key
                    '(intern-key-prefix "A" emacs-like-prefix-alist))
  (assert-uim-equal 'Meta_key
                    '(intern-key-prefix "M" emacs-like-prefix-alist))
  (assert-uim-equal 'Super_key
                    '(intern-key-prefix "Z" emacs-like-prefix-alist))
  (assert-uim-equal 'Hyper_key
                    '(intern-key-prefix "H" emacs-like-prefix-alist))
  (assert-uim-equal 'IgnoreCase
                    '(intern-key-prefix "I" emacs-like-prefix-alist))
  (assert-uim-equal 'IgnoreShift
                    '(intern-key-prefix "J" emacs-like-prefix-alist))
  (assert-uim-equal 'IgnoreRegularShift
                    '(intern-key-prefix "K" emacs-like-prefix-alist))
  (assert-uim-false '(intern-key-prefix "N" emacs-like-prefix-alist))
  #f)

(define (test-parse-tag-prefix-symbol)
  (assert-uim-equal '(Shift_key)
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t")))
  (assert-uim-equal '(Control_key)
                    '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l")))
  (assert-uim-equal '(Alt_key)
                    '(parse-tag-prefix-symbol "" '("A" "l" "t")))
  (assert-uim-equal '(Meta_key)
                    '(parse-tag-prefix-symbol "" '("M" "e" "t" "a")))
  (assert-uim-equal '(Super_key)
                    '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r")))
  (assert-uim-equal '(Hyper_key)
                    '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r")))
  (assert-uim-equal (list (uim #f))
                    '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t")))
  (assert-uim-equal (list (uim #f))
                    '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T")))
  (assert-uim-equal (list (uim #f))
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t")))

  ;; parsing is terminated at boundary char
  (assert-uim-equal '(Shift_key ">")
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">")))
  (assert-uim-equal '(Control_key ">")
                    '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">")))
  (assert-uim-equal '(Alt_key ">")
                    '(parse-tag-prefix-symbol "" '("A" "l" "t" ">")))
  (assert-uim-equal '(Meta_key ">")
                    '(parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">")))
  (assert-uim-equal '(Super_key ">")
                    '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">")))
  (assert-uim-equal '(Hyper_key ">")
                    '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">")))
  (assert-uim-equal (list (uim #f) ">")
                    '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">")))
  (assert-uim-equal (list (uim #f) ">")
                    '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">")))
  (assert-uim-equal (list (uim #f) ">")
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">")))

  ;; all chars that following boundary char remains
  (assert-uim-equal '(Shift_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal '(Control_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal '(Alt_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("A" "l" "t" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal '(Meta_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal '(Super_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal '(Hyper_key ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">" "<" "A" "l" "t" ">")))
  (assert-uim-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                    '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">" "<" "A" "l" "t" ">")))

  ;; nonexistent symbol is parsed as #f
  (assert-uim-equal (list (uim #f))
                    '(parse-tag-prefix-symbol "" '("_" "F" "o" "o" "1")))
  ;; tag-prefix symbol must be consist of alphanumeric or "_"
  (assert-uim-equal (list (uim #f) "-" "F" "o" "o" "1")
                    '(parse-tag-prefix-symbol "" '("-" "F" "o" "o" "1")))
  #f)

(define (test-parse-tag-prefix)
  (assert-uim-equal '(Shift_key . "")
                    '(parse-tag-prefix "<Shift>"))
  (assert-uim-equal '(Control_key . "")
                    '(parse-tag-prefix "<Control>"))
  (assert-uim-equal '(Alt_key . "")
                    '(parse-tag-prefix "<Alt>"))
  (assert-uim-equal '(Meta_key . "")
                    '(parse-tag-prefix "<Meta>"))
  (assert-uim-equal '(Super_key . "")
                    '(parse-tag-prefix "<Super>"))
  (assert-uim-equal '(Hyper_key . "")
                    '(parse-tag-prefix "<Hyper>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-tag-prefix "<NonExistent>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-tag-prefix "<SHIFT>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-tag-prefix "<Shiftt>"))

  (assert-uim-equal '(Shift_key . "<Alt>")
                    '(parse-tag-prefix "<Shift><Alt>"))
  (assert-uim-equal '(Control_key . "<Alt>")
                    '(parse-tag-prefix "<Control><Alt>"))
  (assert-uim-equal '(Alt_key . "<Alt>")
                    '(parse-tag-prefix "<Alt><Alt>"))
  (assert-uim-equal '(Meta_key . "<Alt>")
                    '(parse-tag-prefix "<Meta><Alt>"))
  (assert-uim-equal '(Super_key . "<Alt>")
                    '(parse-tag-prefix "<Super><Alt>"))
  (assert-uim-equal '(Hyper_key . "<Alt>")
                    '(parse-tag-prefix "<Hyper><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-tag-prefix "<NonExistent><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-tag-prefix "<SHIFT><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-tag-prefix "<Shiftt><Alt>"))

  (assert-uim-equal '(Shift_key . "a")
                    '(parse-tag-prefix "<Shift>a"))
  (assert-uim-equal '(Control_key . "a")
                    '(parse-tag-prefix "<Control>a"))
  (assert-uim-equal '(Alt_key . "a")
                    '(parse-tag-prefix "<Alt>a"))
  (assert-uim-equal '(Meta_key . "a")
                    '(parse-tag-prefix "<Meta>a"))
  (assert-uim-equal '(Super_key . "a")
                    '(parse-tag-prefix "<Super>a"))
  (assert-uim-equal '(Hyper_key . "a")
                    '(parse-tag-prefix "<Hyper>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-tag-prefix "<NonExistent>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-tag-prefix "<SHIFT>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-tag-prefix "<Shiftt>a"))
  #f)

(define (test-parse-emacs-like-prefix)
  (assert-uim-equal '(Shift_key . "")
                    '(parse-emacs-like-prefix "S-"))
  (assert-uim-equal '(Control_key . "")
                    '(parse-emacs-like-prefix "C-"))
  (assert-uim-equal '(Alt_key . "")
                    '(parse-emacs-like-prefix "A-"))
  (assert-uim-equal '(Meta_key . "")
                    '(parse-emacs-like-prefix "M-"))
  (assert-uim-equal '(Super_key . "")
                    '(parse-emacs-like-prefix "Z-"))
  (assert-uim-equal '(Hyper_key . "")
                    '(parse-emacs-like-prefix "H-"))
  (assert-uim-equal (cons (uim #f) "N-")
                    '(parse-emacs-like-prefix "N-"))
  (assert-uim-equal (cons (uim #f) "s-")
                    '(parse-emacs-like-prefix "s-"))
  (assert-uim-equal (cons (uim #f) "SS-")
                    '(parse-emacs-like-prefix "SS-"))

  (assert-uim-equal '(Shift_key . "A-")
                    '(parse-emacs-like-prefix "S-A-"))
  (assert-uim-equal '(Control_key . "A-")
                    '(parse-emacs-like-prefix "C-A-"))
  (assert-uim-equal '(Alt_key . "A-")
                    '(parse-emacs-like-prefix "A-A-"))
  (assert-uim-equal '(Meta_key . "A-")
                    '(parse-emacs-like-prefix "M-A-"))
  (assert-uim-equal '(Super_key . "A-")
                    '(parse-emacs-like-prefix "Z-A-"))
  (assert-uim-equal '(Hyper_key . "A-")
                    '(parse-emacs-like-prefix "H-A-"))
  (assert-uim-equal (cons (uim #f) "N-A-")
                    '(parse-emacs-like-prefix "N-A-"))
  (assert-uim-equal (cons (uim #f) "s-A-")
                    '(parse-emacs-like-prefix "s-A-"))
  (assert-uim-equal (cons (uim #f) "SS-A-")
                    '(parse-emacs-like-prefix "SS-A-"))

  (assert-uim-equal '(Shift_key . "a")
                    '(parse-emacs-like-prefix "S-a"))
  (assert-uim-equal '(Control_key . "a")
                    '(parse-emacs-like-prefix "C-a"))
  (assert-uim-equal '(Alt_key . "a")
                    '(parse-emacs-like-prefix "A-a"))
  (assert-uim-equal '(Meta_key . "a")
                    '(parse-emacs-like-prefix "M-a"))
  (assert-uim-equal '(Super_key . "a")
                    '(parse-emacs-like-prefix "Z-a"))
  (assert-uim-equal '(Hyper_key . "a")
                    '(parse-emacs-like-prefix "H-a"))
  (assert-uim-equal (cons (uim #f) "N-a")
                    '(parse-emacs-like-prefix "N-a"))
  (assert-uim-equal (cons (uim #f) "s-a")
                    '(parse-emacs-like-prefix "s-a"))
  (assert-uim-equal (cons (uim #f) "SS-a")
                    '(parse-emacs-like-prefix "SS-a"))
  #f)

(define (test-parse-key-prefix)
  (assert-uim-equal '(Shift_key . "")
                    '(parse-key-prefix "<Shift>"))
  (assert-uim-equal '(Control_key . "")
                    '(parse-key-prefix "<Control>"))
  (assert-uim-equal '(Alt_key . "")
                    '(parse-key-prefix "<Alt>"))
  (assert-uim-equal '(Meta_key . "")
                    '(parse-key-prefix "<Meta>"))
  (assert-uim-equal '(Super_key . "")
                    '(parse-key-prefix "<Super>"))
  (assert-uim-equal '(Hyper_key . "")
                    '(parse-key-prefix "<Hyper>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-key-prefix "<NonExistent>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-key-prefix "<SHIFT>"))
  (assert-uim-equal (cons (uim #f) "")
                    '(parse-key-prefix "<Shiftt>"))

  (assert-uim-equal '(Shift_key . "<Alt>")
                    '(parse-key-prefix "<Shift><Alt>"))
  (assert-uim-equal '(Control_key . "<Alt>")
                    '(parse-key-prefix "<Control><Alt>"))
  (assert-uim-equal '(Alt_key . "<Alt>")
                    '(parse-key-prefix "<Alt><Alt>"))
  (assert-uim-equal '(Meta_key . "<Alt>")
                    '(parse-key-prefix "<Meta><Alt>"))
  (assert-uim-equal '(Super_key . "<Alt>")
                    '(parse-key-prefix "<Super><Alt>"))
  (assert-uim-equal '(Hyper_key . "<Alt>")
                    '(parse-key-prefix "<Hyper><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-key-prefix "<NonExistent><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-key-prefix "<SHIFT><Alt>"))
  (assert-uim-equal (cons (uim #f) "<Alt>")
                    '(parse-key-prefix "<Shiftt><Alt>"))

  (assert-uim-equal '(Shift_key . "a")
                    '(parse-key-prefix "<Shift>a"))
  (assert-uim-equal '(Control_key . "a")
                    '(parse-key-prefix "<Control>a"))
  (assert-uim-equal '(Alt_key . "a")
                    '(parse-key-prefix "<Alt>a"))
  (assert-uim-equal '(Meta_key . "a")
                    '(parse-key-prefix "<Meta>a"))
  (assert-uim-equal '(Super_key . "a")
                    '(parse-key-prefix "<Super>a"))
  (assert-uim-equal '(Hyper_key . "a")
                    '(parse-key-prefix "<Hyper>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-key-prefix "<NonExistent>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-key-prefix "<SHIFT>a"))
  (assert-uim-equal (cons (uim #f) "a")
                    '(parse-key-prefix "<Shiftt>a"))

  (assert-uim-equal '(Shift_key . "")
                    '(parse-key-prefix "S-"))
  (assert-uim-equal '(Control_key . "")
                    '(parse-key-prefix "C-"))
  (assert-uim-equal '(Alt_key . "")
                    '(parse-key-prefix "A-"))
  (assert-uim-equal '(Meta_key . "")
                    '(parse-key-prefix "M-"))
  (assert-uim-equal '(Super_key . "")
                    '(parse-key-prefix "Z-"))
  (assert-uim-equal '(Hyper_key . "")
                    '(parse-key-prefix "H-"))
  (assert-uim-equal (cons (uim #f) "N-")
                    '(parse-key-prefix "N-"))
  (assert-uim-equal (cons (uim #f) "s-")
                    '(parse-key-prefix "s-"))
  (assert-uim-equal (cons (uim #f) "SS-")
                    '(parse-key-prefix "SS-"))

  (assert-uim-equal '(Shift_key . "A-")
                    '(parse-key-prefix "S-A-"))
  (assert-uim-equal '(Control_key . "A-")
                    '(parse-key-prefix "C-A-"))
  (assert-uim-equal '(Alt_key . "A-")
                    '(parse-key-prefix "A-A-"))
  (assert-uim-equal '(Meta_key . "A-")
                    '(parse-key-prefix "M-A-"))
  (assert-uim-equal '(Super_key . "A-")
                    '(parse-key-prefix "Z-A-"))
  (assert-uim-equal '(Hyper_key . "A-")
                    '(parse-key-prefix "H-A-"))
  (assert-uim-equal (cons (uim #f) "N-A-")
                    '(parse-key-prefix "N-A-"))
  (assert-uim-equal (cons (uim #f) "s-A-")
                    '(parse-key-prefix "s-A-"))
  (assert-uim-equal (cons (uim #f) "SS-A-")
                    '(parse-key-prefix "SS-A-"))

  (assert-uim-equal '(Shift_key . "a")
                    '(parse-key-prefix "S-a"))
  (assert-uim-equal '(Control_key . "a")
                    '(parse-key-prefix "C-a"))
  (assert-uim-equal '(Alt_key . "a")
                    '(parse-key-prefix "A-a"))
  (assert-uim-equal '(Meta_key . "a")
                    '(parse-key-prefix "M-a"))
  (assert-uim-equal '(Super_key . "a")
                    '(parse-key-prefix "Z-a"))
  (assert-uim-equal '(Hyper_key . "a")
                    '(parse-key-prefix "H-a"))
  (assert-uim-equal (cons (uim #f) "N-a")
                    '(parse-key-prefix "N-a"))
  (assert-uim-equal (cons (uim #f) "s-a")
                    '(parse-key-prefix "s-a"))
  (assert-uim-equal (cons (uim #f) "SS-a")
                    '(parse-key-prefix "SS-a"))
  #f)

(define (test-parse-key-str)
  ;; single key
  (assert-uim-equal (list "" () 32 0)
                    '(parse-key-str " " () 0 0))
  (assert-uim-equal (list "" () 33 0)
                    '(parse-key-str "!" () 0 0))
  (assert-uim-equal (list "" () 48 0)
                    '(parse-key-str "0" () 0 0))
  (assert-uim-equal (list "" () 65 0)
                    '(parse-key-str "A" () 0 0))
  (assert-uim-equal (list "" () 97 0)
                    '(parse-key-str "a" () 0 0))
  (assert-uim-equal (list "" () 'return 0)
                    '(parse-key-str "return" () 0 0))

  ;; single key with single modifier
  (assert-uim-equal (uim '(list "" () 32 test-shift-state))
                    '(parse-key-str "<Shift> " () 0 0))
  (assert-uim-equal (uim '(list "" () 33 test-shift-state))
                    '(parse-key-str "<Shift>!" () 0 0))
  (assert-uim-equal (uim '(list "" () 48 test-shift-state))
                    '(parse-key-str "<Shift>0" () 0 0))
  (assert-uim-equal (uim '(list "" () 65 test-shift-state))
                    '(parse-key-str "<Shift>A" () 0 0))
  (assert-uim-equal (uim '(list "" () 97 test-shift-state))
                    '(parse-key-str "<Shift>a" () 0 0))
  (assert-uim-equal (uim '(list "" () 'return test-shift-state))
                    '(parse-key-str "<Shift>return" () 0 0))

  ;; single key with multiple modifiers
  (assert-uim-equal (uim '(list "" () 32 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta> " () 0 0))
  (assert-uim-equal (uim '(list "" () 33 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta>!" () 0 0))
  (assert-uim-equal (uim '(list "" () 48 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta>0" () 0 0))
  (assert-uim-equal (uim '(list "" () 65 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta>A" () 0 0))
  (assert-uim-equal (uim '(list "" () 97 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta>a" () 0 0))
  (assert-uim-equal (uim '(list "" () 'return (+ test-shift-state
                                             test-control-state
                                             test-meta-state)))
                    '(parse-key-str "<Shift><Control><Meta>return" () 0 0))

  ;; single key with single translator
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift> " () 0 0))))
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift>!" () 0 0))))
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift>0" () 0 0))))
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift>A" () 0 0))))
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift>a" () 0 0))))
  (assert-uim-equal 1
                    '(length (cadr (parse-key-str "<IgnoreShift>return" () 0 0))))

  ;; single key with multiple translators
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " () 0 0))))
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" () 0 0))))
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" () 0 0))))
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" () 0 0))))
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" () 0 0))))
  (assert-uim-equal 2
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" () 0 0))))

  ;; single key with multiple translators (2)
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " (list (lambda () #t)) 0 0))))
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" (list (lambda () #t)) 0 0))))
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" (list (lambda () #t)) 0 0))))
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" (list (lambda () #t)) 0 0))))
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" (list (lambda () #t)) 0 0))))
  (assert-uim-equal 3
                    '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" (list (lambda () #t)) 0 0))))
  #f)

(provide "test/key/test-base")
