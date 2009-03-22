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
  (assert-equal 'backspace
                (uim '(intern-key-symbol "backspace")))
  (assert-equal 'delete
                (uim '(intern-key-symbol "delete")))
  (assert-equal 'zenkaku-hankaku
                (uim '(intern-key-symbol "zenkaku-hankaku")))
  (assert-equal 'F10
                (uim '(intern-key-symbol "F10")))
  (assert-equal 'Private9
                (uim '(intern-key-symbol "Private9")))
  (assert-equal 'Hyper_key
                (uim '(intern-key-symbol "Hyper_key")))
  (assert-false (uim-bool '(intern-key-symbol "nonexistent")))
  #f)

(define (test-modifier-key-mask-predicates)
  (assert-true  (uim-bool '(shift-key-mask test-shift-state)))
  (assert-false (uim-bool '(shift-key-mask test-control-state)))
  (assert-false (uim-bool '(shift-key-mask test-alt-state)))
  (assert-false (uim-bool '(shift-key-mask test-meta-state)))
  (assert-false (uim-bool '(shift-key-mask test-super-state)))
  (assert-false (uim-bool '(shift-key-mask test-hyper-state)))
  (assert-false (uim-bool '(shift-key-mask 0)))

  (assert-false (uim-bool '(control-key-mask test-shift-state)))
  (assert-true  (uim-bool '(control-key-mask test-control-state)))
  (assert-false (uim-bool '(control-key-mask test-alt-state)))
  (assert-false (uim-bool '(control-key-mask test-meta-state)))
  (assert-false (uim-bool '(control-key-mask test-super-state)))
  (assert-false (uim-bool '(control-key-mask test-hyper-state)))
  (assert-false (uim-bool '(control-key-mask 0)))

  (assert-false (uim-bool '(alt-key-mask test-shift-state)))
  (assert-false (uim-bool '(alt-key-mask test-control-state)))
  (assert-true  (uim-bool '(alt-key-mask test-alt-state)))
  (assert-false (uim-bool '(alt-key-mask test-meta-state)))
  (assert-false (uim-bool '(alt-key-mask test-super-state)))
  (assert-false (uim-bool '(alt-key-mask test-hyper-state)))
  (assert-false (uim-bool '(alt-key-mask 0)))

  (assert-false (uim-bool '(meta-key-mask test-shift-state)))
  (assert-false (uim-bool '(meta-key-mask test-control-state)))
  (assert-false (uim-bool '(meta-key-mask test-alt-state)))
  (assert-true  (uim-bool '(meta-key-mask test-meta-state)))
  (assert-false (uim-bool '(meta-key-mask test-super-state)))
  (assert-false (uim-bool '(meta-key-mask test-hyper-state)))
  (assert-false (uim-bool '(meta-key-mask 0)))

  (assert-false (uim-bool '(super-key-mask test-shift-state)))
  (assert-false (uim-bool '(super-key-mask test-control-state)))
  (assert-false (uim-bool '(super-key-mask test-alt-state)))
  (assert-false (uim-bool '(super-key-mask test-meta-state)))
  (assert-true  (uim-bool '(super-key-mask test-super-state)))
  (assert-false (uim-bool '(super-key-mask test-hyper-state)))
  (assert-false (uim-bool '(super-key-mask 0)))

  (assert-false (uim-bool '(hyper-key-mask test-shift-state)))
  (assert-false (uim-bool '(hyper-key-mask test-control-state)))
  (assert-false (uim-bool '(hyper-key-mask test-alt-state)))
  (assert-false (uim-bool '(hyper-key-mask test-meta-state)))
  (assert-false (uim-bool '(hyper-key-mask test-super-state)))
  (assert-true  (uim-bool '(hyper-key-mask test-hyper-state)))
  (assert-false (uim-bool '(hyper-key-mask 0)))

  (assert-true  (uim-bool '(modifier-key-mask test-shift-state)))
  (assert-true  (uim-bool '(modifier-key-mask test-control-state)))
  (assert-true  (uim-bool '(modifier-key-mask test-alt-state)))
  (assert-true  (uim-bool '(modifier-key-mask test-meta-state)))
  (assert-true  (uim-bool '(modifier-key-mask test-super-state)))
  (assert-true  (uim-bool '(modifier-key-mask test-hyper-state)))
  (assert-false (uim-bool '(modifier-key-mask 0)))
  #f)

(define (test-modifier-key?)
  (assert-true  (uim-bool '(modifier-key? 'Shift_key 0)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key 0)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key 0)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key 0)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key 0)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key 0)))
  (assert-false (uim-bool '(modifier-key? 'return 0)))
  (assert-false (uim-bool '(modifier-key? 'escape 0)))
  (assert-false (uim-bool '(modifier-key? 0 0)))   ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 0)))  ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-shift-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-shift-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-shift-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-shift-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-shift-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-shift-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-shift-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-shift-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-shift-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-shift-state))) ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-control-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-control-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-control-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-control-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-control-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-control-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-control-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-control-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-control-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-control-state))) ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-alt-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-alt-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-alt-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-alt-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-alt-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-alt-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-alt-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-alt-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-alt-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-alt-state))) ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-meta-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-meta-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-meta-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-meta-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-meta-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-meta-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-meta-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-meta-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-meta-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-meta-state))) ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-super-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-super-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-super-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-super-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-super-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-super-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-super-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-super-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-super-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-super-state))) ;; a

  (assert-true  (uim-bool '(modifier-key? 'Shift_key test-hyper-state)))
  (assert-true  (uim-bool '(modifier-key? 'Control_key test-hyper-state)))
  (assert-true  (uim-bool '(modifier-key? 'Alt_key test-hyper-state)))
  (assert-true  (uim-bool '(modifier-key? 'Meta_key test-hyper-state)))
  (assert-true  (uim-bool '(modifier-key? 'Super_key test-hyper-state)))
  (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-hyper-state)))
  (assert-false (uim-bool '(modifier-key? 'return test-hyper-state)))
  (assert-false (uim-bool '(modifier-key? 'escape test-hyper-state)))
  (assert-false (uim-bool '(modifier-key? 0 test-hyper-state))) ;; NUL
  (assert-false (uim-bool '(modifier-key? 97 test-hyper-state))) ;; a
  #f)

(define (test-translator-prefix?)
  (assert-true  (uim-bool '(translator-prefix? 'IgnoreCase)))
  (assert-true  (uim-bool '(translator-prefix? 'IgnoreShift)))
  (assert-true  (uim-bool '(translator-prefix? 'IgnoreRegularShift)))
  (assert-false (uim-bool '(translator-prefix? 'NonExistent)))
  #f)

(define (test-intern-key-prefix)
  (assert-equal 'Shift_key
                (uim '(intern-key-prefix "Shift" tag-prefix-alist)))
  (assert-equal 'Control_key
                (uim '(intern-key-prefix "Control" tag-prefix-alist)))
  (assert-equal 'Alt_key
                (uim '(intern-key-prefix "Alt" tag-prefix-alist)))
  (assert-equal 'Meta_key
                (uim '(intern-key-prefix "Meta" tag-prefix-alist)))
  (assert-equal 'Super_key
                (uim '(intern-key-prefix "Super" tag-prefix-alist)))
  (assert-equal 'Hyper_key
                (uim '(intern-key-prefix "Hyper" tag-prefix-alist)))
  (assert-equal 'IgnoreCase
                (uim '(intern-key-prefix "IgnoreCase" tag-prefix-alist)))
  (assert-equal 'IgnoreShift
                (uim '(intern-key-prefix "IgnoreShift" tag-prefix-alist)))
  (assert-equal 'IgnoreRegularShift
                (uim '(intern-key-prefix "IgnoreRegularShift" tag-prefix-alist)))
  (assert-false (uim-bool '(intern-key-prefix "NonExistent" tag-prefix-alist)))

  (assert-equal 'Shift_key
                (uim '(intern-key-prefix "S" emacs-like-prefix-alist)))
  (assert-equal 'Control_key
                (uim '(intern-key-prefix "C" emacs-like-prefix-alist)))
  (assert-equal 'Alt_key
                (uim '(intern-key-prefix "A" emacs-like-prefix-alist)))
  (assert-equal 'Meta_key
                (uim '(intern-key-prefix "M" emacs-like-prefix-alist)))
  (assert-equal 'Super_key
                (uim '(intern-key-prefix "Z" emacs-like-prefix-alist)))
  (assert-equal 'Hyper_key
                (uim '(intern-key-prefix "H" emacs-like-prefix-alist)))
  (assert-equal 'IgnoreCase
                (uim '(intern-key-prefix "I" emacs-like-prefix-alist)))
  (assert-equal 'IgnoreShift
                (uim '(intern-key-prefix "J" emacs-like-prefix-alist)))
  (assert-equal 'IgnoreRegularShift
                (uim '(intern-key-prefix "K" emacs-like-prefix-alist)))
  (assert-false (uim-bool '(intern-key-prefix "N" emacs-like-prefix-alist)))
  #f)

(define (test-parse-tag-prefix-symbol)
  (assert-equal '(Shift_key)
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t"))))
  (assert-equal '(Control_key)
                (uim '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l"))))
  (assert-equal '(Alt_key)
                (uim '(parse-tag-prefix-symbol "" '("A" "l" "t"))))
  (assert-equal '(Meta_key)
                (uim '(parse-tag-prefix-symbol "" '("M" "e" "t" "a"))))
  (assert-equal '(Super_key)
                (uim '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r"))))
  (assert-equal '(Hyper_key)
                (uim '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r"))))
  (assert-equal (list (uim #f))
                (uim '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t"))))
  (assert-equal (list (uim #f))
                (uim '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T"))))
  (assert-equal (list (uim #f))
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t"))))

  ;; parsing is terminated at boundary char
  (assert-equal '(Shift_key ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">"))))
  (assert-equal '(Control_key ">")
                (uim '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">"))))
  (assert-equal '(Alt_key ">")
                (uim '(parse-tag-prefix-symbol "" '("A" "l" "t" ">"))))
  (assert-equal '(Meta_key ">")
                (uim '(parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">"))))
  (assert-equal '(Super_key ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">"))))
  (assert-equal '(Hyper_key ">")
                (uim '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">"))))
  (assert-equal (list (uim #f) ">")
                (uim '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">"))))
  (assert-equal (list (uim #f) ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">"))))
  (assert-equal (list (uim #f) ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">"))))

  ;; all chars that following boundary char remains
  (assert-equal '(Shift_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">" "<" "A" "l" "t" ">"))))
  (assert-equal '(Control_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">" "<" "A" "l" "t" ">"))))
  (assert-equal '(Alt_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("A" "l" "t" ">" "<" "A" "l" "t" ">"))))
  (assert-equal '(Meta_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">" "<" "A" "l" "t" ">"))))
  (assert-equal '(Super_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">" "<" "A" "l" "t" ">"))))
  (assert-equal '(Hyper_key ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">" "<" "A" "l" "t" ">"))))
  (assert-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">" "<" "A" "l" "t" ">"))))
  (assert-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">" "<" "A" "l" "t" ">"))))
  (assert-equal (list (uim #f) ">" "<" "A" "l" "t" ">")
                (uim '(parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">" "<" "A" "l" "t" ">"))))

  ;; nonexistent symbol is parsed as #f
  (assert-equal (list (uim #f))
                (uim '(parse-tag-prefix-symbol "" '("_" "F" "o" "o" "1"))))
  ;; tag-prefix symbol must be consist of alphanumeric or "_"
  (assert-equal (list (uim #f) "-" "F" "o" "o" "1")
                (uim '(parse-tag-prefix-symbol "" '("-" "F" "o" "o" "1"))))
  #f)

(define (test-parse-tag-prefix)
  (assert-equal '(Shift_key . "")
                (uim '(parse-tag-prefix "<Shift>")))
  (assert-equal '(Control_key . "")
                (uim '(parse-tag-prefix "<Control>")))
  (assert-equal '(Alt_key . "")
                (uim '(parse-tag-prefix "<Alt>")))
  (assert-equal '(Meta_key . "")
                (uim '(parse-tag-prefix "<Meta>")))
  (assert-equal '(Super_key . "")
                (uim '(parse-tag-prefix "<Super>")))
  (assert-equal '(Hyper_key . "")
                (uim '(parse-tag-prefix "<Hyper>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-tag-prefix "<NonExistent>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-tag-prefix "<SHIFT>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-tag-prefix "<Shiftt>")))

  (assert-equal '(Shift_key . "<Alt>")
                (uim '(parse-tag-prefix "<Shift><Alt>")))
  (assert-equal '(Control_key . "<Alt>")
                (uim '(parse-tag-prefix "<Control><Alt>")))
  (assert-equal '(Alt_key . "<Alt>")
                (uim '(parse-tag-prefix "<Alt><Alt>")))
  (assert-equal '(Meta_key . "<Alt>")
                (uim '(parse-tag-prefix "<Meta><Alt>")))
  (assert-equal '(Super_key . "<Alt>")
                (uim '(parse-tag-prefix "<Super><Alt>")))
  (assert-equal '(Hyper_key . "<Alt>")
                (uim '(parse-tag-prefix "<Hyper><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-tag-prefix "<NonExistent><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-tag-prefix "<SHIFT><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-tag-prefix "<Shiftt><Alt>")))

  (assert-equal '(Shift_key . "a")
                (uim '(parse-tag-prefix "<Shift>a")))
  (assert-equal '(Control_key . "a")
                (uim '(parse-tag-prefix "<Control>a")))
  (assert-equal '(Alt_key . "a")
                (uim '(parse-tag-prefix "<Alt>a")))
  (assert-equal '(Meta_key . "a")
                (uim '(parse-tag-prefix "<Meta>a")))
  (assert-equal '(Super_key . "a")
                (uim '(parse-tag-prefix "<Super>a")))
  (assert-equal '(Hyper_key . "a")
                (uim '(parse-tag-prefix "<Hyper>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-tag-prefix "<NonExistent>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-tag-prefix "<SHIFT>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-tag-prefix "<Shiftt>a")))
  #f)

(define (test-parse-emacs-like-prefix)
  (assert-equal '(Shift_key . "")
                (uim '(parse-emacs-like-prefix "S-")))
  (assert-equal '(Control_key . "")
                (uim '(parse-emacs-like-prefix "C-")))
  (assert-equal '(Alt_key . "")
                (uim '(parse-emacs-like-prefix "A-")))
  (assert-equal '(Meta_key . "")
                (uim '(parse-emacs-like-prefix "M-")))
  (assert-equal '(Super_key . "")
                (uim '(parse-emacs-like-prefix "Z-")))
  (assert-equal '(Hyper_key . "")
                (uim '(parse-emacs-like-prefix "H-")))
  (assert-equal (cons (uim #f) "N-")
                (uim '(parse-emacs-like-prefix "N-")))
  (assert-equal (cons (uim #f) "s-")
                (uim '(parse-emacs-like-prefix "s-")))
  (assert-equal (cons (uim #f) "SS-")
                (uim '(parse-emacs-like-prefix "SS-")))

  (assert-equal '(Shift_key . "A-")
                (uim '(parse-emacs-like-prefix "S-A-")))
  (assert-equal '(Control_key . "A-")
                (uim '(parse-emacs-like-prefix "C-A-")))
  (assert-equal '(Alt_key . "A-")
                (uim '(parse-emacs-like-prefix "A-A-")))
  (assert-equal '(Meta_key . "A-")
                (uim '(parse-emacs-like-prefix "M-A-")))
  (assert-equal '(Super_key . "A-")
                (uim '(parse-emacs-like-prefix "Z-A-")))
  (assert-equal '(Hyper_key . "A-")
                (uim '(parse-emacs-like-prefix "H-A-")))
  (assert-equal (cons (uim #f) "N-A-")
                (uim '(parse-emacs-like-prefix "N-A-")))
  (assert-equal (cons (uim #f) "s-A-")
                (uim '(parse-emacs-like-prefix "s-A-")))
  (assert-equal (cons (uim #f) "SS-A-")
                (uim '(parse-emacs-like-prefix "SS-A-")))

  (assert-equal '(Shift_key . "a")
                (uim '(parse-emacs-like-prefix "S-a")))
  (assert-equal '(Control_key . "a")
                (uim '(parse-emacs-like-prefix "C-a")))
  (assert-equal '(Alt_key . "a")
                (uim '(parse-emacs-like-prefix "A-a")))
  (assert-equal '(Meta_key . "a")
                (uim '(parse-emacs-like-prefix "M-a")))
  (assert-equal '(Super_key . "a")
                (uim '(parse-emacs-like-prefix "Z-a")))
  (assert-equal '(Hyper_key . "a")
                (uim '(parse-emacs-like-prefix "H-a")))
  (assert-equal (cons (uim #f) "N-a")
                (uim '(parse-emacs-like-prefix "N-a")))
  (assert-equal (cons (uim #f) "s-a")
                (uim '(parse-emacs-like-prefix "s-a")))
  (assert-equal (cons (uim #f) "SS-a")
                (uim '(parse-emacs-like-prefix "SS-a")))
  #f)

(define (test-parse-key-prefix)
  (assert-equal '(Shift_key . "")
                (uim '(parse-key-prefix "<Shift>")))
  (assert-equal '(Control_key . "")
                (uim '(parse-key-prefix "<Control>")))
  (assert-equal '(Alt_key . "")
                (uim '(parse-key-prefix "<Alt>")))
  (assert-equal '(Meta_key . "")
                (uim '(parse-key-prefix "<Meta>")))
  (assert-equal '(Super_key . "")
                (uim '(parse-key-prefix "<Super>")))
  (assert-equal '(Hyper_key . "")
                (uim '(parse-key-prefix "<Hyper>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-key-prefix "<NonExistent>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-key-prefix "<SHIFT>")))
  (assert-equal (cons (uim #f) "")
                (uim '(parse-key-prefix "<Shiftt>")))

  (assert-equal '(Shift_key . "<Alt>")
                (uim '(parse-key-prefix "<Shift><Alt>")))
  (assert-equal '(Control_key . "<Alt>")
                (uim '(parse-key-prefix "<Control><Alt>")))
  (assert-equal '(Alt_key . "<Alt>")
                (uim '(parse-key-prefix "<Alt><Alt>")))
  (assert-equal '(Meta_key . "<Alt>")
                (uim '(parse-key-prefix "<Meta><Alt>")))
  (assert-equal '(Super_key . "<Alt>")
                (uim '(parse-key-prefix "<Super><Alt>")))
  (assert-equal '(Hyper_key . "<Alt>")
                (uim '(parse-key-prefix "<Hyper><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-key-prefix "<NonExistent><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-key-prefix "<SHIFT><Alt>")))
  (assert-equal (cons (uim #f) "<Alt>")
                (uim '(parse-key-prefix "<Shiftt><Alt>")))

  (assert-equal '(Shift_key . "a")
                (uim '(parse-key-prefix "<Shift>a")))
  (assert-equal '(Control_key . "a")
                (uim '(parse-key-prefix "<Control>a")))
  (assert-equal '(Alt_key . "a")
                (uim '(parse-key-prefix "<Alt>a")))
  (assert-equal '(Meta_key . "a")
                (uim '(parse-key-prefix "<Meta>a")))
  (assert-equal '(Super_key . "a")
                (uim '(parse-key-prefix "<Super>a")))
  (assert-equal '(Hyper_key . "a")
                (uim '(parse-key-prefix "<Hyper>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-key-prefix "<NonExistent>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-key-prefix "<SHIFT>a")))
  (assert-equal (cons (uim #f) "a")
                (uim '(parse-key-prefix "<Shiftt>a")))

  (assert-equal '(Shift_key . "")
                (uim '(parse-key-prefix "S-")))
  (assert-equal '(Control_key . "")
                (uim '(parse-key-prefix "C-")))
  (assert-equal '(Alt_key . "")
                (uim '(parse-key-prefix "A-")))
  (assert-equal '(Meta_key . "")
                (uim '(parse-key-prefix "M-")))
  (assert-equal '(Super_key . "")
                (uim '(parse-key-prefix "Z-")))
  (assert-equal '(Hyper_key . "")
                (uim '(parse-key-prefix "H-")))
  (assert-equal (cons (uim #f) "N-")
                (uim '(parse-key-prefix "N-")))
  (assert-equal (cons (uim #f) "s-")
                (uim '(parse-key-prefix "s-")))
  (assert-equal (cons (uim #f) "SS-")
                (uim '(parse-key-prefix "SS-")))

  (assert-equal '(Shift_key . "A-")
                (uim '(parse-key-prefix "S-A-")))
  (assert-equal '(Control_key . "A-")
                (uim '(parse-key-prefix "C-A-")))
  (assert-equal '(Alt_key . "A-")
                (uim '(parse-key-prefix "A-A-")))
  (assert-equal '(Meta_key . "A-")
                (uim '(parse-key-prefix "M-A-")))
  (assert-equal '(Super_key . "A-")
                (uim '(parse-key-prefix "Z-A-")))
  (assert-equal '(Hyper_key . "A-")
                (uim '(parse-key-prefix "H-A-")))
  (assert-equal (cons (uim #f) "N-A-")
                (uim '(parse-key-prefix "N-A-")))
  (assert-equal (cons (uim #f) "s-A-")
                (uim '(parse-key-prefix "s-A-")))
  (assert-equal (cons (uim #f) "SS-A-")
                (uim '(parse-key-prefix "SS-A-")))

  (assert-equal '(Shift_key . "a")
                (uim '(parse-key-prefix "S-a")))
  (assert-equal '(Control_key . "a")
                (uim '(parse-key-prefix "C-a")))
  (assert-equal '(Alt_key . "a")
                (uim '(parse-key-prefix "A-a")))
  (assert-equal '(Meta_key . "a")
                (uim '(parse-key-prefix "M-a")))
  (assert-equal '(Super_key . "a")
                (uim '(parse-key-prefix "Z-a")))
  (assert-equal '(Hyper_key . "a")
                (uim '(parse-key-prefix "H-a")))
  (assert-equal (cons (uim #f) "N-a")
                (uim '(parse-key-prefix "N-a")))
  (assert-equal (cons (uim #f) "s-a")
                (uim '(parse-key-prefix "s-a")))
  (assert-equal (cons (uim #f) "SS-a")
                (uim '(parse-key-prefix "SS-a")))
  #f)

(define (test-parse-key-str)
  ;; single key
  (assert-equal (list "" () 32 0)
                (uim '(parse-key-str " " () 0 0)))
  (assert-equal (list "" () 33 0)
                (uim '(parse-key-str "!" () 0 0)))
  (assert-equal (list "" () 48 0)
                (uim '(parse-key-str "0" () 0 0)))
  (assert-equal (list "" () 65 0)
                (uim '(parse-key-str "A" () 0 0)))
  (assert-equal (list "" () 97 0)
                (uim '(parse-key-str "a" () 0 0)))
  (assert-equal (list "" () 'return 0)
                (uim '(parse-key-str "return" () 0 0)))

  ;; single key with single modifier
  (assert-equal (uim '(list "" () 32 test-shift-state))
                (uim '(parse-key-str "<Shift> " () 0 0)))
  (assert-equal (uim '(list "" () 33 test-shift-state))
                (uim '(parse-key-str "<Shift>!" () 0 0)))
  (assert-equal (uim '(list "" () 48 test-shift-state))
                (uim '(parse-key-str "<Shift>0" () 0 0)))
  (assert-equal (uim '(list "" () 65 test-shift-state))
                (uim '(parse-key-str "<Shift>A" () 0 0)))
  (assert-equal (uim '(list "" () 97 test-shift-state))
                (uim '(parse-key-str "<Shift>a" () 0 0)))
  (assert-equal (uim '(list "" () 'return test-shift-state))
                (uim '(parse-key-str "<Shift>return" () 0 0)))

  ;; single key with multiple modifiers
  (assert-equal (uim '(list "" () 32 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta> " () 0 0)))
  (assert-equal (uim '(list "" () 33 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta>!" () 0 0)))
  (assert-equal (uim '(list "" () 48 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta>0" () 0 0)))
  (assert-equal (uim '(list "" () 65 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta>A" () 0 0)))
  (assert-equal (uim '(list "" () 97 (+ test-shift-state
                                        test-control-state
                                        test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta>a" () 0 0)))
  (assert-equal (uim '(list "" () 'return (+ test-shift-state
                                             test-control-state
                                             test-meta-state)))
                (uim '(parse-key-str "<Shift><Control><Meta>return" () 0 0)))

  ;; single key with single translator
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift> " () 0 0)))))
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift>!" () 0 0)))))
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift>0" () 0 0)))))
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift>A" () 0 0)))))
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift>a" () 0 0)))))
  (assert-equal 1
                (uim '(length (cadr (parse-key-str "<IgnoreShift>return" () 0 0)))))

  ;; single key with multiple translators
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " () 0 0)))))
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" () 0 0)))))
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" () 0 0)))))
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" () 0 0)))))
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" () 0 0)))))
  (assert-equal 2
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" () 0 0)))))

  ;; single key with multiple translators (2)
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " (list (lambda () #t)) 0 0)))))
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" (list (lambda () #t)) 0 0)))))
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" (list (lambda () #t)) 0 0)))))
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" (list (lambda () #t)) 0 0)))))
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" (list (lambda () #t)) 0 0)))))
  (assert-equal 3
                (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" (list (lambda () #t)) 0 0)))))
  #f)

(provide "test/key/test-base")
