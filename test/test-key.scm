#!/usr/bin/env gosh

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

;; These tests are passed at revision 5329 (new repository)

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "test key"
  (setup
   (lambda ()
     (uim '(define test-shift-state (cdr (assq 'Shift_key key-state-alist))))
     (uim '(define test-control-state (cdr (assq 'Control_key key-state-alist))))
     (uim '(define test-alt-state (cdr (assq 'Alt_key key-state-alist))))
     (uim '(define test-meta-state (cdr (assq 'Meta_key key-state-alist))))
     (uim '(define test-super-state (cdr (assq 'Super_key key-state-alist))))
     (uim '(define test-hyper-state (cdr (assq 'Hyper_key key-state-alist))))))

  ("test intern-key-symbol"
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
   (assert-false (uim-bool '(intern-key-symbol "nonexistent"))))

  ("test modifier key mask predicates"
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
   (assert-false (uim-bool '(modifier-key-mask 0))))

  ("test modifier-key?"
   (assert-true  (uim-bool '(modifier-key? 'Shift_key 0)))
   (assert-true  (uim-bool '(modifier-key? 'Control_key 0)))
   (assert-true  (uim-bool '(modifier-key? 'Alt_key 0)))
   (assert-true  (uim-bool '(modifier-key? 'Meta_key 0)))
   (assert-true  (uim-bool '(modifier-key? 'Super_key 0)))
   (assert-true  (uim-bool '(modifier-key? 'Hyper_key 0)))
   (assert-false (uim-bool '(modifier-key? 'return 0)))
   (assert-false (uim-bool '(modifier-key? 'escape 0)))
   (assert-false (uim-bool '(modifier-key? 0 0))) ;; NUL
   (assert-false (uim-bool '(modifier-key? 97 0))) ;; a

   (assert-true  (uim-bool '(modifier-key? 'Shift_key test-shift-state)))
   (assert-true  (uim-bool '(modifier-key? 'Control_key test-shift-state)))
   (assert-true  (uim-bool '(modifier-key? 'Alt_key test-shift-state)))
   (assert-true  (uim-bool '(modifier-key? 'Meta_key test-shift-state)))
   (assert-true  (uim-bool '(modifier-key? 'Super_key test-shift-state)))
   (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-shift-state)))
   (assert-false (uim-bool '(modifier-key? 'return test-shift-state)))
   (assert-false (uim-bool '(modifier-key? 'escape test-shift-state)))
   (assert-false (uim-bool '(modifier-key? 0 test-shift-state))) ;; NUL
   (assert-false (uim-bool '(modifier-key? 97 test-shift-state)))	;; a

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
   (assert-false (uim-bool '(modifier-key? 97 test-super-state)))	;; a

   (assert-true  (uim-bool '(modifier-key? 'Shift_key test-hyper-state)))
   (assert-true  (uim-bool '(modifier-key? 'Control_key test-hyper-state)))
   (assert-true  (uim-bool '(modifier-key? 'Alt_key test-hyper-state)))
   (assert-true  (uim-bool '(modifier-key? 'Meta_key test-hyper-state)))
   (assert-true  (uim-bool '(modifier-key? 'Super_key test-hyper-state)))
   (assert-true  (uim-bool '(modifier-key? 'Hyper_key test-hyper-state)))
   (assert-false (uim-bool '(modifier-key? 'return test-hyper-state)))
   (assert-false (uim-bool '(modifier-key? 'escape test-hyper-state)))
   (assert-false (uim-bool '(modifier-key? 0 test-hyper-state)))   ;; NUL
   (assert-false (uim-bool '(modifier-key? 97 test-hyper-state)))) ;; a

  ("test translator-prefix?"
   (assert-true  (uim-bool '(translator-prefix? 'IgnoreCase)))
   (assert-true  (uim-bool '(translator-prefix? 'IgnoreShift)))
   (assert-true  (uim-bool '(translator-prefix? 'IgnoreRegularShift)))
   (assert-false (uim-bool '(translator-prefix? 'NonExistent))))

  ("test intern-key-prefix"
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
   (assert-false (uim-bool '(intern-key-prefix "N" emacs-like-prefix-alist))))

  ("test parse-tag-prefix-symbol"
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
		 (uim '(parse-tag-prefix-symbol "" '("-" "F" "o" "o" "1")))))

  ("test parse-tag-prefix"
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
		 (uim '(parse-tag-prefix "<Shiftt>a"))))

  ("test parse-emacs-like-prefix"
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
		 (uim '(parse-emacs-like-prefix "SS-a"))))

  ("test parse-key-prefix"
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
		 (uim '(parse-key-prefix "SS-a"))))

  ("test parse-key-str"
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
		 (uim '(length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" (list (lambda () #t)) 0 0)))))))

(define-uim-test-case "test key translators"
  (setup
   (lambda ()
     (uim '(define test-shift-state (cdr (assq 'Shift_key key-state-alist))))
     (uim '(define test-ignore-case #f))
     (uim '(define test-ignore-shift #f))
     (uim '(define test-ignore-regular-shift #f))
     (uim '(begin
	     (set! test-ignore-case (car (cadr (parse-key-str "<IgnoreCase>" () 0 0))))
	     (set! test-ignore-shift (car (cadr (parse-key-str "<IgnoreShift>" () 0 0))))
	     (set! test-ignore-regular-shift (car (cadr (parse-key-str "<IgnoreRegularShift>" () 0 0))))
	     #t))))

  ("test IgnoreCase translator"
   (assert-equal '(0   0) (uim '(test-ignore-case 0 0)))     ; NUL
   (assert-equal '(1   0) (uim '(test-ignore-case 1 0)))     ; SOH
   (assert-equal '(31  0) (uim '(test-ignore-case 31 0)))    ; US
   (assert-equal '(32  0) (uim '(test-ignore-case 32 0)))    ; SPACE
   (assert-equal '(33  0) (uim '(test-ignore-case 33 0)))    ; !
   (assert-equal '(47  0) (uim '(test-ignore-case 47 0)))    ; /
   (assert-equal '(48  0) (uim '(test-ignore-case 48 0)))    ; 0
   (assert-equal '(57  0) (uim '(test-ignore-case 57 0)))    ; 9
   (assert-equal '(58  0) (uim '(test-ignore-case 58 0)))    ; :
   (assert-equal '(64  0) (uim '(test-ignore-case 64 0)))    ; @
   (assert-equal '(97  0) (uim '(test-ignore-case 65 0)))    ; A
   (assert-equal '(122 0) (uim '(test-ignore-case 90 0)))    ; Z
   (assert-equal '(91  0) (uim '(test-ignore-case 91 0)))    ; [
   (assert-equal '(96  0) (uim '(test-ignore-case 96 0)))    ; `
   (assert-equal '(97  0) (uim '(test-ignore-case 97 0)))    ; a
   (assert-equal '(122 0) (uim '(test-ignore-case 122 0)))   ; z
   (assert-equal '(123 0) (uim '(test-ignore-case 123 0)))   ; {
   (assert-equal '(126 0) (uim '(test-ignore-case 126 0)))   ; ~
   (assert-equal '(127 0) (uim '(test-ignore-case 127 0))))  ; DEL

  ("test IgnoreShift translator"
   (assert-equal '(0 0) (uim '(test-ignore-shift 0 test-shift-state)))    ; NUL
   (assert-equal '(1 0) (uim '(test-ignore-shift 1 test-shift-state)))    ; SOH
   (assert-equal '(31 0) (uim '(test-ignore-shift 31 test-shift-state)))  ; US
   (assert-equal '(32 0) (uim '(test-ignore-shift 32 test-shift-state)))  ; SPACE
   (assert-equal '(33 0) (uim '(test-ignore-shift 33 test-shift-state)))  ; !
   (assert-equal '(47 0) (uim '(test-ignore-shift 47 test-shift-state)))  ; /
   (assert-equal '(48 0) (uim '(test-ignore-shift 48 test-shift-state)))  ; 0
   (assert-equal '(57 0) (uim '(test-ignore-shift 57 test-shift-state)))  ; 9
   (assert-equal '(58 0) (uim '(test-ignore-shift 58 test-shift-state)))  ; :
   (assert-equal '(64 0) (uim '(test-ignore-shift 64 test-shift-state)))  ; @
   (assert-equal '(65 0) (uim '(test-ignore-shift 65 test-shift-state)))  ; A
   (assert-equal '(90 0) (uim '(test-ignore-shift 90 test-shift-state)))  ; Z
   (assert-equal '(91 0) (uim '(test-ignore-shift 91 test-shift-state)))  ; [
   (assert-equal '(96 0) (uim '(test-ignore-shift 96 test-shift-state)))  ; `
   (assert-equal '(97 0) (uim '(test-ignore-shift 97 test-shift-state)))  ; a
   (assert-equal '(122 0) (uim '(test-ignore-shift 122 test-shift-state))); z
   (assert-equal '(123 0) (uim '(test-ignore-shift 123 test-shift-state))); {
   (assert-equal '(126 0) (uim '(test-ignore-shift 126 test-shift-state))); ~
   (assert-equal '(127 0) (uim '(test-ignore-shift 127 test-shift-state))) ; DEL

   (assert-equal '(0   0) (uim '(test-ignore-shift 0 0)))     ; NUL
   (assert-equal '(1   0) (uim '(test-ignore-shift 1 0)))     ; SOH
   (assert-equal '(31  0) (uim '(test-ignore-shift 31 0)))    ; US
   (assert-equal '(32  0) (uim '(test-ignore-shift 32 0)))    ; SPACE
   (assert-equal '(33  0) (uim '(test-ignore-shift 33 0)))    ; !
   (assert-equal '(47  0) (uim '(test-ignore-shift 47 0)))    ; /
   (assert-equal '(48  0) (uim '(test-ignore-shift 48 0)))    ; 0
   (assert-equal '(57  0) (uim '(test-ignore-shift 57 0)))    ; 9
   (assert-equal '(58  0) (uim '(test-ignore-shift 58 0)))    ; :
   (assert-equal '(64  0) (uim '(test-ignore-shift 64 0)))    ; @
   (assert-equal '(65  0) (uim '(test-ignore-shift 65 0)))    ; A
   (assert-equal '(90  0) (uim '(test-ignore-shift 90 0)))    ; Z
   (assert-equal '(91  0) (uim '(test-ignore-shift 91 0)))    ; [
   (assert-equal '(96  0) (uim '(test-ignore-shift 96 0)))    ; `
   (assert-equal '(97  0) (uim '(test-ignore-shift 97 0)))    ; a
   (assert-equal '(122 0) (uim '(test-ignore-shift 122 0)))   ; z
   (assert-equal '(123 0) (uim '(test-ignore-shift 123 0)))   ; {
   (assert-equal '(126 0) (uim '(test-ignore-shift 126 0)))   ; ~
   (assert-equal '(127 0) (uim '(test-ignore-shift 127 0))))  ; DEL

  ("test IgnoreRegularShift translator"
   (assert-equal (uim '(list 0 test-shift-state))
		 (uim '(test-ignore-regular-shift 0 test-shift-state)))   ; NUL
   (assert-equal (uim '(list 1 test-shift-state))
		 (uim '(test-ignore-regular-shift 1 test-shift-state)))   ; SOH
   (assert-equal (uim '(list 31 test-shift-state))
		 (uim '(test-ignore-regular-shift 31 test-shift-state)))  ; US
   (assert-equal (uim '(list 32 test-shift-state))
		 (uim '(test-ignore-regular-shift 32 test-shift-state)))  ; SPACE
   (assert-equal '(33 0)
		 (uim '(test-ignore-regular-shift 33 test-shift-state)))  ; !
   (assert-equal '(47 0)
		 (uim '(test-ignore-regular-shift 47 test-shift-state)))  ; /
   (assert-equal '(48 0)
		 (uim '(test-ignore-regular-shift 48 test-shift-state)))  ; 0
   (assert-equal '(57 0)
		 (uim '(test-ignore-regular-shift 57 test-shift-state)))  ; 9
   (assert-equal '(58 0)
		 (uim '(test-ignore-regular-shift 58 test-shift-state)))  ; :
   (assert-equal '(64 0)
		 (uim '(test-ignore-regular-shift 64 test-shift-state)))  ; @
   (assert-equal '(65 0)
		 (uim '(test-ignore-regular-shift 65 test-shift-state)))  ; A
   (assert-equal '(90 0)
		 (uim '(test-ignore-regular-shift 90 test-shift-state)))  ; Z
   (assert-equal '(91 0)
		 (uim '(test-ignore-regular-shift 91 test-shift-state)))  ; [
   (assert-equal '(96 0)
		 (uim '(test-ignore-regular-shift 96 test-shift-state)))  ; `
   (assert-equal '(97 0)
		 (uim '(test-ignore-regular-shift 97 test-shift-state)))  ; a
   (assert-equal '(122 0)
		 (uim '(test-ignore-regular-shift 122 test-shift-state))) ; z
   (assert-equal '(123 0)
		 (uim '(test-ignore-regular-shift 123 test-shift-state))) ; {
   (assert-equal '(126 0)
		 (uim '(test-ignore-regular-shift 126 test-shift-state))) ; ~
   (assert-equal (uim '(list 127 test-shift-state))
		 (uim '(test-ignore-regular-shift 127 test-shift-state))) ; DEL

   (assert-equal '(0   0) (uim '(test-ignore-regular-shift 0 0)))     ; NUL
   (assert-equal '(1   0) (uim '(test-ignore-regular-shift 1 0)))     ; SOH
   (assert-equal '(31  0) (uim '(test-ignore-regular-shift 31 0)))    ; US
   (assert-equal '(32  0) (uim '(test-ignore-regular-shift 32 0)))    ; SPACE
   (assert-equal '(33  0) (uim '(test-ignore-regular-shift 33 0)))    ; !
   (assert-equal '(47  0) (uim '(test-ignore-regular-shift 47 0)))    ; /
   (assert-equal '(48  0) (uim '(test-ignore-regular-shift 48 0)))    ; 0
   (assert-equal '(57  0) (uim '(test-ignore-regular-shift 57 0)))    ; 9
   (assert-equal '(58  0) (uim '(test-ignore-regular-shift 58 0)))    ; :
   (assert-equal '(64  0) (uim '(test-ignore-regular-shift 64 0)))    ; @
   (assert-equal '(65  0) (uim '(test-ignore-regular-shift 65 0)))    ; A
   (assert-equal '(90  0) (uim '(test-ignore-regular-shift 90 0)))    ; Z
   (assert-equal '(91  0) (uim '(test-ignore-regular-shift 91 0)))    ; [
   (assert-equal '(96  0) (uim '(test-ignore-regular-shift 96 0)))    ; `
   (assert-equal '(97  0) (uim '(test-ignore-regular-shift 97 0)))    ; a
   (assert-equal '(122 0) (uim '(test-ignore-regular-shift 122 0)))   ; z
   (assert-equal '(123 0) (uim '(test-ignore-regular-shift 123 0)))   ; {
   (assert-equal '(126 0) (uim '(test-ignore-regular-shift 126 0)))   ; ~
   (assert-equal '(127 0) (uim '(test-ignore-regular-shift 127 0))))  ; DEL

  ("test apply-translators"
   ;; apply single translator
   (assert-equal (uim '(list () 0 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  0 test-shift-state)))   ; NUL
   (assert-equal (uim '(list () 1 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  1 test-shift-state)))   ; SOH
   (assert-equal (uim '(list () 31 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  31 test-shift-state)))  ; US
   (assert-equal (uim '(list () 32 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  32 test-shift-state)))  ; SPACE
   (assert-equal '(() 33 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  33 test-shift-state)))  ; !
   (assert-equal '(() 47 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  47 test-shift-state)))  ; /
   (assert-equal '(() 48 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  48 test-shift-state)))  ; 0
   (assert-equal '(() 57 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  57 test-shift-state)))  ; 9
   (assert-equal '(() 58 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  58 test-shift-state)))  ; :
   (assert-equal '(() 64 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  64 test-shift-state)))  ; @
   (assert-equal '(() 65 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  65 test-shift-state)))  ; A
   (assert-equal '(() 90 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  90 test-shift-state)))  ; Z
   (assert-equal '(() 91 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  91 test-shift-state)))  ; [
   (assert-equal '(() 96 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  96 test-shift-state)))  ; `
   (assert-equal '(() 97 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  97 test-shift-state)))  ; a
   (assert-equal '(() 122 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  122 test-shift-state))) ; z
   (assert-equal '(() 123 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  123 test-shift-state))) ; {
   (assert-equal '(() 126 0)
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  126 test-shift-state))) ; ~
   (assert-equal (uim '(list () 127 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift)
					  127 test-shift-state))) ; DEL

   ;; apply multiple translator
   (assert-equal (uim '(list () 0 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  0 test-shift-state)))   ; NUL
   (assert-equal (uim '(list () 1 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  1 test-shift-state)))   ; SOH
   (assert-equal (uim '(list () 31 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  31 test-shift-state)))  ; US
   (assert-equal (uim '(list () 32 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  32 test-shift-state)))  ; SPACE
   (assert-equal '(() 33 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  33 test-shift-state)))  ; !
   (assert-equal '(() 47 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  47 test-shift-state)))  ; /
   (assert-equal '(() 48 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  48 test-shift-state)))  ; 0
   (assert-equal '(() 57 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  57 test-shift-state)))  ; 9
   (assert-equal '(() 58 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  58 test-shift-state)))  ; :
   (assert-equal '(() 64 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  64 test-shift-state)))  ; @
   (assert-equal '(() 97 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  65 test-shift-state)))  ; A
   (assert-equal '(() 122 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  90 test-shift-state)))  ; Z
   (assert-equal '(() 91 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  91 test-shift-state)))  ; [
   (assert-equal '(() 96 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  96 test-shift-state)))  ; `
   (assert-equal '(() 97 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  97 test-shift-state)))  ; a
   (assert-equal '(() 122 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  122 test-shift-state))) ; z
   (assert-equal '(() 123 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  123 test-shift-state))) ; {
   (assert-equal '(() 126 0)
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  126 test-shift-state))) ; ~
   (assert-equal (uim '(list () 127 test-shift-state))
		 (uim '(apply-translators (list test-ignore-regular-shift
						test-ignore-case)
					  127 test-shift-state))))) ; DEL

(define-uim-test-case "test key key-predicates"
  (setup
   (lambda ()
     (uim '(define test-shift-state (cdr (assq 'Shift_key key-state-alist))))
     (uim '(define test-control-state (cdr (assq 'Control_key key-state-alist))))
     (uim '(define test-alt-state (cdr (assq 'Alt_key key-state-alist))))
     (uim '(define test-meta-state (cdr (assq 'Meta_key key-state-alist))))
     (uim '(define test-super-state (cdr (assq 'Super_key key-state-alist))))
     (uim '(define test-hyper-state (cdr (assq 'Hyper_key key-state-alist))))))

  ("test make-single-key-predicate"
   ;; null key-str matches with nothing
   (assert-false (uim-bool '((make-single-key-predicate "")
			     0 0)))    ; NUL
   (assert-false (uim-bool '((make-single-key-predicate "")
			     1 0)))    ; SOH
   (assert-false (uim-bool '((make-single-key-predicate "")
			     31 0)))   ; US
   (assert-false (uim-bool '((make-single-key-predicate "")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "")
			     127 0)))  ; DEL
   (assert-false (uim-bool '((make-single-key-predicate "")
			     'return 0)))  ; return
   ;; space
   (assert-true  (uim-bool '((make-single-key-predicate " ")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate " ")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate " ")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate " ")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate " ")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate " ")
			     'return 0)))  ; return
   ;; !
   (assert-false (uim-bool '((make-single-key-predicate "!")
			     32 0)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "!")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "!")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "!")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "!")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "!")
			     'return 0)))  ; return
   ;; 0
   (assert-false (uim-bool '((make-single-key-predicate "0")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "0")
			     33 0)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "0")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "0")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "0")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "0")
			     'return 0)))  ; return
   ;; A
   (assert-false (uim-bool '((make-single-key-predicate "A")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "A")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "A")
			     48 0)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "A")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "A")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "A")
			     'return 0)))  ; return
   ;; a
   (assert-false (uim-bool '((make-single-key-predicate "a")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "a")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "a")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "a")
			     65 0)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "a")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "a")
			     'return 0)))  ; return
   ;; return
   (assert-false (uim-bool '((make-single-key-predicate "return")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "return")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "return")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "return")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "return")
			     97 0)))   ; a
   (assert-true  (uim-bool '((make-single-key-predicate "return")
			     'return 0)))  ; return

   ;; single key with single modifier (success)
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift> ")
			     32 test-shift-state)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift>!")
			     33 test-shift-state)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift>0")
			     48 test-shift-state)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift>A")
			     65 test-shift-state)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift>a")
			     97 test-shift-state)))   ; a
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift>return")
			     'return test-shift-state)))  ; return
   ;; single key with single modifier (fail)
   (assert-false (uim-bool '((make-single-key-predicate "<Shift> ")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>!")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>0")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>A")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>a")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>return")
			     'return 0)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate "<Shift> ")
			     32 test-control-state)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>!")
			     33 test-control-state)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>0")
			     48 test-control-state)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>A")
			     65 test-control-state)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>a")
			     97 test-control-state)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<Shift>return")
			     'return test-control-state)))  ; return

   ;; single key with multiple modifier (success)
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control> ")
			     32 (+ test-shift-state   ; SPACE
				   test-control-state))))
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control>!")
			     33 (+ test-shift-state   ; !
				   test-control-state))))
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control>0")
			     48 (+ test-shift-state   ; 0
				   test-control-state))))
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control>A")
			     65 (+ test-shift-state   ; A
				   test-control-state))))
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control>a")
			     97 (+ test-shift-state   ; a
				   test-control-state))))
   (assert-true  (uim-bool '((make-single-key-predicate "<Shift><Control>return")
			     'return (+ test-shift-state  ; return
					test-control-state))))
   ;; single key with multiple modifier (fail)
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control> ")
			     32 0)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>!")
			     33 0)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>0")
			     48 0)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>A")
			     65 0)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>a")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>return")
			     'return 0)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control> ")
			     32 test-control-state)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>!")
			     33 test-control-state)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>0")
			     48 test-control-state)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>A")
			     65 test-control-state)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>a")
			     97 test-control-state)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<Shift><Control>return")
			     'return test-control-state)))  ; return

   ;; single key with single translator
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift> ")
			     32 test-shift-state)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>!")
			     33 test-shift-state)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>0")
			     48 test-shift-state)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>A")
			     65 test-shift-state)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>a")
			     97 test-shift-state)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>return")
			     'return test-shift-state)))  ; return
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift> ")
			     32 0)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>!")
			     33 0)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>0")
			     48 0)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>A")
			     65 0)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>a")
			     97 0)))   ; a
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>return")
			     'return 0)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift> ")
			     32 test-control-state)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>!")
			     33 test-control-state)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>0")
			     48 test-control-state)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>A")
			     65 test-control-state)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>a")
			     97 test-control-state)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift>return")
			     'return test-control-state)))  ; return

   ;; single key with single translator and single modifier
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
			     32 test-shift-state)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
			     33 test-shift-state)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
			     48 test-shift-state)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
			     65 test-shift-state)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
			     97 test-shift-state)))   ; a
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
			     32 0)))   ; SPACE
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
			     33 0)))   ; !
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
			     48 0)))   ; 0
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
			     65 0)))   ; A
   (assert-true  (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
			     97 0)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
			     'return 0)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
			     32 test-control-state)))   ; SPACE
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
			     33 test-control-state)))   ; !
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
			     48 test-control-state)))   ; 0
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
			     65 test-control-state)))   ; A
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
			     97 test-control-state)))   ; a
   (assert-false (uim-bool '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
			     'return test-control-state)))  ; return

   (uim '(define test-return-key? #f))
   (uim '(begin (set! test-return-key? (make-single-key-predicate
					"<IgnoreRegularShift><Shift>return"))
		#t)) ;; supress closure result

   ;; make up from preexisting predicate
   (assert-true  (uim-bool '((make-single-key-predicate test-return-key?)
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate test-return-key?)
			     'return 0)))  ; return

   ;; make up from preexisting predicate symbol
   (assert-true  (uim-bool '((make-single-key-predicate 'test-return-key?)
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-single-key-predicate 'test-return-key?)
			     'return 0))))  ; return

  ("test make-key-predicate"
   (uim '(define test-return-key? #f))
   (uim '(begin (set! test-return-key? (make-single-key-predicate
					"<IgnoreRegularShift><Shift>return"))
		#t)) ;; supress closure result

   (uim '(define test-a-key? #f))
   (uim '(begin (set! test-a-key? (make-single-key-predicate "a"))
		#t)) ;; supress closure result

   ;; make up from key-str
   (assert-true  (uim-bool '((make-key-predicate
			      "<IgnoreRegularShift><Shift>return")
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate
			      "<IgnoreRegularShift><Shift>return")
			     'return 0)))  ; return

   ;; make up from preexisting predicate
   (assert-true  (uim-bool '((make-key-predicate test-return-key?)
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate test-return-key?)
			     'return 0)))  ; return

   ;; make up from preexisting predicate symbol
   (assert-true  (uim-bool '((make-key-predicate 'test-return-key?)
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate 'test-return-key?)
			     'return 0)))  ; return

   ;; make up from key-str in a list
   (assert-true  (uim-bool '((make-key-predicate
			      '("<IgnoreRegularShift><Shift>return"))
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate
			      '("<IgnoreRegularShift><Shift>return"))
			     'return 0)))  ; return

   ;; make up from preexisting predicate in a list
   (assert-true  (uim-bool '((make-key-predicate (list test-return-key?))
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate (list test-return-key?))
			     'return 0)))  ; return

   ;; make up from preexisting predicate symbol in a list
   (assert-true  (uim-bool '((make-key-predicate '(test-return-key?))
			     'return test-shift-state)))  ; return
   (assert-false (uim-bool '((make-key-predicate '(test-return-key?))
			     'return 0)))  ; return

   ;; make up from or'ed predicates (success)
   (assert-true  (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     'return test-shift-state)))  ; return
   (assert-true  (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     97 0)))  ; a
   (assert-true  (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     98 test-control-state)))  ; b
   ;; make up from or'ed predicates (fail)
   (assert-false (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     'return 0)))  ; return
   (assert-false (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     97 test-shift-state)))  ; a
   (assert-false (uim-bool '((make-key-predicate (list
						  test-a-key?
						  'test-return-key?
						  "<Control>b"))
			     98 0))))  ; b

  ("test modify-key-strs-implicitly"
   (assert-equal "<IgnoreRegularShift>return"
		 (uim '(modify-key-strs-implicitly "return")))
   (assert-equal '("<IgnoreRegularShift>return")
		 (uim '(modify-key-strs-implicitly '("return"))))
   (assert-equal '("<IgnoreRegularShift>return"
		   "<IgnoreRegularShift>a"
		   "<IgnoreRegularShift><Shift>b")
		 (uim '(modify-key-strs-implicitly '("return" "a" "<Shift>b"))))
   (assert-equal '("<IgnoreRegularShift>return"
		   "<IgnoreRegularShift>a"
		   foo
		   "<IgnoreRegularShift><Shift>b")
		 (uim '(modify-key-strs-implicitly '("return"
						     "a"
						     foo
						     "<Shift>b")))))

  ("test define-key-internal"
   (assert-false (uim-bool '(symbol-bound? 'test-foo-key?)))
   (uim '(begin
	   (define-key-internal 'test-foo-key? "<Shift>return")
	   (define-key-internal 'test-bar-key? "<Shift>a")
	   (define-key-internal 'test-baz-key? "b")
	   (define test-explicit-bar-key? (make-key-predicate "<Shift>a"))
	   (define-key-internal 'test-quux-key? (list
						 test-foo-key?
						 'test-explicit-bar-key?
						 "b"))
	   #t))
   (assert-true  (uim-bool '(symbol-bound? 'test-foo-key?)))
   ;; implicit <IgnoreRegularShift> not affects to 'return
   (assert-true  (uim-bool '(test-foo-key? 'return test-shift-state)))
   (assert-false (uim-bool '(test-foo-key? 'return 0)))
   (assert-false (uim-bool '(test-foo-key? 'return test-control-state)))
   ;; always matches by implicit <IgnoreRegularShift>
   (assert-true  (uim-bool '(test-bar-key? 97 test-shift-state)))
   (assert-true  (uim-bool '(test-bar-key? 97 0)))
   (assert-false (uim-bool '(test-bar-key? 97 test-control-state)))
   ;; always matches by implicit <IgnoreRegularShift>
   (assert-true  (uim-bool '(test-baz-key? 98 test-shift-state)))
   (assert-true  (uim-bool '(test-baz-key? 98 0)))
   (assert-false (uim-bool '(test-baz-key? 98 test-control-state)))

   ;; implicit <IgnoreRegularShift> not affects to test-foo-key?
   (assert-true  (uim-bool '(test-quux-key? 'return test-shift-state)))
   (assert-false (uim-bool '(test-quux-key? 'return 0)))
   (assert-false (uim-bool '(test-quux-key? 'return test-control-state)))
   ;;  implicit <IgnoreRegularShift> not affects to 'test-explicit-bar-key?
   (assert-true  (uim-bool '(test-quux-key? 97 test-shift-state)))
   (assert-false (uim-bool '(test-quux-key? 97 0)))
   (assert-false (uim-bool '(test-quux-key? 97 test-control-state)))
   ;; always matches by implicit <IgnoreRegularShift>
   (assert-true  (uim-bool '(test-quux-key? 98 test-shift-state)))
   (assert-true  (uim-bool '(test-quux-key? 98 0)))
   (assert-false (uim-bool '(test-quux-key? 98 test-control-state))))

  ("test valid-key-str?"
   ;; null key fails
   (assert-false (uim-bool '(valid-key-str? "")))

   ;; invalid key definitions
   (assert-false  (uim-bool '(valid-key-str? "nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "<Shift>nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "<Nonexistent>a")))
   (assert-false  (uim-bool '(valid-key-str? "<Nonexistent>nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "<Nonexistent><Shift>a")))
   (assert-false  (uim-bool '(valid-key-str? "<Nonexistent><Shift>nonexistent")))

   (assert-false  (uim-bool '(valid-key-str? "nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "S-nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "N-a")))
   (assert-false  (uim-bool '(valid-key-str? "N-nonexistent")))
   (assert-false  (uim-bool '(valid-key-str? "N-S-a")))
   (assert-false  (uim-bool '(valid-key-str? "N-S-nonexistent")))

   ;; single key
   (assert-true  (uim-bool '(valid-key-str? " ")))
   (assert-true  (uim-bool '(valid-key-str? "!")))
   (assert-true  (uim-bool '(valid-key-str? "0")))
   (assert-true  (uim-bool '(valid-key-str? "A")))
   (assert-true  (uim-bool '(valid-key-str? "a")))
   (assert-true  (uim-bool '(valid-key-str? "return")))

   ;; single key with single modifier
   (assert-true  (uim-bool '(valid-key-str? "<Shift> ")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift>!")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift>0")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift>A")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift>a")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift>return")))
   (assert-true  (uim-bool '(valid-key-str? "<Control>return")))
   (assert-true  (uim-bool '(valid-key-str? "<Alt>return")))
   (assert-true  (uim-bool '(valid-key-str? "<Meta>return")))
   (assert-true  (uim-bool '(valid-key-str? "<Super>return")))
   (assert-true  (uim-bool '(valid-key-str? "<Hyper>return")))

   (assert-true  (uim-bool '(valid-key-str? "S- ")))
   (assert-true  (uim-bool '(valid-key-str? "S-!")))
   (assert-true  (uim-bool '(valid-key-str? "S-0")))
   (assert-true  (uim-bool '(valid-key-str? "S-A")))
   (assert-true  (uim-bool '(valid-key-str? "S-a")))
   (assert-true  (uim-bool '(valid-key-str? "S-return")))
   (assert-true  (uim-bool '(valid-key-str? "C-return")))
   (assert-true  (uim-bool '(valid-key-str? "A-return")))
   (assert-true  (uim-bool '(valid-key-str? "M-return")))
   (assert-true  (uim-bool '(valid-key-str? "S-return")))
   (assert-true  (uim-bool '(valid-key-str? "H-return")))

   ;; single key with multiple modifiers
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta> ")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta>!")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta>0")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta>A")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta>a")))
   (assert-true  (uim-bool '(valid-key-str? "<Shift><Control><Meta>return")))

   (assert-true  (uim-bool '(valid-key-str? "S-C-M- ")))
   (assert-true  (uim-bool '(valid-key-str? "S-C-M-!")))
   (assert-true  (uim-bool '(valid-key-str? "S-C-M-0")))
   (assert-true  (uim-bool '(valid-key-str? "S-C-M-A")))
   (assert-true  (uim-bool '(valid-key-str? "S-C-M-a")))
   (assert-true  (uim-bool '(valid-key-str? "S-C-M-return")))

   ;; single key with single translator
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift> ")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift>!")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift>0")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift>A")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift>a")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift>return")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreRegularShift>return")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreCase>return")))

   (assert-true  (uim-bool '(valid-key-str? "J- ")))
   (assert-true  (uim-bool '(valid-key-str? "J-!")))
   (assert-true  (uim-bool '(valid-key-str? "J-0")))
   (assert-true  (uim-bool '(valid-key-str? "J-A")))
   (assert-true  (uim-bool '(valid-key-str? "J-a")))
   (assert-true  (uim-bool '(valid-key-str? "J-return")))
   (assert-true  (uim-bool '(valid-key-str? "K-return")))
   (assert-true  (uim-bool '(valid-key-str? "I-return")))

   ;; single key with multiple translators
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase> ")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase>!")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase>0")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase>A")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase>a")))
   (assert-true  (uim-bool '(valid-key-str? "<IgnoreShift><IgnoreCase>return")))

   (assert-true  (uim-bool '(valid-key-str? "J-I- ")))
   (assert-true  (uim-bool '(valid-key-str? "J-I-!")))
   (assert-true  (uim-bool '(valid-key-str? "J-I-0")))
   (assert-true  (uim-bool '(valid-key-str? "J-I-A")))
   (assert-true  (uim-bool '(valid-key-str? "J-I-a")))
   (assert-true  (uim-bool '(valid-key-str? "J-I-return"))))

  ("test valid-strict-key-str?"
   ;; null key fails
   (assert-false (uim-bool '(valid-strict-key-str? "")))

   ;; invalid key definitions
   (assert-false  (uim-bool '(valid-strict-key-str? "nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "<Shift>nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "<Nonexistent>a")))
   (assert-false  (uim-bool '(valid-strict-key-str? "<Nonexistent>nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "<Nonexistent><Shift>a")))
   (assert-false  (uim-bool '(valid-strict-key-str? "<Nonexistent><Shift>nonexistent")))

   (assert-false  (uim-bool '(valid-strict-key-str? "nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "S-nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "N-a")))
   (assert-false  (uim-bool '(valid-strict-key-str? "N-nonexistent")))
   (assert-false  (uim-bool '(valid-strict-key-str? "N-S-a")))
   (assert-false  (uim-bool '(valid-strict-key-str? "N-S-nonexistent")))

   ;; single key
   (assert-true  (uim-bool '(valid-strict-key-str? " ")))
   (assert-true  (uim-bool '(valid-strict-key-str? "!")))
   (assert-true  (uim-bool '(valid-strict-key-str? "0")))
   (assert-true  (uim-bool '(valid-strict-key-str? "A")))
   (assert-true  (uim-bool '(valid-strict-key-str? "a")))
   (assert-true  (uim-bool '(valid-strict-key-str? "return")))

   ;; single key with single modifier
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift> ")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>!")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>0")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>A")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>a")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift>return")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Control>return")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Alt>return")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Meta>return")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Super>return")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Hyper>return")))

   (assert-false (uim-bool '(valid-strict-key-str? "S- ")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-!")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-0")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-A")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-a")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "C-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "A-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "M-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "H-return")))

   ;; single key with multiple modifiers
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta> ")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>!")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>0")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>A")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>a")))
   (assert-true  (uim-bool '(valid-strict-key-str? "<Shift><Control><Meta>return")))

   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M- ")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M-!")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M-0")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M-A")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M-a")))
   (assert-false (uim-bool '(valid-strict-key-str? "S-C-M-return")))

   ;; single key with single translator
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift> ")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>!")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>0")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>A")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>a")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift>return")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreRegularShift>return")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreCase>return")))

   (assert-false (uim-bool '(valid-strict-key-str? "J- ")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-!")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-0")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-A")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-a")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "K-return")))
   (assert-false (uim-bool '(valid-strict-key-str? "I-return")))

   ;; single key with multiple translators
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase> ")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>!")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>0")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>A")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>a")))
   (assert-false (uim-bool '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>return")))

   (assert-false (uim-bool '(valid-strict-key-str? "J-I- ")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-I-!")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-I-0")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-I-A")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-I-a")))
   (assert-false (uim-bool '(valid-strict-key-str? "J-I-return")))))
