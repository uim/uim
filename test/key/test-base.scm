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

(define test-shift-state (cdr (assq 'Shift_key key-state-alist)))
(define test-control-state (cdr (assq 'Control_key key-state-alist)))
(define test-alt-state (cdr (assq 'Alt_key key-state-alist)))
(define test-meta-state (cdr (assq 'Meta_key key-state-alist)))
(define test-super-state (cdr (assq 'Super_key key-state-alist)))
(define test-hyper-state (cdr (assq 'Hyper_key key-state-alist)))

(test-begin "intern-key-symbol")
(test-equal 'backspace
            (intern-key-symbol "backspace"))
(test-equal 'delete
            (intern-key-symbol "delete"))
(test-equal 'zenkaku-hankaku
            (intern-key-symbol "zenkaku-hankaku"))
(test-equal 'F10
            (intern-key-symbol "F10"))
(test-equal 'Private9
            (intern-key-symbol "Private9"))
(test-equal 'Hyper_key
            (intern-key-symbol "Hyper_key"))
(test-false (intern-key-symbol "nonexistent"))
(test-end)

(test-begin "modifier-key-mask-predicates")
(test-eq #t (shift-key-mask test-shift-state))
(test-false (shift-key-mask test-control-state))
(test-false (shift-key-mask test-alt-state))
(test-false (shift-key-mask test-meta-state))
(test-false (shift-key-mask test-super-state))
(test-false (shift-key-mask test-hyper-state))
(test-false (shift-key-mask 0))

(test-false (control-key-mask test-shift-state))
(test-eq #t (control-key-mask test-control-state))
(test-false (control-key-mask test-alt-state))
(test-false (control-key-mask test-meta-state))
(test-false (control-key-mask test-super-state))
(test-false (control-key-mask test-hyper-state))
(test-false (control-key-mask 0))

(test-false (alt-key-mask test-shift-state))
(test-false (alt-key-mask test-control-state))
(test-eq #t (alt-key-mask test-alt-state))
(test-false (alt-key-mask test-meta-state))
(test-false (alt-key-mask test-super-state))
(test-false (alt-key-mask test-hyper-state))
(test-false (alt-key-mask 0))

(test-false (meta-key-mask test-shift-state))
(test-false (meta-key-mask test-control-state))
(test-false (meta-key-mask test-alt-state))
(test-eq #t (meta-key-mask test-meta-state))
(test-false (meta-key-mask test-super-state))
(test-false (meta-key-mask test-hyper-state))
(test-false (meta-key-mask 0))

(test-false (super-key-mask test-shift-state))
(test-false (super-key-mask test-control-state))
(test-false (super-key-mask test-alt-state))
(test-false (super-key-mask test-meta-state))
(test-eq #t (super-key-mask test-super-state))
(test-false (super-key-mask test-hyper-state))
(test-false (super-key-mask 0))

(test-false (hyper-key-mask test-shift-state))
(test-false (hyper-key-mask test-control-state))
(test-false (hyper-key-mask test-alt-state))
(test-false (hyper-key-mask test-meta-state))
(test-false (hyper-key-mask test-super-state))
(test-eq #t (hyper-key-mask test-hyper-state))
(test-false (hyper-key-mask 0))

(test-eq #t (modifier-key-mask test-shift-state))
(test-eq #t (modifier-key-mask test-control-state))
(test-eq #t (modifier-key-mask test-alt-state))
(test-eq #t (modifier-key-mask test-meta-state))
(test-eq #t (modifier-key-mask test-super-state))
(test-eq #t (modifier-key-mask test-hyper-state))
(test-false (modifier-key-mask 0))
(test-end)

(test-begin "modifier-key?")
(test-eq #t (modifier-key? 'Shift_key 0))
(test-eq #t (modifier-key? 'Control_key 0))
(test-eq #t (modifier-key? 'Alt_key 0))
(test-eq #t (modifier-key? 'Meta_key 0))
(test-eq #t (modifier-key? 'Super_key 0))
(test-eq #t (modifier-key? 'Hyper_key 0))
(test-false (modifier-key? 'return 0))
(test-false (modifier-key? 'escape 0))
(test-false (modifier-key? 0 0))   ;; NUL
(test-false (modifier-key? 97 0))  ;; a

(test-eq #t (modifier-key? 'Shift_key test-shift-state))
(test-eq #t (modifier-key? 'Control_key test-shift-state))
(test-eq #t (modifier-key? 'Alt_key test-shift-state))
(test-eq #t (modifier-key? 'Meta_key test-shift-state))
(test-eq #t (modifier-key? 'Super_key test-shift-state))
(test-eq #t (modifier-key? 'Hyper_key test-shift-state))
(test-false (modifier-key? 'return test-shift-state))
(test-false (modifier-key? 'escape test-shift-state))
(test-false (modifier-key? 0 test-shift-state)) ;; NUL
(test-false (modifier-key? 97 test-shift-state)) ;; a

(test-eq #t (modifier-key? 'Shift_key test-control-state))
(test-eq #t (modifier-key? 'Control_key test-control-state))
(test-eq #t (modifier-key? 'Alt_key test-control-state))
(test-eq #t (modifier-key? 'Meta_key test-control-state))
(test-eq #t (modifier-key? 'Super_key test-control-state))
(test-eq #t (modifier-key? 'Hyper_key test-control-state))
(test-false (modifier-key? 'return test-control-state))
(test-false (modifier-key? 'escape test-control-state))
(test-false (modifier-key? 0 test-control-state)) ;; NUL
(test-false (modifier-key? 97 test-control-state)) ;; a

(test-eq #t (modifier-key? 'Shift_key test-alt-state))
(test-eq #t (modifier-key? 'Control_key test-alt-state))
(test-eq #t (modifier-key? 'Alt_key test-alt-state))
(test-eq #t (modifier-key? 'Meta_key test-alt-state))
(test-eq #t (modifier-key? 'Super_key test-alt-state))
(test-eq #t (modifier-key? 'Hyper_key test-alt-state))
(test-false (modifier-key? 'return test-alt-state))
(test-false (modifier-key? 'escape test-alt-state))
(test-false (modifier-key? 0 test-alt-state)) ;; NUL
(test-false (modifier-key? 97 test-alt-state)) ;; a

(test-eq #t (modifier-key? 'Shift_key test-meta-state))
(test-eq #t (modifier-key? 'Control_key test-meta-state))
(test-eq #t (modifier-key? 'Alt_key test-meta-state))
(test-eq #t (modifier-key? 'Meta_key test-meta-state))
(test-eq #t (modifier-key? 'Super_key test-meta-state))
(test-eq #t (modifier-key? 'Hyper_key test-meta-state))
(test-false (modifier-key? 'return test-meta-state))
(test-false (modifier-key? 'escape test-meta-state))
(test-false (modifier-key? 0 test-meta-state)) ;; NUL
(test-false (modifier-key? 97 test-meta-state)) ;; a

(test-eq #t (modifier-key? 'Shift_key test-super-state))
(test-eq #t (modifier-key? 'Control_key test-super-state))
(test-eq #t (modifier-key? 'Alt_key test-super-state))
(test-eq #t (modifier-key? 'Meta_key test-super-state))
(test-eq #t (modifier-key? 'Super_key test-super-state))
(test-eq #t (modifier-key? 'Hyper_key test-super-state))
(test-false (modifier-key? 'return test-super-state))
(test-false (modifier-key? 'escape test-super-state))
(test-false (modifier-key? 0 test-super-state)) ;; NUL
(test-false (modifier-key? 97 test-super-state)) ;; a

(test-eq #t (modifier-key? 'Shift_key test-hyper-state))
(test-eq #t (modifier-key? 'Control_key test-hyper-state))
(test-eq #t (modifier-key? 'Alt_key test-hyper-state))
(test-eq #t (modifier-key? 'Meta_key test-hyper-state))
(test-eq #t (modifier-key? 'Super_key test-hyper-state))
(test-eq #t (modifier-key? 'Hyper_key test-hyper-state))
(test-false (modifier-key? 'return test-hyper-state))
(test-false (modifier-key? 'escape test-hyper-state))
(test-false (modifier-key? 0 test-hyper-state)) ;; NUL
(test-false (modifier-key? 97 test-hyper-state)) ;; a
(test-end)

(test-begin "translator-prefix?")
(test-eq #t (translator-prefix? 'IgnoreCase))
(test-eq #t (translator-prefix? 'IgnoreShift))
(test-eq #t (translator-prefix? 'IgnoreRegularShift))
(test-false (translator-prefix? 'NonExistent))
(test-end)

(test-begin "intern-key-prefix")
(test-equal 'Shift_key
            (intern-key-prefix "Shift" tag-prefix-alist))
(test-equal 'Control_key
            (intern-key-prefix "Control" tag-prefix-alist))
(test-equal 'Alt_key
            (intern-key-prefix "Alt" tag-prefix-alist))
(test-equal 'Meta_key
            (intern-key-prefix "Meta" tag-prefix-alist))
(test-equal 'Super_key
            (intern-key-prefix "Super" tag-prefix-alist))
(test-equal 'Hyper_key
            (intern-key-prefix "Hyper" tag-prefix-alist))
(test-equal 'IgnoreCase
            (intern-key-prefix "IgnoreCase" tag-prefix-alist))
(test-equal 'IgnoreShift
            (intern-key-prefix "IgnoreShift" tag-prefix-alist))
(test-equal 'IgnoreRegularShift
            (intern-key-prefix "IgnoreRegularShift" tag-prefix-alist))
(test-false (intern-key-prefix "NonExistent" tag-prefix-alist))

(test-equal 'Shift_key
            (intern-key-prefix "S" emacs-like-prefix-alist))
(test-equal 'Control_key
            (intern-key-prefix "C" emacs-like-prefix-alist))
(test-equal 'Alt_key
            (intern-key-prefix "A" emacs-like-prefix-alist))
(test-equal 'Meta_key
            (intern-key-prefix "M" emacs-like-prefix-alist))
(test-equal 'Super_key
            (intern-key-prefix "Z" emacs-like-prefix-alist))
(test-equal 'Hyper_key
            (intern-key-prefix "H" emacs-like-prefix-alist))
(test-equal 'IgnoreCase
            (intern-key-prefix "I" emacs-like-prefix-alist))
(test-equal 'IgnoreShift
            (intern-key-prefix "J" emacs-like-prefix-alist))
(test-equal 'IgnoreRegularShift
            (intern-key-prefix "K" emacs-like-prefix-alist))
(test-false (intern-key-prefix "N" emacs-like-prefix-alist))
(test-end)

(test-begin "parse-tag-prefix-symbol")
(test-equal '(Shift_key)
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t")))
(test-equal '(Control_key)
            (parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l")))
(test-equal '(Alt_key)
            (parse-tag-prefix-symbol "" '("A" "l" "t")))
(test-equal '(Meta_key)
            (parse-tag-prefix-symbol "" '("M" "e" "t" "a")))
(test-equal '(Super_key)
            (parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r")))
(test-equal '(Hyper_key)
            (parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r")))
(test-equal (list #f)
            (parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t")))
(test-equal (list #f)
            (parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T")))
(test-equal (list #f)
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t")))

;; parsing is terminated at boundary char
(test-equal '(Shift_key ">")
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">")))
(test-equal '(Control_key ">")
            (parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">")))
(test-equal '(Alt_key ">")
            (parse-tag-prefix-symbol "" '("A" "l" "t" ">")))
(test-equal '(Meta_key ">")
            (parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">")))
(test-equal '(Super_key ">")
            (parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">")))
(test-equal '(Hyper_key ">")
            (parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">")))
(test-equal (list #f ">")
            (parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">")))
(test-equal (list #f ">")
            (parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">")))
(test-equal (list #f ">")
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">")))

;; all chars that following boundary char remains
(test-equal '(Shift_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" ">" "<" "A" "l" "t" ">")))
(test-equal '(Control_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("C" "o" "n" "t" "r" "o" "l" ">" "<" "A" "l" "t" ">")))
(test-equal '(Alt_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("A" "l" "t" ">" "<" "A" "l" "t" ">")))
(test-equal '(Meta_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("M" "e" "t" "a" ">" "<" "A" "l" "t" ">")))
(test-equal '(Super_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("S" "u" "p" "e" "r" ">" "<" "A" "l" "t" ">")))
(test-equal '(Hyper_key ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("H" "y" "p" "e" "r" ">" "<" "A" "l" "t" ">")))
(test-equal (list #f ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("N" "o" "n" "E" "x" "i" "s" "t" "e" "n" "t" ">" "<" "A" "l" "t" ">")))
(test-equal (list #f ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("S" "H" "I" "F" "T" ">" "<" "A" "l" "t" ">")))
(test-equal (list #f ">" "<" "A" "l" "t" ">")
            (parse-tag-prefix-symbol "" '("S" "h" "i" "f" "t" "t" ">" "<" "A" "l" "t" ">")))

;; nonexistent symbol is parsed as #f
(test-equal (list #f)
            (parse-tag-prefix-symbol "" '("_" "F" "o" "o" "1")))
;; tag-prefix symbol must be consist of alphanumeric or "_"
(test-equal (list #f "-" "F" "o" "o" "1")
            (parse-tag-prefix-symbol "" '("-" "F" "o" "o" "1")))
(test-end)

(test-begin "parse-tag-prefix")
(test-equal '(Shift_key . "")
            (parse-tag-prefix "<Shift>"))
(test-equal '(Control_key . "")
            (parse-tag-prefix "<Control>"))
(test-equal '(Alt_key . "")
            (parse-tag-prefix "<Alt>"))
(test-equal '(Meta_key . "")
            (parse-tag-prefix "<Meta>"))
(test-equal '(Super_key . "")
            (parse-tag-prefix "<Super>"))
(test-equal '(Hyper_key . "")
            (parse-tag-prefix "<Hyper>"))
(test-equal (cons #f "")
            (parse-tag-prefix "<NonExistent>"))
(test-equal (cons #f "")
            (parse-tag-prefix "<SHIFT>"))
(test-equal (cons #f "")
            (parse-tag-prefix "<Shiftt>"))

(test-equal '(Shift_key . "<Alt>")
            (parse-tag-prefix "<Shift><Alt>"))
(test-equal '(Control_key . "<Alt>")
            (parse-tag-prefix "<Control><Alt>"))
(test-equal '(Alt_key . "<Alt>")
            (parse-tag-prefix "<Alt><Alt>"))
(test-equal '(Meta_key . "<Alt>")
            (parse-tag-prefix "<Meta><Alt>"))
(test-equal '(Super_key . "<Alt>")
            (parse-tag-prefix "<Super><Alt>"))
(test-equal '(Hyper_key . "<Alt>")
            (parse-tag-prefix "<Hyper><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-tag-prefix "<NonExistent><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-tag-prefix "<SHIFT><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-tag-prefix "<Shiftt><Alt>"))

(test-equal '(Shift_key . "a")
            (parse-tag-prefix "<Shift>a"))
(test-equal '(Control_key . "a")
            (parse-tag-prefix "<Control>a"))
(test-equal '(Alt_key . "a")
            (parse-tag-prefix "<Alt>a"))
(test-equal '(Meta_key . "a")
            (parse-tag-prefix "<Meta>a"))
(test-equal '(Super_key . "a")
            (parse-tag-prefix "<Super>a"))
(test-equal '(Hyper_key . "a")
            (parse-tag-prefix "<Hyper>a"))
(test-equal (cons #f "a")
            (parse-tag-prefix "<NonExistent>a"))
(test-equal (cons #f "a")
            (parse-tag-prefix "<SHIFT>a"))
(test-equal (cons #f "a")
            (parse-tag-prefix "<Shiftt>a"))
(test-end)

(test-begin "parse-emacs-like-prefix")
(test-equal '(Shift_key . "")
            (parse-emacs-like-prefix "S-"))
(test-equal '(Control_key . "")
            (parse-emacs-like-prefix "C-"))
(test-equal '(Alt_key . "")
            (parse-emacs-like-prefix "A-"))
(test-equal '(Meta_key . "")
            (parse-emacs-like-prefix "M-"))
(test-equal '(Super_key . "")
            (parse-emacs-like-prefix "Z-"))
(test-equal '(Hyper_key . "")
            (parse-emacs-like-prefix "H-"))
(test-equal (cons #f "N-")
            (parse-emacs-like-prefix "N-"))
(test-equal (cons #f "s-")
            (parse-emacs-like-prefix "s-"))
(test-equal (cons #f "SS-")
            (parse-emacs-like-prefix "SS-"))

(test-equal '(Shift_key . "A-")
            (parse-emacs-like-prefix "S-A-"))
(test-equal '(Control_key . "A-")
            (parse-emacs-like-prefix "C-A-"))
(test-equal '(Alt_key . "A-")
            (parse-emacs-like-prefix "A-A-"))
(test-equal '(Meta_key . "A-")
            (parse-emacs-like-prefix "M-A-"))
(test-equal '(Super_key . "A-")
            (parse-emacs-like-prefix "Z-A-"))
(test-equal '(Hyper_key . "A-")
            (parse-emacs-like-prefix "H-A-"))
(test-equal (cons #f "N-A-")
            (parse-emacs-like-prefix "N-A-"))
(test-equal (cons #f "s-A-")
            (parse-emacs-like-prefix "s-A-"))
(test-equal (cons #f "SS-A-")
            (parse-emacs-like-prefix "SS-A-"))

(test-equal '(Shift_key . "a")
            (parse-emacs-like-prefix "S-a"))
(test-equal '(Control_key . "a")
            (parse-emacs-like-prefix "C-a"))
(test-equal '(Alt_key . "a")
            (parse-emacs-like-prefix "A-a"))
(test-equal '(Meta_key . "a")
            (parse-emacs-like-prefix "M-a"))
(test-equal '(Super_key . "a")
            (parse-emacs-like-prefix "Z-a"))
(test-equal '(Hyper_key . "a")
            (parse-emacs-like-prefix "H-a"))
(test-equal (cons #f "N-a")
            (parse-emacs-like-prefix "N-a"))
(test-equal (cons #f "s-a")
            (parse-emacs-like-prefix "s-a"))
(test-equal (cons #f "SS-a")
            (parse-emacs-like-prefix "SS-a"))
(test-end)

(test-begin "parse-key-prefix")
(test-equal '(Shift_key . "")
            (parse-key-prefix "<Shift>"))
(test-equal '(Control_key . "")
            (parse-key-prefix "<Control>"))
(test-equal '(Alt_key . "")
            (parse-key-prefix "<Alt>"))
(test-equal '(Meta_key . "")
            (parse-key-prefix "<Meta>"))
(test-equal '(Super_key . "")
            (parse-key-prefix "<Super>"))
(test-equal '(Hyper_key . "")
            (parse-key-prefix "<Hyper>"))
(test-equal (cons #f "")
            (parse-key-prefix "<NonExistent>"))
(test-equal (cons #f "")
            (parse-key-prefix "<SHIFT>"))
(test-equal (cons #f "")
            (parse-key-prefix "<Shiftt>"))

(test-equal '(Shift_key . "<Alt>")
            (parse-key-prefix "<Shift><Alt>"))
(test-equal '(Control_key . "<Alt>")
            (parse-key-prefix "<Control><Alt>"))
(test-equal '(Alt_key . "<Alt>")
            (parse-key-prefix "<Alt><Alt>"))
(test-equal '(Meta_key . "<Alt>")
            (parse-key-prefix "<Meta><Alt>"))
(test-equal '(Super_key . "<Alt>")
            (parse-key-prefix "<Super><Alt>"))
(test-equal '(Hyper_key . "<Alt>")
            (parse-key-prefix "<Hyper><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-key-prefix "<NonExistent><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-key-prefix "<SHIFT><Alt>"))
(test-equal (cons #f "<Alt>")
            (parse-key-prefix "<Shiftt><Alt>"))

(test-equal '(Shift_key . "a")
            (parse-key-prefix "<Shift>a"))
(test-equal '(Control_key . "a")
            (parse-key-prefix "<Control>a"))
(test-equal '(Alt_key . "a")
            (parse-key-prefix "<Alt>a"))
(test-equal '(Meta_key . "a")
            (parse-key-prefix "<Meta>a"))
(test-equal '(Super_key . "a")
            (parse-key-prefix "<Super>a"))
(test-equal '(Hyper_key . "a")
            (parse-key-prefix "<Hyper>a"))
(test-equal (cons #f "a")
            (parse-key-prefix "<NonExistent>a"))
(test-equal (cons #f "a")
            (parse-key-prefix "<SHIFT>a"))
(test-equal (cons #f "a")
            (parse-key-prefix "<Shiftt>a"))

(test-equal '(Shift_key . "")
            (parse-key-prefix "S-"))
(test-equal '(Control_key . "")
            (parse-key-prefix "C-"))
(test-equal '(Alt_key . "")
            (parse-key-prefix "A-"))
(test-equal '(Meta_key . "")
            (parse-key-prefix "M-"))
(test-equal '(Super_key . "")
            (parse-key-prefix "Z-"))
(test-equal '(Hyper_key . "")
            (parse-key-prefix "H-"))
(test-equal (cons #f "N-")
            (parse-key-prefix "N-"))
(test-equal (cons #f "s-")
            (parse-key-prefix "s-"))
(test-equal (cons #f "SS-")
            (parse-key-prefix "SS-"))

(test-equal '(Shift_key . "A-")
            (parse-key-prefix "S-A-"))
(test-equal '(Control_key . "A-")
            (parse-key-prefix "C-A-"))
(test-equal '(Alt_key . "A-")
            (parse-key-prefix "A-A-"))
(test-equal '(Meta_key . "A-")
            (parse-key-prefix "M-A-"))
(test-equal '(Super_key . "A-")
            (parse-key-prefix "Z-A-"))
(test-equal '(Hyper_key . "A-")
            (parse-key-prefix "H-A-"))
(test-equal (cons #f "N-A-")
            (parse-key-prefix "N-A-"))
(test-equal (cons #f "s-A-")
            (parse-key-prefix "s-A-"))
(test-equal (cons #f "SS-A-")
            (parse-key-prefix "SS-A-"))

(test-equal '(Shift_key . "a")
            (parse-key-prefix "S-a"))
(test-equal '(Control_key . "a")
            (parse-key-prefix "C-a"))
(test-equal '(Alt_key . "a")
            (parse-key-prefix "A-a"))
(test-equal '(Meta_key . "a")
            (parse-key-prefix "M-a"))
(test-equal '(Super_key . "a")
            (parse-key-prefix "Z-a"))
(test-equal '(Hyper_key . "a")
            (parse-key-prefix "H-a"))
(test-equal (cons #f "N-a")
            (parse-key-prefix "N-a"))
(test-equal (cons #f "s-a")
            (parse-key-prefix "s-a"))
(test-equal (cons #f "SS-a")
            (parse-key-prefix "SS-a"))
(test-end)

(test-begin "parse-key-str")
;; single key
(test-equal (list "" '() 32 0)
            (parse-key-str " " () 0 0))
(test-equal (list "" '() 33 0)
            (parse-key-str "!" () 0 0))
(test-equal (list "" '() 48 0)
            (parse-key-str "0" () 0 0))
(test-equal (list "" '() 65 0)
            (parse-key-str "A" () 0 0))
(test-equal (list "" '() 97 0)
            (parse-key-str "a" () 0 0))
(test-equal (list "" '() 'return 0)
            (parse-key-str "return" () 0 0))

;; single key with single modifier
(test-equal (list "" () 32 test-shift-state)
            (parse-key-str "<Shift> " () 0 0))
(test-equal (list "" () 33 test-shift-state)
            (parse-key-str "<Shift>!" () 0 0))
(test-equal (list "" () 48 test-shift-state)
            (parse-key-str "<Shift>0" () 0 0))
(test-equal (list "" () 65 test-shift-state)
            (parse-key-str "<Shift>A" () 0 0))
(test-equal (list "" () 97 test-shift-state)
            (parse-key-str "<Shift>a" () 0 0))
(test-equal (list "" () 'return test-shift-state)
            (parse-key-str "<Shift>return" () 0 0))

;; single key with multiple modifiers
(test-equal (list "" () 32 (+ test-shift-state
                          test-control-state
                          test-meta-state))
            (parse-key-str "<Shift><Control><Meta> " () 0 0))
(test-equal (list "" () 33 (+ test-shift-state
                          test-control-state
                          test-meta-state))
            (parse-key-str "<Shift><Control><Meta>!" () 0 0))
(test-equal (list "" () 48 (+ test-shift-state
                          test-control-state
                          test-meta-state))
            (parse-key-str "<Shift><Control><Meta>0" () 0 0))
(test-equal (list "" () 65 (+ test-shift-state
                          test-control-state
                          test-meta-state))
            (parse-key-str "<Shift><Control><Meta>A" () 0 0))
(test-equal (list "" () 97 (+ test-shift-state
                          test-control-state
                          test-meta-state))
            (parse-key-str "<Shift><Control><Meta>a" () 0 0))
(test-equal (list "" () 'return (+ test-shift-state
                               test-control-state
                               test-meta-state))
            (parse-key-str "<Shift><Control><Meta>return" () 0 0))

;; single key with single translator
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift> " () 0 0))))
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift>!" () 0 0))))
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift>0" () 0 0))))
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift>A" () 0 0))))
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift>a" () 0 0))))
(test-equal 1
            (length (cadr (parse-key-str "<IgnoreShift>return" () 0 0))))

;; single key with multiple translators
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " () 0 0))))
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" () 0 0))))
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" () 0 0))))
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" () 0 0))))
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" () 0 0))))
(test-equal 2
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" () 0 0))))

;; single key with multiple translators (2)
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase> " (list (lambda () #t)) 0 0))))
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>!" (list (lambda () #t)) 0 0))))
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>0" (list (lambda () #t)) 0 0))))
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>A" (list (lambda () #t)) 0 0))))
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>a" (list (lambda () #t)) 0 0))))
(test-equal 3
            (length (cadr (parse-key-str "<IgnoreShift><IgnoreCase>return" (list (lambda () #t)) 0 0))))
(test-end)

(test-report-result)
