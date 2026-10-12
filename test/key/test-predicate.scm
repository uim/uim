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

(test-begin "make-single-key-predicate")
;; null key-str matches with nothing
(test-false ((make-single-key-predicate "") 0 0))        ; NUL
(test-false ((make-single-key-predicate "") 1 0))        ; SOH
(test-false ((make-single-key-predicate "") 31 0))       ; US
(test-false ((make-single-key-predicate "") 32 0))       ; SPACE
(test-false ((make-single-key-predicate "") 33 0))       ; !
(test-false ((make-single-key-predicate "") 48 0))       ; 0
(test-false ((make-single-key-predicate "") 65 0))       ; A
(test-false ((make-single-key-predicate "") 97 0))       ; a
(test-false ((make-single-key-predicate "") 127 0))      ; DEL
(test-false ((make-single-key-predicate "") 'return 0))  ; return
;; space
(test-eq #t ((make-single-key-predicate " ") 32 0))      ; SPACE
(test-false ((make-single-key-predicate " ") 33 0))      ; !
(test-false ((make-single-key-predicate " ") 48 0))      ; 0
(test-false ((make-single-key-predicate " ") 65 0))      ; A
(test-false ((make-single-key-predicate " ") 97 0))      ; a
(test-false ((make-single-key-predicate " ") 'return 0)) ; return
;; !
(test-false ((make-single-key-predicate "!") 32 0))      ; SPACE
(test-eq #t ((make-single-key-predicate "!") 33 0))      ; !
(test-false ((make-single-key-predicate "!") 48 0))      ; 0
(test-false ((make-single-key-predicate "!") 65 0))      ; A
(test-false ((make-single-key-predicate "!") 97 0))      ; a
(test-false ((make-single-key-predicate "!") 'return 0)) ; return
;; 0
(test-false ((make-single-key-predicate "0") 32 0))      ; SPACE
(test-false ((make-single-key-predicate "0") 33 0))      ; !
(test-eq #t ((make-single-key-predicate "0") 48 0))      ; 0
(test-false ((make-single-key-predicate "0") 65 0))      ; A
(test-false ((make-single-key-predicate "0") 97 0))      ; a
(test-false ((make-single-key-predicate "0") 'return 0)) ; return
;; A
(test-false ((make-single-key-predicate "A") 32 0))     ; SPACE
(test-false ((make-single-key-predicate "A") 33 0))     ; !
(test-false ((make-single-key-predicate "A") 48 0))     ; 0
(test-eq #t ((make-single-key-predicate "A") 65 0))     ; A
(test-false ((make-single-key-predicate "A") 97 0))     ; a
(test-false ((make-single-key-predicate "A") 'return 0)) ; return
;; a
(test-false ((make-single-key-predicate "a") 32 0))     ; SPACE
(test-false ((make-single-key-predicate "a") 33 0))     ; !
(test-false ((make-single-key-predicate "a") 48 0))     ; 0
(test-false ((make-single-key-predicate "a") 65 0))     ; A
(test-eq #t ((make-single-key-predicate "a") 97 0))     ; a
(test-false ((make-single-key-predicate "a") 'return 0)) ; return
;; return
(test-false ((make-single-key-predicate "return") 32 0))     ; SPACE
(test-false ((make-single-key-predicate "return") 33 0))     ; !
(test-false ((make-single-key-predicate "return") 48 0))     ; 0
(test-false ((make-single-key-predicate "return") 65 0))     ; A
(test-false ((make-single-key-predicate "return") 97 0))     ; a
(test-eq #t ((make-single-key-predicate "return") 'return 0)) ; return

;; single key with single modifier (success)
(test-eq #t ((make-single-key-predicate "<Shift> ")
             32 test-shift-state)) ; SPACE
(test-eq #t ((make-single-key-predicate "<Shift>!")
             33 test-shift-state)) ; !
(test-eq #t ((make-single-key-predicate "<Shift>0")
             48 test-shift-state)) ; 0
(test-eq #t ((make-single-key-predicate "<Shift>A")
             65 test-shift-state)) ; A
(test-eq #t ((make-single-key-predicate "<Shift>a")
             97 test-shift-state)) ; a
(test-eq #t ((make-single-key-predicate "<Shift>return")
             'return test-shift-state)) ; return
;; single key with single modifier (fail)
(test-false ((make-single-key-predicate "<Shift> ") 32 0))     ; SPACE
(test-false ((make-single-key-predicate "<Shift>!") 33 0))     ; !
(test-false ((make-single-key-predicate "<Shift>0") 48 0))     ; 0
(test-false ((make-single-key-predicate "<Shift>A") 65 0))     ; A
(test-false ((make-single-key-predicate "<Shift>a") 97 0))     ; a
(test-false ((make-single-key-predicate "<Shift>return")
             'return 0)) ; return
(test-false ((make-single-key-predicate "<Shift> ")
             32 test-control-state)) ; SPACE
(test-false ((make-single-key-predicate "<Shift>!")
             33 test-control-state)) ; !
(test-false ((make-single-key-predicate "<Shift>0")
             48 test-control-state)) ; 0
(test-false ((make-single-key-predicate "<Shift>A")
             65 test-control-state)) ; A
(test-false ((make-single-key-predicate "<Shift>a")
             97 test-control-state)) ; a
(test-false ((make-single-key-predicate "<Shift>return")
             'return test-control-state)) ; return

;; single key with multiple modifier (success)
(test-eq #t ((make-single-key-predicate "<Shift><Control> ")
             32 (+ test-shift-state ; SPACE
                   test-control-state)))
(test-eq #t ((make-single-key-predicate "<Shift><Control>!")
             33 (+ test-shift-state ; !
                   test-control-state)))
(test-eq #t ((make-single-key-predicate "<Shift><Control>0")
             48 (+ test-shift-state ; 0
                   test-control-state)))
(test-eq #t ((make-single-key-predicate "<Shift><Control>A")
             65 (+ test-shift-state ; A
                   test-control-state)))
(test-eq #t ((make-single-key-predicate "<Shift><Control>a")
             97 (+ test-shift-state ; a
                   test-control-state)))
(test-eq #t ((make-single-key-predicate "<Shift><Control>return")
             'return (+ test-shift-state ; return
                        test-control-state)))
;; single key with multiple modifier (fail)
(test-false ((make-single-key-predicate "<Shift><Control> ")
             32 0))     ; SPACE
(test-false ((make-single-key-predicate "<Shift><Control>!")
             33 0))     ; !
(test-false ((make-single-key-predicate "<Shift><Control>0")
             48 0))     ; 0
(test-false ((make-single-key-predicate "<Shift><Control>A")
             65 0))     ; A
(test-false ((make-single-key-predicate "<Shift><Control>a")
             97 0))     ; a
(test-false ((make-single-key-predicate "<Shift><Control>return")
             'return 0)) ; return
(test-false ((make-single-key-predicate "<Shift><Control> ")
             32 test-control-state)) ; SPACE
(test-false ((make-single-key-predicate "<Shift><Control>!")
             33 test-control-state)) ; !
(test-false ((make-single-key-predicate "<Shift><Control>0")
             48 test-control-state)) ; 0
(test-false ((make-single-key-predicate "<Shift><Control>A")
             65 test-control-state)) ; A
(test-false ((make-single-key-predicate "<Shift><Control>a")
             97 test-control-state)) ; a
(test-false ((make-single-key-predicate "<Shift><Control>return")
             'return test-control-state)) ; return

;; single key with single translator
(test-false ((make-single-key-predicate "<IgnoreRegularShift> ")
             32 test-shift-state)) ; SPACE
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>!")
             33 test-shift-state)) ; !
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>0")
             48 test-shift-state)) ; 0
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>A")
             65 test-shift-state)) ; A
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>a")
             97 test-shift-state)) ; a
(test-false ((make-single-key-predicate "<IgnoreRegularShift>return")
             'return test-shift-state)) ; return
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift> ")
             32 0))     ; SPACE
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>!")
             33 0))     ; !
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>0")
             48 0))     ; 0
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>A")
             65 0))     ; A
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>a")
             97 0))     ; a
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift>return")
             'return 0)) ; return
(test-false ((make-single-key-predicate "<IgnoreRegularShift> ")
             32 test-control-state)) ; SPACE
(test-false ((make-single-key-predicate "<IgnoreRegularShift>!")
             33 test-control-state)) ; !
(test-false ((make-single-key-predicate "<IgnoreRegularShift>0")
             48 test-control-state)) ; 0
(test-false ((make-single-key-predicate "<IgnoreRegularShift>A")
             65 test-control-state)) ; A
(test-false ((make-single-key-predicate "<IgnoreRegularShift>a")
             97 test-control-state)) ; a
(test-false ((make-single-key-predicate "<IgnoreRegularShift>return")
             'return test-control-state)) ; return

;; single key with single translator and single modifier
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
             32 test-shift-state)) ; SPACE
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
             33 test-shift-state)) ; !
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
             48 test-shift-state)) ; 0
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
             65 test-shift-state)) ; A
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
             97 test-shift-state)) ; a
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
             'return test-shift-state)) ; return
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
             32 0))     ; SPACE
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
             33 0))     ; !
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
             48 0))     ; 0
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
             65 0))     ; A
(test-eq #t ((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
             97 0))     ; a
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
             'return 0)) ; return
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
             32 test-control-state)) ; SPACE
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
             33 test-control-state)) ; !
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
             48 test-control-state)) ; 0
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
             65 test-control-state)) ; A
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
             97 test-control-state)) ; a
(test-false ((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
             'return test-control-state)) ; return

(define test-return-key?
  (make-single-key-predicate
   "<IgnoreRegularShift><Shift>return"))

;; make up from preexisting predicate
(test-eq #t ((make-single-key-predicate test-return-key?)
             'return test-shift-state)) ; return
(test-false ((make-single-key-predicate test-return-key?)
             'return 0)) ; return

;; make up from preexisting predicate symbol
(test-eq #t ((make-single-key-predicate 'test-return-key?)
             'return test-shift-state)) ; return
(test-false ((make-single-key-predicate 'test-return-key?)
             'return 0)) ; return
(test-end)

(test-begin "make-key-predicate")
(begin
  (define test-return-key?
    (make-single-key-predicate
     "<IgnoreRegularShift><Shift>return"))
  (define test-a-key?
    (make-single-key-predicate "a")))

;; make up from key-str
(test-eq #t ((make-key-predicate
              "<IgnoreRegularShift><Shift>return")
             'return test-shift-state)) ; return
(test-false ((make-key-predicate
              "<IgnoreRegularShift><Shift>return")
             'return 0)) ; return

;; make up from preexisting predicate
(test-eq #t ((make-single-key-predicate test-return-key?)
             'return test-shift-state)) ; return
(test-false ((make-key-predicate test-return-key?)
              'return 0)) ; return

;; make up from preexisting predicate symbol
(test-eq #t ((make-key-predicate 'test-return-key?)
             'return test-shift-state)) ; return
(test-false ((make-key-predicate 'test-return-key?)
             'return 0)) ; return

;; make up from key-str in a list
(test-eq #t ((make-key-predicate
              '("<IgnoreRegularShift><Shift>return"))
             'return test-shift-state)) ; return
(test-false ((make-key-predicate
              '("<IgnoreRegularShift><Shift>return"))
             'return 0)) ; return

;; make up from preexisting predicate in a list
(test-eq #t ((make-key-predicate (list test-return-key?))
             'return test-shift-state)) ; return
(test-false ((make-key-predicate (list test-return-key?))
             'return 0)) ; return

;; make up from preexisting predicate symbol in a list
(test-eq #t ((make-key-predicate '(test-return-key?))
             'return test-shift-state)) ; return
(test-false ((make-key-predicate '(test-return-key?))
             'return 0)) ; return

;; make up from or'ed predicates (success)
(test-eq #t ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             'return test-shift-state)) ; return
(test-eq #t ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             97 0))     ; a
(test-eq #t ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             98 test-control-state)) ; b
;; make up from or'ed predicates (fail)
(test-false ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             'return 0)) ; return
(test-false ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             97 test-shift-state)) ; a
(test-false ((make-key-predicate (list
                                  test-a-key?
                                  'test-return-key?
                                  "<Control>b"))
             98 0))    ; b
(test-end)

(test-begin "modify-key-strs-implicitly")
(test-equal "<IgnoreRegularShift>return"
            (modify-key-strs-implicitly "return"))
(test-equal '("<IgnoreRegularShift>return")
            (modify-key-strs-implicitly '("return")))
(test-equal '("<IgnoreRegularShift>return"
              "<IgnoreRegularShift>a"
              "<IgnoreRegularShift><Shift>b")
            (modify-key-strs-implicitly '("return" "a" "<Shift>b")))
(test-equal '("<IgnoreRegularShift>return"
              "<IgnoreRegularShift>a"
              foo
              "<IgnoreRegularShift><Shift>b")
            (modify-key-strs-implicitly '("return"
                                          "a"
                                          foo
                                          "<Shift>b")))
(test-end)

(test-begin "define-key-internal")
(test-false (symbol-bound? 'test-foo-key?))
(begin
  (define-key-internal 'test-foo-key? "<Shift>return")
  (define-key-internal 'test-bar-key? "<Shift>a")
  (define-key-internal 'test-baz-key? "b")
  (define test-explicit-bar-key? (make-key-predicate "<Shift>a"))
  (define-key-internal 'test-quux-key? (list
                                        test-foo-key?
                                        'test-explicit-bar-key?
                                        "b")))
(test-eq #t (symbol-bound? 'test-foo-key?))
;; implicit <IgnoreRegularShift> not affects to 'return
(test-eq #t (test-foo-key? 'return test-shift-state))
(test-false (test-foo-key? 'return 0))
(test-false (test-foo-key? 'return test-control-state))
;; always matches by implicit <IgnoreRegularShift>
(test-eq #t (test-bar-key? 97 test-shift-state))
(test-eq #t (test-bar-key? 97 0))
(test-false (test-bar-key? 97 test-control-state))
;; always matches by implicit <IgnoreRegularShift>
(test-eq #t (test-baz-key? 98 test-shift-state))
(test-eq #t (test-baz-key? 98 0))
(test-false (test-baz-key? 98 test-control-state))

;; implicit <IgnoreRegularShift> not affects to test-foo-key?
(test-eq #t (test-quux-key? 'return test-shift-state))
(test-false (test-quux-key? 'return 0))
(test-false (test-quux-key? 'return test-control-state))
;;  implicit <IgnoreRegularShift> not affects to 'test-explicit-bar-key?
(test-eq #t (test-quux-key? 97 test-shift-state))
(test-false (test-quux-key? 97 0))
(test-false (test-quux-key? 97 test-control-state))
;; always matches by implicit <IgnoreRegularShift>
(test-eq #t (test-quux-key? 98 test-shift-state))
(test-eq #t (test-quux-key? 98 0))
(test-false (test-quux-key? 98 test-control-state))
(test-end)

(test-begin "valid-key-str?")
;; null key fails
(test-false (valid-key-str? ""))

;; invalid key definitions
(test-false  (valid-key-str? "nonexistent"))
(test-false  (valid-key-str? "<Shift>nonexistent"))
(test-false  (valid-key-str? "<Nonexistent>a"))
(test-false  (valid-key-str? "<Nonexistent>nonexistent"))
(test-false  (valid-key-str? "<Nonexistent><Shift>a"))
(test-false  (valid-key-str? "<Nonexistent><Shift>nonexistent"))

(test-false  (valid-key-str? "nonexistent"))
(test-false  (valid-key-str? "S-nonexistent"))
(test-false  (valid-key-str? "N-a"))
(test-false  (valid-key-str? "N-nonexistent"))
(test-false  (valid-key-str? "N-S-a"))
(test-false  (valid-key-str? "N-S-nonexistent"))

;; single key
(test-eq #t (valid-key-str? " "))
(test-eq #t (valid-key-str? "!"))
(test-eq #t (valid-key-str? "0"))
(test-eq #t (valid-key-str? "A"))
(test-eq #t (valid-key-str? "a"))
(test-eq #t (valid-key-str? "return"))

;; single key with single modifier
(test-eq #t (valid-key-str? "<Shift> "))
(test-eq #t (valid-key-str? "<Shift>!"))
(test-eq #t (valid-key-str? "<Shift>0"))
(test-eq #t (valid-key-str? "<Shift>A"))
(test-eq #t (valid-key-str? "<Shift>a"))
(test-eq #t (valid-key-str? "<Shift>return"))
(test-eq #t (valid-key-str? "<Control>return"))
(test-eq #t (valid-key-str? "<Alt>return"))
(test-eq #t (valid-key-str? "<Meta>return"))
(test-eq #t (valid-key-str? "<Super>return"))
(test-eq #t (valid-key-str? "<Hyper>return"))

(test-eq #t (valid-key-str? "S- "))
(test-eq #t (valid-key-str? "S-!"))
(test-eq #t (valid-key-str? "S-0"))
(test-eq #t (valid-key-str? "S-A"))
(test-eq #t (valid-key-str? "S-a"))
(test-eq #t (valid-key-str? "S-return"))
(test-eq #t (valid-key-str? "C-return"))
(test-eq #t (valid-key-str? "A-return"))
(test-eq #t (valid-key-str? "M-return"))
(test-eq #t (valid-key-str? "S-return"))
(test-eq #t (valid-key-str? "H-return"))

;; single key with multiple modifiers
(test-eq #t (valid-key-str? "<Shift><Control><Meta> "))
(test-eq #t (valid-key-str? "<Shift><Control><Meta>!"))
(test-eq #t (valid-key-str? "<Shift><Control><Meta>0"))
(test-eq #t (valid-key-str? "<Shift><Control><Meta>A"))
(test-eq #t (valid-key-str? "<Shift><Control><Meta>a"))
(test-eq #t (valid-key-str? "<Shift><Control><Meta>return"))

(test-eq #t (valid-key-str? "S-C-M- "))
(test-eq #t (valid-key-str? "S-C-M-!"))
(test-eq #t (valid-key-str? "S-C-M-0"))
(test-eq #t (valid-key-str? "S-C-M-A"))
(test-eq #t (valid-key-str? "S-C-M-a"))
(test-eq #t (valid-key-str? "S-C-M-return"))

;; single key with single translator
(test-eq #t (valid-key-str? "<IgnoreShift> "))
(test-eq #t (valid-key-str? "<IgnoreShift>!"))
(test-eq #t (valid-key-str? "<IgnoreShift>0"))
(test-eq #t (valid-key-str? "<IgnoreShift>A"))
(test-eq #t (valid-key-str? "<IgnoreShift>a"))
(test-eq #t (valid-key-str? "<IgnoreShift>return"))
(test-eq #t (valid-key-str? "<IgnoreRegularShift>return"))
(test-eq #t (valid-key-str? "<IgnoreCase>return"))

(test-eq #t (valid-key-str? "J- "))
(test-eq #t (valid-key-str? "J-!"))
(test-eq #t (valid-key-str? "J-0"))
(test-eq #t (valid-key-str? "J-A"))
(test-eq #t (valid-key-str? "J-a"))
(test-eq #t (valid-key-str? "J-return"))
(test-eq #t (valid-key-str? "K-return"))
(test-eq #t (valid-key-str? "I-return"))

;; single key with multiple translators
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase> "))
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase>!"))
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase>0"))
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase>A"))
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase>a"))
(test-eq #t (valid-key-str? "<IgnoreShift><IgnoreCase>return"))

(test-eq #t (valid-key-str? "J-I- "))
(test-eq #t (valid-key-str? "J-I-!"))
(test-eq #t (valid-key-str? "J-I-0"))
(test-eq #t (valid-key-str? "J-I-A"))
(test-eq #t (valid-key-str? "J-I-a"))
(test-eq #t (valid-key-str? "J-I-return"))
(test-end)

(test-begin "valid-strict-key-str?")
;; null key fails
(test-false (valid-strict-key-str? ""))

;; invalid key definitions
(test-false  (valid-strict-key-str? "nonexistent"))
(test-false  (valid-strict-key-str? "<Shift>nonexistent"))
(test-false  (valid-strict-key-str? "<Nonexistent>a"))
(test-false  (valid-strict-key-str? "<Nonexistent>nonexistent"))
(test-false  (valid-strict-key-str? "<Nonexistent><Shift>a"))
(test-false  (valid-strict-key-str? "<Nonexistent><Shift>nonexistent"))

(test-false  (valid-strict-key-str? "nonexistent"))
(test-false  (valid-strict-key-str? "S-nonexistent"))
(test-false  (valid-strict-key-str? "N-a"))
(test-false  (valid-strict-key-str? "N-nonexistent"))
(test-false  (valid-strict-key-str? "N-S-a"))
(test-false  (valid-strict-key-str? "N-S-nonexistent"))

;; single key
(test-eq #t (valid-strict-key-str? " "))
(test-eq #t (valid-strict-key-str? "!"))
(test-eq #t (valid-strict-key-str? "0"))
(test-eq #t (valid-strict-key-str? "A"))
(test-eq #t (valid-strict-key-str? "a"))
(test-eq #t (valid-strict-key-str? "return"))

;; single key with single modifier
(test-eq #t (valid-strict-key-str? "<Shift> "))
(test-eq #t (valid-strict-key-str? "<Shift>!"))
(test-eq #t (valid-strict-key-str? "<Shift>0"))
(test-eq #t (valid-strict-key-str? "<Shift>A"))
(test-eq #t (valid-strict-key-str? "<Shift>a"))
(test-eq #t (valid-strict-key-str? "<Shift>return"))
(test-eq #t (valid-strict-key-str? "<Control>return"))
(test-eq #t (valid-strict-key-str? "<Alt>return"))
(test-eq #t (valid-strict-key-str? "<Meta>return"))
(test-eq #t (valid-strict-key-str? "<Super>return"))
(test-eq #t (valid-strict-key-str? "<Hyper>return"))

(test-false (valid-strict-key-str? "S- "))
(test-false (valid-strict-key-str? "S-!"))
(test-false (valid-strict-key-str? "S-0"))
(test-false (valid-strict-key-str? "S-A"))
(test-false (valid-strict-key-str? "S-a"))
(test-false (valid-strict-key-str? "S-return"))
(test-false (valid-strict-key-str? "C-return"))
(test-false (valid-strict-key-str? "A-return"))
(test-false (valid-strict-key-str? "M-return"))
(test-false (valid-strict-key-str? "S-return"))
(test-false (valid-strict-key-str? "H-return"))

;; single key with multiple modifiers
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta> "))
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta>!"))
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta>0"))
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta>A"))
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta>a"))
(test-eq #t (valid-strict-key-str? "<Shift><Control><Meta>return"))

(test-false (valid-strict-key-str? "S-C-M- "))
(test-false (valid-strict-key-str? "S-C-M-!"))
(test-false (valid-strict-key-str? "S-C-M-0"))
(test-false (valid-strict-key-str? "S-C-M-A"))
(test-false (valid-strict-key-str? "S-C-M-a"))
(test-false (valid-strict-key-str? "S-C-M-return"))

;; single key with single translator
(test-false (valid-strict-key-str? "<IgnoreShift> "))
(test-false (valid-strict-key-str? "<IgnoreShift>!"))
(test-false (valid-strict-key-str? "<IgnoreShift>0"))
(test-false (valid-strict-key-str? "<IgnoreShift>A"))
(test-false (valid-strict-key-str? "<IgnoreShift>a"))
(test-false (valid-strict-key-str? "<IgnoreShift>return"))
(test-false (valid-strict-key-str? "<IgnoreRegularShift>return"))
(test-false (valid-strict-key-str? "<IgnoreCase>return"))

(test-false (valid-strict-key-str? "J- "))
(test-false (valid-strict-key-str? "J-!"))
(test-false (valid-strict-key-str? "J-0"))
(test-false (valid-strict-key-str? "J-A"))
(test-false (valid-strict-key-str? "J-a"))
(test-false (valid-strict-key-str? "J-return"))
(test-false (valid-strict-key-str? "K-return"))
(test-false (valid-strict-key-str? "I-return"))

;; single key with multiple translators
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase> "))
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase>!"))
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase>0"))
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase>A"))
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase>a"))
(test-false (valid-strict-key-str? "<IgnoreShift><IgnoreCase>return"))

(test-false (valid-strict-key-str? "J-I- "))
(test-false (valid-strict-key-str? "J-I-!"))
(test-false (valid-strict-key-str? "J-I-0"))
(test-false (valid-strict-key-str? "J-I-A"))
(test-false (valid-strict-key-str? "J-I-a"))
(test-false (valid-strict-key-str? "J-I-return"))
(test-end)

(test-report-result)
