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

(define-module test.key.test-predicate
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.key.test-predicate)

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

(define (test-make-single-key-predicate)
  ;; null key-str matches with nothing
  (assert-uim-false '((make-single-key-predicate "") 0 0))        ; NUL
  (assert-uim-false '((make-single-key-predicate "") 1 0))        ; SOH
  (assert-uim-false '((make-single-key-predicate "") 31 0))       ; US
  (assert-uim-false '((make-single-key-predicate "") 32 0))       ; SPACE
  (assert-uim-false '((make-single-key-predicate "") 33 0))       ; !
  (assert-uim-false '((make-single-key-predicate "") 48 0))       ; 0
  (assert-uim-false '((make-single-key-predicate "") 65 0))       ; A
  (assert-uim-false '((make-single-key-predicate "") 97 0))       ; a
  (assert-uim-false '((make-single-key-predicate "") 127 0))      ; DEL
  (assert-uim-false '((make-single-key-predicate "") 'return 0))  ; return
  ;; space
  (assert-uim-true  '((make-single-key-predicate " ") 32 0))      ; SPACE
  (assert-uim-false '((make-single-key-predicate " ") 33 0))      ; !
  (assert-uim-false '((make-single-key-predicate " ") 48 0))      ; 0
  (assert-uim-false '((make-single-key-predicate " ") 65 0))      ; A
  (assert-uim-false '((make-single-key-predicate " ") 97 0))      ; a
  (assert-uim-false '((make-single-key-predicate " ") 'return 0)) ; return
  ;; !
  (assert-uim-false '((make-single-key-predicate "!") 32 0))      ; SPACE
  (assert-uim-true  '((make-single-key-predicate "!") 33 0))      ; !
  (assert-uim-false '((make-single-key-predicate "!") 48 0))      ; 0
  (assert-uim-false '((make-single-key-predicate "!") 65 0))      ; A
  (assert-uim-false '((make-single-key-predicate "!") 97 0))      ; a
  (assert-uim-false '((make-single-key-predicate "!") 'return 0)) ; return
  ;; 0
  (assert-uim-false '((make-single-key-predicate "0") 32 0))      ; SPACE
  (assert-uim-false '((make-single-key-predicate "0") 33 0))      ; !
  (assert-uim-true  '((make-single-key-predicate "0") 48 0))      ; 0
  (assert-uim-false '((make-single-key-predicate "0") 65 0))      ; A
  (assert-uim-false '((make-single-key-predicate "0") 97 0))      ; a
  (assert-uim-false '((make-single-key-predicate "0") 'return 0)) ; return
  ;; A
  (assert-uim-false '((make-single-key-predicate "A") 32 0))     ; SPACE
  (assert-uim-false '((make-single-key-predicate "A") 33 0))     ; !
  (assert-uim-false '((make-single-key-predicate "A") 48 0))     ; 0
  (assert-uim-true  '((make-single-key-predicate "A") 65 0))     ; A
  (assert-uim-false '((make-single-key-predicate "A") 97 0))     ; a
  (assert-uim-false '((make-single-key-predicate "A") 'return 0)) ; return
  ;; a
  (assert-uim-false '((make-single-key-predicate "a") 32 0))     ; SPACE
  (assert-uim-false '((make-single-key-predicate "a") 33 0))     ; !
  (assert-uim-false '((make-single-key-predicate "a") 48 0))     ; 0
  (assert-uim-false '((make-single-key-predicate "a") 65 0))     ; A
  (assert-uim-true  '((make-single-key-predicate "a") 97 0))     ; a
  (assert-uim-false '((make-single-key-predicate "a") 'return 0)) ; return
  ;; return
  (assert-uim-false '((make-single-key-predicate "return") 32 0))     ; SPACE
  (assert-uim-false '((make-single-key-predicate "return") 33 0))     ; !
  (assert-uim-false '((make-single-key-predicate "return") 48 0))     ; 0
  (assert-uim-false '((make-single-key-predicate "return") 65 0))     ; A
  (assert-uim-false '((make-single-key-predicate "return") 97 0))     ; a
  (assert-uim-true  '((make-single-key-predicate "return") 'return 0)) ; return

  ;; single key with single modifier (success)
  (assert-uim-true  '((make-single-key-predicate "<Shift> ")
                      32 test-shift-state)) ; SPACE
  (assert-uim-true  '((make-single-key-predicate "<Shift>!")
                      33 test-shift-state)) ; !
  (assert-uim-true  '((make-single-key-predicate "<Shift>0")
                      48 test-shift-state)) ; 0
  (assert-uim-true  '((make-single-key-predicate "<Shift>A")
                      65 test-shift-state)) ; A
  (assert-uim-true  '((make-single-key-predicate "<Shift>a")
                      97 test-shift-state)) ; a
  (assert-uim-true  '((make-single-key-predicate "<Shift>return")
                      'return test-shift-state)) ; return
  ;; single key with single modifier (fail)
  (assert-uim-false '((make-single-key-predicate "<Shift> ") 32 0))     ; SPACE
  (assert-uim-false '((make-single-key-predicate "<Shift>!") 33 0))     ; !
  (assert-uim-false '((make-single-key-predicate "<Shift>0") 48 0))     ; 0
  (assert-uim-false '((make-single-key-predicate "<Shift>A") 65 0))     ; A
  (assert-uim-false '((make-single-key-predicate "<Shift>a") 97 0))     ; a
  (assert-uim-false '((make-single-key-predicate "<Shift>return")
                      'return 0)) ; return
  (assert-uim-false '((make-single-key-predicate "<Shift> ")
                      32 test-control-state)) ; SPACE
  (assert-uim-false '((make-single-key-predicate "<Shift>!")
                      33 test-control-state)) ; !
  (assert-uim-false '((make-single-key-predicate "<Shift>0")
                      48 test-control-state)) ; 0
  (assert-uim-false '((make-single-key-predicate "<Shift>A")
                      65 test-control-state)) ; A
  (assert-uim-false '((make-single-key-predicate "<Shift>a")
                      97 test-control-state)) ; a
  (assert-uim-false '((make-single-key-predicate "<Shift>return")
                      'return test-control-state)) ; return

  ;; single key with multiple modifier (success)
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control> ")
                      32 (+ test-shift-state ; SPACE
                            test-control-state)))
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control>!")
                      33 (+ test-shift-state ; !
                            test-control-state)))
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control>0")
                      48 (+ test-shift-state ; 0
                            test-control-state)))
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control>A")
                      65 (+ test-shift-state ; A
                            test-control-state)))
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control>a")
                      97 (+ test-shift-state ; a
                            test-control-state)))
  (assert-uim-true  '((make-single-key-predicate "<Shift><Control>return")
                      'return (+ test-shift-state ; return
                                 test-control-state)))
  ;; single key with multiple modifier (fail)
  (assert-uim-false '((make-single-key-predicate "<Shift><Control> ")
                      32 0))     ; SPACE
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>!")
                      33 0))     ; !
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>0")
                      48 0))     ; 0
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>A")
                      65 0))     ; A
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>a")
                      97 0))     ; a
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>return")
                      'return 0)) ; return
  (assert-uim-false '((make-single-key-predicate "<Shift><Control> ")
                      32 test-control-state)) ; SPACE
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>!")
                      33 test-control-state)) ; !
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>0")
                      48 test-control-state)) ; 0
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>A")
                      65 test-control-state)) ; A
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>a")
                      97 test-control-state)) ; a
  (assert-uim-false '((make-single-key-predicate "<Shift><Control>return")
                      'return test-control-state)) ; return

  ;; single key with single translator
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift> ")
                      32 test-shift-state)) ; SPACE
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>!")
                      33 test-shift-state)) ; !
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>0")
                      48 test-shift-state)) ; 0
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>A")
                      65 test-shift-state)) ; A
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>a")
                      97 test-shift-state)) ; a
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>return")
                      'return test-shift-state)) ; return
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift> ")
                      32 0))     ; SPACE
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>!")
                      33 0))     ; !
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>0")
                      48 0))     ; 0
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>A")
                      65 0))     ; A
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>a")
                      97 0))     ; a
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift>return")
                      'return 0)) ; return
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift> ")
                      32 test-control-state)) ; SPACE
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>!")
                      33 test-control-state)) ; !
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>0")
                      48 test-control-state)) ; 0
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>A")
                      65 test-control-state)) ; A
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>a")
                      97 test-control-state)) ; a
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift>return")
                      'return test-control-state)) ; return

  ;; single key with single translator and single modifier
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
                      32 test-shift-state)) ; SPACE
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
                      33 test-shift-state)) ; !
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
                      48 test-shift-state)) ; 0
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
                      65 test-shift-state)) ; A
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
                      97 test-shift-state)) ; a
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
                      32 0))     ; SPACE
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
                      33 0))     ; !
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
                      48 0))     ; 0
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
                      65 0))     ; A
  (assert-uim-true  '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
                      97 0))     ; a
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
                      'return 0)) ; return
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift> ")
                      32 test-control-state)) ; SPACE
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>!")
                      33 test-control-state)) ; !
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>0")
                      48 test-control-state)) ; 0
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>A")
                      65 test-control-state)) ; A
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>a")
                      97 test-control-state)) ; a
  (assert-uim-false '((make-single-key-predicate "<IgnoreRegularShift><Shift>return")
                      'return test-control-state)) ; return

  (uim-eval '(define test-return-key?
               (make-single-key-predicate
                "<IgnoreRegularShift><Shift>return")))

  ;; make up from preexisting predicate
  (assert-uim-true  '((make-single-key-predicate test-return-key?)
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-single-key-predicate test-return-key?)
                      'return 0)) ; return

  ;; make up from preexisting predicate symbol
  (assert-uim-true  '((make-single-key-predicate 'test-return-key?)
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-single-key-predicate 'test-return-key?)
                      'return 0)) ; return
  #f)

(define (test-make-key-predicate)
  (uim-eval
   '(begin
      (define test-return-key?
        (make-single-key-predicate
         "<IgnoreRegularShift><Shift>return"))
      (define test-a-key?
        (make-single-key-predicate "a"))))

  ;; make up from key-str
  (assert-uim-true  '((make-key-predicate
                       "<IgnoreRegularShift><Shift>return")
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate
                       "<IgnoreRegularShift><Shift>return")
                      'return 0)) ; return

  ;; make up from preexisting predicate
  (assert-uim-true  '((make-single-key-predicate test-return-key?)
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate test-return-key?)
                       'return 0)) ; return

  ;; make up from preexisting predicate symbol
  (assert-uim-true  '((make-key-predicate 'test-return-key?)
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate 'test-return-key?)
                      'return 0)) ; return

  ;; make up from key-str in a list
  (assert-uim-true  '((make-key-predicate
                       '("<IgnoreRegularShift><Shift>return"))
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate
                       '("<IgnoreRegularShift><Shift>return"))
                      'return 0)) ; return

  ;; make up from preexisting predicate in a list
  (assert-uim-true  '((make-key-predicate (list test-return-key?))
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate (list test-return-key?))
                      'return 0)) ; return

  ;; make up from preexisting predicate symbol in a list
  (assert-uim-true  '((make-key-predicate '(test-return-key?))
                      'return test-shift-state)) ; return
  (assert-uim-false '((make-key-predicate '(test-return-key?))
                      'return 0)) ; return

  ;; make up from or'ed predicates (success)
  (assert-uim-true  '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      'return test-shift-state)) ; return
  (assert-uim-true  '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      97 0))     ; a
  (assert-uim-true  '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      98 test-control-state)) ; b
  ;; make up from or'ed predicates (fail)
  (assert-uim-false '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      'return 0)) ; return
  (assert-uim-false '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      97 test-shift-state)) ; a
  (assert-uim-false '((make-key-predicate (list
                                           test-a-key?
                                           'test-return-key?
                                           "<Control>b"))
                      98 0))    ; b
  #f)

(define (test-modify-key-strs-implicitly)
  (assert-uim-equal "<IgnoreRegularShift>return"
                    '(modify-key-strs-implicitly "return"))
  (assert-uim-equal '("<IgnoreRegularShift>return")
                    '(modify-key-strs-implicitly '("return")))
  (assert-uim-equal '("<IgnoreRegularShift>return"
                      "<IgnoreRegularShift>a"
                      "<IgnoreRegularShift><Shift>b")
                    '(modify-key-strs-implicitly '("return" "a" "<Shift>b")))
  (assert-uim-equal '("<IgnoreRegularShift>return"
                      "<IgnoreRegularShift>a"
                      foo
                      "<IgnoreRegularShift><Shift>b")
                    '(modify-key-strs-implicitly '("return"
                                                   "a"
                                                   foo
                                                   "<Shift>b")))
  #f)

(define (test-define-key-internal)
  (assert-uim-false '(symbol-bound? 'test-foo-key?))
  (uim-eval
   '(begin
      (define-key-internal 'test-foo-key? "<Shift>return")
      (define-key-internal 'test-bar-key? "<Shift>a")
      (define-key-internal 'test-baz-key? "b")
      (define test-explicit-bar-key? (make-key-predicate "<Shift>a"))
      (define-key-internal 'test-quux-key? (list
                                            test-foo-key?
                                            'test-explicit-bar-key?
                                            "b"))))
  (assert-uim-true  '(symbol-bound? 'test-foo-key?))
  ;; implicit <IgnoreRegularShift> not affects to 'return
  (assert-uim-true  '(test-foo-key? 'return test-shift-state))
  (assert-uim-false '(test-foo-key? 'return 0))
  (assert-uim-false '(test-foo-key? 'return test-control-state))
  ;; always matches by implicit <IgnoreRegularShift>
  (assert-uim-true  '(test-bar-key? 97 test-shift-state))
  (assert-uim-true  '(test-bar-key? 97 0))
  (assert-uim-false '(test-bar-key? 97 test-control-state))
  ;; always matches by implicit <IgnoreRegularShift>
  (assert-uim-true  '(test-baz-key? 98 test-shift-state))
  (assert-uim-true  '(test-baz-key? 98 0))
  (assert-uim-false '(test-baz-key? 98 test-control-state))

  ;; implicit <IgnoreRegularShift> not affects to test-foo-key?
  (assert-uim-true  '(test-quux-key? 'return test-shift-state))
  (assert-uim-false '(test-quux-key? 'return 0))
  (assert-uim-false '(test-quux-key? 'return test-control-state))
  ;;  implicit <IgnoreRegularShift> not affects to 'test-explicit-bar-key?
  (assert-uim-true  '(test-quux-key? 97 test-shift-state))
  (assert-uim-false '(test-quux-key? 97 0))
  (assert-uim-false '(test-quux-key? 97 test-control-state))
  ;; always matches by implicit <IgnoreRegularShift>
  (assert-uim-true  '(test-quux-key? 98 test-shift-state))
  (assert-uim-true  '(test-quux-key? 98 0))
  (assert-uim-false '(test-quux-key? 98 test-control-state))
  #f)

(define (test-valid-key-str?)
  ;; null key fails
  (assert-uim-false '(valid-key-str? ""))

  ;; invalid key definitions
  (assert-uim-false  '(valid-key-str? "nonexistent"))
  (assert-uim-false  '(valid-key-str? "<Shift>nonexistent"))
  (assert-uim-false  '(valid-key-str? "<Nonexistent>a"))
  (assert-uim-false  '(valid-key-str? "<Nonexistent>nonexistent"))
  (assert-uim-false  '(valid-key-str? "<Nonexistent><Shift>a"))
  (assert-uim-false  '(valid-key-str? "<Nonexistent><Shift>nonexistent"))

  (assert-uim-false  '(valid-key-str? "nonexistent"))
  (assert-uim-false  '(valid-key-str? "S-nonexistent"))
  (assert-uim-false  '(valid-key-str? "N-a"))
  (assert-uim-false  '(valid-key-str? "N-nonexistent"))
  (assert-uim-false  '(valid-key-str? "N-S-a"))
  (assert-uim-false  '(valid-key-str? "N-S-nonexistent"))

  ;; single key
  (assert-uim-true  '(valid-key-str? " "))
  (assert-uim-true  '(valid-key-str? "!"))
  (assert-uim-true  '(valid-key-str? "0"))
  (assert-uim-true  '(valid-key-str? "A"))
  (assert-uim-true  '(valid-key-str? "a"))
  (assert-uim-true  '(valid-key-str? "return"))

  ;; single key with single modifier
  (assert-uim-true  '(valid-key-str? "<Shift> "))
  (assert-uim-true  '(valid-key-str? "<Shift>!"))
  (assert-uim-true  '(valid-key-str? "<Shift>0"))
  (assert-uim-true  '(valid-key-str? "<Shift>A"))
  (assert-uim-true  '(valid-key-str? "<Shift>a"))
  (assert-uim-true  '(valid-key-str? "<Shift>return"))
  (assert-uim-true  '(valid-key-str? "<Control>return"))
  (assert-uim-true  '(valid-key-str? "<Alt>return"))
  (assert-uim-true  '(valid-key-str? "<Meta>return"))
  (assert-uim-true  '(valid-key-str? "<Super>return"))
  (assert-uim-true  '(valid-key-str? "<Hyper>return"))

  (assert-uim-true  '(valid-key-str? "S- "))
  (assert-uim-true  '(valid-key-str? "S-!"))
  (assert-uim-true  '(valid-key-str? "S-0"))
  (assert-uim-true  '(valid-key-str? "S-A"))
  (assert-uim-true  '(valid-key-str? "S-a"))
  (assert-uim-true  '(valid-key-str? "S-return"))
  (assert-uim-true  '(valid-key-str? "C-return"))
  (assert-uim-true  '(valid-key-str? "A-return"))
  (assert-uim-true  '(valid-key-str? "M-return"))
  (assert-uim-true  '(valid-key-str? "S-return"))
  (assert-uim-true  '(valid-key-str? "H-return"))

  ;; single key with multiple modifiers
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta> "))
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta>!"))
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta>0"))
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta>A"))
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta>a"))
  (assert-uim-true  '(valid-key-str? "<Shift><Control><Meta>return"))

  (assert-uim-true  '(valid-key-str? "S-C-M- "))
  (assert-uim-true  '(valid-key-str? "S-C-M-!"))
  (assert-uim-true  '(valid-key-str? "S-C-M-0"))
  (assert-uim-true  '(valid-key-str? "S-C-M-A"))
  (assert-uim-true  '(valid-key-str? "S-C-M-a"))
  (assert-uim-true  '(valid-key-str? "S-C-M-return"))

  ;; single key with single translator
  (assert-uim-true  '(valid-key-str? "<IgnoreShift> "))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift>!"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift>0"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift>A"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift>a"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift>return"))
  (assert-uim-true  '(valid-key-str? "<IgnoreRegularShift>return"))
  (assert-uim-true  '(valid-key-str? "<IgnoreCase>return"))

  (assert-uim-true  '(valid-key-str? "J- "))
  (assert-uim-true  '(valid-key-str? "J-!"))
  (assert-uim-true  '(valid-key-str? "J-0"))
  (assert-uim-true  '(valid-key-str? "J-A"))
  (assert-uim-true  '(valid-key-str? "J-a"))
  (assert-uim-true  '(valid-key-str? "J-return"))
  (assert-uim-true  '(valid-key-str? "K-return"))
  (assert-uim-true  '(valid-key-str? "I-return"))

  ;; single key with multiple translators
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase> "))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase>!"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase>0"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase>A"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase>a"))
  (assert-uim-true  '(valid-key-str? "<IgnoreShift><IgnoreCase>return"))

  (assert-uim-true  '(valid-key-str? "J-I- "))
  (assert-uim-true  '(valid-key-str? "J-I-!"))
  (assert-uim-true  '(valid-key-str? "J-I-0"))
  (assert-uim-true  '(valid-key-str? "J-I-A"))
  (assert-uim-true  '(valid-key-str? "J-I-a"))
  (assert-uim-true  '(valid-key-str? "J-I-return"))
  #f)

(define (test-valid-strict-key-str?)
  ;; null key fails
  (assert-uim-false '(valid-strict-key-str? ""))

  ;; invalid key definitions
  (assert-uim-false  '(valid-strict-key-str? "nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "<Shift>nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "<Nonexistent>a"))
  (assert-uim-false  '(valid-strict-key-str? "<Nonexistent>nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "<Nonexistent><Shift>a"))
  (assert-uim-false  '(valid-strict-key-str? "<Nonexistent><Shift>nonexistent"))

  (assert-uim-false  '(valid-strict-key-str? "nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "S-nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "N-a"))
  (assert-uim-false  '(valid-strict-key-str? "N-nonexistent"))
  (assert-uim-false  '(valid-strict-key-str? "N-S-a"))
  (assert-uim-false  '(valid-strict-key-str? "N-S-nonexistent"))

  ;; single key
  (assert-uim-true  '(valid-strict-key-str? " "))
  (assert-uim-true  '(valid-strict-key-str? "!"))
  (assert-uim-true  '(valid-strict-key-str? "0"))
  (assert-uim-true  '(valid-strict-key-str? "A"))
  (assert-uim-true  '(valid-strict-key-str? "a"))
  (assert-uim-true  '(valid-strict-key-str? "return"))

  ;; single key with single modifier
  (assert-uim-true  '(valid-strict-key-str? "<Shift> "))
  (assert-uim-true  '(valid-strict-key-str? "<Shift>!"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift>0"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift>A"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift>a"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift>return"))
  (assert-uim-true  '(valid-strict-key-str? "<Control>return"))
  (assert-uim-true  '(valid-strict-key-str? "<Alt>return"))
  (assert-uim-true  '(valid-strict-key-str? "<Meta>return"))
  (assert-uim-true  '(valid-strict-key-str? "<Super>return"))
  (assert-uim-true  '(valid-strict-key-str? "<Hyper>return"))

  (assert-uim-false '(valid-strict-key-str? "S- "))
  (assert-uim-false '(valid-strict-key-str? "S-!"))
  (assert-uim-false '(valid-strict-key-str? "S-0"))
  (assert-uim-false '(valid-strict-key-str? "S-A"))
  (assert-uim-false '(valid-strict-key-str? "S-a"))
  (assert-uim-false '(valid-strict-key-str? "S-return"))
  (assert-uim-false '(valid-strict-key-str? "C-return"))
  (assert-uim-false '(valid-strict-key-str? "A-return"))
  (assert-uim-false '(valid-strict-key-str? "M-return"))
  (assert-uim-false '(valid-strict-key-str? "S-return"))
  (assert-uim-false '(valid-strict-key-str? "H-return"))

  ;; single key with multiple modifiers
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta> "))
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta>!"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta>0"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta>A"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta>a"))
  (assert-uim-true  '(valid-strict-key-str? "<Shift><Control><Meta>return"))

  (assert-uim-false '(valid-strict-key-str? "S-C-M- "))
  (assert-uim-false '(valid-strict-key-str? "S-C-M-!"))
  (assert-uim-false '(valid-strict-key-str? "S-C-M-0"))
  (assert-uim-false '(valid-strict-key-str? "S-C-M-A"))
  (assert-uim-false '(valid-strict-key-str? "S-C-M-a"))
  (assert-uim-false '(valid-strict-key-str? "S-C-M-return"))

  ;; single key with single translator
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift> "))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift>!"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift>0"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift>A"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift>a"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift>return"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreRegularShift>return"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreCase>return"))

  (assert-uim-false '(valid-strict-key-str? "J- "))
  (assert-uim-false '(valid-strict-key-str? "J-!"))
  (assert-uim-false '(valid-strict-key-str? "J-0"))
  (assert-uim-false '(valid-strict-key-str? "J-A"))
  (assert-uim-false '(valid-strict-key-str? "J-a"))
  (assert-uim-false '(valid-strict-key-str? "J-return"))
  (assert-uim-false '(valid-strict-key-str? "K-return"))
  (assert-uim-false '(valid-strict-key-str? "I-return"))

  ;; single key with multiple translators
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase> "))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>!"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>0"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>A"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>a"))
  (assert-uim-false '(valid-strict-key-str? "<IgnoreShift><IgnoreCase>return"))

  (assert-uim-false '(valid-strict-key-str? "J-I- "))
  (assert-uim-false '(valid-strict-key-str? "J-I-!"))
  (assert-uim-false '(valid-strict-key-str? "J-I-0"))
  (assert-uim-false '(valid-strict-key-str? "J-I-A"))
  (assert-uim-false '(valid-strict-key-str? "J-I-a"))
  (assert-uim-false '(valid-strict-key-str? "J-I-return"))
  #f)

(provide "test/key/test-predicate")
