;;  FileName : test-srfi2.scm
;;  About    : unit test for the SRFI-2 'and-let*'
;;
;;  Copyright (C) 2005 Kazuki Ohta <mover AT hct.zaq.ne.jp>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "./test/unittest.scm")
(use srfi-2)

;; (and-let* <claws> <body>)
;; 
;; <claws> ::= '() | (cons <claw> <claws>)
;; <claw>  ::=  (<variable> <expression>) | (<expression>)
;;              | <bound-variable>

(define true #t)
(define false #f)

; and-let*
(assert-true  "and-let* test 1" (and-let* () #t))
(assert-true  "and-let* test 2" (and-let* () #t #t))
(assert-true  "and-let* test 3" (and-let* () #t #t #t))
(assert-false "and-let* test 4" (and-let* () #f))
(assert-false "and-let* test 5" (and-let* () #t #f))
(assert-false "and-let* test 6" (and-let* () #t #t #f))
(assert-false "and-let* test 7" (and-let* ((false (< 2 1)))
                                          #t))
(assert-false "and-let* test 8" (and-let* ((true  (< 1 2))
                                           (false (< 2 1)))
                                          #t))
(assert-true  "and-let* test 9" (and-let* ((one 1)
                                           (two (+ one 1))
                                           (three (+ two 1)))
                                          (= three 3)))
(assert-false "and-let* test 10" (and-let* ((one 1)
                                            (two (+ one 1))
                                            (three (+ two 1)))
                                           (= three 4)))

;; <bound-variable> style claw
(assert-true  "and-let* #11" (and-let* (true)
                               'ok))
(assert-true  "and-let* #12" (and-let* (even?)
                               'ok))
(assert-false "and-let* #13" (and-let* (false)
                               'ok))
(assert-true  "and-let* #14" (and-let* (even?
                                        true)
                               'ok))
(assert-false "and-let* #15" (and-let* (even?
                                        true
                                        false)
                               'ok))

;; (<expression>) style claw
(assert-true  "and-let* #16" (and-let* ((#t))
                               'ok))
(assert-false "and-let* #17" (and-let* ((#f))
                               'ok))
(assert-true  "and-let* #18" (and-let* (((integer? 1)))
                               'ok))
(assert-false "and-let* #19" (and-let* (((integer? #t)))
                               'ok))
(assert-true  "and-let* #20" (and-let* (((integer? 1))
                                        ((integer? 2)))
                               'ok))
(assert-false "and-let* #21" (and-let* (((integer? 1))
                                        ((integer? 2))
                                        ((integer? #t)))
                               'ok))
;; procedure itself as value
(assert-true "and-let* #22" (and-let* ((even?))
                               'ok))

;; combined form
(assert-true  "and-let* #23" (and-let* (true
                                        even?
                                        ((integer? 1)))
                               'ok))
(assert-true  "and-let* #24" (and-let* (true
                                        even?
                                        ((integer? 1))
                                        (foo '(1 2 3))
                                        ((list? foo))
                                        (bar foo))
                               'ok))
(assert-false "and-let* #25" (and-let* (true
                                        even?
                                        ((integer? 1))
                                        (foo #(1 2 3))
                                        ((list? foo))
                                        (bar foo))
                               'ok))
(assert-false "and-let* #26" (and-let* (true
                                        even?
                                        ((integer? 1))
                                        (foo '(1 2 3))
                                        (bar (car foo))
                                        bar
                                        ((null? bar)))
                               'ok))

(total-report)
