;;  Filename : test-syntax.scm
;;  About    : unit test for R5RS syntaxes
;;
;;  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

(define *test-track-progress* #f)
(define tn test-name)

(define tee #t)
(define ef #f)


;;
;; if
;;
(tn "if invalid form")
(assert-error  (tn) (lambda () (if)))
(assert-error  (tn) (lambda () (if #t)))
(assert-error  (tn) (lambda () (if #f)))
(assert-error  (tn) (lambda () (if #t 'true 'false 'excessive)))
(assert-error  (tn) (lambda () (if #f 'true 'false 'excessive)))

(tn "if")
(assert-equal? (tn) 'true  (if #t 'true 'false))
(assert-equal? (tn) 'true  (if #t 'true))
(assert-equal? (tn) 'false (if #f 'true 'false))
;; check that does not cause error
(assert-equal? (tn) (if #f 'true) (if #f 'true))
;; check that <test> is evaluated
(assert-equal? (tn) 'true  (if tee 'true 'false))
(assert-equal? (tn) 'false (if ef 'true 'false))

;;
;; set!
;;
(tn "set! invalid form")
(define test-var #f)
(assert-error  (tn) (lambda () (set!)))
(assert-error  (tn) (lambda () (set! test-unbound)))
(assert-error  (tn) (lambda () (set! test-var)))
(assert-error  (tn) (lambda () (set! test-var #t #t)))
(assert-error  (tn) (lambda () (set! 1 #t)))

(tn "set!")
(assert-true   (tn) (set! test-var 'foo))
(assert-equal? (tn) 'foo test-var)
(assert-true   (tn) (let ((test-var #f))
                      (set! test-var 'bar)))
(assert-equal? (tn) 'foo test-var)
(assert-true   (tn) (let ((test-var #f))
                      (let ((test-var2 #f))
                        (set! test-var 'baz))
                      (assert-equal? (tn) 'baz test-var)))
(assert-equal? (tn) 'foo test-var)
(assert-error  (tn) (lambda () (set! test-unbound 'foo)))
(assert-error  (tn) (lambda ()
                      (let ((test-var #f))
                        (set! test-unbound 'foo))))

;;
;; cond
;;
(tn "cond invalid form")
;; 'cond' must contain at least one clause
(assert-error  (tn) (lambda ()
                      (cond)))
;; empty clause
(assert-error  (tn) (lambda ()
                      (cond
                       ())))
;; empty clause with 'else'
(assert-error  (tn) (lambda ()
                      (cond
                       ()
                       (else #t))))
;; 'else' followed by another caluse
(assert-error  (tn) (lambda ()
                      (cond
                       (else #t)
                       (#t))))
;; 'else' clause must contain at least one expression (7.1.3 Expressions)
(assert-error  (tn) (lambda ()
                      (cond
                       (else))))
;; '=>' is interpreted as unbound symbol if not followed by <exp>
(assert-error  (tn) (lambda ()
                      (cond
                       (#t =>))))
;; '=>' is interpreted as unbound symbol
(assert-error  (tn) (lambda ()
                      (cond
                       (=>))))
;; evaluation of '=>' causes error even if 'else' clause exists
(assert-error  (tn) (lambda ()
                      (cond
                       (#t =>)
                       (else #t))))
;; '=>' is interpreted as unbound symbol even if in 'else' clause
(assert-error  (tn) (lambda ()
                      (cond
                       (else =>))))
;; '=>' is interpreted as unbound symbol even if in 'else' clause
(assert-error  (tn) (lambda ()
                      (cond
                       (else => 1 3))))
;; not a procedure
(assert-error  (tn) (lambda ()
                      (cond
                       (#t => #t))))
(assert-error  (tn) (lambda ()
                      (cond
                       (#t => #f))))
;; procedure but argument number mismatch
(assert-error  (tn) (lambda ()
                      (cond
                       (#t => eq?))))
;; not a procedure but a syntax
(assert-error  (tn) (lambda ()
                      (cond
                       (#t => delay))))
;; '=>' is not applicable at 'else' clause
(assert-error  (tn) (lambda ()
                      (cond
                       (else => values))))

(tn "cond unspecified behavior")
;; not specified in R5RS, but SigScheme surely returns #<undef>
(if (provided? "sigscheme")
    (assert-equal?  (tn)
                    (undef)
                    (cond
                     (#f))))
(if (provided? "sigscheme")
    (assert-equal?  (tn)
                    (undef)
                    (cond
                     ((even? 3) #f)
                     ((positive? -1) #f))))


(tn "cond")
;; R5RS: If the selected <clause> contains only the <test> and no
;; <expression>s, then the value of the <test> is returned as the result.
(assert-equal?  (tn)
                #t
                (cond
                 (#t)))
(assert-equal?  (tn)
                3
                (cond
                 (#f)
                 (3)))
(assert-equal?  (tn)
                3
                (cond
                 ((not #t))
                 ((+ 1 2))))
(assert-equal?  (tn)
                3
                (cond
                 (#f)
                 (3)
                 (4)))

(assert-equal? (tn)
               'greater
               (cond
                ((> 3 2) 'greater)
                ((< 3 2) 'less)))
(assert-equal? (tn)
               'equal
               (cond
                ((> 3 3) 'greater)
                ((< 3 3) 'less)
                (else 'equal)))
(assert-equal? (tn)
               #t
               (cond
                ((> 3 2))
                ((< 3 4) 'less)
                (else 'equal)))
(assert-equal? (tn)
               2
               (cond
                ((assv 'b '((a 1) (b 2))) => cadr)
                (else #f)))
(assert-equal? (tn)
               2
               (cond
                ((assv 'b '((a 1) (b 2))) => (car (list cadr cdar)))
                (else #f)))
(assert-equal? (tn)
               #f
               (cond
                ((assv 'c '((a 1) (b 2))) => cadr)
                (else #f)))
(assert-equal? (tn)
               'greater1
               (cond
                ((> 3 2) 'greater0 'greater1)
                (else #f)))
;; single 'else' clause is allowed
(assert-equal? (tn)
               'else
               (cond
                (else 'else)))
;; '=>' is interpreted as ordinary symbol if not followed by exactly one <exp>
(assert-equal? (tn)
               'defined
               (let ((=> 'defined))
                 (cond
                  (#t =>))))
(assert-equal? (tn)
               3
               (let ((=> 'defined))
                 (cond
                  (#t => 1 3))))


;;
;; case
;;
(tn "case invalid form")
(assert-error  (tn)
               (lambda ()
                 (case)))
(assert-error  (tn)
               (lambda ()
                 (case 'key)))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   ())))
;; case requires at least one <exp> in a clause
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   ((key)))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   ((1 key)))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   (1))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   ((1 . 2)))))
;; not a ((<datum> ...) <exp>...) style clause
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   (1 'matched))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   (key 'matched))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                   (#(key) 'matched))))

(if (provided? "sigscheme")
    (begin
      ;; improper clause does not cause error if not evaled
      (assert-equal? (tn)
                     (undef)
                     (case 'key
                       ((1) . 2)))
      (assert-equal?  (tn)
                      (undef)
                      (case 'key
                        ((1) #t . 2)))
      ;; causes error when evaled
      (assert-error  (tn)
                     (lambda ()
                       (case 1
                         ((1) . 2))))
      (assert-error  (tn)
                     (lambda ()
                       (case 1
                         ((1) #t . 2))))))

(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ()
                  (else #t))))
;; 'else' followed by another caluse
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (else #t)
                  (#t))))
;; not specified in R5RS, but SigScheme should cause error
(if (provided? "sigscheme")
    (assert-error  (tn)
                   (lambda ()
                     (case 'key
                      (else)))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (=>))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (#t =>))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (#t =>)
                  (else #t))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (else =>))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (else => symbol?))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  (else => #t))))
;; (<exp1> => <exp2>) clause is not supported by 'case'
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ((key) => values))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ((key) => eq?))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ((key) => delay))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ((key) => #t))))
(assert-error  (tn)
               (lambda ()
                 (case 'key
                  ((key) => #f))))

(tn "case unspecified behavior")
;; not specified in R5RS, but SigScheme surely returns #<undef>
(if (provided? "sigscheme")
    (assert-equal?  (tn)
                    (undef)
                    (case 'key
                      ((#f) #f))))
(if (provided? "sigscheme")
    (assert-equal?  (tn)
                    (undef)
                    (case 'key
                      ((foo) #f)
                      ((bar) #f))))

(tn "case")
(assert-equal? (tn)
               'odd
               (case 3
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)))
(assert-equal? (tn)
               'unknown
               (case 0
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? (tn)
               'odd
               (case (+ 1 2)
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? (tn)
               'second
               (case 3
                 ((1 3 5) 'first 'second)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? (tn)
               'second
               (case (+ 1 2)
                 ((1 3 5) 'first 'second)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? (tn)
               'third
               (case 'key
                 ((1 foo 5) 'first 'second)
                 ((2 key 6) 'third)
                 (else 'unknown)))
(assert-equal? (tn)
               'third
               (case (cadr '(foo key bar))
                 ((1 foo 5) 'first 'second)
                 ((2 key 6) 'third)
                 (else 'unknown)))
;; single 'else' clause is allowed
(assert-equal? (tn)
               'else
               (case 'key
                (else 'else)))

;;
;; and
;;
(tn "and")
(assert-error  (tn) (lambda () (and . #t)))
(assert-error  (tn) (lambda () (and #t . #t)))
(assert-equal? (tn) #t (and))
(assert-equal? (tn) #t (and (= 2 2) (> 2 1)))
(assert-equal? (tn) #f (and (= 2 2) (< 2 1)))
(assert-equal? (tn) '(f g) (and 1 2 'c '(f g)))
(assert-equal? (tn) #f (and #t #f))
(assert-equal? (tn) 3  (and #t (+ 1 2)))
(assert-equal? (tn) #f (and #t (not 3) (+ 1 2)))

;;
;; or
;;
(tn "or")
(assert-error  (tn) (lambda () (or . #t)))
(assert-error  (tn) (lambda () (or #t . #t)))
(assert-equal? (tn) #f (or))
(assert-equal? (tn) #t (or (= 2 2) (> 2 1)))
(assert-equal? (tn) #t (or (= 2 2) (< 2 1)))
(assert-equal? (tn) #f (or #f #f #f))
(assert-equal? (tn) '(b c) (or (memq 'b '(a b c))
                               (/ 3 0)))
(assert-equal? (tn) 3  (or #f (+ 1 2)))
(assert-equal? (tn) 3  (or #f (not 3) (+ 1 2) (not 4)))


(total-report)
