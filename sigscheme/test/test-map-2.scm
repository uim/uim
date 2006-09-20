;;  Filename : test-map-2.scm
;;  About    : unit tests for R5RS map
;;
;;  Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

(load "test/unittest.scm")

(define tn test-name)


;;
;; map
;;

(tn "map invalid forms")
(assert-error  (tn) (lambda () (map)))
(assert-error  (tn) (lambda () (map even?)))

(tn "map single-arg invalid forms")
(assert-error  (tn) (lambda () (map #t '(0 1 2))))
(assert-error  (tn) (lambda () (map even? '(0 1 2 . 3))))
(assert-error  (tn) (lambda () (map even? #t)))
(assert-error  (tn) (lambda () (map even? '#(0 1 2))))
(if (not (and (provided? "sigscheme")
              (provided? "siod-bugs")))
    (assert-error  (tn) (lambda () (map even? #f))))

(tn "map single-arg")
;; not applicable
(assert-error  (tn) (lambda () (map car '(0 1 2))))
;; Evaluation order is unspecified.
(assert-true   (tn) (let* ((count 0)
                           (res (map (lambda (dummy)
                                       (set! count (+ count 1))
                                       count)
                                     '(a b))))
                      (or (equal? res '(1 2))
                          (equal? res '(2 1)))))
;; builtin procedure
(assert-equal? (tn)
               '()
               (map cadr '()))
(assert-equal? (tn)
               '(b)
               (map cadr '((a b))))
(assert-equal? (tn)
               '(b d f h)
               (map cadr '((a b) (c d) (e f) (g h))))
(assert-equal? (tn)
               '(b d g i)
               (map cadr '((a b) (c d e) (f g) (h i))))
;; closure
(assert-equal? (tn)
               '()
               (map (lambda (l) (car (cdr l)))
                    '()))
(assert-equal? (tn)
               '(b)
               (map (lambda (l) (car (cdr l)))
                    '((a b))))
(assert-equal? (tn)
               '(b d f h)
               (map (lambda (l) (car (cdr l)))
                    '((a b) (c d) (e f) (g h))))

(tn "map multiple-args invalid forms")
(assert-error  (tn) (lambda () (map #t '(0 1 2) '(3 4 5))))
(assert-error  (tn) (lambda () (map + '(0 1 2 . 3)  '(3 4 5 . 6))))
(assert-error  (tn) (lambda () (map + '(0 1 2) '(3 4 5) . #t)))
(assert-error  (tn) (lambda () (map + #t #t)))
(assert-error  (tn) (lambda () (map + '#(0 1 2) '#(3 4 5))))
(if (not (and (provided? "sigscheme")
              (provided? "siod-bugs")))
    (assert-error  (tn) (lambda () (map even? #f #f))))

(tn "map multiple-args")
;; not applicable
(assert-error  (tn) (lambda () (map car '(0 1 2) '(3 4 5))))
;; Evaluation order is unspecified.
(assert-true   (tn) (let* ((count 0)
                           (res (map (lambda (dummy1 dummy2 dummy3)
                                       (set! count (+ count 1))
                                       count)
                                     '(a b)
                                     '(c d)
                                     '(e f))))
                      (or (equal? res '(1 2))
                          (equal? res '(2 1)))))

(assert-equal? (tn)
               '()
               (map cons '() '()))
(assert-equal? (tn)
               '((0 . 3) (1 . 4) (2 . 5))
               (map cons '(0 1 2) '(3 4 5)))
;; R5RS: 6.4 Control features
;; > If more than one list is given, then they must all be the same length.
;; SigScheme rejects such user-error explicitly.
(assert-error  (tn) (lambda () (map cons '(0 1 2) '(3 4 5 6))))
(assert-error  (tn) (lambda () (map cons '(0 1 2) '(3 4))))
(assert-error  (tn) (lambda () (map cons '(0) '())))
(assert-error  (tn) (lambda () (map cons '() '(0))))

(assert-equal? (tn)
               '()
               (map +
                    '()
                    '()
                    '()))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '(0)
                      '()
                      '())))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '()
                      '(0)
                      '())))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '()
                      '()
                      '(0))))

(assert-equal? (tn)
               '(9 12 15)
               (map +
                    '(0 1 2)
                    '(3 4 5)
                    '(6 7 8)))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '(0 1 2 3)
                      '(3 4 5)
                      '(6 7 8))))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '(0 1 2)
                      '(3 4 5 6)
                      '(6 7 8))))
(assert-error  (tn)
               (lambda ()
                 (map +
                      '(0 1 2)
                      '(3 4 5)
                      '(6 7 8 9))))

;; closure
(assert-equal? (tn)
               '()
               (map (lambda (x y)
                      (cons x y))
                    '()))
(assert-equal? (tn)
               '((0 . 3) (1 . 4) (2 . 5))
               (map (lambda (x y)
                      (cons x y))
                    '(0 1 2) '(3 4 5)))
(assert-equal? (tn)
               '()
               (map (lambda (x y z)
                      (+ x y z))
                    '()
                    '()
                    '()))
(assert-equal? (tn)
               '(9 12 15)
               (map (lambda (x y z)
                      (+ x y z))
                    '(0 1 2)
                    '(3 4 5)
                    '(6 7 8)))

;;
;; for-each
;;

(tn "for-each invalid forms")
(assert-error  (tn) (lambda () (for-each)))
(assert-error  (tn) (lambda () (for-each even?)))

(tn "for-each single-arg invalid forms")
(assert-error  (tn) (lambda () (for-each #t '(0 1 2))))
(assert-error  (tn) (lambda () (for-each even? '(0 1 2 . 3))))
(assert-error  (tn) (lambda () (for-each even? #t)))
(assert-error  (tn) (lambda () (for-each even? '#(0 1 2))))
(if (not (and (provided? "sigscheme")
              (provided? "siod-bugs")))
    (assert-error  (tn) (lambda () (for-each even? #f))))

(tn "for-each single-arg")
;; not applicable
(assert-error  (tn) (lambda () (for-each car '(0 1 2))))
;; Evaluation order is guaranteed.
(assert-equal? (tn)
               '(2 1)
               (let* ((count 0)
                      (res '()))
                 (for-each (lambda (dummy)
                             (set! count (+ count 1))
                             (set! res (cons count res)))
                           '(a b))
                 res))

;; builtin procedure
(assert-equal? (tn)
               (undef)
               (for-each cadr '()))
(assert-equal? (tn)
               (undef)
               (for-each cadr '((a b))))
(assert-equal? (tn)
               (undef)
               (for-each cadr '((a b) (c d) (e f) (g h))))
(assert-equal? (tn)
               (undef)
               (for-each cadr '((a b) (c d e) (f g) (h i))))

;; closure
(assert-equal? (tn)
               (undef)
               (for-each (lambda (l)
                           (car (cdr l)))
                         '()))
(assert-equal? (tn)
               'not-modified
               (let ((res 'not-modified))
                 (for-each (lambda (l)
                             (set! res (car (cdr l))))
                           '())
                 res))

(assert-equal? (tn)
               (undef)
               (for-each (lambda (l)
                           (car (cdr l)))
                         '((a b))))
(assert-equal? (tn)
               'b
               (let ((res 'not-modified))
                 (for-each (lambda (l)
                             (set! res (car (cdr l))))
                           '((a b)))
                 res))

(assert-equal? (tn)
               (undef)
               (for-each (lambda (l)
                           (car (cdr l)))
                         '((a b) (c d) (e f) (g h))))
(assert-equal? (tn)
               '(h f d b)
               (let ((res '()))
                 (for-each (lambda (l)
                             (set! res (cons (car (cdr l))
                                             res)))
                           '((a b) (c d) (e f) (g h)))
                 res))

(tn "for-each multiple-args invalid forms")
(assert-error  (tn) (lambda () (for-each #t '(0 1 2) '(3 4 5))))
(assert-error  (tn) (lambda () (for-each + '(0 1 2 . 3)  '(3 4 5 . 6))))
(assert-error  (tn) (lambda () (for-each + '(0 1 2) '(3 4 5) . #t)))
(assert-error  (tn) (lambda () (for-each + #t #t)))
(assert-error  (tn) (lambda () (for-each + '#(0 1 2) '#(3 4 5))))
(if (not (and (provided? "sigscheme")
              (provided? "siod-bugs")))
    (assert-error  (tn) (lambda () (for-each even? #f #f))))

(tn "for-each multiple-args")
;; not applicable
(assert-error  (tn) (lambda () (for-each car '(0 1 2) '(3 4 5))))
;; Evaluation order is guaranteed.
(assert-equal? (tn)
               '(2 1)
               (let* ((count 0)
                      (res '()))
                 (for-each (lambda (dummy1 dummy2 dummy3)
                             (set! count (+ count 1))
                             (set! res (cons count res)))
                           '(a b)
                           '(c d)
                           '(e f))
                 res))

(assert-equal? (tn)
               (undef)
               (for-each cons '() '()))
(assert-equal? (tn)
               (undef)
               (for-each cons '(0 1 2) '(3 4 5)))
;; R5RS: 6.4 Control features
;; > If more than one list is given, then they must all be the same length.
;; SigScheme rejects such user-error explicitly.
(assert-error  (tn) (lambda () (for-each cons '(0 1 2) '(3 4 5 6))))
(assert-error  (tn) (lambda () (for-each cons '(0 1 2) '(3 4))))
(assert-error  (tn) (lambda () (for-each cons '(0) '())))
(assert-error  (tn) (lambda () (for-each cons '() '(0))))

(assert-equal? (tn)
               (undef)
               (for-each +
                         '()
                         '()
                         '()))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '(0)
                           '()
                           '())))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '()
                           '(0)
                           '())))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '()
                           '()
                           '(0))))

(assert-equal? (tn)
               (undef)
               (for-each +
                         '(0 1 2)
                         '(3 4 5)
                         '(6 7 8)))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '(0 1 2 3)
                           '(3 4 5)
                           '(6 7 8))))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '(0 1 2)
                           '(3 4 5 6)
                           '(6 7 8))))
(assert-error  (tn)
               (lambda ()
                 (for-each +
                           '(0 1 2)
                           '(3 4 5)
                           '(6 7 8 9))))

;; closure
(assert-equal? (tn)
               (undef)
               (for-each (lambda (x y)
                           (cons x y))
                         '()))
(assert-equal? (tn)
               'not-modified
               (let ((res 'not-modified))
                 (for-each (lambda (x y)
                             (set! res (cons x y)))
                           '())
                 res))

(assert-equal? (tn)
               (undef)
               (for-each (lambda (x y)
                           (cons x y))
                         '(0 1 2) '(3 4 5)))
(assert-equal? (tn)
               '((2 . 5) (1 . 4) (0 . 3))
               (let ((res '()))
                 (for-each (lambda (x y)
                             (set! res (cons (cons x y)
                                             res)))
                           '(0 1 2) '(3 4 5))
                 res))

(assert-equal? (tn)
               (undef)
               (for-each (lambda (x y z)
                           (+ x y z))
                         '()
                         '()
                         '()))
(assert-equal? (tn)
               'not-modified
               (let ((res 'not-modified))
                 (for-each (lambda (x y z)
                             (set! res (+ x y z)))
                           '()
                           '()
                           '())
                 res))

(assert-equal? (tn)
               (undef)
               (for-each (lambda (x y z)
                           (+ x y z))
                         '(0 1 2)
                         '(3 4 5)
                         '(6 7 8)))
(assert-equal? (tn)
               '(15 12 9)
               (let ((res '()))
                 (for-each (lambda (x y z)
                             (set! res (cons (+ x y z)
                                             res)))
                           '(0 1 2)
                           '(3 4 5)
                           '(6 7 8))
                 res))


(total-report)
