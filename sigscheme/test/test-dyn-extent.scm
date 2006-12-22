;;  Filename : test-dyn-extent.scm
;;  About    : unit test for dynamic extent
;;
;;  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

(if (not (symbol-bound? 'dynamic-wind))
    (test-skip "R5RS dynamic-wind is not enabled"))

(define *test-track-progress* #f)
(define tn test-name)

;;
;; dynamic-wind
;;

(define dynwind-res '())
(define append-sym!
  (lambda (sym)
    (set! dynwind-res (append dynwind-res (list sym)))))

(tn "dynamic-wind: without escape")
;; no escape with depth 1
(set! dynwind-res '())
(assert-equal? (tn)
               '(before thunk after)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before))
                     (lambda ()
                       (append-sym! 'thunk))
                     (lambda ()
                       (append-sym! 'after)))
                 dynwind-res))
                   
;; no escape with depth 2
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (dynamic-wind
                           (lambda ()
                             (append-sym! 'before2))
                           (lambda ()
                             (append-sym! 'thunk2))
                           (lambda ()
                             (append-sym! 'after2))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

;; no escape with depth 3
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 before3 thunk3
                 after3 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (dynamic-wind
                           (lambda ()
                             (append-sym! 'before2))
                           (lambda ()
                             (append-sym! 'thunk2)
                             (dynamic-wind
                                 (lambda ()
                                   (append-sym! 'before3))
                                 (lambda ()
                                   (append-sym! 'thunk3))
                                 (lambda ()
                                   (append-sym! 'after3))))
                           (lambda ()
                             (append-sym! 'after2))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

(tn "dynamic-wind: escape from deeper thunk")
;; escape from thunk1
(set! dynwind-res '())
(assert-equal? (tn)
               '(before thunk after)
               (begin
                 (call/cc
                  (lambda (k)
                    (dynamic-wind
                        (lambda ()
                          (append-sym! 'before))
                        (lambda ()
                          (append-sym! 'thunk)
                          (k #f))
                        (lambda ()
                          (append-sym! 'after)))))
                 dynwind-res))

;; escape from thunk2
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 after2 after1)
               (begin
                 (call/cc
                  (lambda (k)
                    (dynamic-wind
                        (lambda ()
                          (append-sym! 'before1))
                        (lambda ()
                          (append-sym! 'thunk1)
                          (dynamic-wind
                              (lambda ()
                                (append-sym! 'before2))
                              (lambda ()
                                (append-sym! 'thunk2)
                                (k #f))
                              (lambda ()
                                (append-sym! 'after2))))
                        (lambda ()
                          (append-sym! 'after1)))))
                 dynwind-res))

;; escape from thunk3
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 before3 thunk3
                         after3 after2 after1)
               (begin
                 (call/cc
                  (lambda (k)
                    (dynamic-wind
                        (lambda ()
                          (append-sym! 'before1))
                        (lambda ()
                          (append-sym! 'thunk1)
                          (dynamic-wind
                              (lambda ()
                                (append-sym! 'before2))
                              (lambda ()
                                (append-sym! 'thunk2)
                                (dynamic-wind
                                    (lambda ()
                                      (append-sym! 'before3))
                                    (lambda ()
                                      (append-sym! 'thunk3)
                                      (k #f))
                                    (lambda ()
                                      (append-sym! 'after3))))
                              (lambda ()
                                (append-sym! 'after2))))
                        (lambda ()
                          (append-sym! 'after1)))))
                 dynwind-res))

;; escape from thunk3 to thunk1
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 before3 thunk3
                         after3 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (call/cc
                        (lambda (k)
                          (dynamic-wind
                              (lambda ()
                                (append-sym! 'before2))
                              (lambda ()
                                (append-sym! 'thunk2)
                                (dynamic-wind
                                    (lambda ()
                                      (append-sym! 'before3))
                                    (lambda ()
                                      (append-sym! 'thunk3)
                                      (k #f))
                                    (lambda ()
                                      (append-sym! 'after3))))
                              (lambda ()
                                (append-sym! 'after2))))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

;; escape from thunk3 to thunk2
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 before3 thunk3
                         after3 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (dynamic-wind
                           (lambda ()
                             (append-sym! 'before2))
                           (lambda ()
                             (append-sym! 'thunk2)
                             (call/cc
                              (lambda (k)
                                (dynamic-wind
                                    (lambda ()
                                      (append-sym! 'before3))
                                    (lambda ()
                                      (append-sym! 'thunk3)
                                      (k #f))
                                    (lambda ()
                                      (append-sym! 'after3))))))
                           (lambda ()
                             (append-sym! 'after2))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

;; escape from thunk3 to thunk3
(set! dynwind-res '())
(assert-equal? (tn)
               '(before1 thunk1 before2 thunk2 before3 thunk3
                         after3 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (dynamic-wind
                           (lambda ()
                             (append-sym! 'before2))
                           (lambda ()
                             (append-sym! 'thunk2)
                                (dynamic-wind
                                    (lambda ()
                                      (append-sym! 'before3))
                                    (lambda ()
                                      (call/cc
                                       (lambda (k)
                                         (append-sym! 'thunk3)
                                         (k #f))))
                                    (lambda ()
                                      (append-sym! 'after3))))
                           (lambda ()
                             (append-sym! 'after2))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

(tn "dynamic-wind: SigScheme-specific escape behavior")
;; R5RS: 6.4 Control features
;; > The effect of using a captured continuation to enter or exit the dynamic
;; > extent of a call to before or after is undefined.

;; escape from before3 to thunk1
(set! dynwind-res '())
(if (provided? "sigscheme")
    (assert-equal? (tn)
                   '(before1 thunk1 before2 thunk2 before3 after2 after1)
                   (begin
                     (dynamic-wind
                         (lambda ()
                           (append-sym! 'before1))
                         (lambda ()
                           (append-sym! 'thunk1)
                           (call/cc
                            (lambda (k)
                              (dynamic-wind
                                  (lambda ()
                                    (append-sym! 'before2))
                                  (lambda ()
                                    (append-sym! 'thunk2)
                                    (dynamic-wind
                                        (lambda ()
                                          (append-sym! 'before3)
                                          (k #f))
                                        (lambda ()
                                          (append-sym! 'thunk3))
                                        (lambda ()
                                          (append-sym! 'after3))))
                                  (lambda ()
                                    (append-sym! 'after2))))))
                         (lambda ()
                           (append-sym! 'after1)))
                     dynwind-res)))

;; escape from after3 to thunk1
(set! dynwind-res '())
(if (provided? "sigscheme")
    (assert-equal? (tn)
                   '(before1 thunk1 before2 thunk2 before3 thunk3
                             after3 after2 after1)
                   (begin
                     (dynamic-wind
                         (lambda ()
                           (append-sym! 'before1))
                         (lambda ()
                           (append-sym! 'thunk1)
                           (call/cc
                            (lambda (k)
                              (dynamic-wind
                                  (lambda ()
                                    (append-sym! 'before2))
                                  (lambda ()
                                    (append-sym! 'thunk2)
                                    (dynamic-wind
                                        (lambda ()
                                          (append-sym! 'before3))
                                        (lambda ()
                                          (append-sym! 'thunk3))
                                        (lambda ()
                                          (append-sym! 'after3)
                                          (k #f))))
                                  (lambda ()
                                    (append-sym! 'after2))))))
                         (lambda ()
                           (append-sym! 'after1)))
                     dynwind-res)))

;; thunk3 -> after3 -> thunk1
(set! dynwind-res '())
(if (provided? "sigscheme")
    (assert-equal? (tn)
                   '(before1 thunk1 before2 thunk2 before3 thunk3
                             after3 after2 after1)
                   (begin
                     (dynamic-wind
                         (lambda ()
                           (append-sym! 'before1))
                         (lambda ()
                           (append-sym! 'thunk1)
                           (call/cc
                            (lambda (k)
                              (dynamic-wind
                                  (lambda ()
                                    (append-sym! 'before2))
                                  (lambda ()
                                    (append-sym! 'thunk2)
                                    (call/cc
                                     (lambda (j)
                                       (dynamic-wind
                                           (lambda ()
                                             (append-sym! 'before3))
                                           (lambda ()
                                             (append-sym! 'thunk3)
                                             (j #f))
                                           (lambda ()
                                             (append-sym! 'after3)
                                             (k #f))))))
                                  (lambda ()
                                    (append-sym! 'after2))))))
                         (lambda ()
                           (append-sym! 'after1)))
                     dynwind-res)))


(total-report)
