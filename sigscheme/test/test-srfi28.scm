;;  FileName : test-srfi28.scm
;;  About    : unit test for SRFI-28
;;
;;  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

;;(set! *test-track-progress* #t)

(use srfi-28)

(define tn test-name)

(tn "format invalid form")
(assert-error  (tn) (lambda () (format)))
(assert-error  (tn) (lambda () (format #f)))
(assert-error  (tn) (lambda () (format #\a)))
;; FIXME: assertion failed
;;(assert-error  (tn) (lambda () (format "~")))

(tn "format unknown directives")
(assert-error  (tn) (lambda () (format "~z")))
(assert-error  (tn) (lambda () (format "~Z")))
(assert-error  (tn) (lambda () (format "~'")))
(assert-error  (tn) (lambda () (format "~$")))

(tn "format SRFI-48 directives")
(if (not (provided? "srfi-48"))
    (begin
      (assert-error  (tn) (lambda () (format "~w" 0)))
      (assert-error  (tn) (lambda () (format "~d" 0)))
      (assert-error  (tn) (lambda () (format "~x" 0)))
      (assert-error  (tn) (lambda () (format "~o" 0)))
      (assert-error  (tn) (lambda () (format "~b" 0)))
      (assert-error  (tn) (lambda () (format "~c" #\a)))
      (assert-error  (tn) (lambda () (format "~f" 0)))
      (assert-error  (tn) (lambda () (format "~2f" 0)))
      (assert-error  (tn) (lambda () (format "~2,3f" 0)))
      (assert-error  (tn) (lambda () (format "~?" "~s" '(0))))
      (assert-error  (tn) (lambda () (format "~k" "~s" '(0))))
      (assert-error  (tn) (lambda () (format "~y" '(0))))

      (assert-error  (tn) (lambda () (format "~t")))
      (assert-error  (tn) (lambda () (format "~_")))
      (assert-error  (tn) (lambda () (format "~&")))
      (assert-error  (tn) (lambda () (format "~h")))))

(tn "format no directive")
(assert-error  (tn) (lambda () (format "" 0)))
(assert-equal? (tn)
               ""
               (format ""))
(assert-equal? (tn)
               "aBc"
               (format "aBc"))

(tn "format ~a")
(assert-error  (tn) (lambda () (format "~a")))
(assert-error  (tn) (lambda () (format "~a" 0 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~a" #f))
(assert-equal? (tn)
               "#t"
               (format "~a" #t))
(assert-equal? (tn)
               "123"
               (format "~a" 123))
(assert-equal? (tn)
               "a"
               (format "~a" #\a))
(assert-equal? (tn)
               "aBc"
               (format "~a" "aBc"))
(assert-equal? (tn)
               "(#t 123 a aBc (0))"
               (format "~a" '(#t 123 #\a "aBc" (0))))

(tn "format ~A")
(assert-error  (tn) (lambda () (format "~A")))
(assert-error  (tn) (lambda () (format "~A" 0 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~A" #f))
(assert-equal? (tn)
               "#t"
               (format "~A" #t))
(assert-equal? (tn)
               "123"
               (format "~A" 123))
(assert-equal? (tn)
               "a"
               (format "~A" #\a))
(assert-equal? (tn)
               "aBc"
               (format "~A" "aBc"))
(assert-equal? (tn)
               "(#t 123 a aBc (0))"
               (format "~A" '(#t 123 #\a "aBc" (0))))

(tn "format ~s")
(assert-error  (tn) (lambda () (format "~s")))
(assert-error  (tn) (lambda () (format "~s" 0 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~s" #f))
(assert-equal? (tn)
               "#t"
               (format "~s" #t))
(assert-equal? (tn)
               "123"
               (format "~s" 123))
(assert-equal? (tn)
               "#\\a"
               (format "~s" #\a))
(assert-equal? (tn)
               "\"aBc\""
               (format "~s" "aBc"))
(assert-equal? (tn)
               "(#t 123 #\\a \"aBc\" (0))"
               (format "~s" '(#t 123 #\a "aBc" (0))))

(tn "format ~S")
(assert-error  (tn) (lambda () (format "~S")))
(assert-error  (tn) (lambda () (format "~S" 0 1)))
(assert-equal? (tn)
               (if (and (provided? "sigscheme")
                        (provided? "siod-bugs"))
                   "()"
                   "#f")
               (format "~S" #f))
(assert-equal? (tn)
               "#t"
               (format "~S" #t))
(assert-equal? (tn)
               "123"
               (format "~S" 123))
(assert-equal? (tn)
               "#\\a"
               (format "~S" #\a))
(assert-equal? (tn)
               "\"aBc\""
               (format "~S" "aBc"))
(assert-equal? (tn)
               "(#t 123 #\\a \"aBc\" (0))"
               (format "~S" '(#t 123 #\a "aBc" (0))))

(tn "format ~%")
(assert-error  (tn) (lambda () (format "~%" 0)))
(assert-equal? (tn)
               "\n"
               (format "~%"))

(tn "format ~~")
(assert-error  (tn) (lambda () (format "~~" 0)))
(assert-equal? (tn)
               "~"
               (format "~~"))

(tn "format mixed directives")
(assert-equal? (tn)
               "~\n"
               (format "~~~%"))
(assert-equal? (tn)
               "slashified: #\\a\nany: a\n"
               (format "slashified: ~s~%any: ~a~%" #\a #\a))

(total-report)
