;;  Filename : test-string-core.scm
;;  About    : unit test for core procedures of R5RS string
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

(define tn test-name)
(define cp string-copy)

(if (and (provided? "sigscheme")
         (not (symbol-bound? 'string?)))
    (test-skip "core string part of R5RS is not enabled"))

;;
;; All procedures that take a string as argument are tested with
;; both immutable and mutable string.
;;
;; See "3.4 Storage model" of R5RS
;;


(tn "string?")
(assert-eq? (tn) #f (string? #f))
(assert-eq? (tn) #f (string? #t))
(assert-eq? (tn) #f (string? '()))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (string? (eof)))
      (assert-eq? (tn) #f (string? (undef)))))
(assert-eq? (tn) #f (string? 0))
(assert-eq? (tn) #f (string? 1))
(assert-eq? (tn) #f (string? 3))
(assert-eq? (tn) #f (string? -1))
(assert-eq? (tn) #f (string? -3))
(assert-eq? (tn) #f (string? 'symbol))
(assert-eq? (tn) #f (string? 'SYMBOL))
(assert-eq? (tn) #f (string? #\a))
(assert-eq? (tn) #f (string? #\あ))
;; immutable
(assert-eq? (tn) #t (string? ""))
(assert-eq? (tn) #t (string? " "))
(assert-eq? (tn) #t (string? "a"))
(assert-eq? (tn) #t (string? "A"))
(assert-eq? (tn) #t (string? "aBc12!"))
(assert-eq? (tn) #t (string? "あ"))
(assert-eq? (tn) #t (string? "あ0イう12!"))
;; mutable
(assert-eq? (tn) #t (string? (cp "")))
(assert-eq? (tn) #t (string? (cp " ")))
(assert-eq? (tn) #t (string? (cp "a")))
(assert-eq? (tn) #t (string? (cp "A")))
(assert-eq? (tn) #t (string? (cp "aBc12!")))
(assert-eq? (tn) #t (string? (cp "あ")))
(assert-eq? (tn) #t (string? (cp "あ0イう12!")))
(assert-eq? (tn) #f (string? +))
(assert-eq? (tn) #f (string? (lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (string? k))))
(assert-eq? (tn) #f (string? (current-output-port)))
(assert-eq? (tn) #f (string? '(#t . #t)))
(assert-eq? (tn) #f (string? (cons #t #t)))
(assert-eq? (tn) #f (string? '(0 1 2)))
(assert-eq? (tn) #f (string? (list 0 1 2)))
(assert-eq? (tn) #f (string? '#()))
(assert-eq? (tn) #f (string? (vector)))
(assert-eq? (tn) #f (string? '#(0 1 2)))
(assert-eq? (tn) #f (string? (vector 0 1 2)))
(tn "string? immutable")
(assert-true (tn) (string? ""))
(assert-true (tn) (string? "abcde"))
(assert-true (tn) (string? (symbol->string 'foo)))
(tn "string? mutable")
(assert-true (tn) (string? (cp "")))
(assert-true (tn) (string? (cp "abcde")))

(tn "string-length invalid objects")
(assert-error  (tn) (lambda () (string-length #t)))
(assert-error  (tn) (lambda () (string-length #f)))
(tn "string-length immutable")
(assert-equal? (tn) 0 (string-length ""))
(assert-equal? (tn) 1 (string-length " "))
(assert-equal? (tn) 1 (string-length "a"))
(assert-equal? (tn) 1 (string-length "A"))
(assert-equal? (tn) 5 (string-length "abcde"))
(assert-equal? (tn) 6 (string-length "aBc12!"))
(if (and (provided? "sigscheme")
         (not (provided? "utf-8")))
    (begin
      (assert-equal? (tn) 3 (string-length "あ"))
      (assert-equal? (tn) 13 (string-length "あ0イう12!")))
    (begin
      (assert-equal? (tn) 1 (string-length "あ"))
      (assert-equal? (tn) 7 (string-length "あ0イう12!"))))
(assert-equal? (tn) 1 (string-length "\""))
(assert-equal? (tn) 2 (string-length "\"\""))
(assert-equal? (tn) 3 (string-length "\"\\\""))
(assert-equal? (tn) 4 (string-length "\\\"\\\""))
(assert-equal? (tn) 1 (string-length "\\"))
(assert-equal? (tn) 2 (string-length "\\\\"))
(assert-equal? (tn) 3 (string-length "\\\\\\"))
(tn "string-length mutable")
(assert-equal? (tn) 0 (string-length (cp "")))
(assert-equal? (tn) 1 (string-length (cp " ")))
(assert-equal? (tn) 1 (string-length (cp "a")))
(assert-equal? (tn) 1 (string-length (cp "A")))
(assert-equal? (tn) 5 (string-length (cp "abcde")))
(assert-equal? (tn) 6 (string-length (cp "aBc12!")))
(if (and (provided? "sigscheme")
         (not (provided? "utf-8")))
    (begin
      (assert-equal? (tn) 3 (string-length (cp "あ")))
      (assert-equal? (tn) 13 (string-length (cp "あ0イう12!"))))
    (begin
      (assert-equal? (tn) 1 (string-length (cp "あ")))
      (assert-equal? (tn) 7 (string-length (cp "あ0イう12!")))))
(assert-equal? (tn) 1 (string-length (cp "\"")))
(assert-equal? (tn) 2 (string-length (cp "\"\"")))
(assert-equal? (tn) 3 (string-length (cp "\"\\\"")))
(assert-equal? (tn) 4 (string-length (cp "\\\"\\\"")))
(assert-equal? (tn) 1 (string-length (cp "\\")))
(assert-equal? (tn) 2 (string-length (cp "\\\\")))
(assert-equal? (tn) 3 (string-length (cp "\\\\\\")))

(tn "string=? invalid objects")
(assert-error  (tn) (lambda () (string=? "" #f)))
(assert-error  (tn) (lambda () (string=? "" #t)))
(assert-error  (tn) (lambda () (string=? #f "")))
(assert-error  (tn) (lambda () (string=? #t "")))
(tn "string=? immutable")
(assert-true   (tn) (string=? "" ""))
(assert-true   (tn) (string=? " " " "))
(assert-false  (tn) (string=? "" " "))
(assert-false  (tn) (string=? " " ""))
(assert-true   (tn) (string=? "a" "a"))
(assert-true   (tn) (string=? "A" "A"))
(assert-false  (tn) (string=? "A" "a"))
(assert-false  (tn) (string=? "a" "A"))
(assert-true   (tn) (string=? "\"" "\""))
(assert-true   (tn) (string=? "\\" "\\"))
(assert-true   (tn) (string=? "\\\\" "\\\\"))
(assert-true   (tn) (string=? "aBc12!" "aBc12!"))
(assert-false  (tn) (string=? "aBc12!" "aBc12"))
(assert-false  (tn) (string=? "aBc12!" "aBC12!"))
(assert-false  (tn) (string=? "aBc12" "aBc12!"))
(assert-false  (tn) (string=? "aBC12!" "aBc12!"))
(assert-true (tn) (string=? "abcde" "abcde"))
(assert-true (tn) (string=? "foo" "foo"))
(assert-true (tn) (string=? "foo" (symbol->string 'foo)))
(assert-true (tn) (string=? (symbol->string 'foo) "foo"))
(assert-true (tn) (string=? (symbol->string 'foo) (symbol->string 'foo)))
;; identical strings
(let ((s1 "")
      (s2 "aBc12!"))
  (assert-true   (tn) (string=? s1 s1))
  (assert-true   (tn) (string=? s2 s2)))
(tn "string=? mutable")
(assert-true   (tn) (string=? (cp "") (cp "")))
(assert-true   (tn) (string=? (cp " ") (cp " ")))
(assert-false  (tn) (string=? (cp "") (cp " ")))
(assert-false  (tn) (string=? (cp " ") (cp "")))
(assert-true   (tn) (string=? (cp "a") (cp "a")))
(assert-true   (tn) (string=? (cp "A") (cp "A")))
(assert-false  (tn) (string=? (cp "A") (cp "a")))
(assert-false  (tn) (string=? (cp "a") (cp "A")))
(assert-true   (tn) (string=? (cp "\"") (cp "\"")))
(assert-true   (tn) (string=? (cp "\\") (cp "\\")))
(assert-true   (tn) (string=? (cp "\\\\") (cp "\\\\")))
(assert-true   (tn) (string=? (cp "aBc12!") (cp "aBc12!")))
(assert-false  (tn) (string=? (cp "aBc12!") (cp "aBc12")))
(assert-false  (tn) (string=? (cp "aBc12!") (cp "aBC12!")))
(assert-false  (tn) (string=? (cp "aBc12") (cp "aBc12!")))
(assert-false  (tn) (string=? (cp "aBC12!") (cp "aBc12!")))
(assert-true (tn) (string=? (cp "abcde") (cp "abcde")))
(assert-true (tn) (string=? (cp "foo") (cp "foo")))
(assert-true (tn) (string=? (cp "foo") (symbol->string 'foo)))
(assert-true (tn) (string=? (symbol->string 'foo) (cp "foo")))
(assert-true (tn) (string=? (symbol->string 'foo) (symbol->string 'foo)))
;; identical strings
(let ((s1 (cp ""))
      (s2 (cp "aBc12!")))
  (assert-true   (tn) (string=? s1 s1))
  (assert-true   (tn) (string=? s2 s2)))
(tn "string=? mixed")
(assert-true (tn) (string=? (cp "") ""))
(assert-true (tn) (string=? (cp "foo") "foo"))
(assert-true (tn) (string=? (cp "foo") (symbol->string 'foo)))

(tn "string-append immutable")
(assert-equal? (tn) ""       (string-append))
(assert-equal? (tn) 0 (string-length (string-append)))
(assert-equal? (tn) ""       (string-append ""))
(assert-equal? (tn) ""       (string-append "" ""))
(assert-equal? (tn) ""       (string-append "" "" ""))
(assert-equal? (tn) 0 (string-length (string-append "" "" "")))
(assert-equal? (tn) "a"      (string-append "a"))
(assert-equal? (tn) "ab"     (string-append "a" "b"))
(assert-equal? (tn) "abc"    (string-append "a" "b" "c"))
(assert-equal? (tn) 3 (string-length (string-append "a" "b" "c")))
(assert-equal? (tn) "ab"     (string-append "ab"))
(assert-equal? (tn) "abcd"   (string-append "ab" "cd"))
(assert-equal? (tn) "abcdef" (string-append "ab" "cd" "ef"))
(assert-equal? (tn) 6 (string-length (string-append "ab" "cd" "ef")))
(assert-equal? (tn) "あ0イう12!" (string-append "あ0" "イ" "う12!"))
(if (and (provided? "sigscheme")
         (not (provided? "utf-8")))
    (assert-equal? (tn) 13 (string-length (string-append "あ0" "イ" "う12!")))
    (assert-equal? (tn) 7 (string-length (string-append "あ0" "イ" "う12!"))))
(let ((s1 "")
      (s2 "a")
      (s3 "aBc12!"))
  (assert-equal? (tn) "" (string-append s1))
  (assert-true   (tn) (string=? "" (string-append s1)))
  (assert-false  (tn) (eq? "" (string-append s1)))
  (assert-equal? (tn) "a" (string-append s2))
  (assert-true   (tn) (string=? "a" (string-append s2)))
  (assert-false  (tn) (eq? "a" (string-append s2)))
  (assert-equal? (tn) "aBc12!" (string-append s3))
  (assert-true   (tn) (string=? "aBc12!" (string-append s3)))
  (assert-false  (tn) (eq? "aBc12!" (string-append s3))))
(tn "string-append mutable")
(assert-equal? (tn) ""       (string-append (cp "")))
(assert-equal? (tn) ""       (string-append (cp "") (cp "")))
(assert-equal? (tn) ""       (string-append (cp "") (cp "") (cp "")))
(assert-equal? (tn) 0 (string-length (string-append (cp "") (cp "") (cp ""))))
(assert-equal? (tn) "a"      (string-append (cp "a")))
(assert-equal? (tn) "ab"     (string-append (cp "a") (cp "b")))
(assert-equal? (tn) "abc"    (string-append (cp "a") (cp "b") (cp "c")))
(assert-equal? (tn)
               3
               (string-length (string-append (cp "a") (cp "b") (cp "c"))))
(assert-equal? (tn) "ab"     (string-append (cp "ab")))
(assert-equal? (tn) "abcd"   (string-append (cp "ab") (cp "cd")))
(assert-equal? (tn) "abcdef" (string-append (cp "ab") (cp "cd") (cp "ef")))
(assert-equal? (tn)
               6
               (string-length (string-append (cp "ab") (cp "cd") (cp "ef"))))
(assert-equal? (tn)
               "あ0イう12!"
               (string-append (cp "あ0") (cp "イ") (cp "う12!")))
(if (and (provided? "sigscheme")
         (not (provided? "utf-8")))
    (assert-equal? (tn) 13 (string-length
                            (string-append (cp "あ0") (cp "イ") (cp "う12!"))))
    (assert-equal? (tn) 7 (string-length
                           (string-append (cp "あ0") (cp "イ") (cp "う12!")))))
(tn "string-append mixed")
(assert-equal? (tn) ""    (string-append (cp "") ""))
(assert-equal? (tn) 0 (string-length (string-append (cp "") "")))
(assert-equal? (tn) "ab"  (string-append (cp "a") "b"))
(assert-equal? (tn) "abc" (string-append "a" (cp "b") (cp "c")))
(assert-equal? (tn) "abc" (string-append (cp "a") "b" (cp "c")))
(assert-equal? (tn) "abc" (string-append (cp "a") (cp "b") "c"))
(assert-equal? (tn) "abc" (string-append "a" "b" (cp "c")))
(assert-equal? (tn) "abc" (string-append "a" (cp "b") "c"))
(assert-equal? (tn) "abc" (string-append (cp "a") "b" "c"))
(assert-equal? (tn) 3 (string-length (string-append (cp "a") "b" "c")))
(assert-equal? (tn)
               "あ0イう12!"
               (string-append "あ0" (cp "イ") (cp "う12!")))
(assert-equal? (tn) 3 (string-length (string-append (cp "a") "b" "c")))
(if (and (provided? "sigscheme")
         (not (provided? "utf-8")))
    (assert-equal? (tn) 13 (string-length
                            (string-append "あ0" (cp "イ") (cp "う12!"))))
    (assert-equal? (tn) 7 (string-length
                           (string-append "あ0" (cp "イ") (cp "う12!")))))

(tn "string-copy")
(let* ((s1 "")
       (s2 "aBc12!")
       (s3 (string-copy s2)))
  (assert-false  (tn) (eq? s1 (string-copy s1)))
  (assert-true   (tn) (string=? "" (string-copy s1)))
  (assert-equal? (tn) "" (string-copy s1))
  (if (symbol-bound? 'string-set!)
      (assert-error  (tn) (lambda () (string-set! s1 0 #\z))))
  (assert-false  (tn) (eq? s2 (string-copy s2)))
  (assert-true   (tn) (string=? "aBc12!" (string-copy s2)))
  (assert-equal? (tn) "aBc12!" (string-copy s2))
  (if (symbol-bound? 'string-set!)
      (begin
        (assert-error  (tn) (lambda () (string-set! s2 0 #\z)))
        (string-set! s3 0 #\z)
        (assert-true   (tn) (string=? "zBc12!" s3))
        (assert-true   (tn) (string=? s3 "zBc12!"))
        (assert-equal? (tn) "zBc12!" s3)
        (assert-equal? (tn) s3 "zBc12!"))))


(total-report)
