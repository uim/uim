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


(tn "string? immutable")
(assert-true (tn) (string? ""))
(assert-true (tn) (string? "abcde"))
(assert-true (tn) (string? (symbol->string 'foo)))
(tn "string? mutable")
(assert-true (tn) (string? (cp "")))
(assert-true (tn) (string? (cp "abcde")))

(tn "string-length immutable")
(assert-equal? (tn) 0 (string-length ""))
(assert-equal? (tn) 5 (string-length "abcde"))
(assert-equal? (tn) 1 (string-length "\\"))
(assert-equal? (tn) 2 (string-length "\\\\"))
(assert-equal? (tn) 3 (string-length "\\\\\\"))
(tn "string-length mutable")
(assert-equal? (tn) 0 (string-length (cp "")))
(assert-equal? (tn) 5 (string-length (cp "abcde")))
(assert-equal? (tn) 1 (string-length (cp "\\")))
(assert-equal? (tn) 2 (string-length (cp "\\\\")))
(assert-equal? (tn) 3 (string-length (cp "\\\\\\")))

(tn "string=? immutable")
(assert-true (tn) (string=? "" ""))
(assert-true (tn) (string=? "abcde" "abcde"))
(assert-true (tn) (string=? "foo" "foo"))
(assert-true (tn) (string=? "foo" (symbol->string 'foo)))
(assert-true (tn) (string=? (symbol->string 'foo) "foo"))
(assert-true (tn) (string=? (symbol->string 'foo) (symbol->string 'foo)))
(tn "string=? mutable")
(assert-true (tn) (string=? (cp "") (cp "")))
(assert-true (tn) (string=? (cp "foo") (cp "foo")))
(tn "string=? mixed")
(assert-true (tn) (string=? (cp "") ""))
(assert-true (tn) (string=? (cp "foo") "foo"))
(assert-true (tn) (string=? (cp "foo") (symbol->string 'foo)))

(tn "string-append immutable")
(assert-equal? (tn) ""       (string-append ""))
(assert-equal? (tn) ""       (string-append "" ""))
(assert-equal? (tn) ""       (string-append "" "" ""))
(assert-equal? (tn) "a"      (string-append "a"))
(assert-equal? (tn) "ab"     (string-append "a" "b"))
(assert-equal? (tn) "abc"    (string-append "a" "b" "c"))
(assert-equal? (tn) "ab"     (string-append "ab"))
(assert-equal? (tn) "abcd"   (string-append "ab" "cd"))
(assert-equal? (tn) "abcdef" (string-append "ab" "cd" "ef"))
(tn "string-append mutable")
(assert-equal? (tn) ""       (string-append (cp "")))
(assert-equal? (tn) ""       (string-append (cp "") (cp "")))
(assert-equal? (tn) ""       (string-append (cp "") (cp "") (cp "")))
(assert-equal? (tn) "a"      (string-append (cp "a")))
(assert-equal? (tn) "ab"     (string-append (cp "a") (cp "b")))
(assert-equal? (tn) "abc"    (string-append (cp "a") (cp "b") (cp "c")))
(assert-equal? (tn) "ab"     (string-append (cp "ab")))
(assert-equal? (tn) "abcd"   (string-append (cp "ab") (cp "cd")))
(assert-equal? (tn) "abcdef" (string-append (cp "ab") (cp "cd") (cp "ef")))
(tn "string-append mixed")
(assert-equal? (tn) ""    (string-append (cp "") ""))
(assert-equal? (tn) "ab"  (string-append (cp "a") "b"))
(assert-equal? (tn) "abc" (string-append "a" (cp "b") (cp "c")))
(assert-equal? (tn) "abc" (string-append (cp "a") "b" (cp "c")))
(assert-equal? (tn) "abc" (string-append (cp "a") (cp "b") "c"))
(assert-equal? (tn) "abc" (string-append "a" "b" (cp "c")))
(assert-equal? (tn) "abc" (string-append "a" (cp "b") "c"))
(assert-equal? (tn) "abc" (string-append (cp "a") "b" "c"))

(tn "string-copy")
(assert-equal? (tn) ""   (string-copy ""))
(assert-equal? (tn) "a"  (string-copy "a"))
(assert-equal? (tn) "ab" (string-copy "ab"))


(total-report)
