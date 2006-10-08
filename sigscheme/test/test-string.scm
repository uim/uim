;;  Filename : test-string.scm
;;  About    : unit test for R5RS string
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

;; See also test-string-{core,null}.scm

(load "./test/unittest.scm")

(define tn test-name)
(define cp string-copy)

(if (and (provided? "sigscheme")
         (not (symbol-bound? 'make-string)))
    (test-skip "string part of R5RS is not enabled"))

;;
;; All procedures that take a string as argument are tested with
;; both immutable and mutable string.
;;
;; See "3.4 Storage model" of R5RS
;;


(tn "make-string")
(assert-equal? (tn) ""   (make-string 0))
(assert-equal? (tn) " "  (make-string 1))
(assert-equal? (tn) "  " (make-string 2))
(assert-equal? (tn) ""   (make-string 0 #\a))
(assert-equal? (tn) "a"  (make-string 1 #\a))
(assert-equal? (tn) "aa" (make-string 2 #\a))

(tn "string-ref immutable")
(assert-equal? (tn) #\a (string-ref "abcde" 0))
(assert-equal? (tn) #\e (string-ref "abcde" 4))
(assert-error  (tn) (lambda ()
                      (string-ref "abcde" -1)))
(assert-error  (tn) (lambda ()
                      (string-ref "abcde" 5)))
(tn "string-ref mutable")
(assert-equal? (tn) #\a (string-ref (cp "abcde") 0))
(assert-equal? (tn) #\e (string-ref (cp "abcde") 4))
(assert-error  (tn) (lambda ()
                      (string-ref (cp "abcde") -1)))
(assert-error  (tn) (lambda ()
                      (string-ref (cp "abcde") 5)))

(tn "string-set! immutable")
(assert-error (tn) (lambda ()
                     (string-set! "foo" 0 #\b)))
(assert-error (tn) (lambda ()
                     (string-set! (symbol->string 'foo) 0 #\b)))
(assert-error (tn) (lambda ()
                     (define immutable-str "foo")
                     (string-set! immutable-str 0 #\b)
                     immutable-str))
(tn "string-set! mutable")
(assert-equal? (tn)
               "zbcdef"
	       (begin
		 (define tmpstr (cp "abcdef"))
		 (string-set! tmpstr 0 #\z)
		 tmpstr))
(assert-equal? (tn)
               "abzdef"
	       (begin
		 (define tmpstr (cp "abcdef"))
		 (string-set! tmpstr 2 #\z)
		 tmpstr))
(assert-equal? (tn)
               "abcdez"
	       (begin
		 (define tmpstr (cp "abcdef"))
		 (string-set! tmpstr 5 #\z)
		 tmpstr))
(assert-error  (tn) (lambda ()
                      (string-set! (cp "abcdef") -1 #\z)))
(assert-error  (tn) (lambda ()
                      (string-set! (cp "abcdef")  6 #\z)))

(tn "substring immutable")
(assert-error  (tn) (lambda () (substring "foo" 0 -1)))
(assert-equal? (tn) ""    (substring "foo" 0 0))
(assert-equal? (tn) "f"   (substring "foo" 0 1))
(assert-equal? (tn) "fo"  (substring "foo" 0 2))
(assert-equal? (tn) "foo" (substring "foo" 0 3))
(assert-error  (tn) (lambda () (substring "foo" 0 4)))
(assert-error  (tn) (lambda () (substring "foo" -1 0)))
(assert-error  (tn) (lambda () (substring "foo" 1 0)))
(assert-equal? (tn) "oo"  (substring "foo" 1 3))
(assert-equal? (tn) "o"   (substring "foo" 2 3))
(assert-equal? (tn) ""    (substring "foo" 3 3))
(assert-error  (tn) (lambda () (substring "foo" 4 3)))
(assert-error  (tn) (lambda () (substring "foo" 4 4)))
(assert-equal? (tn) "foo" (substring (symbol->string 'foo) 0 3))
(tn "substring mutable")
(assert-equal? (tn) ""    (substring (cp "abcde") 0 0))
(assert-equal? (tn) "a"   (substring (cp "abcde") 0 1))
(assert-equal? (tn) "bc"  (substring (cp "abcde") 1 3))
(assert-equal? (tn) "bcd" (substring (cp "abcde") 1 4))
(assert-error  (tn) (lambda ()
                      (substring (cp "abcde") 1 -1)))
(assert-error  (tn) (lambda ()
                      (substring (cp "abcde") -1 1)))
(assert-error  (tn) (lambda ()
                      (substring (cp "abcde") -1 -1)))
(assert-error  (tn) (lambda ()
                      (substring (cp "abcde") 2 1)))

(tn "string->list immutable")
(assert-equal? (tn) '()                (string->list ""))
(assert-equal? (tn) '(#\\)             (string->list "\\"))
(assert-equal? (tn) '(#\\ #\\)         (string->list "\\\\"))
(assert-equal? (tn) '(#\\ #\\ #\\)     (string->list "\\\\\\"))
;;(assert-equal? (tn) '(#\tab)           (string->list "\t"))
(assert-equal? (tn) '(#\	)      (string->list "\t"))
;;(assert-equal? (tn) '(#\return)        (string->list "\r"))
(assert-equal? (tn) '(#\)            (string->list "\r"))
(assert-equal? (tn) '(#\ #\)       (string->list "\r\r"))
(assert-equal? (tn) '(#\newline)           (string->list "\n"))
(assert-equal? (tn) '(#\newline #\newline) (string->list "\n\n"))
(assert-equal? (tn) '(#\space)         (string->list " "))
(assert-equal? (tn) '(#\space #\space) (string->list "  "))
(assert-equal? (tn) '(#\")             (string->list "\""))
(assert-equal? (tn) '(#\" #\")         (string->list "\"\""))
(tn "string->list mutable")
(assert-equal? (tn) '()                    (string->list (cp "")))
(assert-equal? (tn) '(#\\)                 (string->list (cp "\\")))
(assert-equal? (tn) '(#\\ #\\)             (string->list (cp "\\\\")))
(assert-equal? (tn) '(#\\ #\\ #\\)         (string->list (cp "\\\\\\")))
;;(assert-equal? (tn) '(#\tab)           (string->list (cp "\t")))
(assert-equal? (tn) '(#\	)            (string->list (cp "\t")))
;;(assert-equal? (tn) '(#\return)        (string->list (cp "\r")))
(assert-equal? (tn) '(#\)                (string->list (cp "\r")))
(assert-equal? (tn) '(#\ #\)           (string->list (cp "\r\r")))
(assert-equal? (tn) '(#\newline)           (string->list (cp "\n")))
(assert-equal? (tn) '(#\newline #\newline) (string->list (cp "\n\n")))
(assert-equal? (tn) '(#\space)             (string->list (cp " ")))
(assert-equal? (tn) '(#\space #\space)     (string->list (cp "  ")))
(assert-equal? (tn) '(#\")                 (string->list (cp "\"")))
(assert-equal? (tn) '(#\" #\")             (string->list (cp "\"\"")))

(tn "list->string")
(assert-equal? (tn) ""     (list->string '()))
(assert-equal? (tn) "\\"     (list->string '(#\\)))
(assert-equal? (tn) "\\\\"   (list->string '(#\\ #\\)))
(assert-equal? (tn) "\\\\\\" (list->string '(#\\ #\\ #\\)))
(assert-equal? (tn) "\t" (list->string '(#\	)))
;;(assert-equal? (tn) "\t" (list->string '(#\tab)))
(assert-equal? (tn) "\r" (list->string '(#\)))
;;(assert-equal? (tn) "\r" (list->string '(#\return)))
(assert-equal? (tn) "\n" (list->string '(#\
)))
(assert-equal? (tn) "\n" (list->string '(#\newline)))
(assert-equal? (tn) " " (list->string '(#\ )))
(assert-equal? (tn) " " (list->string '(#\space)))
(assert-equal? (tn) " " (list->string '(#\ )))
(assert-equal? (tn) "\"" (list->string '(#\")))
(assert-equal? (tn) "\"a\"" (list->string '(#\" #\a #\")))

(tn "string-fill! immutable")
(assert-error (tn) (lambda ()
                     (string-fill! "" #\j)))
(assert-error (tn) (lambda ()
                     (string-fill! "foo" #\j)))
(assert-error (tn) (lambda ()
                     (string-fill! (string->symbol 'foo) #\j)))
(tn "string-fill! mutable")
(assert-equal? (tn)
               ""
               (begin
                 (define tmpstr (cp ""))
                 (string-fill! tmpstr #\j)
                 tmpstr))
(assert-equal? (tn)
               "jjjjj"
               (begin
                 (define tmpstr (cp "abcde"))
                 (string-fill! tmpstr #\j)
                 tmpstr))
(assert-equal? (tn)
               "\\\\\\"
               (begin
                 (define tmpstr (cp "abc"))
                 (string-fill! tmpstr #\\)
                 tmpstr))

(tn "symbol->string")
(assert-equal? (tn) "a"  (symbol->string 'a))
(assert-equal? (tn) "ab" (symbol->string 'ab))

;; TODO: need to investigate (string->symbol "") behavior
(tn "string->symbol immutable")
(assert-equal? (tn) 'a  (string->symbol "a"))
(assert-equal? (tn) 'ab (string->symbol "ab"))
(tn "string->symbol mutable")
(assert-equal? (tn) 'a  (string->symbol (cp "a")))
(assert-equal? (tn) 'ab (string->symbol (cp "ab")))

;;
;; escape sequences
;;

(define integer->string
  (lambda (i)
    (list->string (list (integer->char i)))))

;; R5RS compliant
(tn "R5RS escape sequence")
(assert-equal? (tn) (integer->string 34)        "\"")  ;; 34
(assert-equal? (tn) (list->string '(#\"))       "\"")  ;; 34
(assert-equal? (tn) '(#\")       (string->list "\""))  ;; 34
(assert-equal? (tn) (integer->string 92)        "\\")  ;; 92
(assert-equal? (tn) (list->string '(#\\))       "\\")  ;; 92
(assert-equal? (tn) '(#\\)       (string->list "\\"))  ;; 92
(assert-equal? (tn) (integer->string 10)        "\n")  ;; 110
(assert-equal? (tn) (list->string '(#\newline)) "\n")  ;; 110
(assert-equal? (tn) '(#\newline) (string->list "\n"))  ;; 110

;; R6RS(SRFI-75) compliant
(tn "R6RS escape sequence")
;; See also test-string-null.scm for "\x00" tests
(assert-equal? (tn) (integer->string 7)           "\a")  ;; 97
(assert-equal? (tn) (list->string '(#\alarm))     "\a")  ;; 97
(assert-equal? (tn) '(#\alarm)  (string->list    "\a"))  ;; 97
(assert-equal? (tn) (integer->string 8)           "\b")  ;; 98
(assert-equal? (tn) (list->string '(#\backspace)) "\b")  ;; 98
(assert-equal? (tn) '(#\backspace) (string->list "\b"))  ;; 98
(assert-equal? (tn) (integer->string 12)          "\f")  ;; 102
(assert-equal? (tn) (list->string '(#\page))      "\f")  ;; 102
(assert-equal? (tn) '(#\page)   (string->list    "\f"))  ;; 102
(assert-equal? (tn) (integer->string 13)          "\r")  ;; 114
(assert-equal? (tn) (list->string '(#\return))    "\r")  ;; 114
(assert-equal? (tn) '(#\return) (string->list    "\r"))  ;; 114
(assert-equal? (tn) (integer->string 9)           "\t")  ;; 116
(assert-equal? (tn) (list->string '(#\tab))       "\t")  ;; 116
(assert-equal? (tn) '(#\tab)    (string->list    "\t"))  ;; 116
(assert-equal? (tn) (integer->string 11)          "\v")  ;; 118
(assert-equal? (tn) (list->string '(#\vtab))      "\v")  ;; 118
(assert-equal? (tn) '(#\vtab)   (string->list    "\v"))  ;; 118
(assert-equal? (tn) (integer->string 124)         "\|")  ;; 124

;; All these conventional escape sequences should cause parse error as defined
;; in SRFI-75: "Any other character in a string after a backslash is an
;; error".
(tn "conventional escape sequence")
;;                            "\0"   ;; 0
(assert-parse-error (tn) "\"\\ \"")  ;; 32
(assert-parse-error (tn) "\"\\!\"")  ;; 33
;;                            "\""   ;; 34
(assert-parse-error (tn) "\"\\#\"")  ;; 35
(assert-parse-error (tn) "\"\\$\"")  ;; 36
(assert-parse-error (tn) "\"\\%\"")  ;; 37
(assert-parse-error (tn) "\"\\&\"")  ;; 38
(assert-parse-error (tn) "\"\\'\"")  ;; 39
(assert-parse-error (tn) "\"\\(\"")  ;; 40
(assert-parse-error (tn) "\"\\)\"")  ;; 41
(assert-parse-error (tn) "\"\\*\"")  ;; 42
(assert-parse-error (tn) "\"\\+\"")  ;; 43
(assert-parse-error (tn) "\"\\,\"")  ;; 44
(assert-parse-error (tn) "\"\\-\"")  ;; 45
(assert-parse-error (tn) "\"\\.\"")  ;; 46
(assert-parse-error (tn) "\"\\/\"")  ;; 47
(assert-parse-error (tn) "\"\\0\"")  ;; 48
(assert-parse-error (tn) "\"\\1\"")  ;; 49
(assert-parse-error (tn) "\"\\2\"")  ;; 50
(assert-parse-error (tn) "\"\\3\"")  ;; 51
(assert-parse-error (tn) "\"\\4\"")  ;; 52
(assert-parse-error (tn) "\"\\5\"")  ;; 53
(assert-parse-error (tn) "\"\\6\"")  ;; 54
(assert-parse-error (tn) "\"\\7\"")  ;; 55
(assert-parse-error (tn) "\"\\8\"")  ;; 56
(assert-parse-error (tn) "\"\\9\"")  ;; 57
(assert-parse-error (tn) "\"\\:\"")  ;; 58
(assert-parse-error (tn) "\"\\;\"")  ;; 59
(assert-parse-error (tn) "\"\\<\"")  ;; 60
(assert-parse-error (tn) "\"\\=\"")  ;; 61
(assert-parse-error (tn) "\"\\>\"")  ;; 62
(assert-parse-error (tn) "\"\\?\"")  ;; 63
(assert-parse-error (tn) "\"\\@\"")  ;; 64
(assert-parse-error (tn) "\"\\A\"")  ;; 65
(assert-parse-error (tn) "\"\\B\"")  ;; 66
(assert-parse-error (tn) "\"\\C\"")  ;; 67
(assert-parse-error (tn) "\"\\D\"")  ;; 68
(assert-parse-error (tn) "\"\\E\"")  ;; 69
(assert-parse-error (tn) "\"\\F\"")  ;; 70
(assert-parse-error (tn) "\"\\G\"")  ;; 71
(assert-parse-error (tn) "\"\\H\"")  ;; 72
(assert-parse-error (tn) "\"\\I\"")  ;; 73
(assert-parse-error (tn) "\"\\J\"")  ;; 74
(assert-parse-error (tn) "\"\\K\"")  ;; 75
(assert-parse-error (tn) "\"\\L\"")  ;; 76
(assert-parse-error (tn) "\"\\M\"")  ;; 77
(assert-parse-error (tn) "\"\\N\"")  ;; 78
(assert-parse-error (tn) "\"\\O\"")  ;; 79
(assert-parse-error (tn) "\"\\P\"")  ;; 80
(assert-parse-error (tn) "\"\\Q\"")  ;; 81
(assert-parse-error (tn) "\"\\R\"")  ;; 82
(assert-parse-error (tn) "\"\\S\"")  ;; 83
(assert-parse-error (tn) "\"\\T\"")  ;; 84
(assert-parse-error (tn) "\"\\U\"")  ;; 85
(assert-parse-error (tn) "\"\\V\"")  ;; 86
(assert-parse-error (tn) "\"\\W\"")  ;; 87
(assert-parse-error (tn) "\"\\X\"")  ;; 88
(assert-parse-error (tn) "\"\\Y\"")  ;; 89
(assert-parse-error (tn) "\"\\Z\"")  ;; 90
(assert-parse-error (tn) "\"\\[\"")  ;; 91
;;                            "\\"   ;; 92
(assert-parse-error (tn) "\"\\]\"")  ;; 93
(assert-parse-error (tn) "\"\\^\"")  ;; 94
(assert-parse-error (tn) "\"\\_\"")  ;; 95
(assert-parse-error (tn) "\"\\`\"")  ;; 96
;;                            "\a"   ;; 97
;;                            "\b"   ;; 98
(assert-parse-error (tn) "\"\\c\"")  ;; 99
(assert-parse-error (tn) "\"\\d\"")  ;; 100
(assert-parse-error (tn) "\"\\e\"")  ;; 101
;;                            "\f"   ;; 102
(assert-parse-error (tn) "\"\\g\"")  ;; 103
(assert-parse-error (tn) "\"\\h\"")  ;; 104
(assert-parse-error (tn) "\"\\i\"")  ;; 105
(assert-parse-error (tn) "\"\\j\"")  ;; 106
(assert-parse-error (tn) "\"\\k\"")  ;; 107
(assert-parse-error (tn) "\"\\l\"")  ;; 108
(assert-parse-error (tn) "\"\\m\"")  ;; 109
;;                            "\n"   ;; 110
(assert-parse-error (tn) "\"\\o\"")  ;; 111
(assert-parse-error (tn) "\"\\p\"")  ;; 112
(assert-parse-error (tn) "\"\\q\"")  ;; 113
;;                            "\r"   ;; 114
(assert-parse-error (tn) "\"\\s\"")  ;; 115
;;                            "\t"   ;; 116
(assert-parse-error (tn) "\"\\u\"")  ;; 117
;;                            "\v"   ;; 118
(assert-parse-error (tn) "\"\\w\"")  ;; 119
(assert-parse-error (tn) "\"\\x\"")  ;; 120
(assert-parse-error (tn) "\"\\y\"")  ;; 121
(assert-parse-error (tn) "\"\\z\"")  ;; 122
(assert-parse-error (tn) "\"\\{\"")  ;; 123
;;                            "\|"   ;; 124
(assert-parse-error (tn) "\"\\}\"")  ;; 125
(assert-parse-error (tn) "\"\\~\"")  ;; 126

;; raw control chars
(tn "raw control char in string literal")
;; See also test-string-null.scm for " " (charcode 0) test
(assert-equal? (tn) (integer->string   1) "")  ;; 1
(assert-equal? (tn) (integer->string   2) "")  ;; 2
(assert-equal? (tn) (integer->string   3) "")  ;; 3
(assert-equal? (tn) (integer->string   4) "")  ;; 4
(assert-equal? (tn) (integer->string   5) "")  ;; 5
(assert-equal? (tn) (integer->string   6) "")  ;; 6
(assert-equal? (tn) (integer->string   7) "")  ;; 7
(assert-equal? (tn) (integer->string   8) "")  ;; 8  ;; DON'T EDIT THIS LINE!
(assert-equal? (tn) (integer->string   9) "	")  ;; 9
(assert-equal? (tn) (integer->string  10) "
")  ;; 10 ;; DON'T EDIT THIS LINE!
(assert-equal? (tn) (integer->string  11) "")  ;; 11
(assert-equal? (tn) (integer->string  12) "")  ;; 12
(assert-equal? (tn) (integer->string  13) "")  ;; 13 ;; DON'T EDIT THIS LINE!
(assert-equal? (tn) (integer->string  14) "")  ;; 14
(assert-equal? (tn) (integer->string  15) "")  ;; 15
(assert-equal? (tn) (integer->string  16) "")  ;; 16
(assert-equal? (tn) (integer->string  17) "")  ;; 17
(assert-equal? (tn) (integer->string  18) "")  ;; 18
(assert-equal? (tn) (integer->string  19) "")  ;; 19
(assert-equal? (tn) (integer->string  20) "")  ;; 20
(assert-equal? (tn) (integer->string  21) "")  ;; 21
(assert-equal? (tn) (integer->string  22) "")  ;; 22
(assert-equal? (tn) (integer->string  23) "")  ;; 23
(assert-equal? (tn) (integer->string  24) "")  ;; 24
(assert-equal? (tn) (integer->string  25) "")  ;; 25 ;; DON'T EDIT THIS LINE!
(assert-equal? (tn) (integer->string  26) "")  ;; 26
(assert-equal? (tn) (integer->string  27) "")  ;; 27
(assert-equal? (tn) (integer->string  28) "")  ;; 28
(assert-equal? (tn) (integer->string  29) "")  ;; 29
(assert-equal? (tn) (integer->string  30) "")  ;; 30
(assert-equal? (tn) (integer->string  31) "")  ;; 31
(assert-equal? (tn) (integer->string 127) "")  ;; 127

;; escaped raw control chars
(tn "escaped raw control char in string literal")
;; See also test-string-null.scm for " " (charcode 0) test
(assert-parse-error (tn) "\"\\\"")  ;; 1
(assert-parse-error (tn) "\"\\\"")  ;; 2
(assert-parse-error (tn) "\"\\\"")  ;; 3
(assert-parse-error (tn) "\"\\\"")  ;; 4
(assert-parse-error (tn) "\"\\\"")  ;; 5
(assert-parse-error (tn) "\"\\\"")  ;; 6
(assert-parse-error (tn) "\"\\\"")  ;; 7
(assert-parse-error (tn) "\"\\\"")  ;; 8  ;; DON'T EDIT THIS LINE!
(assert-parse-error (tn) "\"\\	\"")  ;; 9
(assert-parse-error (tn) "\"\\
\"")  ;; 10  ;; DON'T EDIT THIS LINE!
(assert-parse-error (tn) "\"\\\"")  ;; 11
(assert-parse-error (tn) "\"\\\"")  ;; 12
(assert-parse-error (tn) "\"\\\"")  ;; 13  ;; DON'T EDIT THIS LINE!
(assert-parse-error (tn) "\"\\\"")  ;; 14
(assert-parse-error (tn) "\"\\\"")  ;; 15
(assert-parse-error (tn) "\"\\\"")  ;; 16
(assert-parse-error (tn) "\"\\\"")  ;; 17
(assert-parse-error (tn) "\"\\\"")  ;; 18
(assert-parse-error (tn) "\"\\\"")  ;; 19
(assert-parse-error (tn) "\"\\\"")  ;; 20
(assert-parse-error (tn) "\"\\\"")  ;; 21
(assert-parse-error (tn) "\"\\\"")  ;; 22
(assert-parse-error (tn) "\"\\\"")  ;; 23
(assert-parse-error (tn) "\"\\\"")  ;; 24
(assert-parse-error (tn) "\"\\\"")  ;; 25  ;; DON'T EDIT THIS LINE!
(assert-parse-error (tn) "\"\\\"")  ;; 26
(assert-parse-error (tn) "\"\\\"")  ;; 27
(assert-parse-error (tn) "\"\\\"")  ;; 28
(assert-parse-error (tn) "\"\\\"")  ;; 29
(assert-parse-error (tn) "\"\\\"")  ;; 30
(assert-parse-error (tn) "\"\\\"")  ;; 31
(assert-parse-error (tn) "\"\\\"")  ;; 127

(total-report)
