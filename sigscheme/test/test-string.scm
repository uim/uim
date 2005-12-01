;;  FileName : test-string.scm
;;  About    : unit test for R5RS string
;;
;;  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

;; check string?
(assert-true "string? check" (string? ""))
(assert-true "string? check" (string? "abcde"))

;; check make-string
(assert-equal? "make-string check" ""   (make-string 0))
(assert-equal? "make-string check" " "  (make-string 1))
(assert-equal? "make-string check" "  " (make-string 2))
(assert-equal? "make-string check" ""   (make-string 0 #\a))
(assert-equal? "make-string check" "a"  (make-string 1 #\a))
(assert-equal? "make-string check" "aa" (make-string 2 #\a))

;; check string-ref
(assert-equal? "string-ref check" #\a (string-ref "abcde" 0))
(assert-equal? "string-ref check" #\e (string-ref "abcde" 4))
(assert-error  "string-ref check" (lambda ()
				    (string-ref "abcde" -1)))
(assert-error  "string-ref check" (lambda ()
				    (string-ref "abcde" 5)))

;; check string-set!
(assert-equal? "string-set! check" "zbcdef"
	       (begin
		 (define tmpstr (string-copy "abcdef"))
		 (string-set! tmpstr 0 #\z)
		 tmpstr))
(assert-equal? "string-set! check" "abzdef"
	       (begin
		 (define tmpstr (string-copy "abcdef"))
		 (string-set! tmpstr 2 #\z)
		 tmpstr))
(assert-equal? "string-set! check" "abcdez"
	       (begin
		 (define tmpstr (string-copy "abcdef"))
		 (string-set! tmpstr 5 #\z)
		 tmpstr))
(assert-error  "string-set! check" (lambda ()
				     (string-set! (string-copy "abcdef") -1 #\z)))
(assert-error  "string-set! check" (lambda ()
				     (string-set! (string-copy "abcdef")  6 #\z)))

;; check string-length
(assert-equal? "string-length check" 0 (string-length ""))
(assert-equal? "string-length check" 5 (string-length "abcde"))
(assert-equal? "string-length check" 1 (string-length "\\"))
(assert-equal? "string-length check" 2 (string-length "\\\\"))
(assert-equal? "string-length check" 3 (string-length "\\\\\\"))

;; string=? check
(assert-true "string=? check" (string=? "" ""))
(assert-true "string=? check" (string=? "abcde" "abcde"))

;; substring check
(assert-equal? "substring check" ""    (substring (string-copy "abcde") 0 0))
(assert-equal? "substring check" "a"   (substring (string-copy "abcde") 0 1))
(assert-equal? "substring check" "bc"  (substring (string-copy "abcde") 1 3))
(assert-equal? "substring check" "bcd" (substring (string-copy "abcde") 1 4))
(assert-error  "substring check" (lambda ()
				   (substring (string-copy "abcde") 1 -1)))
(assert-error  "substring check" (lambda ()
				   (substring (string-copy "abcde") -1 1)))
(assert-error  "substring check" (lambda ()
				   (substring (string-copy "abcde") -1 -1)))
(assert-error  "substring check" (lambda ()
				   (substring (string-copy "abcde") 2 1)))


;; string-append check
(assert-equal? "string-append check" ""    (string-append ""))
(assert-equal? "string-append check" "a"   (string-append "a"))
(assert-equal? "string-append check" "ab"  (string-append "a" "b"))
(assert-equal? "string-append check" "abc" (string-append "a" "b" "c"))
(assert-equal? "string-append check" "ab"     (string-append "ab"))
(assert-equal? "string-append check" "abcd"   (string-append "ab" "cd"))
(assert-equal? "string-append check" "abcdef" (string-append "ab" "cd" "ef"))

;; string->list
(assert-equal? "string->list check" '()                (string->list ""))
(assert-equal? "string->list check" '(#\\)             (string->list "\\"))
(assert-equal? "string->list check" '(#\\ #\\)         (string->list "\\\\"))
(assert-equal? "string->list check" '(#\\ #\\ #\\)     (string->list "\\\\\\"))
;;(assert-equal? "string->list check" '(#\tab)           (string->list "\t"))
(assert-equal? "string->list check" '(#\	)      (string->list "\t"))
;;(assert-equal? "string->list check" '(#\return)        (string->list "\r"))
(assert-equal? "string->list check" '(#\)            (string->list "\r"))
(assert-equal? "string->list check" '(#\ #\)       (string->list "\r\r"))
(assert-equal? "string->list check" '(#\newline)           (string->list "\n"))
(assert-equal? "string->list check" '(#\newline #\newline) (string->list "\n\n"))
(assert-equal? "string->list check" '(#\space)         (string->list " "))
(assert-equal? "string->list check" '(#\space #\space) (string->list "  "))
(assert-equal? "string->list check" '(#\")             (string->list "\""))
(assert-equal? "string->list check" '(#\" #\")         (string->list "\"\""))

;; list->string
(assert-equal? "list->string check" ""     (list->string '()))
(assert-equal? "list->string check" "\\"     (list->string '(#\\)))
(assert-equal? "list->string check" "\\\\"   (list->string '(#\\ #\\)))
(assert-equal? "list->string check" "\\\\\\" (list->string '(#\\ #\\ #\\)))
(assert-equal? "list->string check" "\t" (list->string '(#\	)))
;;(assert-equal? "list->string check" "\t" (list->string '(#\tab)))
(assert-equal? "list->string check" "\r" (list->string '(#\)))
;;(assert-equal? "list->string check" "\r" (list->string '(#\return)))
(assert-equal? "list->string check" "\n" (list->string '(#\
)))
(assert-equal? "list->string check" "\n" (list->string '(#\newline)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" " " (list->string '(#\space)))
(assert-equal? "list->string check" " " (list->string '(#\ )))
(assert-equal? "list->string check" "\"" (list->string '(#\")))
(assert-equal? "list->string check" "\"a\"" (list->string '(#\" #\a #\")))

;; string-fill!
(assert-equal? "string-fill! check" "jjjjj" (begin
					      (define tmpstr (string-copy "abcde"))
					      (string-fill! tmpstr #\j)
					      tmpstr))
(assert-equal? "string-fill! check" "\\\\\\" (begin
					       (define tmpstr (string-copy "abc"))
					       (string-fill! tmpstr #\\)
					       tmpstr))

;; string-copy
(assert-equal? "string copy check" ""   (string-copy ""))
(assert-equal? "string copy check" "a"  (string-copy "a"))
(assert-equal? "string copy check" "ab" (string-copy "ab"))

;; symbol->string
(assert-equal? "symbol->string check" "a"  (symbol->string 'a))
(assert-equal? "symbol->string check" "ab" (symbol->string 'ab))

;; string->symbol
;; TODO: need to investigate (string->symbol "") behavior
(assert-equal? "string->symbol check" 'a  (string->symbol "a"))
(assert-equal? "string->symbol check" 'ab (string->symbol "ab"))

;;
;; immutable strings: See "3.4 Storage model" of R5RS
;;
(assert-error "string-set! on constant string #1"
              (lambda ()
                (string-set! "foo" 0 #\b)))
(assert-error "string-set! on constant string #2"
              (lambda ()
                (string-set! (symbol->string 'foo) 0 #\b)))


;;
;; escape sequences
;;

(define integer->string
  (lambda (i)
    (list->string (list (integer->char i)))))

;; R5RS compliant
(assert-equal? "R5RS escape sequence" (integer->string 34)        "\"")  ;; 34
(assert-equal? "R5RS escape sequence" (list->string '(#\"))       "\"")  ;; 34
(assert-equal? "R5RS escape sequence" '(#\")       (string->list "\""))  ;; 34
(assert-equal? "R5RS escape sequence" (integer->string 92)        "\\")  ;; 92
(assert-equal? "R5RS escape sequence" (list->string '(#\\))       "\\")  ;; 92
(assert-equal? "R5RS escape sequence" '(#\\)       (string->list "\\"))  ;; 92
(assert-equal? "R5RS escape sequence" (integer->string 10)        "\n")  ;; 110
(assert-equal? "R5RS escape sequence" (list->string '(#\newline)) "\n")  ;; 110
(assert-equal? "R5RS escape sequence" '(#\newline) (string->list "\n"))  ;; 110

;; R6RS(SRFI-75) compliant
(assert-equal? "R6RS escape sequence" (integer->string 0)      "\x00")  ;; 0
(assert-equal? "R6RS escape sequence" (list->string '(#\nul))  "\x00")  ;; 0
(assert-equal? "R6RS escape sequence" '(#\nul)  (string->list "\x00"))  ;; 0
(assert-equal? "R6RS escape sequence" (integer->string 7)        "\a")  ;; 97
(assert-equal? "R6RS escape sequence" (list->string '(#\alarm))  "\a")  ;; 97
(assert-equal? "R6RS escape sequence" '(#\alarm)  (string->list "\a"))  ;; 97
(assert-equal? "R6RS escape sequence" (integer->string 8)        "\b")  ;; 98
(assert-equal? "R6RS escape sequence" (list->string '(#\backspace)) "\b")  ;; 98
(assert-equal? "R6RS escape sequence" '(#\backspace) (string->list "\b"))  ;; 98
(assert-equal? "R6RS escape sequence" (integer->string 12)       "\f")  ;; 102
(assert-equal? "R6RS escape sequence" (list->string '(#\page))   "\f")  ;; 102
(assert-equal? "R6RS escape sequence" '(#\page)   (string->list "\f"))  ;; 102
(assert-equal? "R6RS escape sequence" (integer->string 13)       "\r")  ;; 114
(assert-equal? "R6RS escape sequence" (list->string '(#\return)) "\r")  ;; 114
(assert-equal? "R6RS escape sequence" '(#\return) (string->list "\r"))  ;; 114
(assert-equal? "R6RS escape sequence" (integer->string 9)        "\t")  ;; 116
(assert-equal? "R6RS escape sequence" (list->string '(#\tab))    "\t")  ;; 116
(assert-equal? "R6RS escape sequence" '(#\tab)    (string->list "\t"))  ;; 116
(assert-equal? "R6RS escape sequence" (integer->string 11)       "\v")  ;; 118
(assert-equal? "R6RS escape sequence" (list->string '(#\vtab))   "\v")  ;; 118
(assert-equal? "R6RS escape sequence" '(#\vtab)   (string->list "\v"))  ;; 118
(assert-equal? "R6RS escape sequence" (integer->string 124)      "\|")  ;; 124

;; All these conventional escape sequences should cause parse error as defined
;; in SRFI-75: "Any other character in a string after a backslash is an
;; error".
;;                                                      "\0"   ;; 0
(assert-parse-error "conventional escape sequence" "\"\\ \"")  ;; 32
(assert-parse-error "conventional escape sequence" "\"\\!\"")  ;; 33
;;                                                      "\""   ;; 34
(assert-parse-error "conventional escape sequence" "\"\\#\"")  ;; 35
(assert-parse-error "conventional escape sequence" "\"\\$\"")  ;; 36
(assert-parse-error "conventional escape sequence" "\"\\%\"")  ;; 37
(assert-parse-error "conventional escape sequence" "\"\\&\"")  ;; 38
(assert-parse-error "conventional escape sequence" "\"\\'\"")  ;; 39
(assert-parse-error "conventional escape sequence" "\"\\(\"")  ;; 40
(assert-parse-error "conventional escape sequence" "\"\\)\"")  ;; 41
(assert-parse-error "conventional escape sequence" "\"\\*\"")  ;; 42
(assert-parse-error "conventional escape sequence" "\"\\+\"")  ;; 43
(assert-parse-error "conventional escape sequence" "\"\\,\"")  ;; 44
(assert-parse-error "conventional escape sequence" "\"\\-\"")  ;; 45
(assert-parse-error "conventional escape sequence" "\"\\.\"")  ;; 46
(assert-parse-error "conventional escape sequence" "\"\\/\"")  ;; 47
(assert-parse-error "conventional escape sequence" "\"\\0\"")  ;; 48
(assert-parse-error "conventional escape sequence" "\"\\1\"")  ;; 49
(assert-parse-error "conventional escape sequence" "\"\\2\"")  ;; 50
(assert-parse-error "conventional escape sequence" "\"\\3\"")  ;; 51
(assert-parse-error "conventional escape sequence" "\"\\4\"")  ;; 52
(assert-parse-error "conventional escape sequence" "\"\\5\"")  ;; 53
(assert-parse-error "conventional escape sequence" "\"\\6\"")  ;; 54
(assert-parse-error "conventional escape sequence" "\"\\7\"")  ;; 55
(assert-parse-error "conventional escape sequence" "\"\\8\"")  ;; 56
(assert-parse-error "conventional escape sequence" "\"\\9\"")  ;; 57
(assert-parse-error "conventional escape sequence" "\"\\:\"")  ;; 58
(assert-parse-error "conventional escape sequence" "\"\\;\"")  ;; 59
(assert-parse-error "conventional escape sequence" "\"\\<\"")  ;; 60
(assert-parse-error "conventional escape sequence" "\"\\=\"")  ;; 61
(assert-parse-error "conventional escape sequence" "\"\\>\"")  ;; 62
(assert-parse-error "conventional escape sequence" "\"\\?\"")  ;; 63
(assert-parse-error "conventional escape sequence" "\"\\@\"")  ;; 64
(assert-parse-error "conventional escape sequence" "\"\\A\"")  ;; 65
(assert-parse-error "conventional escape sequence" "\"\\B\"")  ;; 66
(assert-parse-error "conventional escape sequence" "\"\\C\"")  ;; 67
(assert-parse-error "conventional escape sequence" "\"\\D\"")  ;; 68
(assert-parse-error "conventional escape sequence" "\"\\E\"")  ;; 69
(assert-parse-error "conventional escape sequence" "\"\\F\"")  ;; 70
(assert-parse-error "conventional escape sequence" "\"\\G\"")  ;; 71
(assert-parse-error "conventional escape sequence" "\"\\H\"")  ;; 72
(assert-parse-error "conventional escape sequence" "\"\\I\"")  ;; 73
(assert-parse-error "conventional escape sequence" "\"\\J\"")  ;; 74
(assert-parse-error "conventional escape sequence" "\"\\K\"")  ;; 75
(assert-parse-error "conventional escape sequence" "\"\\L\"")  ;; 76
(assert-parse-error "conventional escape sequence" "\"\\M\"")  ;; 77
(assert-parse-error "conventional escape sequence" "\"\\N\"")  ;; 78
(assert-parse-error "conventional escape sequence" "\"\\O\"")  ;; 79
(assert-parse-error "conventional escape sequence" "\"\\P\"")  ;; 80
(assert-parse-error "conventional escape sequence" "\"\\Q\"")  ;; 81
(assert-parse-error "conventional escape sequence" "\"\\R\"")  ;; 82
(assert-parse-error "conventional escape sequence" "\"\\S\"")  ;; 83
(assert-parse-error "conventional escape sequence" "\"\\T\"")  ;; 84
(assert-parse-error "conventional escape sequence" "\"\\U\"")  ;; 85
(assert-parse-error "conventional escape sequence" "\"\\V\"")  ;; 86
(assert-parse-error "conventional escape sequence" "\"\\W\"")  ;; 87
(assert-parse-error "conventional escape sequence" "\"\\X\"")  ;; 88
(assert-parse-error "conventional escape sequence" "\"\\Y\"")  ;; 89
(assert-parse-error "conventional escape sequence" "\"\\Z\"")  ;; 90
(assert-parse-error "conventional escape sequence" "\"\\[\"")  ;; 91
;;                                                      "\\"   ;; 92
(assert-parse-error "conventional escape sequence" "\"\\]\"")  ;; 93
(assert-parse-error "conventional escape sequence" "\"\\^\"")  ;; 94
(assert-parse-error "conventional escape sequence" "\"\\_\"")  ;; 95
(assert-parse-error "conventional escape sequence" "\"\\`\"")  ;; 96
;;                                                      "\a"   ;; 97
;;                                                      "\b"   ;; 98
(assert-parse-error "conventional escape sequence" "\"\\c\"")  ;; 99
(assert-parse-error "conventional escape sequence" "\"\\d\"")  ;; 100
(assert-parse-error "conventional escape sequence" "\"\\e\"")  ;; 101
;;                                                      "\f"   ;; 102
(assert-parse-error "conventional escape sequence" "\"\\g\"")  ;; 103
(assert-parse-error "conventional escape sequence" "\"\\h\"")  ;; 104
(assert-parse-error "conventional escape sequence" "\"\\i\"")  ;; 105
(assert-parse-error "conventional escape sequence" "\"\\j\"")  ;; 106
(assert-parse-error "conventional escape sequence" "\"\\k\"")  ;; 107
(assert-parse-error "conventional escape sequence" "\"\\l\"")  ;; 108
(assert-parse-error "conventional escape sequence" "\"\\m\"")  ;; 109
;;                                                      "\n"   ;; 110
(assert-parse-error "conventional escape sequence" "\"\\o\"")  ;; 111
(assert-parse-error "conventional escape sequence" "\"\\p\"")  ;; 112
(assert-parse-error "conventional escape sequence" "\"\\q\"")  ;; 113
;;                                                      "\r"   ;; 114
(assert-parse-error "conventional escape sequence" "\"\\s\"")  ;; 115
;;                                                      "\t"   ;; 116
(assert-parse-error "conventional escape sequence" "\"\\u\"")  ;; 117
;;                                                      "\v"   ;; 118
(assert-parse-error "conventional escape sequence" "\"\\w\"")  ;; 119
(assert-parse-error "conventional escape sequence" "\"\\x\"")  ;; 120
(assert-parse-error "conventional escape sequence" "\"\\y\"")  ;; 121
(assert-parse-error "conventional escape sequence" "\"\\z\"")  ;; 122
(assert-parse-error "conventional escape sequence" "\"\\{\"")  ;; 123
;;                                                      "\|"   ;; 124
(assert-parse-error "conventional escape sequence" "\"\\}\"")  ;; 125
(assert-parse-error "conventional escape sequence" "\"\\~\"")  ;; 126

;; raw control chars
(assert-equal? "raw control char in string literal" (integer->string   0) " ")  ;; 0
(assert-equal? "raw control char in string literal" (integer->string   1) "")  ;; 1
(assert-equal? "raw control char in string literal" (integer->string   2) "")  ;; 2
(assert-equal? "raw control char in string literal" (integer->string   3) "")  ;; 3
(assert-equal? "raw control char in string literal" (integer->string   4) "")  ;; 4
(assert-equal? "raw control char in string literal" (integer->string   5) "")  ;; 5
(assert-equal? "raw control char in string literal" (integer->string   6) "")  ;; 6
(assert-equal? "raw control char in string literal" (integer->string   7) "")  ;; 7
(assert-equal? "raw control char in string literal" (integer->string   8) "")  ;; 8  ;; DON'T EDIT THIS LINE!
(assert-equal? "raw control char in string literal" (integer->string   9) "	")  ;; 9
(assert-equal? "raw control char in string literal" (integer->string  10) "
")  ;; 10  ;; DON'T EDIT THIS LINE!
(assert-equal? "raw control char in string literal" (integer->string  11) "")  ;; 11
(assert-equal? "raw control char in string literal" (integer->string  12) "")  ;; 12
(assert-equal? "raw control char in string literal" (integer->string  13) "")  ;; 13  ;; DON'T EDIT THIS LINE!
(assert-equal? "raw control char in string literal" (integer->string  14) "")  ;; 14
(assert-equal? "raw control char in string literal" (integer->string  15) "")  ;; 15
(assert-equal? "raw control char in string literal" (integer->string  16) "")  ;; 16
(assert-equal? "raw control char in string literal" (integer->string  17) "")  ;; 17
(assert-equal? "raw control char in string literal" (integer->string  18) "")  ;; 18
(assert-equal? "raw control char in string literal" (integer->string  19) "")  ;; 19
(assert-equal? "raw control char in string literal" (integer->string  20) "")  ;; 20
(assert-equal? "raw control char in string literal" (integer->string  21) "")  ;; 21
(assert-equal? "raw control char in string literal" (integer->string  22) "")  ;; 22
(assert-equal? "raw control char in string literal" (integer->string  23) "")  ;; 23
(assert-equal? "raw control char in string literal" (integer->string  24) "")  ;; 24
(assert-equal? "raw control char in string literal" (integer->string  25) "")  ;; 25  ;; DON'T EDIT THIS LINE!
(assert-equal? "raw control char in string literal" (integer->string  26) "")  ;; 26
(assert-equal? "raw control char in string literal" (integer->string  27) "")  ;; 27
(assert-equal? "raw control char in string literal" (integer->string  28) "")  ;; 28
(assert-equal? "raw control char in string literal" (integer->string  29) "")  ;; 29
(assert-equal? "raw control char in string literal" (integer->string  30) "")  ;; 30
(assert-equal? "raw control char in string literal" (integer->string  31) "")  ;; 31
(assert-equal? "raw control char in string literal" (integer->string 127) "")  ;; 127

;; escaped raw control chars
;;(assert-parse-error "escaped raw control char in string literal" "\"\\ \"")  ;; 0  ;; cannot read by string port
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 1
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 2
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 3
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 4
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 5
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 6
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 7
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 8  ;; DON'T EDIT THIS LINE!
(assert-parse-error "escaped raw control char in string literal" "\"\\	\"")  ;; 9
(assert-parse-error "escaped raw control char in string literal" "\"\\
\"")  ;; 10  ;; DON'T EDIT THIS LINE!
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 11
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 12
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 13  ;; DON'T EDIT THIS LINE!
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 14
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 15
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 16
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 17
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 18
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 19
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 20
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 21
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 22
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 23
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 24
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 25  ;; DON'T EDIT THIS LINE!
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 26
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 27
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 28
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 29
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 30
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 31
(assert-parse-error "escaped raw control char in string literal" "\"\\\"")  ;; 127

(total-report)
