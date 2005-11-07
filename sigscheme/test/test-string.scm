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
(assert-true"string? check" (string? "aiueo"))

;; check make-string
(assert-true"null make-string" (string? (make-string 6)))
(assert-true"alphabet make-string check" (string=? "aaa" (make-string 3 #\a)))
(assert-true"hiragana make-string check" (string=? "あああ" (make-string 3 #\あ)))

;; check string-ref
(assert-equal? "alphabet string-ref check" #\o  (string-ref "aiueo" 4))
(assert-equal? "hiragana string-ref check" #\お (string-ref "あいうえお" 4))
(assert-equal? "mixed string-ref check"    #\お (string-ref "あiueお" 4))
(assert-equal? "alphabet string-ref 0 check" #\a  (string-ref "aiueo" 0))
(assert-equal? "hiragena string-ref 0 check" #\あ (string-ref "あいうえお" 0))

;; check string-set!
(assert-true"alphabet string-set! check" (string=? "aikeo"
					       (begin
						 (define str "aiueo")
						 (string-set! str 2 #\k)
						 str)))
(assert-true"hiragana string-set! check" (string=? "あいかえお"
					       (begin
						 (define str "あいうえお")
						 (string-set! str 2 #\か)
						 str)))
(assert-true"mixed string-set! check" (string=? "aiueo"
					    (begin
					      (define str "aiうeo")
					      (string-set! str 2 #\u)
					      str)))

;; immutable strings: See "3.4 Storage model" of R5RS
(assert-error "string-set! on constant string #1"
              (lambda ()
                (string-set! "foo" 0 #\b)))
(assert-error "string-set! on constant string #2"
              (lambda ()
                (string-set! (symbol->string 'foo) 0 #\b)))

;; check string-length
(assert-equal? "alphabet string-length check" 5 (string-length "aiueo"))
(assert-equal? "hiragana string-length check" 5 (string-length "あいうえお"))
(assert-equal? "backslash string-length check" 1 (string-length "\\"))
(assert-equal? "backslash string-length check" 2 (string-length "\\\\"))
(assert-equal? "backslash string-length check" 3 (string-length "\\\\\\"))

;; string=? check
(assert-equal? "alphabet string=? check" #t (string=? "aiueo" "aiueo"))
(assert-equal? "hiragana string=? check" #t (string=? "あいうえお" "あいうえお"))
(assert-equal? "mixed string=? check"    #t (string=? "aいうえo" "aいうえo"))

;; substring check
(assert-true"alphabet substring check" (string=? "iu"   (substring "aiueo" 1 3)))
(assert-true"hiragana substring check" (string=? "いう" (substring "あいうえお" 1 3)))
(assert-true"mixed substring check"    (string=? "いu"  (substring "aいuえo" 1 3)))


;; string-append check
(assert-true"alphabet 1 string-append check" (string=? "a"   (string-append "a")))
(assert-true"alphabet 2 string-append check" (string=? "ai"  (string-append "a" "i")))
(assert-true"alphabet 3 string-append check" (string=? "aiu" (string-append "a" "i" "u")))
(assert-true"hiragana 1 string-append check" (string=? "あ"     (string-append "あ")))
(assert-true"hiragana 2 string-append check" (string=? "あい"   (string-append "あ" "い")))
(assert-true"hiragana 3 string-append check" (string=? "あいう" (string-append "あ" "い" "う")))
(assert-true"mixed 2 string-append check" (string=? "あi"   (string-append "あ" "i")))
(assert-true"mixed 3 string-append check" (string=? "あiう" (string-append "あ" "i" "う")))

;; string->list
(assert-true"string->list check" (equal? '(#\あ #\i #\う #\e #\お) (string->list "あiうeお")))
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
(assert-equal? "list->string check" "あaい" (list->string '(#\あ #\a #\い)))
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
(assert-true"alphabet string-fill! check" (string=? "jjjjj" (begin
							  (define str "aiueo")
							  (string-fill! str #\j)
							  str)))
(assert-true"hiragana string-fill! check" (string=? "あああああ" (begin
							       (define str "aiueo")
							       (string-fill! str #\あ)
							       str)))
(assert-true"mixed string-fill! by alphabet check" (string=? "aaaaa" (begin
								   (define str "aいうえo")
								   (string-fill! str #\a)
								   str)))
(assert-true"mixed string-fill! by hiragana check" (string=? "いいいいい" (begin
									(define str "aいうえo")
									(string-fill! str #\い)
									str)))

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
(assert-equal? "R6RS escape sequence" (integer->string 0)        "\0")  ;; 0
(assert-equal? "R6RS escape sequence" (list->string '(#\nul))    "\0")  ;; 0
(assert-equal? "R6RS escape sequence" '(#\nul)    (string->list "\0"))  ;; 0
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
;; error". Since no way exist for testing parse error, the tests are simply
;; disabled.
;;;;                                                "\0"   ;; 0
;;(assert-equal? "conventional escape sequence" " " "\ ")  ;; 32
;;(assert-equal? "conventional escape sequence" "!" "\!")  ;; 33
;;;;                                                "\""   ;; 34
;;(assert-equal? "conventional escape sequence" "#" "\#")  ;; 35
;;(assert-equal? "conventional escape sequence" "$" "\$")  ;; 36
;;(assert-equal? "conventional escape sequence" "%" "\%")  ;; 37
;;(assert-equal? "conventional escape sequence" "&" "\&")  ;; 38
;;(assert-equal? "conventional escape sequence" "'" "\'")  ;; 39
;;(assert-equal? "conventional escape sequence" "(" "\(")  ;; 40
;;(assert-equal? "conventional escape sequence" ")" "\)")  ;; 41
;;(assert-equal? "conventional escape sequence" "*" "\*")  ;; 42
;;(assert-equal? "conventional escape sequence" "+" "\+")  ;; 43
;;(assert-equal? "conventional escape sequence" "," "\,")  ;; 44
;;(assert-equal? "conventional escape sequence" "-" "\-")  ;; 45
;;(assert-equal? "conventional escape sequence" "." "\.")  ;; 46
;;(assert-equal? "conventional escape sequence" "/" "\/")  ;; 47
;;(assert-equal? "conventional escape sequence" "0" "\0")  ;; 48
;;(assert-equal? "conventional escape sequence" "1" "\1")  ;; 49
;;(assert-equal? "conventional escape sequence" "2" "\2")  ;; 50
;;(assert-equal? "conventional escape sequence" "3" "\3")  ;; 51
;;(assert-equal? "conventional escape sequence" "4" "\4")  ;; 52
;;(assert-equal? "conventional escape sequence" "5" "\5")  ;; 53
;;(assert-equal? "conventional escape sequence" "6" "\6")  ;; 54
;;(assert-equal? "conventional escape sequence" "7" "\7")  ;; 55
;;(assert-equal? "conventional escape sequence" "8" "\8")  ;; 56
;;(assert-equal? "conventional escape sequence" "9" "\9")  ;; 57
;;(assert-equal? "conventional escape sequence" ":" "\:")  ;; 58
;;(assert-equal? "conventional escape sequence" ";" "\;")  ;; 59
;;(assert-equal? "conventional escape sequence" "<" "\<")  ;; 60
;;(assert-equal? "conventional escape sequence" "=" "\=")  ;; 61
;;(assert-equal? "conventional escape sequence" ">" "\>")  ;; 62
;;(assert-equal? "conventional escape sequence" "?" "\?")  ;; 63
;;(assert-equal? "conventional escape sequence" "@" "\@")  ;; 64
;;(assert-equal? "conventional escape sequence" "A" "\A")  ;; 65
;;(assert-equal? "conventional escape sequence" "B" "\B")  ;; 66
;;(assert-equal? "conventional escape sequence" "C" "\C")  ;; 67
;;(assert-equal? "conventional escape sequence" "D" "\D")  ;; 68
;;(assert-equal? "conventional escape sequence" "E" "\E")  ;; 69
;;(assert-equal? "conventional escape sequence" "F" "\F")  ;; 70
;;(assert-equal? "conventional escape sequence" "G" "\G")  ;; 71
;;(assert-equal? "conventional escape sequence" "H" "\H")  ;; 72
;;(assert-equal? "conventional escape sequence" "I" "\I")  ;; 73
;;(assert-equal? "conventional escape sequence" "J" "\J")  ;; 74
;;(assert-equal? "conventional escape sequence" "K" "\K")  ;; 75
;;(assert-equal? "conventional escape sequence" "L" "\L")  ;; 76
;;(assert-equal? "conventional escape sequence" "M" "\M")  ;; 77
;;(assert-equal? "conventional escape sequence" "N" "\N")  ;; 78
;;(assert-equal? "conventional escape sequence" "O" "\O")  ;; 79
;;(assert-equal? "conventional escape sequence" "P" "\P")  ;; 80
;;(assert-equal? "conventional escape sequence" "Q" "\Q")  ;; 81
;;(assert-equal? "conventional escape sequence" "R" "\R")  ;; 82
;;(assert-equal? "conventional escape sequence" "S" "\S")  ;; 83
;;(assert-equal? "conventional escape sequence" "T" "\T")  ;; 84
;;(assert-equal? "conventional escape sequence" "U" "\U")  ;; 85
;;(assert-equal? "conventional escape sequence" "V" "\V")  ;; 86
;;(assert-equal? "conventional escape sequence" "W" "\W")  ;; 87
;;(assert-equal? "conventional escape sequence" "X" "\X")  ;; 88
;;(assert-equal? "conventional escape sequence" "Y" "\Y")  ;; 89
;;(assert-equal? "conventional escape sequence" "Z" "\Z")  ;; 90
;;(assert-equal? "conventional escape sequence" "[" "\[")  ;; 91
;;;;                                                "\\"   ;; 92
;;(assert-equal? "conventional escape sequence" "]" "\]")  ;; 93
;;(assert-equal? "conventional escape sequence" "^" "\^")  ;; 94
;;(assert-equal? "conventional escape sequence" "_" "\_")  ;; 95
;;(assert-equal? "conventional escape sequence" "`" "\`")  ;; 96
;;;;                                                "\a"   ;; 97
;;;;                                                "\b"   ;; 98
;;(assert-equal? "conventional escape sequence" "c" "\c")  ;; 99
;;(assert-equal? "conventional escape sequence" "d" "\d")  ;; 100
;;(assert-equal? "conventional escape sequence" "e" "\e")  ;; 101
;;;;                                                "\f"   ;; 102
;;(assert-equal? "conventional escape sequence" "g" "\g")  ;; 103
;;(assert-equal? "conventional escape sequence" "h" "\h")  ;; 104
;;(assert-equal? "conventional escape sequence" "i" "\i")  ;; 105
;;(assert-equal? "conventional escape sequence" "j" "\j")  ;; 106
;;(assert-equal? "conventional escape sequence" "k" "\k")  ;; 107
;;(assert-equal? "conventional escape sequence" "l" "\l")  ;; 108
;;(assert-equal? "conventional escape sequence" "m" "\m")  ;; 109
;;;;                                                "\n"   ;; 110
;;(assert-equal? "conventional escape sequence" "o" "\o")  ;; 111
;;(assert-equal? "conventional escape sequence" "p" "\p")  ;; 112
;;(assert-equal? "conventional escape sequence" "q" "\q")  ;; 113
;;;;                                                "\r"   ;; 114
;;(assert-equal? "conventional escape sequence" "s" "\s")  ;; 115
;;;;                                                "\t"   ;; 116
;;(assert-equal? "conventional escape sequence" "u" "\u")  ;; 117
;;;;                                                "\v"   ;; 118
;;(assert-equal? "conventional escape sequence" "w" "\w")  ;; 119
;;(assert-equal? "conventional escape sequence" "x" "\x")  ;; 120
;;(assert-equal? "conventional escape sequence" "y" "\y")  ;; 121
;;(assert-equal? "conventional escape sequence" "z" "\z")  ;; 122
;;(assert-equal? "conventional escape sequence" "{" "\{")  ;; 123
;;;;                                                "\|"   ;; 124
;;(assert-equal? "conventional escape sequence" "}" "\}")  ;; 125
;;(assert-equal? "conventional escape sequence" "~" "\~")  ;; 126

(total-report)
