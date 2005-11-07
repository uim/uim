;;  FileName : test-char.scm
;;  About    : unit test for R5RS char
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

;; check char?
(assert-true "alphabet char" (char? #\a))
(assert-true "space 1"       (char? #\space))
(assert-true "space 2"       (char? #\ ))
(assert-true "tab"           (char? #\	))
(assert-true "newline 2"     (char? #\newline))
(assert-true "newline 2"     (char? #\
))
(assert-true "hiragana char" (char? #\дв))
(assert-true "( char"        (char? #\())
(assert-true ") char"        (char? #\)))
(assert-true "\\ char"       (char? #\\))

;; R6RS(SRFI-75) named chars
;; NOTE: #\x0e -style character is defined in R6RS(SRFI-75)
(assert-equal? "R6RS named chars" #\nul       #\x00)  ;; 0
(assert-equal? "R6RS named chars" #\alarm     #\x07)  ;; 7
(assert-equal? "R6RS named chars" #\backspace #\x08)  ;; 8
(assert-equal? "R6RS named chars" #\tab       #\x09)  ;; 9
(assert-equal? "R6RS named chars" #\newline   #\x0a)  ;; 10
(assert-equal? "R6RS named chars" #\vtab      #\x0b)  ;; 11
(assert-equal? "R6RS named chars" #\page      #\x0c)  ;; 12
(assert-equal? "R6RS named chars" #\return    #\x0d)  ;; 13
(assert-equal? "R6RS named chars" #\esc       #\x1b)  ;; 27
(assert-equal? "R6RS named chars" #\space     #\x20)  ;; 32
(assert-equal? "R6RS named chars" #\delete    #\x7f)  ;; 127

;; integer->char
;; NOTE: #\x0e -style character is defined in R6RS(SRFI-75)
(assert-equal? "integer->char" #\nul       (integer->char 0))    ;; 0
(assert-equal? "integer->char" #\x01       (integer->char 1))    ;; 1
(assert-equal? "integer->char" #\x02       (integer->char 2))    ;; 2
(assert-equal? "integer->char" #\x03       (integer->char 3))    ;; 3
(assert-equal? "integer->char" #\x04       (integer->char 4))    ;; 4
(assert-equal? "integer->char" #\x05       (integer->char 5))    ;; 5
(assert-equal? "integer->char" #\x06       (integer->char 6))    ;; 6
(assert-equal? "integer->char" #\alarm     (integer->char 7))    ;; 7
(assert-equal? "integer->char" #\backspace (integer->char 8))    ;; 8
(assert-equal? "integer->char" #\tab       (integer->char 9))    ;; 9
(assert-equal? "integer->char" #\newline   (integer->char 10))   ;; 10
(assert-equal? "integer->char" #\vtab      (integer->char 11))   ;; 11
(assert-equal? "integer->char" #\page      (integer->char 12))   ;; 12
(assert-equal? "integer->char" #\return    (integer->char 13))   ;; 13
(assert-equal? "integer->char" #\x0e       (integer->char 14))   ;; 14
(assert-equal? "integer->char" #\x0f       (integer->char 15))   ;; 15
(assert-equal? "integer->char" #\x10       (integer->char 16))   ;; 16
(assert-equal? "integer->char" #\x11       (integer->char 17))   ;; 17
(assert-equal? "integer->char" #\x12       (integer->char 18))   ;; 18
(assert-equal? "integer->char" #\x13       (integer->char 19))   ;; 19
(assert-equal? "integer->char" #\x14       (integer->char 20))   ;; 20
(assert-equal? "integer->char" #\x15       (integer->char 21))   ;; 21
(assert-equal? "integer->char" #\x16       (integer->char 22))   ;; 22
(assert-equal? "integer->char" #\x17       (integer->char 23))   ;; 23
(assert-equal? "integer->char" #\x18       (integer->char 24))   ;; 24
(assert-equal? "integer->char" #\x19       (integer->char 25))   ;; 25
(assert-equal? "integer->char" #\x1a       (integer->char 26))   ;; 26
(assert-equal? "integer->char" #\esc       (integer->char 27))   ;; 27
(assert-equal? "integer->char" #\x1c       (integer->char 28))   ;; 28
(assert-equal? "integer->char" #\x1d       (integer->char 29))   ;; 29
(assert-equal? "integer->char" #\x1e       (integer->char 30))   ;; 30
(assert-equal? "integer->char" #\x1f       (integer->char 31))   ;; 31
(assert-equal? "integer->char" #\space     (integer->char 32))   ;; 32
(assert-equal? "integer->char" #\!         (integer->char 33))   ;; 33
(assert-equal? "integer->char" #\"         (integer->char 34))   ;; 34
(assert-equal? "integer->char" #\#         (integer->char 35))   ;; 35
(assert-equal? "integer->char" #\$         (integer->char 36))   ;; 36
(assert-equal? "integer->char" #\%         (integer->char 37))   ;; 37
(assert-equal? "integer->char" #\&         (integer->char 38))   ;; 38
(assert-equal? "integer->char" #\'         (integer->char 39))   ;; 39
(assert-equal? "integer->char" #\(         (integer->char 40))   ;; 40
(assert-equal? "integer->char" #\)         (integer->char 41))   ;; 41
(assert-equal? "integer->char" #\*         (integer->char 42))   ;; 42
(assert-equal? "integer->char" #\+         (integer->char 43))   ;; 43
(assert-equal? "integer->char" #\,         (integer->char 44))   ;; 44
(assert-equal? "integer->char" #\-         (integer->char 45))   ;; 45
(assert-equal? "integer->char" #\.         (integer->char 46))   ;; 46
(assert-equal? "integer->char" #\/         (integer->char 47))   ;; 47
(assert-equal? "integer->char" #\0         (integer->char 48))   ;; 48
(assert-equal? "integer->char" #\1         (integer->char 49))   ;; 49
(assert-equal? "integer->char" #\2         (integer->char 50))   ;; 50
(assert-equal? "integer->char" #\3         (integer->char 51))   ;; 51
(assert-equal? "integer->char" #\4         (integer->char 52))   ;; 52
(assert-equal? "integer->char" #\5         (integer->char 53))   ;; 53
(assert-equal? "integer->char" #\6         (integer->char 54))   ;; 54
(assert-equal? "integer->char" #\7         (integer->char 55))   ;; 55
(assert-equal? "integer->char" #\8         (integer->char 56))   ;; 56
(assert-equal? "integer->char" #\9         (integer->char 57))   ;; 57
(assert-equal? "integer->char" #\:         (integer->char 58))   ;; 58
(assert-equal? "integer->char" #\;         (integer->char 59))   ;; 59
(assert-equal? "integer->char" #\<         (integer->char 60))   ;; 60
(assert-equal? "integer->char" #\=         (integer->char 61))   ;; 61
(assert-equal? "integer->char" #\>         (integer->char 62))   ;; 62
(assert-equal? "integer->char" #\?         (integer->char 63))   ;; 63
(assert-equal? "integer->char" #\@         (integer->char 64))   ;; 64
(assert-equal? "integer->char" #\A         (integer->char 65))   ;; 65
(assert-equal? "integer->char" #\B         (integer->char 66))   ;; 66
(assert-equal? "integer->char" #\C         (integer->char 67))   ;; 67
(assert-equal? "integer->char" #\D         (integer->char 68))   ;; 68
(assert-equal? "integer->char" #\E         (integer->char 69))   ;; 69
(assert-equal? "integer->char" #\F         (integer->char 70))   ;; 70
(assert-equal? "integer->char" #\G         (integer->char 71))   ;; 71
(assert-equal? "integer->char" #\H         (integer->char 72))   ;; 72
(assert-equal? "integer->char" #\I         (integer->char 73))   ;; 73
(assert-equal? "integer->char" #\J         (integer->char 74))   ;; 74
(assert-equal? "integer->char" #\K         (integer->char 75))   ;; 75
(assert-equal? "integer->char" #\L         (integer->char 76))   ;; 76
(assert-equal? "integer->char" #\M         (integer->char 77))   ;; 77
(assert-equal? "integer->char" #\N         (integer->char 78))   ;; 78
(assert-equal? "integer->char" #\O         (integer->char 79))   ;; 79
(assert-equal? "integer->char" #\P         (integer->char 80))   ;; 80
(assert-equal? "integer->char" #\Q         (integer->char 81))   ;; 81
(assert-equal? "integer->char" #\R         (integer->char 82))   ;; 82
(assert-equal? "integer->char" #\S         (integer->char 83))   ;; 83
(assert-equal? "integer->char" #\T         (integer->char 84))   ;; 84
(assert-equal? "integer->char" #\U         (integer->char 85))   ;; 85
(assert-equal? "integer->char" #\V         (integer->char 86))   ;; 86
(assert-equal? "integer->char" #\W         (integer->char 87))   ;; 87
(assert-equal? "integer->char" #\X         (integer->char 88))   ;; 88
(assert-equal? "integer->char" #\Y         (integer->char 89))   ;; 89
(assert-equal? "integer->char" #\Z         (integer->char 90))   ;; 90
(assert-equal? "integer->char" #\[         (integer->char 91))   ;; 91
(assert-equal? "integer->char" #\\         (integer->char 92))   ;; 92
(assert-equal? "integer->char" #\]         (integer->char 93))   ;; 93
(assert-equal? "integer->char" #\^         (integer->char 94))   ;; 94
(assert-equal? "integer->char" #\_         (integer->char 95))   ;; 95
(assert-equal? "integer->char" #\`         (integer->char 96))   ;; 96
(assert-equal? "integer->char" #\a         (integer->char 97))   ;; 97
(assert-equal? "integer->char" #\b         (integer->char 98))   ;; 98
(assert-equal? "integer->char" #\c         (integer->char 99))   ;; 99
(assert-equal? "integer->char" #\d         (integer->char 100))  ;; 100
(assert-equal? "integer->char" #\e         (integer->char 101))  ;; 101
(assert-equal? "integer->char" #\f         (integer->char 102))  ;; 102
(assert-equal? "integer->char" #\g         (integer->char 103))  ;; 103
(assert-equal? "integer->char" #\h         (integer->char 104))  ;; 104
(assert-equal? "integer->char" #\i         (integer->char 105))  ;; 105
(assert-equal? "integer->char" #\j         (integer->char 106))  ;; 106
(assert-equal? "integer->char" #\k         (integer->char 107))  ;; 107
(assert-equal? "integer->char" #\l         (integer->char 108))  ;; 108
(assert-equal? "integer->char" #\m         (integer->char 109))  ;; 109
(assert-equal? "integer->char" #\n         (integer->char 110))  ;; 110
(assert-equal? "integer->char" #\o         (integer->char 111))  ;; 111
(assert-equal? "integer->char" #\p         (integer->char 112))  ;; 112
(assert-equal? "integer->char" #\q         (integer->char 113))  ;; 113
(assert-equal? "integer->char" #\r         (integer->char 114))  ;; 114
(assert-equal? "integer->char" #\s         (integer->char 115))  ;; 115
(assert-equal? "integer->char" #\t         (integer->char 116))  ;; 116
(assert-equal? "integer->char" #\u         (integer->char 117))  ;; 117
(assert-equal? "integer->char" #\v         (integer->char 118))  ;; 118
(assert-equal? "integer->char" #\w         (integer->char 119))  ;; 119
(assert-equal? "integer->char" #\x         (integer->char 120))  ;; 120
(assert-equal? "integer->char" #\y         (integer->char 121))  ;; 121
(assert-equal? "integer->char" #\z         (integer->char 122))  ;; 122
(assert-equal? "integer->char" #\{         (integer->char 123))  ;; 123
(assert-equal? "integer->char" #\|         (integer->char 124))  ;; 124
(assert-equal? "integer->char" #\}         (integer->char 125))  ;; 125
(assert-equal? "integer->char" #\~         (integer->char 126))  ;; 126
(assert-equal? "integer->char" #\delete    (integer->char 127))  ;; 127

(total-report)
