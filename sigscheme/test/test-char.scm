#! /usr/bin/env sscm -C EUC-JP
;; -*- buffer-file-coding-system: euc-jp -*-

;;  Filename : test-char.scm
;;  About    : unit test for R5RS char
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

(if (not (symbol-bound? 'char?))
    (test-skip "R5RS characters is not enabled"))

(define tn test-name)

(define i->chlit
  (lambda (i)
    (obj->literal (integer->char i))))

;; invalid character literal
(tn "invalid char literal")
(assert-parse-error (tn) "#\\nonexistent")

(assert-equal? (tn)
               (integer->char 120)
               (read (open-input-string "#\\x")))
(assert-parse-error (tn) "#\\x0")
(assert-parse-error (tn) "#\\x1")
(assert-parse-error (tn) "#\\x0g")
(assert-parse-error (tn) "#\\x1g")
(assert-parse-error (tn) "#\\x00g")
(assert-parse-error (tn) "#\\x01g")

(assert-parse-error (tn) "#\\x000")
(assert-parse-error (tn) "#\\x010")
(assert-parse-error (tn) "#\\x001")
(assert-parse-error (tn) "#\\x100")
(assert-parse-error (tn) "#\\x00a")
(assert-parse-error (tn) "#\\x0a0")
(assert-parse-error (tn) "#\\xa00")

(assert-parse-error (tn) "#\\x-0")
(assert-parse-error (tn) "#\\x-1")
(assert-parse-error (tn) "#\\x-00")
(assert-parse-error (tn) "#\\x-01")
(assert-parse-error (tn) "#\\x-000")
(assert-parse-error (tn) "#\\x-010")
(assert-parse-error (tn) "#\\x-001")
(assert-parse-error (tn) "#\\x-100")
(assert-parse-error (tn) "#\\x-00a")
(assert-parse-error (tn) "#\\x-0a0")
(assert-parse-error (tn) "#\\x-a00")

(assert-parse-error (tn) "#\\x+0")
(assert-parse-error (tn) "#\\x+1")
(assert-parse-error (tn) "#\\x+00")
(assert-parse-error (tn) "#\\x+01")
(assert-parse-error (tn) "#\\x+000")
(assert-parse-error (tn) "#\\x+010")
(assert-parse-error (tn) "#\\x+001")
(assert-parse-error (tn) "#\\x+100")
(assert-parse-error (tn) "#\\x+00a")
(assert-parse-error (tn) "#\\x+0a0")
(assert-parse-error (tn) "#\\x+a00")

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

(tn "R5RS named chars case-insensitivity")
(assert-equal? (tn) #\newline (integer->char 10))
(assert-equal? (tn) #\Newline (integer->char 10))
(assert-equal? (tn) #\NEWLINE (integer->char 10))
(assert-equal? (tn) #\NeWliNE (integer->char 10))
(assert-equal? (tn) #\space   (integer->char 32))
(assert-equal? (tn) #\Space   (integer->char 32))
(assert-equal? (tn) #\SPACE   (integer->char 32))
(assert-equal? (tn) #\SpACe   (integer->char 32))

;;
;; R6RS(SRFI-75) named chars
;;

;; NOTE: #\x0e -style character is defined in R6RS(SRFI-75)
(tn "R6RS named chars")
(assert-equal? (tn) #\nul       #\x00)  ;; 0
(assert-equal? (tn) #\alarm     #\x07)  ;; 7
(assert-equal? (tn) #\backspace #\x08)  ;; 8
(assert-equal? (tn) #\tab       #\x09)  ;; 9
(assert-equal? (tn) #\newline   #\x0a)  ;; 10
(assert-equal? (tn) #\vtab      #\x0b)  ;; 11
(assert-equal? (tn) #\page      #\x0c)  ;; 12
(assert-equal? (tn) #\return    #\x0d)  ;; 13
(assert-equal? (tn) #\esc       #\x1b)  ;; 27
(assert-equal? (tn) #\space     #\x20)  ;; 32
(assert-equal? (tn) #\delete    #\x7f)  ;; 127

(assert-equal? (tn) "#\\nul"     (obj->literal #\x00))  ;; 0
(assert-equal? (tn) "#\\alarm"   (obj->literal #\x07))  ;; 7
(assert-equal? (tn) "#\\backspace" (obj->literal #\x08))  ;; 8
(assert-equal? (tn) "#\\tab"     (obj->literal #\x09))  ;; 9
(assert-equal? (tn) "#\\newline" (obj->literal #\x0a))  ;; 10
(assert-equal? (tn) "#\\vtab"    (obj->literal #\x0b))  ;; 11
(assert-equal? (tn) "#\\page"    (obj->literal #\x0c))  ;; 12
(assert-equal? (tn) "#\\return"  (obj->literal #\x0d))  ;; 13
(assert-equal? (tn) "#\\esc"     (obj->literal #\x1b))  ;; 27
(assert-equal? (tn) "#\\space"   (obj->literal #\x20))  ;; 32
(assert-equal? (tn) "#\\delete"  (obj->literal #\x7f))  ;; 127

(tn "char literal")
(assert-equal? (tn) "#\\nul"       (obj->literal #\nul))       ;; 0
(assert-equal? (tn) "#\\x01"       (obj->literal #\x01))       ;; 1
(assert-equal? (tn) "#\\x02"       (obj->literal #\x02))       ;; 2
(assert-equal? (tn) "#\\x03"       (obj->literal #\x03))       ;; 3
(assert-equal? (tn) "#\\x04"       (obj->literal #\x04))       ;; 4
(assert-equal? (tn) "#\\x05"       (obj->literal #\x05))       ;; 5
(assert-equal? (tn) "#\\x06"       (obj->literal #\x06))       ;; 6
(assert-equal? (tn) "#\\alarm"     (obj->literal #\alarm))     ;; 7
(assert-equal? (tn) "#\\backspace" (obj->literal #\backspace)) ;; 8
(assert-equal? (tn) "#\\tab"       (obj->literal #\tab))       ;; 9
(assert-equal? (tn) "#\\newline"   (obj->literal #\newline))   ;; 10
(assert-equal? (tn) "#\\vtab"      (obj->literal #\vtab))      ;; 11
(assert-equal? (tn) "#\\page"      (obj->literal #\page))      ;; 12
(assert-equal? (tn) "#\\return"    (obj->literal #\return))    ;; 13
(assert-equal? (tn) "#\\x0e"       (obj->literal #\x0e))       ;; 14
(assert-equal? (tn) "#\\x0f"       (obj->literal #\x0f))       ;; 15
(assert-equal? (tn) "#\\x10"       (obj->literal #\x10))       ;; 16
(assert-equal? (tn) "#\\x11"       (obj->literal #\x11))       ;; 17
(assert-equal? (tn) "#\\x12"       (obj->literal #\x12))       ;; 18
(assert-equal? (tn) "#\\x13"       (obj->literal #\x13))       ;; 19
(assert-equal? (tn) "#\\x14"       (obj->literal #\x14))       ;; 20
(assert-equal? (tn) "#\\x15"       (obj->literal #\x15))       ;; 21
(assert-equal? (tn) "#\\x16"       (obj->literal #\x16))       ;; 22
(assert-equal? (tn) "#\\x17"       (obj->literal #\x17))       ;; 23
(assert-equal? (tn) "#\\x18"       (obj->literal #\x18))       ;; 24
(assert-equal? (tn) "#\\x19"       (obj->literal #\x19))       ;; 25
(assert-equal? (tn) "#\\x1a"       (obj->literal #\x1a))       ;; 26
(assert-equal? (tn) "#\\esc"       (obj->literal #\esc))       ;; 27
(assert-equal? (tn) "#\\x1c"       (obj->literal #\x1c))       ;; 28
(assert-equal? (tn) "#\\x1d"       (obj->literal #\x1d))       ;; 29
(assert-equal? (tn) "#\\x1e"       (obj->literal #\x1e))       ;; 30
(assert-equal? (tn) "#\\x1f"       (obj->literal #\x1f))       ;; 31
(assert-equal? (tn) "#\\space"     (obj->literal #\space))     ;; 32
(assert-equal? (tn) "#\\!"         (obj->literal #\!))         ;; 33
(assert-equal? (tn) "#\\\""        (obj->literal #\"))         ;; 34
(assert-equal? (tn) "#\\#"         (obj->literal #\#))         ;; 35
(assert-equal? (tn) "#\\$"         (obj->literal #\$))         ;; 36
(assert-equal? (tn) "#\\%"         (obj->literal #\%))         ;; 37
(assert-equal? (tn) "#\\&"         (obj->literal #\&))         ;; 38
(assert-equal? (tn) "#\\'"         (obj->literal #\'))         ;; 39
(assert-equal? (tn) "#\\("         (obj->literal #\())         ;; 40
(assert-equal? (tn) "#\\)"         (obj->literal #\)))         ;; 41
(assert-equal? (tn) "#\\*"         (obj->literal #\*))         ;; 42
(assert-equal? (tn) "#\\+"         (obj->literal #\+))         ;; 43
(assert-equal? (tn) "#\\,"         (obj->literal #\,))         ;; 44
(assert-equal? (tn) "#\\-"         (obj->literal #\-))         ;; 45
(assert-equal? (tn) "#\\."         (obj->literal #\.))         ;; 46
(assert-equal? (tn) "#\\/"         (obj->literal #\/))         ;; 47
(assert-equal? (tn) "#\\0"         (obj->literal #\0))         ;; 48
(assert-equal? (tn) "#\\1"         (obj->literal #\1))         ;; 49
(assert-equal? (tn) "#\\2"         (obj->literal #\2))         ;; 50
(assert-equal? (tn) "#\\3"         (obj->literal #\3))         ;; 51
(assert-equal? (tn) "#\\4"         (obj->literal #\4))         ;; 52
(assert-equal? (tn) "#\\5"         (obj->literal #\5))         ;; 53
(assert-equal? (tn) "#\\6"         (obj->literal #\6))         ;; 54
(assert-equal? (tn) "#\\7"         (obj->literal #\7))         ;; 55
(assert-equal? (tn) "#\\8"         (obj->literal #\8))         ;; 56
(assert-equal? (tn) "#\\9"         (obj->literal #\9))         ;; 57
(assert-equal? (tn) "#\\:"         (obj->literal #\:))         ;; 58
(assert-equal? (tn) "#\\;"         (obj->literal #\;))         ;; 59
(assert-equal? (tn) "#\\<"         (obj->literal #\<))         ;; 60
(assert-equal? (tn) "#\\="         (obj->literal #\=))         ;; 61
(assert-equal? (tn) "#\\>"         (obj->literal #\>))         ;; 62
(assert-equal? (tn) "#\\?"         (obj->literal #\?))         ;; 63
(assert-equal? (tn) "#\\@"         (obj->literal #\@))         ;; 64
(assert-equal? (tn) "#\\A"         (obj->literal #\A))         ;; 65
(assert-equal? (tn) "#\\B"         (obj->literal #\B))         ;; 66
(assert-equal? (tn) "#\\C"         (obj->literal #\C))         ;; 67
(assert-equal? (tn) "#\\D"         (obj->literal #\D))         ;; 68
(assert-equal? (tn) "#\\E"         (obj->literal #\E))         ;; 69
(assert-equal? (tn) "#\\F"         (obj->literal #\F))         ;; 70
(assert-equal? (tn) "#\\G"         (obj->literal #\G))         ;; 71
(assert-equal? (tn) "#\\H"         (obj->literal #\H))         ;; 72
(assert-equal? (tn) "#\\I"         (obj->literal #\I))         ;; 73
(assert-equal? (tn) "#\\J"         (obj->literal #\J))         ;; 74
(assert-equal? (tn) "#\\K"         (obj->literal #\K))         ;; 75
(assert-equal? (tn) "#\\L"         (obj->literal #\L))         ;; 76
(assert-equal? (tn) "#\\M"         (obj->literal #\M))         ;; 77
(assert-equal? (tn) "#\\N"         (obj->literal #\N))         ;; 78
(assert-equal? (tn) "#\\O"         (obj->literal #\O))         ;; 79
(assert-equal? (tn) "#\\P"         (obj->literal #\P))         ;; 80
(assert-equal? (tn) "#\\Q"         (obj->literal #\Q))         ;; 81
(assert-equal? (tn) "#\\R"         (obj->literal #\R))         ;; 82
(assert-equal? (tn) "#\\S"         (obj->literal #\S))         ;; 83
(assert-equal? (tn) "#\\T"         (obj->literal #\T))         ;; 84
(assert-equal? (tn) "#\\U"         (obj->literal #\U))         ;; 85
(assert-equal? (tn) "#\\V"         (obj->literal #\V))         ;; 86
(assert-equal? (tn) "#\\W"         (obj->literal #\W))         ;; 87
(assert-equal? (tn) "#\\X"         (obj->literal #\X))         ;; 88
(assert-equal? (tn) "#\\Y"         (obj->literal #\Y))         ;; 89
(assert-equal? (tn) "#\\Z"         (obj->literal #\Z))         ;; 90
(assert-equal? (tn) "#\\["         (obj->literal #\[))         ;; 91
(assert-equal? (tn) "#\\\\"        (obj->literal #\\))         ;; 92
(assert-equal? (tn) "#\\]"         (obj->literal #\]))         ;; 93
(assert-equal? (tn) "#\\^"         (obj->literal #\^))         ;; 94
(assert-equal? (tn) "#\\_"         (obj->literal #\_))         ;; 95
(assert-equal? (tn) "#\\`"         (obj->literal #\`))         ;; 96
(assert-equal? (tn) "#\\a"         (obj->literal #\a))         ;; 97
(assert-equal? (tn) "#\\b"         (obj->literal #\b))         ;; 98
(assert-equal? (tn) "#\\c"         (obj->literal #\c))         ;; 99
(assert-equal? (tn) "#\\d"         (obj->literal #\d))         ;; 100
(assert-equal? (tn) "#\\e"         (obj->literal #\e))         ;; 101
(assert-equal? (tn) "#\\f"         (obj->literal #\f))         ;; 102
(assert-equal? (tn) "#\\g"         (obj->literal #\g))         ;; 103
(assert-equal? (tn) "#\\h"         (obj->literal #\h))         ;; 104
(assert-equal? (tn) "#\\i"         (obj->literal #\i))         ;; 105
(assert-equal? (tn) "#\\j"         (obj->literal #\j))         ;; 106
(assert-equal? (tn) "#\\k"         (obj->literal #\k))         ;; 107
(assert-equal? (tn) "#\\l"         (obj->literal #\l))         ;; 108
(assert-equal? (tn) "#\\m"         (obj->literal #\m))         ;; 109
(assert-equal? (tn) "#\\n"         (obj->literal #\n))         ;; 110
(assert-equal? (tn) "#\\o"         (obj->literal #\o))         ;; 111
(assert-equal? (tn) "#\\p"         (obj->literal #\p))         ;; 112
(assert-equal? (tn) "#\\q"         (obj->literal #\q))         ;; 113
(assert-equal? (tn) "#\\r"         (obj->literal #\r))         ;; 114
(assert-equal? (tn) "#\\s"         (obj->literal #\s))         ;; 115
(assert-equal? (tn) "#\\t"         (obj->literal #\t))         ;; 116
(assert-equal? (tn) "#\\u"         (obj->literal #\u))         ;; 117
(assert-equal? (tn) "#\\v"         (obj->literal #\v))         ;; 118
(assert-equal? (tn) "#\\w"         (obj->literal #\w))         ;; 119
(assert-equal? (tn) "#\\x"         (obj->literal #\x))         ;; 120
(assert-equal? (tn) "#\\y"         (obj->literal #\y))         ;; 121
(assert-equal? (tn) "#\\z"         (obj->literal #\z))         ;; 122
(assert-equal? (tn) "#\\{"         (obj->literal #\{))         ;; 123
(assert-equal? (tn) "#\\|"         (obj->literal #\|))         ;; 124
(assert-equal? (tn) "#\\}"         (obj->literal #\}))         ;; 125
(assert-equal? (tn) "#\\~"         (obj->literal #\~))         ;; 126
(assert-equal? (tn) "#\\delete"    (obj->literal #\delete))    ;; 127

;; R6RS(SRFI-75) hexadecimal character literal
(tn "R6RS hexadecimal char literal")
(assert-equal? (tn) #\nul       #\x00)    ;; 0
(assert-equal? (tn) #\x01       #\x01)    ;; 1
(assert-equal? (tn) #\x02       #\x02)    ;; 2
(assert-equal? (tn) #\x03       #\x03)    ;; 3
(assert-equal? (tn) #\x04       #\x04)    ;; 4
(assert-equal? (tn) #\x05       #\x05)    ;; 5
(assert-equal? (tn) #\x06       #\x06)    ;; 6
(assert-equal? (tn) #\alarm     #\x07)    ;; 7
(assert-equal? (tn) #\backspace #\x08)    ;; 8
(assert-equal? (tn) #\tab       #\x09)    ;; 9
(assert-equal? (tn) #\newline   #\x0a)   ;; 10
(assert-equal? (tn) #\vtab      #\x0b)   ;; 11
(assert-equal? (tn) #\page      #\x0c)   ;; 12
(assert-equal? (tn) #\return    #\x0d)   ;; 13
(assert-equal? (tn) #\x0e       #\x0e)   ;; 14
(assert-equal? (tn) #\x0f       #\x0f)   ;; 15
(assert-equal? (tn) #\x10       #\x10)   ;; 16
(assert-equal? (tn) #\x11       #\x11)   ;; 17
(assert-equal? (tn) #\x12       #\x12)   ;; 18
(assert-equal? (tn) #\x13       #\x13)   ;; 19
(assert-equal? (tn) #\x14       #\x14)   ;; 20
(assert-equal? (tn) #\x15       #\x15)   ;; 21
(assert-equal? (tn) #\x16       #\x16)   ;; 22
(assert-equal? (tn) #\x17       #\x17)   ;; 23
(assert-equal? (tn) #\x18       #\x18)   ;; 24
(assert-equal? (tn) #\x19       #\x19)   ;; 25
(assert-equal? (tn) #\x1a       #\x1a)   ;; 26
(assert-equal? (tn) #\esc       #\x1b)   ;; 27
(assert-equal? (tn) #\x1c       #\x1c)   ;; 28
(assert-equal? (tn) #\x1d       #\x1d)   ;; 29
(assert-equal? (tn) #\x1e       #\x1e)   ;; 30
(assert-equal? (tn) #\x1f       #\x1f)   ;; 31
(assert-equal? (tn) #\space     #\x20)   ;; 32
(assert-equal? (tn) #\!         #\x21)   ;; 33
(assert-equal? (tn) #\"         #\x22)   ;; 34
(assert-equal? (tn) #\#         #\x23)   ;; 35
(assert-equal? (tn) #\$         #\x24)   ;; 36
(assert-equal? (tn) #\%         #\x25)   ;; 37
(assert-equal? (tn) #\&         #\x26)   ;; 38
(assert-equal? (tn) #\'         #\x27)   ;; 39
(assert-equal? (tn) #\(         #\x28)   ;; 40
(assert-equal? (tn) #\)         #\x29)   ;; 41
(assert-equal? (tn) #\*         #\x2a)   ;; 42
(assert-equal? (tn) #\+         #\x2b)   ;; 43
(assert-equal? (tn) #\,         #\x2c)   ;; 44
(assert-equal? (tn) #\-         #\x2d)   ;; 45
(assert-equal? (tn) #\.         #\x2e)   ;; 46
(assert-equal? (tn) #\/         #\x2f)   ;; 47
(assert-equal? (tn) #\0         #\x30)   ;; 48
(assert-equal? (tn) #\1         #\x31)   ;; 49
(assert-equal? (tn) #\2         #\x32)   ;; 50
(assert-equal? (tn) #\3         #\x33)   ;; 51
(assert-equal? (tn) #\4         #\x34)   ;; 52
(assert-equal? (tn) #\5         #\x35)   ;; 53
(assert-equal? (tn) #\6         #\x36)   ;; 54
(assert-equal? (tn) #\7         #\x37)   ;; 55
(assert-equal? (tn) #\8         #\x38)   ;; 56
(assert-equal? (tn) #\9         #\x39)   ;; 57
(assert-equal? (tn) #\:         #\x3a)   ;; 58
(assert-equal? (tn) #\;         #\x3b)   ;; 59
(assert-equal? (tn) #\<         #\x3c)   ;; 60
(assert-equal? (tn) #\=         #\x3d)   ;; 61
(assert-equal? (tn) #\>         #\x3e)   ;; 62
(assert-equal? (tn) #\?         #\x3f)   ;; 63
(assert-equal? (tn) #\@         #\x40)   ;; 64
(assert-equal? (tn) #\A         #\x41)   ;; 65
(assert-equal? (tn) #\B         #\x42)   ;; 66
(assert-equal? (tn) #\C         #\x43)   ;; 67
(assert-equal? (tn) #\D         #\x44)   ;; 68
(assert-equal? (tn) #\E         #\x45)   ;; 69
(assert-equal? (tn) #\F         #\x46)   ;; 70
(assert-equal? (tn) #\G         #\x47)   ;; 71
(assert-equal? (tn) #\H         #\x48)   ;; 72
(assert-equal? (tn) #\I         #\x49)   ;; 73
(assert-equal? (tn) #\J         #\x4a)   ;; 74
(assert-equal? (tn) #\K         #\x4b)   ;; 75
(assert-equal? (tn) #\L         #\x4c)   ;; 76
(assert-equal? (tn) #\M         #\x4d)   ;; 77
(assert-equal? (tn) #\N         #\x4e)   ;; 78
(assert-equal? (tn) #\O         #\x4f)   ;; 79
(assert-equal? (tn) #\P         #\x50)   ;; 80
(assert-equal? (tn) #\Q         #\x51)   ;; 81
(assert-equal? (tn) #\R         #\x52)   ;; 82
(assert-equal? (tn) #\S         #\x53)   ;; 83
(assert-equal? (tn) #\T         #\x54)   ;; 84
(assert-equal? (tn) #\U         #\x55)   ;; 85
(assert-equal? (tn) #\V         #\x56)   ;; 86
(assert-equal? (tn) #\W         #\x57)   ;; 87
(assert-equal? (tn) #\X         #\x58)   ;; 88
(assert-equal? (tn) #\Y         #\x59)   ;; 89
(assert-equal? (tn) #\Z         #\x5a)   ;; 90
(assert-equal? (tn) #\[         #\x5b)   ;; 91
(assert-equal? (tn) #\\         #\x5c)   ;; 92
(assert-equal? (tn) #\]         #\x5d)   ;; 93
(assert-equal? (tn) #\^         #\x5e)   ;; 94
(assert-equal? (tn) #\_         #\x5f)   ;; 95
(assert-equal? (tn) #\`         #\x60)   ;; 96
(assert-equal? (tn) #\a         #\x61)   ;; 97
(assert-equal? (tn) #\b         #\x62)   ;; 98
(assert-equal? (tn) #\c         #\x63)   ;; 99
(assert-equal? (tn) #\d         #\x64)  ;; 100
(assert-equal? (tn) #\e         #\x65)  ;; 101
(assert-equal? (tn) #\f         #\x66)  ;; 102
(assert-equal? (tn) #\g         #\x67)  ;; 103
(assert-equal? (tn) #\h         #\x68)  ;; 104
(assert-equal? (tn) #\i         #\x69)  ;; 105
(assert-equal? (tn) #\j         #\x6a)  ;; 106
(assert-equal? (tn) #\k         #\x6b)  ;; 107
(assert-equal? (tn) #\l         #\x6c)  ;; 108
(assert-equal? (tn) #\m         #\x6d)  ;; 109
(assert-equal? (tn) #\n         #\x6e)  ;; 110
(assert-equal? (tn) #\o         #\x6f)  ;; 111
(assert-equal? (tn) #\p         #\x70)  ;; 112
(assert-equal? (tn) #\q         #\x71)  ;; 113
(assert-equal? (tn) #\r         #\x72)  ;; 114
(assert-equal? (tn) #\s         #\x73)  ;; 115
(assert-equal? (tn) #\t         #\x74)  ;; 116
(assert-equal? (tn) #\u         #\x75)  ;; 117
(assert-equal? (tn) #\v         #\x76)  ;; 118
(assert-equal? (tn) #\w         #\x77)  ;; 119
(assert-equal? (tn) #\x         #\x78)  ;; 120
(assert-equal? (tn) #\y         #\x79)  ;; 121
(assert-equal? (tn) #\z         #\x7a)  ;; 122
(assert-equal? (tn) #\{         #\x7b)  ;; 123
(assert-equal? (tn) #\|         #\x7c)  ;; 124
(assert-equal? (tn) #\}         #\x7d)  ;; 125
(assert-equal? (tn) #\~         #\x7e)  ;; 126
(assert-equal? (tn) #\delete    #\x7f)  ;; 127

(tn "R6RS hexadecimal char literal (string form)")
(assert-equal? (tn) "#\\nul"       (obj->literal #\x00))  ;; 0
(assert-equal? (tn) "#\\x01"       (obj->literal #\x01))  ;; 1
(assert-equal? (tn) "#\\x02"       (obj->literal #\x02))  ;; 2
(assert-equal? (tn) "#\\x03"       (obj->literal #\x03))  ;; 3
(assert-equal? (tn) "#\\x04"       (obj->literal #\x04))  ;; 4
(assert-equal? (tn) "#\\x05"       (obj->literal #\x05))  ;; 5
(assert-equal? (tn) "#\\x06"       (obj->literal #\x06))  ;; 6
(assert-equal? (tn) "#\\alarm"     (obj->literal #\x07))  ;; 7
(assert-equal? (tn) "#\\backspace" (obj->literal #\x08))  ;; 8
(assert-equal? (tn) "#\\tab"       (obj->literal #\x09))  ;; 9
(assert-equal? (tn) "#\\newline"   (obj->literal #\x0a))  ;; 10
(assert-equal? (tn) "#\\vtab"      (obj->literal #\x0b))  ;; 11
(assert-equal? (tn) "#\\page"      (obj->literal #\x0c))  ;; 12
(assert-equal? (tn) "#\\return"    (obj->literal #\x0d))  ;; 13
(assert-equal? (tn) "#\\x0e"       (obj->literal #\x0e))  ;; 14
(assert-equal? (tn) "#\\x0f"       (obj->literal #\x0f))  ;; 15
(assert-equal? (tn) "#\\x10"       (obj->literal #\x10))  ;; 16
(assert-equal? (tn) "#\\x11"       (obj->literal #\x11))  ;; 17
(assert-equal? (tn) "#\\x12"       (obj->literal #\x12))  ;; 18
(assert-equal? (tn) "#\\x13"       (obj->literal #\x13))  ;; 19
(assert-equal? (tn) "#\\x14"       (obj->literal #\x14))  ;; 20
(assert-equal? (tn) "#\\x15"       (obj->literal #\x15))  ;; 21
(assert-equal? (tn) "#\\x16"       (obj->literal #\x16))  ;; 22
(assert-equal? (tn) "#\\x17"       (obj->literal #\x17))  ;; 23
(assert-equal? (tn) "#\\x18"       (obj->literal #\x18))  ;; 24
(assert-equal? (tn) "#\\x19"       (obj->literal #\x19))  ;; 25
(assert-equal? (tn) "#\\x1a"       (obj->literal #\x1a))  ;; 26
(assert-equal? (tn) "#\\esc"       (obj->literal #\x1b))  ;; 27
(assert-equal? (tn) "#\\x1c"       (obj->literal #\x1c))  ;; 28
(assert-equal? (tn) "#\\x1d"       (obj->literal #\x1d))  ;; 29
(assert-equal? (tn) "#\\x1e"       (obj->literal #\x1e))  ;; 30
(assert-equal? (tn) "#\\x1f"       (obj->literal #\x1f))  ;; 31
(assert-equal? (tn) "#\\space"     (obj->literal #\x20))  ;; 32
(assert-equal? (tn) "#\\!"         (obj->literal #\x21))  ;; 33
(assert-equal? (tn) "#\\\""        (obj->literal #\x22))  ;; 34
(assert-equal? (tn) "#\\#"         (obj->literal #\x23))  ;; 35
(assert-equal? (tn) "#\\$"         (obj->literal #\x24))  ;; 36
(assert-equal? (tn) "#\\%"         (obj->literal #\x25))  ;; 37
(assert-equal? (tn) "#\\&"         (obj->literal #\x26))  ;; 38
(assert-equal? (tn) "#\\'"         (obj->literal #\x27))  ;; 39
(assert-equal? (tn) "#\\("         (obj->literal #\x28))  ;; 40
(assert-equal? (tn) "#\\)"         (obj->literal #\x29))  ;; 41
(assert-equal? (tn) "#\\*"         (obj->literal #\x2a))  ;; 42
(assert-equal? (tn) "#\\+"         (obj->literal #\x2b))  ;; 43
(assert-equal? (tn) "#\\,"         (obj->literal #\x2c))  ;; 44
(assert-equal? (tn) "#\\-"         (obj->literal #\x2d))  ;; 45
(assert-equal? (tn) "#\\."         (obj->literal #\x2e))  ;; 46
(assert-equal? (tn) "#\\/"         (obj->literal #\x2f))  ;; 47
(assert-equal? (tn) "#\\0"         (obj->literal #\x30))  ;; 48
(assert-equal? (tn) "#\\1"         (obj->literal #\x31))  ;; 49
(assert-equal? (tn) "#\\2"         (obj->literal #\x32))  ;; 50
(assert-equal? (tn) "#\\3"         (obj->literal #\x33))  ;; 51
(assert-equal? (tn) "#\\4"         (obj->literal #\x34))  ;; 52
(assert-equal? (tn) "#\\5"         (obj->literal #\x35))  ;; 53
(assert-equal? (tn) "#\\6"         (obj->literal #\x36))  ;; 54
(assert-equal? (tn) "#\\7"         (obj->literal #\x37))  ;; 55
(assert-equal? (tn) "#\\8"         (obj->literal #\x38))  ;; 56
(assert-equal? (tn) "#\\9"         (obj->literal #\x39))  ;; 57
(assert-equal? (tn) "#\\:"         (obj->literal #\x3a))  ;; 58
(assert-equal? (tn) "#\\;"         (obj->literal #\x3b))  ;; 59
(assert-equal? (tn) "#\\<"         (obj->literal #\x3c))  ;; 60
(assert-equal? (tn) "#\\="         (obj->literal #\x3d))  ;; 61
(assert-equal? (tn) "#\\>"         (obj->literal #\x3e))  ;; 62
(assert-equal? (tn) "#\\?"         (obj->literal #\x3f))  ;; 63
(assert-equal? (tn) "#\\@"         (obj->literal #\x40))  ;; 64
(assert-equal? (tn) "#\\A"         (obj->literal #\x41))  ;; 65
(assert-equal? (tn) "#\\B"         (obj->literal #\x42))  ;; 66
(assert-equal? (tn) "#\\C"         (obj->literal #\x43))  ;; 67
(assert-equal? (tn) "#\\D"         (obj->literal #\x44))  ;; 68
(assert-equal? (tn) "#\\E"         (obj->literal #\x45))  ;; 69
(assert-equal? (tn) "#\\F"         (obj->literal #\x46))  ;; 70
(assert-equal? (tn) "#\\G"         (obj->literal #\x47))  ;; 71
(assert-equal? (tn) "#\\H"         (obj->literal #\x48))  ;; 72
(assert-equal? (tn) "#\\I"         (obj->literal #\x49))  ;; 73
(assert-equal? (tn) "#\\J"         (obj->literal #\x4a))  ;; 74
(assert-equal? (tn) "#\\K"         (obj->literal #\x4b))  ;; 75
(assert-equal? (tn) "#\\L"         (obj->literal #\x4c))  ;; 76
(assert-equal? (tn) "#\\M"         (obj->literal #\x4d))  ;; 77
(assert-equal? (tn) "#\\N"         (obj->literal #\x4e))  ;; 78
(assert-equal? (tn) "#\\O"         (obj->literal #\x4f))  ;; 79
(assert-equal? (tn) "#\\P"         (obj->literal #\x50))  ;; 80
(assert-equal? (tn) "#\\Q"         (obj->literal #\x51))  ;; 81
(assert-equal? (tn) "#\\R"         (obj->literal #\x52))  ;; 82
(assert-equal? (tn) "#\\S"         (obj->literal #\x53))  ;; 83
(assert-equal? (tn) "#\\T"         (obj->literal #\x54))  ;; 84
(assert-equal? (tn) "#\\U"         (obj->literal #\x55))  ;; 85
(assert-equal? (tn) "#\\V"         (obj->literal #\x56))  ;; 86
(assert-equal? (tn) "#\\W"         (obj->literal #\x57))  ;; 87
(assert-equal? (tn) "#\\X"         (obj->literal #\x58))  ;; 88
(assert-equal? (tn) "#\\Y"         (obj->literal #\x59))  ;; 89
(assert-equal? (tn) "#\\Z"         (obj->literal #\x5a))  ;; 90
(assert-equal? (tn) "#\\["         (obj->literal #\x5b))  ;; 91
(assert-equal? (tn) "#\\\\"        (obj->literal #\x5c))  ;; 92
(assert-equal? (tn) "#\\]"         (obj->literal #\x5d))  ;; 93
(assert-equal? (tn) "#\\^"         (obj->literal #\x5e))  ;; 94
(assert-equal? (tn) "#\\_"         (obj->literal #\x5f))  ;; 95
(assert-equal? (tn) "#\\`"         (obj->literal #\x60))  ;; 96
(assert-equal? (tn) "#\\a"         (obj->literal #\x61))  ;; 97
(assert-equal? (tn) "#\\b"         (obj->literal #\x62))  ;; 98
(assert-equal? (tn) "#\\c"         (obj->literal #\x63))  ;; 99
(assert-equal? (tn) "#\\d"         (obj->literal #\x64))  ;; 100
(assert-equal? (tn) "#\\e"         (obj->literal #\x65))  ;; 101
(assert-equal? (tn) "#\\f"         (obj->literal #\x66))  ;; 102
(assert-equal? (tn) "#\\g"         (obj->literal #\x67))  ;; 103
(assert-equal? (tn) "#\\h"         (obj->literal #\x68))  ;; 104
(assert-equal? (tn) "#\\i"         (obj->literal #\x69))  ;; 105
(assert-equal? (tn) "#\\j"         (obj->literal #\x6a))  ;; 106
(assert-equal? (tn) "#\\k"         (obj->literal #\x6b))  ;; 107
(assert-equal? (tn) "#\\l"         (obj->literal #\x6c))  ;; 108
(assert-equal? (tn) "#\\m"         (obj->literal #\x6d))  ;; 109
(assert-equal? (tn) "#\\n"         (obj->literal #\x6e))  ;; 110
(assert-equal? (tn) "#\\o"         (obj->literal #\x6f))  ;; 111
(assert-equal? (tn) "#\\p"         (obj->literal #\x70))  ;; 112
(assert-equal? (tn) "#\\q"         (obj->literal #\x71))  ;; 113
(assert-equal? (tn) "#\\r"         (obj->literal #\x72))  ;; 114
(assert-equal? (tn) "#\\s"         (obj->literal #\x73))  ;; 115
(assert-equal? (tn) "#\\t"         (obj->literal #\x74))  ;; 116
(assert-equal? (tn) "#\\u"         (obj->literal #\x75))  ;; 117
(assert-equal? (tn) "#\\v"         (obj->literal #\x76))  ;; 118
(assert-equal? (tn) "#\\w"         (obj->literal #\x77))  ;; 119
(assert-equal? (tn) "#\\x"         (obj->literal #\x78))  ;; 120
(assert-equal? (tn) "#\\y"         (obj->literal #\x79))  ;; 121
(assert-equal? (tn) "#\\z"         (obj->literal #\x7a))  ;; 122
(assert-equal? (tn) "#\\{"         (obj->literal #\x7b))  ;; 123
(assert-equal? (tn) "#\\|"         (obj->literal #\x7c))  ;; 124
(assert-equal? (tn) "#\\}"         (obj->literal #\x7d))  ;; 125
(assert-equal? (tn) "#\\~"         (obj->literal #\x7e))  ;; 126
(assert-equal? (tn) "#\\delete"    (obj->literal #\x7f))  ;; 127

(tn "R6RS hexadecimal char literal (compared with integer-originated char)")
(assert-equal? (tn) (integer->char   0) #\x00)  ;; 0
(assert-equal? (tn) (integer->char   1) #\x01)  ;; 1
(assert-equal? (tn) (integer->char   2) #\x02)  ;; 2
(assert-equal? (tn) (integer->char   3) #\x03)  ;; 3
(assert-equal? (tn) (integer->char   4) #\x04)  ;; 4
(assert-equal? (tn) (integer->char   5) #\x05)  ;; 5
(assert-equal? (tn) (integer->char   6) #\x06)  ;; 6
(assert-equal? (tn) (integer->char   7) #\x07)  ;; 7
(assert-equal? (tn) (integer->char   8) #\x08)  ;; 8
(assert-equal? (tn) (integer->char   9) #\x09)  ;; 9
(assert-equal? (tn) (integer->char  10) #\x0a)  ;; 10
(assert-equal? (tn) (integer->char  11) #\x0b)  ;; 11
(assert-equal? (tn) (integer->char  12) #\x0c)  ;; 12
(assert-equal? (tn) (integer->char  13) #\x0d)  ;; 13
(assert-equal? (tn) (integer->char  14) #\x0e)  ;; 14
(assert-equal? (tn) (integer->char  15) #\x0f)  ;; 15
(assert-equal? (tn) (integer->char  16) #\x10)  ;; 16
(assert-equal? (tn) (integer->char  17) #\x11)  ;; 17
(assert-equal? (tn) (integer->char  18) #\x12)  ;; 18
(assert-equal? (tn) (integer->char  19) #\x13)  ;; 19
(assert-equal? (tn) (integer->char  20) #\x14)  ;; 20
(assert-equal? (tn) (integer->char  21) #\x15)  ;; 21
(assert-equal? (tn) (integer->char  22) #\x16)  ;; 22
(assert-equal? (tn) (integer->char  23) #\x17)  ;; 23
(assert-equal? (tn) (integer->char  24) #\x18)  ;; 24
(assert-equal? (tn) (integer->char  25) #\x19)  ;; 25
(assert-equal? (tn) (integer->char  26) #\x1a)  ;; 26
(assert-equal? (tn) (integer->char  27) #\x1b)  ;; 27
(assert-equal? (tn) (integer->char  28) #\x1c)  ;; 28
(assert-equal? (tn) (integer->char  29) #\x1d)  ;; 29
(assert-equal? (tn) (integer->char  30) #\x1e)  ;; 30
(assert-equal? (tn) (integer->char  31) #\x1f)  ;; 31
(assert-equal? (tn) (integer->char  32) #\x20)  ;; 32
(assert-equal? (tn) (integer->char  33) #\x21)  ;; 33
(assert-equal? (tn) (integer->char  34) #\x22)  ;; 34
(assert-equal? (tn) (integer->char  35) #\x23)  ;; 35
(assert-equal? (tn) (integer->char  36) #\x24)  ;; 36
(assert-equal? (tn) (integer->char  37) #\x25)  ;; 37
(assert-equal? (tn) (integer->char  38) #\x26)  ;; 38
(assert-equal? (tn) (integer->char  39) #\x27)  ;; 39
(assert-equal? (tn) (integer->char  40) #\x28)  ;; 40
(assert-equal? (tn) (integer->char  41) #\x29)  ;; 41
(assert-equal? (tn) (integer->char  42) #\x2a)  ;; 42
(assert-equal? (tn) (integer->char  43) #\x2b)  ;; 43
(assert-equal? (tn) (integer->char  44) #\x2c)  ;; 44
(assert-equal? (tn) (integer->char  45) #\x2d)  ;; 45
(assert-equal? (tn) (integer->char  46) #\x2e)  ;; 46
(assert-equal? (tn) (integer->char  47) #\x2f)  ;; 47
(assert-equal? (tn) (integer->char  48) #\x30)  ;; 48
(assert-equal? (tn) (integer->char  49) #\x31)  ;; 49
(assert-equal? (tn) (integer->char  50) #\x32)  ;; 50
(assert-equal? (tn) (integer->char  51) #\x33)  ;; 51
(assert-equal? (tn) (integer->char  52) #\x34)  ;; 52
(assert-equal? (tn) (integer->char  53) #\x35)  ;; 53
(assert-equal? (tn) (integer->char  54) #\x36)  ;; 54
(assert-equal? (tn) (integer->char  55) #\x37)  ;; 55
(assert-equal? (tn) (integer->char  56) #\x38)  ;; 56
(assert-equal? (tn) (integer->char  57) #\x39)  ;; 57
(assert-equal? (tn) (integer->char  58) #\x3a)  ;; 58
(assert-equal? (tn) (integer->char  59) #\x3b)  ;; 59
(assert-equal? (tn) (integer->char  60) #\x3c)  ;; 60
(assert-equal? (tn) (integer->char  61) #\x3d)  ;; 61
(assert-equal? (tn) (integer->char  62) #\x3e)  ;; 62
(assert-equal? (tn) (integer->char  63) #\x3f)  ;; 63
(assert-equal? (tn) (integer->char  64) #\x40)  ;; 64
(assert-equal? (tn) (integer->char  65) #\x41)  ;; 65
(assert-equal? (tn) (integer->char  66) #\x42)  ;; 66
(assert-equal? (tn) (integer->char  67) #\x43)  ;; 67
(assert-equal? (tn) (integer->char  68) #\x44)  ;; 68
(assert-equal? (tn) (integer->char  69) #\x45)  ;; 69
(assert-equal? (tn) (integer->char  70) #\x46)  ;; 70
(assert-equal? (tn) (integer->char  71) #\x47)  ;; 71
(assert-equal? (tn) (integer->char  72) #\x48)  ;; 72
(assert-equal? (tn) (integer->char  73) #\x49)  ;; 73
(assert-equal? (tn) (integer->char  74) #\x4a)  ;; 74
(assert-equal? (tn) (integer->char  75) #\x4b)  ;; 75
(assert-equal? (tn) (integer->char  76) #\x4c)  ;; 76
(assert-equal? (tn) (integer->char  77) #\x4d)  ;; 77
(assert-equal? (tn) (integer->char  78) #\x4e)  ;; 78
(assert-equal? (tn) (integer->char  79) #\x4f)  ;; 79
(assert-equal? (tn) (integer->char  80) #\x50)  ;; 80
(assert-equal? (tn) (integer->char  81) #\x51)  ;; 81
(assert-equal? (tn) (integer->char  82) #\x52)  ;; 82
(assert-equal? (tn) (integer->char  83) #\x53)  ;; 83
(assert-equal? (tn) (integer->char  84) #\x54)  ;; 84
(assert-equal? (tn) (integer->char  85) #\x55)  ;; 85
(assert-equal? (tn) (integer->char  86) #\x56)  ;; 86
(assert-equal? (tn) (integer->char  87) #\x57)  ;; 87
(assert-equal? (tn) (integer->char  88) #\x58)  ;; 88
(assert-equal? (tn) (integer->char  89) #\x59)  ;; 89
(assert-equal? (tn) (integer->char  90) #\x5a)  ;; 90
(assert-equal? (tn) (integer->char  91) #\x5b)  ;; 91
(assert-equal? (tn) (integer->char  92) #\x5c)  ;; 92
(assert-equal? (tn) (integer->char  93) #\x5d)  ;; 93
(assert-equal? (tn) (integer->char  94) #\x5e)  ;; 94
(assert-equal? (tn) (integer->char  95) #\x5f)  ;; 95
(assert-equal? (tn) (integer->char  96) #\x60)  ;; 96
(assert-equal? (tn) (integer->char  97) #\x61)  ;; 97
(assert-equal? (tn) (integer->char  98) #\x62)  ;; 98
(assert-equal? (tn) (integer->char  99) #\x63)  ;; 99
(assert-equal? (tn) (integer->char 100) #\x64)  ;; 100
(assert-equal? (tn) (integer->char 101) #\x65)  ;; 101
(assert-equal? (tn) (integer->char 102) #\x66)  ;; 102
(assert-equal? (tn) (integer->char 103) #\x67)  ;; 103
(assert-equal? (tn) (integer->char 104) #\x68)  ;; 104
(assert-equal? (tn) (integer->char 105) #\x69)  ;; 105
(assert-equal? (tn) (integer->char 106) #\x6a)  ;; 106
(assert-equal? (tn) (integer->char 107) #\x6b)  ;; 107
(assert-equal? (tn) (integer->char 108) #\x6c)  ;; 108
(assert-equal? (tn) (integer->char 109) #\x6d)  ;; 109
(assert-equal? (tn) (integer->char 110) #\x6e)  ;; 110
(assert-equal? (tn) (integer->char 111) #\x6f)  ;; 111
(assert-equal? (tn) (integer->char 112) #\x70)  ;; 112
(assert-equal? (tn) (integer->char 113) #\x71)  ;; 113
(assert-equal? (tn) (integer->char 114) #\x72)  ;; 114
(assert-equal? (tn) (integer->char 115) #\x73)  ;; 115
(assert-equal? (tn) (integer->char 116) #\x74)  ;; 116
(assert-equal? (tn) (integer->char 117) #\x75)  ;; 117
(assert-equal? (tn) (integer->char 118) #\x76)  ;; 118
(assert-equal? (tn) (integer->char 119) #\x77)  ;; 119
(assert-equal? (tn) (integer->char 120) #\x78)  ;; 120
(assert-equal? (tn) (integer->char 121) #\x79)  ;; 121
(assert-equal? (tn) (integer->char 122) #\x7a)  ;; 122
(assert-equal? (tn) (integer->char 123) #\x7b)  ;; 123
(assert-equal? (tn) (integer->char 124) #\x7c)  ;; 124
(assert-equal? (tn) (integer->char 125) #\x7d)  ;; 125
(assert-equal? (tn) (integer->char 126) #\x7e)  ;; 126
(assert-equal? (tn) (integer->char 127) #\x7f)  ;; 127

(tn "R6RS hexadecimal char literal (capitalized hexadecimal)")
(assert-equal? (tn) (integer->char   0) #\x00)  ;; 0
(assert-equal? (tn) (integer->char   1) #\x01)  ;; 1
(assert-equal? (tn) (integer->char   2) #\x02)  ;; 2
(assert-equal? (tn) (integer->char   3) #\x03)  ;; 3
(assert-equal? (tn) (integer->char   4) #\x04)  ;; 4
(assert-equal? (tn) (integer->char   5) #\x05)  ;; 5
(assert-equal? (tn) (integer->char   6) #\x06)  ;; 6
(assert-equal? (tn) (integer->char   7) #\x07)  ;; 7
(assert-equal? (tn) (integer->char   8) #\x08)  ;; 8
(assert-equal? (tn) (integer->char   9) #\x09)  ;; 9
(assert-equal? (tn) (integer->char  10) #\x0A)  ;; 10
(assert-equal? (tn) (integer->char  11) #\x0B)  ;; 11
(assert-equal? (tn) (integer->char  12) #\x0C)  ;; 12
(assert-equal? (tn) (integer->char  13) #\x0D)  ;; 13
(assert-equal? (tn) (integer->char  14) #\x0E)  ;; 14
(assert-equal? (tn) (integer->char  15) #\x0F)  ;; 15
(assert-equal? (tn) (integer->char  16) #\x10)  ;; 16
(assert-equal? (tn) (integer->char  17) #\x11)  ;; 17
(assert-equal? (tn) (integer->char  18) #\x12)  ;; 18
(assert-equal? (tn) (integer->char  19) #\x13)  ;; 19
(assert-equal? (tn) (integer->char  20) #\x14)  ;; 20
(assert-equal? (tn) (integer->char  21) #\x15)  ;; 21
(assert-equal? (tn) (integer->char  22) #\x16)  ;; 22
(assert-equal? (tn) (integer->char  23) #\x17)  ;; 23
(assert-equal? (tn) (integer->char  24) #\x18)  ;; 24
(assert-equal? (tn) (integer->char  25) #\x19)  ;; 25
(assert-equal? (tn) (integer->char  26) #\x1A)  ;; 26
(assert-equal? (tn) (integer->char  27) #\x1B)  ;; 27
(assert-equal? (tn) (integer->char  28) #\x1C)  ;; 28
(assert-equal? (tn) (integer->char  29) #\x1D)  ;; 29
(assert-equal? (tn) (integer->char  30) #\x1E)  ;; 30
(assert-equal? (tn) (integer->char  31) #\x1F)  ;; 31
(assert-equal? (tn) (integer->char  32) #\x20)  ;; 32
(assert-equal? (tn) (integer->char  33) #\x21)  ;; 33
(assert-equal? (tn) (integer->char  34) #\x22)  ;; 34
(assert-equal? (tn) (integer->char  35) #\x23)  ;; 35
(assert-equal? (tn) (integer->char  36) #\x24)  ;; 36
(assert-equal? (tn) (integer->char  37) #\x25)  ;; 37
(assert-equal? (tn) (integer->char  38) #\x26)  ;; 38
(assert-equal? (tn) (integer->char  39) #\x27)  ;; 39
(assert-equal? (tn) (integer->char  40) #\x28)  ;; 40
(assert-equal? (tn) (integer->char  41) #\x29)  ;; 41
(assert-equal? (tn) (integer->char  42) #\x2A)  ;; 42
(assert-equal? (tn) (integer->char  43) #\x2B)  ;; 43
(assert-equal? (tn) (integer->char  44) #\x2C)  ;; 44
(assert-equal? (tn) (integer->char  45) #\x2D)  ;; 45
(assert-equal? (tn) (integer->char  46) #\x2E)  ;; 46
(assert-equal? (tn) (integer->char  47) #\x2F)  ;; 47
(assert-equal? (tn) (integer->char  48) #\x30)  ;; 48
(assert-equal? (tn) (integer->char  49) #\x31)  ;; 49
(assert-equal? (tn) (integer->char  50) #\x32)  ;; 50
(assert-equal? (tn) (integer->char  51) #\x33)  ;; 51
(assert-equal? (tn) (integer->char  52) #\x34)  ;; 52
(assert-equal? (tn) (integer->char  53) #\x35)  ;; 53
(assert-equal? (tn) (integer->char  54) #\x36)  ;; 54
(assert-equal? (tn) (integer->char  55) #\x37)  ;; 55
(assert-equal? (tn) (integer->char  56) #\x38)  ;; 56
(assert-equal? (tn) (integer->char  57) #\x39)  ;; 57
(assert-equal? (tn) (integer->char  58) #\x3A)  ;; 58
(assert-equal? (tn) (integer->char  59) #\x3B)  ;; 59
(assert-equal? (tn) (integer->char  60) #\x3C)  ;; 60
(assert-equal? (tn) (integer->char  61) #\x3D)  ;; 61
(assert-equal? (tn) (integer->char  62) #\x3E)  ;; 62
(assert-equal? (tn) (integer->char  63) #\x3F)  ;; 63
(assert-equal? (tn) (integer->char  64) #\x40)  ;; 64
(assert-equal? (tn) (integer->char  65) #\x41)  ;; 65
(assert-equal? (tn) (integer->char  66) #\x42)  ;; 66
(assert-equal? (tn) (integer->char  67) #\x43)  ;; 67
(assert-equal? (tn) (integer->char  68) #\x44)  ;; 68
(assert-equal? (tn) (integer->char  69) #\x45)  ;; 69
(assert-equal? (tn) (integer->char  70) #\x46)  ;; 70
(assert-equal? (tn) (integer->char  71) #\x47)  ;; 71
(assert-equal? (tn) (integer->char  72) #\x48)  ;; 72
(assert-equal? (tn) (integer->char  73) #\x49)  ;; 73
(assert-equal? (tn) (integer->char  74) #\x4A)  ;; 74
(assert-equal? (tn) (integer->char  75) #\x4B)  ;; 75
(assert-equal? (tn) (integer->char  76) #\x4C)  ;; 76
(assert-equal? (tn) (integer->char  77) #\x4D)  ;; 77
(assert-equal? (tn) (integer->char  78) #\x4E)  ;; 78
(assert-equal? (tn) (integer->char  79) #\x4F)  ;; 79
(assert-equal? (tn) (integer->char  80) #\x50)  ;; 80
(assert-equal? (tn) (integer->char  81) #\x51)  ;; 81
(assert-equal? (tn) (integer->char  82) #\x52)  ;; 82
(assert-equal? (tn) (integer->char  83) #\x53)  ;; 83
(assert-equal? (tn) (integer->char  84) #\x54)  ;; 84
(assert-equal? (tn) (integer->char  85) #\x55)  ;; 85
(assert-equal? (tn) (integer->char  86) #\x56)  ;; 86
(assert-equal? (tn) (integer->char  87) #\x57)  ;; 87
(assert-equal? (tn) (integer->char  88) #\x58)  ;; 88
(assert-equal? (tn) (integer->char  89) #\x59)  ;; 89
(assert-equal? (tn) (integer->char  90) #\x5A)  ;; 90
(assert-equal? (tn) (integer->char  91) #\x5B)  ;; 91
(assert-equal? (tn) (integer->char  92) #\x5C)  ;; 92
(assert-equal? (tn) (integer->char  93) #\x5D)  ;; 93
(assert-equal? (tn) (integer->char  94) #\x5E)  ;; 94
(assert-equal? (tn) (integer->char  95) #\x5F)  ;; 95
(assert-equal? (tn) (integer->char  96) #\x60)  ;; 96
(assert-equal? (tn) (integer->char  97) #\x61)  ;; 97
(assert-equal? (tn) (integer->char  98) #\x62)  ;; 98
(assert-equal? (tn) (integer->char  99) #\x63)  ;; 99
(assert-equal? (tn) (integer->char 100) #\x64)  ;; 100
(assert-equal? (tn) (integer->char 101) #\x65)  ;; 101
(assert-equal? (tn) (integer->char 102) #\x66)  ;; 102
(assert-equal? (tn) (integer->char 103) #\x67)  ;; 103
(assert-equal? (tn) (integer->char 104) #\x68)  ;; 104
(assert-equal? (tn) (integer->char 105) #\x69)  ;; 105
(assert-equal? (tn) (integer->char 106) #\x6A)  ;; 106
(assert-equal? (tn) (integer->char 107) #\x6B)  ;; 107
(assert-equal? (tn) (integer->char 108) #\x6C)  ;; 108
(assert-equal? (tn) (integer->char 109) #\x6D)  ;; 109
(assert-equal? (tn) (integer->char 110) #\x6E)  ;; 110
(assert-equal? (tn) (integer->char 111) #\x6F)  ;; 111
(assert-equal? (tn) (integer->char 112) #\x70)  ;; 112
(assert-equal? (tn) (integer->char 113) #\x71)  ;; 113
(assert-equal? (tn) (integer->char 114) #\x72)  ;; 114
(assert-equal? (tn) (integer->char 115) #\x73)  ;; 115
(assert-equal? (tn) (integer->char 116) #\x74)  ;; 116
(assert-equal? (tn) (integer->char 117) #\x75)  ;; 117
(assert-equal? (tn) (integer->char 118) #\x76)  ;; 118
(assert-equal? (tn) (integer->char 119) #\x77)  ;; 119
(assert-equal? (tn) (integer->char 120) #\x78)  ;; 120
(assert-equal? (tn) (integer->char 121) #\x79)  ;; 121
(assert-equal? (tn) (integer->char 122) #\x7A)  ;; 122
(assert-equal? (tn) (integer->char 123) #\x7B)  ;; 123
(assert-equal? (tn) (integer->char 124) #\x7C)  ;; 124
(assert-equal? (tn) (integer->char 125) #\x7D)  ;; 125
(assert-equal? (tn) (integer->char 126) #\x7E)  ;; 126
(assert-equal? (tn) (integer->char 127) #\x7F)  ;; 127

;; integer->char
;; NOTE: #\x0e -style character is defined in R6RS(SRFI-75)
(tn "integer->char")
(assert-equal? (tn) #\nul       (integer->char 0))    ;; 0
(assert-equal? (tn) #\x01       (integer->char 1))    ;; 1
(assert-equal? (tn) #\x02       (integer->char 2))    ;; 2
(assert-equal? (tn) #\x03       (integer->char 3))    ;; 3
(assert-equal? (tn) #\x04       (integer->char 4))    ;; 4
(assert-equal? (tn) #\x05       (integer->char 5))    ;; 5
(assert-equal? (tn) #\x06       (integer->char 6))    ;; 6
(assert-equal? (tn) #\alarm     (integer->char 7))    ;; 7
(assert-equal? (tn) #\backspace (integer->char 8))    ;; 8
(assert-equal? (tn) #\tab       (integer->char 9))    ;; 9
(assert-equal? (tn) #\newline   (integer->char 10))   ;; 10
(assert-equal? (tn) #\vtab      (integer->char 11))   ;; 11
(assert-equal? (tn) #\page      (integer->char 12))   ;; 12
(assert-equal? (tn) #\return    (integer->char 13))   ;; 13
(assert-equal? (tn) #\x0e       (integer->char 14))   ;; 14
(assert-equal? (tn) #\x0f       (integer->char 15))   ;; 15
(assert-equal? (tn) #\x10       (integer->char 16))   ;; 16
(assert-equal? (tn) #\x11       (integer->char 17))   ;; 17
(assert-equal? (tn) #\x12       (integer->char 18))   ;; 18
(assert-equal? (tn) #\x13       (integer->char 19))   ;; 19
(assert-equal? (tn) #\x14       (integer->char 20))   ;; 20
(assert-equal? (tn) #\x15       (integer->char 21))   ;; 21
(assert-equal? (tn) #\x16       (integer->char 22))   ;; 22
(assert-equal? (tn) #\x17       (integer->char 23))   ;; 23
(assert-equal? (tn) #\x18       (integer->char 24))   ;; 24
(assert-equal? (tn) #\x19       (integer->char 25))   ;; 25
(assert-equal? (tn) #\x1a       (integer->char 26))   ;; 26
(assert-equal? (tn) #\esc       (integer->char 27))   ;; 27
(assert-equal? (tn) #\x1c       (integer->char 28))   ;; 28
(assert-equal? (tn) #\x1d       (integer->char 29))   ;; 29
(assert-equal? (tn) #\x1e       (integer->char 30))   ;; 30
(assert-equal? (tn) #\x1f       (integer->char 31))   ;; 31
(assert-equal? (tn) #\space     (integer->char 32))   ;; 32
(assert-equal? (tn) #\!         (integer->char 33))   ;; 33
(assert-equal? (tn) #\"         (integer->char 34))   ;; 34
(assert-equal? (tn) #\#         (integer->char 35))   ;; 35
(assert-equal? (tn) #\$         (integer->char 36))   ;; 36
(assert-equal? (tn) #\%         (integer->char 37))   ;; 37
(assert-equal? (tn) #\&         (integer->char 38))   ;; 38
(assert-equal? (tn) #\'         (integer->char 39))   ;; 39
(assert-equal? (tn) #\(         (integer->char 40))   ;; 40
(assert-equal? (tn) #\)         (integer->char 41))   ;; 41
(assert-equal? (tn) #\*         (integer->char 42))   ;; 42
(assert-equal? (tn) #\+         (integer->char 43))   ;; 43
(assert-equal? (tn) #\,         (integer->char 44))   ;; 44
(assert-equal? (tn) #\-         (integer->char 45))   ;; 45
(assert-equal? (tn) #\.         (integer->char 46))   ;; 46
(assert-equal? (tn) #\/         (integer->char 47))   ;; 47
(assert-equal? (tn) #\0         (integer->char 48))   ;; 48
(assert-equal? (tn) #\1         (integer->char 49))   ;; 49
(assert-equal? (tn) #\2         (integer->char 50))   ;; 50
(assert-equal? (tn) #\3         (integer->char 51))   ;; 51
(assert-equal? (tn) #\4         (integer->char 52))   ;; 52
(assert-equal? (tn) #\5         (integer->char 53))   ;; 53
(assert-equal? (tn) #\6         (integer->char 54))   ;; 54
(assert-equal? (tn) #\7         (integer->char 55))   ;; 55
(assert-equal? (tn) #\8         (integer->char 56))   ;; 56
(assert-equal? (tn) #\9         (integer->char 57))   ;; 57
(assert-equal? (tn) #\:         (integer->char 58))   ;; 58
(assert-equal? (tn) #\;         (integer->char 59))   ;; 59
(assert-equal? (tn) #\<         (integer->char 60))   ;; 60
(assert-equal? (tn) #\=         (integer->char 61))   ;; 61
(assert-equal? (tn) #\>         (integer->char 62))   ;; 62
(assert-equal? (tn) #\?         (integer->char 63))   ;; 63
(assert-equal? (tn) #\@         (integer->char 64))   ;; 64
(assert-equal? (tn) #\A         (integer->char 65))   ;; 65
(assert-equal? (tn) #\B         (integer->char 66))   ;; 66
(assert-equal? (tn) #\C         (integer->char 67))   ;; 67
(assert-equal? (tn) #\D         (integer->char 68))   ;; 68
(assert-equal? (tn) #\E         (integer->char 69))   ;; 69
(assert-equal? (tn) #\F         (integer->char 70))   ;; 70
(assert-equal? (tn) #\G         (integer->char 71))   ;; 71
(assert-equal? (tn) #\H         (integer->char 72))   ;; 72
(assert-equal? (tn) #\I         (integer->char 73))   ;; 73
(assert-equal? (tn) #\J         (integer->char 74))   ;; 74
(assert-equal? (tn) #\K         (integer->char 75))   ;; 75
(assert-equal? (tn) #\L         (integer->char 76))   ;; 76
(assert-equal? (tn) #\M         (integer->char 77))   ;; 77
(assert-equal? (tn) #\N         (integer->char 78))   ;; 78
(assert-equal? (tn) #\O         (integer->char 79))   ;; 79
(assert-equal? (tn) #\P         (integer->char 80))   ;; 80
(assert-equal? (tn) #\Q         (integer->char 81))   ;; 81
(assert-equal? (tn) #\R         (integer->char 82))   ;; 82
(assert-equal? (tn) #\S         (integer->char 83))   ;; 83
(assert-equal? (tn) #\T         (integer->char 84))   ;; 84
(assert-equal? (tn) #\U         (integer->char 85))   ;; 85
(assert-equal? (tn) #\V         (integer->char 86))   ;; 86
(assert-equal? (tn) #\W         (integer->char 87))   ;; 87
(assert-equal? (tn) #\X         (integer->char 88))   ;; 88
(assert-equal? (tn) #\Y         (integer->char 89))   ;; 89
(assert-equal? (tn) #\Z         (integer->char 90))   ;; 90
(assert-equal? (tn) #\[         (integer->char 91))   ;; 91
(assert-equal? (tn) #\\         (integer->char 92))   ;; 92
(assert-equal? (tn) #\]         (integer->char 93))   ;; 93
(assert-equal? (tn) #\^         (integer->char 94))   ;; 94
(assert-equal? (tn) #\_         (integer->char 95))   ;; 95
(assert-equal? (tn) #\`         (integer->char 96))   ;; 96
(assert-equal? (tn) #\a         (integer->char 97))   ;; 97
(assert-equal? (tn) #\b         (integer->char 98))   ;; 98
(assert-equal? (tn) #\c         (integer->char 99))   ;; 99
(assert-equal? (tn) #\d         (integer->char 100))  ;; 100
(assert-equal? (tn) #\e         (integer->char 101))  ;; 101
(assert-equal? (tn) #\f         (integer->char 102))  ;; 102
(assert-equal? (tn) #\g         (integer->char 103))  ;; 103
(assert-equal? (tn) #\h         (integer->char 104))  ;; 104
(assert-equal? (tn) #\i         (integer->char 105))  ;; 105
(assert-equal? (tn) #\j         (integer->char 106))  ;; 106
(assert-equal? (tn) #\k         (integer->char 107))  ;; 107
(assert-equal? (tn) #\l         (integer->char 108))  ;; 108
(assert-equal? (tn) #\m         (integer->char 109))  ;; 109
(assert-equal? (tn) #\n         (integer->char 110))  ;; 110
(assert-equal? (tn) #\o         (integer->char 111))  ;; 111
(assert-equal? (tn) #\p         (integer->char 112))  ;; 112
(assert-equal? (tn) #\q         (integer->char 113))  ;; 113
(assert-equal? (tn) #\r         (integer->char 114))  ;; 114
(assert-equal? (tn) #\s         (integer->char 115))  ;; 115
(assert-equal? (tn) #\t         (integer->char 116))  ;; 116
(assert-equal? (tn) #\u         (integer->char 117))  ;; 117
(assert-equal? (tn) #\v         (integer->char 118))  ;; 118
(assert-equal? (tn) #\w         (integer->char 119))  ;; 119
(assert-equal? (tn) #\x         (integer->char 120))  ;; 120
(assert-equal? (tn) #\y         (integer->char 121))  ;; 121
(assert-equal? (tn) #\z         (integer->char 122))  ;; 122
(assert-equal? (tn) #\{         (integer->char 123))  ;; 123
(assert-equal? (tn) #\|         (integer->char 124))  ;; 124
(assert-equal? (tn) #\}         (integer->char 125))  ;; 125
(assert-equal? (tn) #\~         (integer->char 126))  ;; 126
(assert-equal? (tn) #\delete    (integer->char 127))  ;; 127

(tn "integer->char (string form)")
(assert-equal? (tn) "#\\nul"       (i->chlit 0))    ;; 0
(assert-equal? (tn) "#\\x01"       (i->chlit 1))    ;; 1
(assert-equal? (tn) "#\\x02"       (i->chlit 2))    ;; 2
(assert-equal? (tn) "#\\x03"       (i->chlit 3))    ;; 3
(assert-equal? (tn) "#\\x04"       (i->chlit 4))    ;; 4
(assert-equal? (tn) "#\\x05"       (i->chlit 5))    ;; 5
(assert-equal? (tn) "#\\x06"       (i->chlit 6))    ;; 6
(assert-equal? (tn) "#\\alarm"     (i->chlit 7))    ;; 7
(assert-equal? (tn) "#\\backspace" (i->chlit 8))    ;; 8
(assert-equal? (tn) "#\\tab"       (i->chlit 9))    ;; 9
(assert-equal? (tn) "#\\newline"   (i->chlit 10))   ;; 10
(assert-equal? (tn) "#\\vtab"      (i->chlit 11))   ;; 11
(assert-equal? (tn) "#\\page"      (i->chlit 12))   ;; 12
(assert-equal? (tn) "#\\return"    (i->chlit 13))   ;; 13
(assert-equal? (tn) "#\\x0e"       (i->chlit 14))   ;; 14
(assert-equal? (tn) "#\\x0f"       (i->chlit 15))   ;; 15
(assert-equal? (tn) "#\\x10"       (i->chlit 16))   ;; 16
(assert-equal? (tn) "#\\x11"       (i->chlit 17))   ;; 17
(assert-equal? (tn) "#\\x12"       (i->chlit 18))   ;; 18
(assert-equal? (tn) "#\\x13"       (i->chlit 19))   ;; 19
(assert-equal? (tn) "#\\x14"       (i->chlit 20))   ;; 20
(assert-equal? (tn) "#\\x15"       (i->chlit 21))   ;; 21
(assert-equal? (tn) "#\\x16"       (i->chlit 22))   ;; 22
(assert-equal? (tn) "#\\x17"       (i->chlit 23))   ;; 23
(assert-equal? (tn) "#\\x18"       (i->chlit 24))   ;; 24
(assert-equal? (tn) "#\\x19"       (i->chlit 25))   ;; 25
(assert-equal? (tn) "#\\x1a"       (i->chlit 26))   ;; 26
(assert-equal? (tn) "#\\esc"       (i->chlit 27))   ;; 27
(assert-equal? (tn) "#\\x1c"       (i->chlit 28))   ;; 28
(assert-equal? (tn) "#\\x1d"       (i->chlit 29))   ;; 29
(assert-equal? (tn) "#\\x1e"       (i->chlit 30))   ;; 30
(assert-equal? (tn) "#\\x1f"       (i->chlit 31))   ;; 31
(assert-equal? (tn) "#\\space"     (i->chlit 32))   ;; 32
(assert-equal? (tn) "#\\!"         (i->chlit 33))   ;; 33
(assert-equal? (tn) "#\\\""        (i->chlit 34))   ;; 34
(assert-equal? (tn) "#\\#"         (i->chlit 35))   ;; 35
(assert-equal? (tn) "#\\$"         (i->chlit 36))   ;; 36
(assert-equal? (tn) "#\\%"         (i->chlit 37))   ;; 37
(assert-equal? (tn) "#\\&"         (i->chlit 38))   ;; 38
(assert-equal? (tn) "#\\'"         (i->chlit 39))   ;; 39
(assert-equal? (tn) "#\\("         (i->chlit 40))   ;; 40
(assert-equal? (tn) "#\\)"         (i->chlit 41))   ;; 41
(assert-equal? (tn) "#\\*"         (i->chlit 42))   ;; 42
(assert-equal? (tn) "#\\+"         (i->chlit 43))   ;; 43
(assert-equal? (tn) "#\\,"         (i->chlit 44))   ;; 44
(assert-equal? (tn) "#\\-"         (i->chlit 45))   ;; 45
(assert-equal? (tn) "#\\."         (i->chlit 46))   ;; 46
(assert-equal? (tn) "#\\/"         (i->chlit 47))   ;; 47
(assert-equal? (tn) "#\\0"         (i->chlit 48))   ;; 48
(assert-equal? (tn) "#\\1"         (i->chlit 49))   ;; 49
(assert-equal? (tn) "#\\2"         (i->chlit 50))   ;; 50
(assert-equal? (tn) "#\\3"         (i->chlit 51))   ;; 51
(assert-equal? (tn) "#\\4"         (i->chlit 52))   ;; 52
(assert-equal? (tn) "#\\5"         (i->chlit 53))   ;; 53
(assert-equal? (tn) "#\\6"         (i->chlit 54))   ;; 54
(assert-equal? (tn) "#\\7"         (i->chlit 55))   ;; 55
(assert-equal? (tn) "#\\8"         (i->chlit 56))   ;; 56
(assert-equal? (tn) "#\\9"         (i->chlit 57))   ;; 57
(assert-equal? (tn) "#\\:"         (i->chlit 58))   ;; 58
(assert-equal? (tn) "#\\;"         (i->chlit 59))   ;; 59
(assert-equal? (tn) "#\\<"         (i->chlit 60))   ;; 60
(assert-equal? (tn) "#\\="         (i->chlit 61))   ;; 61
(assert-equal? (tn) "#\\>"         (i->chlit 62))   ;; 62
(assert-equal? (tn) "#\\?"         (i->chlit 63))   ;; 63
(assert-equal? (tn) "#\\@"         (i->chlit 64))   ;; 64
(assert-equal? (tn) "#\\A"         (i->chlit 65))   ;; 65
(assert-equal? (tn) "#\\B"         (i->chlit 66))   ;; 66
(assert-equal? (tn) "#\\C"         (i->chlit 67))   ;; 67
(assert-equal? (tn) "#\\D"         (i->chlit 68))   ;; 68
(assert-equal? (tn) "#\\E"         (i->chlit 69))   ;; 69
(assert-equal? (tn) "#\\F"         (i->chlit 70))   ;; 70
(assert-equal? (tn) "#\\G"         (i->chlit 71))   ;; 71
(assert-equal? (tn) "#\\H"         (i->chlit 72))   ;; 72
(assert-equal? (tn) "#\\I"         (i->chlit 73))   ;; 73
(assert-equal? (tn) "#\\J"         (i->chlit 74))   ;; 74
(assert-equal? (tn) "#\\K"         (i->chlit 75))   ;; 75
(assert-equal? (tn) "#\\L"         (i->chlit 76))   ;; 76
(assert-equal? (tn) "#\\M"         (i->chlit 77))   ;; 77
(assert-equal? (tn) "#\\N"         (i->chlit 78))   ;; 78
(assert-equal? (tn) "#\\O"         (i->chlit 79))   ;; 79
(assert-equal? (tn) "#\\P"         (i->chlit 80))   ;; 80
(assert-equal? (tn) "#\\Q"         (i->chlit 81))   ;; 81
(assert-equal? (tn) "#\\R"         (i->chlit 82))   ;; 82
(assert-equal? (tn) "#\\S"         (i->chlit 83))   ;; 83
(assert-equal? (tn) "#\\T"         (i->chlit 84))   ;; 84
(assert-equal? (tn) "#\\U"         (i->chlit 85))   ;; 85
(assert-equal? (tn) "#\\V"         (i->chlit 86))   ;; 86
(assert-equal? (tn) "#\\W"         (i->chlit 87))   ;; 87
(assert-equal? (tn) "#\\X"         (i->chlit 88))   ;; 88
(assert-equal? (tn) "#\\Y"         (i->chlit 89))   ;; 89
(assert-equal? (tn) "#\\Z"         (i->chlit 90))   ;; 90
(assert-equal? (tn) "#\\["         (i->chlit 91))   ;; 91
(assert-equal? (tn) "#\\\\"        (i->chlit 92))   ;; 92
(assert-equal? (tn) "#\\]"         (i->chlit 93))   ;; 93
(assert-equal? (tn) "#\\^"         (i->chlit 94))   ;; 94
(assert-equal? (tn) "#\\_"         (i->chlit 95))   ;; 95
(assert-equal? (tn) "#\\`"         (i->chlit 96))   ;; 96
(assert-equal? (tn) "#\\a"         (i->chlit 97))   ;; 97
(assert-equal? (tn) "#\\b"         (i->chlit 98))   ;; 98
(assert-equal? (tn) "#\\c"         (i->chlit 99))   ;; 99
(assert-equal? (tn) "#\\d"         (i->chlit 100))  ;; 100
(assert-equal? (tn) "#\\e"         (i->chlit 101))  ;; 101
(assert-equal? (tn) "#\\f"         (i->chlit 102))  ;; 102
(assert-equal? (tn) "#\\g"         (i->chlit 103))  ;; 103
(assert-equal? (tn) "#\\h"         (i->chlit 104))  ;; 104
(assert-equal? (tn) "#\\i"         (i->chlit 105))  ;; 105
(assert-equal? (tn) "#\\j"         (i->chlit 106))  ;; 106
(assert-equal? (tn) "#\\k"         (i->chlit 107))  ;; 107
(assert-equal? (tn) "#\\l"         (i->chlit 108))  ;; 108
(assert-equal? (tn) "#\\m"         (i->chlit 109))  ;; 109
(assert-equal? (tn) "#\\n"         (i->chlit 110))  ;; 110
(assert-equal? (tn) "#\\o"         (i->chlit 111))  ;; 111
(assert-equal? (tn) "#\\p"         (i->chlit 112))  ;; 112
(assert-equal? (tn) "#\\q"         (i->chlit 113))  ;; 113
(assert-equal? (tn) "#\\r"         (i->chlit 114))  ;; 114
(assert-equal? (tn) "#\\s"         (i->chlit 115))  ;; 115
(assert-equal? (tn) "#\\t"         (i->chlit 116))  ;; 116
(assert-equal? (tn) "#\\u"         (i->chlit 117))  ;; 117
(assert-equal? (tn) "#\\v"         (i->chlit 118))  ;; 118
(assert-equal? (tn) "#\\w"         (i->chlit 119))  ;; 119
(assert-equal? (tn) "#\\x"         (i->chlit 120))  ;; 120
(assert-equal? (tn) "#\\y"         (i->chlit 121))  ;; 121
(assert-equal? (tn) "#\\z"         (i->chlit 122))  ;; 122
(assert-equal? (tn) "#\\{"         (i->chlit 123))  ;; 123
(assert-equal? (tn) "#\\|"         (i->chlit 124))  ;; 124
(assert-equal? (tn) "#\\}"         (i->chlit 125))  ;; 125
(assert-equal? (tn) "#\\~"         (i->chlit 126))  ;; 126
(assert-equal? (tn) "#\\delete"    (i->chlit 127))  ;; 127

(total-report)
