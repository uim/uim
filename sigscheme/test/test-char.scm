#! /usr/bin/env sscm -C EUC-JP
;; -*- buffer-file-coding-system: euc-jp -*-

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

(define i->chlit
  (lambda (i)
    (obj->literal (integer->char i))))

;; invalid character literal
(assert-parse-error "invalid char literal" "#\\nonexistent")

(assert-equal? "invalid char literal"
               (integer->char 120)
               (read (open-input-string "#\\x")))
(assert-parse-error "invalid char literal" "#\\x0")
(assert-parse-error "invalid char literal" "#\\x1")
(assert-parse-error "invalid char literal" "#\\x0g")
(assert-parse-error "invalid char literal" "#\\x1g")
(assert-parse-error "invalid char literal" "#\\x00g")
(assert-parse-error "invalid char literal" "#\\x01g")

(assert-parse-error "invalid char literal" "#\\x000")
(assert-parse-error "invalid char literal" "#\\x010")
(assert-parse-error "invalid char literal" "#\\x001")
(assert-parse-error "invalid char literal" "#\\x100")
(assert-parse-error "invalid char literal" "#\\x00a")
(assert-parse-error "invalid char literal" "#\\x0a0")
(assert-parse-error "invalid char literal" "#\\xa00")

(assert-parse-error "invalid char literal" "#\\x-0")
(assert-parse-error "invalid char literal" "#\\x-1")
(assert-parse-error "invalid char literal" "#\\x-00")
(assert-parse-error "invalid char literal" "#\\x-01")
(assert-parse-error "invalid char literal" "#\\x-000")
(assert-parse-error "invalid char literal" "#\\x-010")
(assert-parse-error "invalid char literal" "#\\x-001")
(assert-parse-error "invalid char literal" "#\\x-100")
(assert-parse-error "invalid char literal" "#\\x-00a")
(assert-parse-error "invalid char literal" "#\\x-0a0")
(assert-parse-error "invalid char literal" "#\\x-a00")

(assert-parse-error "invalid char literal" "#\\x+0")
(assert-parse-error "invalid char literal" "#\\x+1")
(assert-parse-error "invalid char literal" "#\\x+00")
(assert-parse-error "invalid char literal" "#\\x+01")
(assert-parse-error "invalid char literal" "#\\x+000")
(assert-parse-error "invalid char literal" "#\\x+010")
(assert-parse-error "invalid char literal" "#\\x+001")
(assert-parse-error "invalid char literal" "#\\x+100")
(assert-parse-error "invalid char literal" "#\\x+00a")
(assert-parse-error "invalid char literal" "#\\x+0a0")
(assert-parse-error "invalid char literal" "#\\x+a00")

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

(assert-equal? "R6RS named chars #2" "#\\nul"     (obj->literal #\x00))  ;; 0
(assert-equal? "R6RS named chars #2" "#\\alarm"   (obj->literal #\x07))  ;; 7
(assert-equal? "R6RS named chars #2" "#\\backspace" (obj->literal #\x08))  ;; 8
(assert-equal? "R6RS named chars #2" "#\\tab"     (obj->literal #\x09))  ;; 9
(assert-equal? "R6RS named chars #2" "#\\newline" (obj->literal #\x0a))  ;; 10
(assert-equal? "R6RS named chars #2" "#\\vtab"    (obj->literal #\x0b))  ;; 11
(assert-equal? "R6RS named chars #2" "#\\page"    (obj->literal #\x0c))  ;; 12
(assert-equal? "R6RS named chars #2" "#\\return"  (obj->literal #\x0d))  ;; 13
(assert-equal? "R6RS named chars #2" "#\\esc"     (obj->literal #\x1b))  ;; 27
(assert-equal? "R6RS named chars #2" "#\\space"   (obj->literal #\x20))  ;; 32
(assert-equal? "R6RS named chars #2" "#\\delete"  (obj->literal #\x7f))  ;; 127

(assert-equal? "char literal" "#\\nul"       (obj->literal #\nul))       ;; 0
(assert-equal? "char literal" "#\\x01"       (obj->literal #\x01))       ;; 1
(assert-equal? "char literal" "#\\x02"       (obj->literal #\x02))       ;; 2
(assert-equal? "char literal" "#\\x03"       (obj->literal #\x03))       ;; 3
(assert-equal? "char literal" "#\\x04"       (obj->literal #\x04))       ;; 4
(assert-equal? "char literal" "#\\x05"       (obj->literal #\x05))       ;; 5
(assert-equal? "char literal" "#\\x06"       (obj->literal #\x06))       ;; 6
(assert-equal? "char literal" "#\\alarm"     (obj->literal #\alarm))     ;; 7
(assert-equal? "char literal" "#\\backspace" (obj->literal #\backspace)) ;; 8
(assert-equal? "char literal" "#\\tab"       (obj->literal #\tab))       ;; 9
(assert-equal? "char literal" "#\\newline"   (obj->literal #\newline))   ;; 10
(assert-equal? "char literal" "#\\vtab"      (obj->literal #\vtab))      ;; 11
(assert-equal? "char literal" "#\\page"      (obj->literal #\page))      ;; 12
(assert-equal? "char literal" "#\\return"    (obj->literal #\return))    ;; 13
(assert-equal? "char literal" "#\\x0e"       (obj->literal #\x0e))       ;; 14
(assert-equal? "char literal" "#\\x0f"       (obj->literal #\x0f))       ;; 15
(assert-equal? "char literal" "#\\x10"       (obj->literal #\x10))       ;; 16
(assert-equal? "char literal" "#\\x11"       (obj->literal #\x11))       ;; 17
(assert-equal? "char literal" "#\\x12"       (obj->literal #\x12))       ;; 18
(assert-equal? "char literal" "#\\x13"       (obj->literal #\x13))       ;; 19
(assert-equal? "char literal" "#\\x14"       (obj->literal #\x14))       ;; 20
(assert-equal? "char literal" "#\\x15"       (obj->literal #\x15))       ;; 21
(assert-equal? "char literal" "#\\x16"       (obj->literal #\x16))       ;; 22
(assert-equal? "char literal" "#\\x17"       (obj->literal #\x17))       ;; 23
(assert-equal? "char literal" "#\\x18"       (obj->literal #\x18))       ;; 24
(assert-equal? "char literal" "#\\x19"       (obj->literal #\x19))       ;; 25
(assert-equal? "char literal" "#\\x1a"       (obj->literal #\x1a))       ;; 26
(assert-equal? "char literal" "#\\esc"       (obj->literal #\esc))       ;; 27
(assert-equal? "char literal" "#\\x1c"       (obj->literal #\x1c))       ;; 28
(assert-equal? "char literal" "#\\x1d"       (obj->literal #\x1d))       ;; 29
(assert-equal? "char literal" "#\\x1e"       (obj->literal #\x1e))       ;; 30
(assert-equal? "char literal" "#\\x1f"       (obj->literal #\x1f))       ;; 31
(assert-equal? "char literal" "#\\space"     (obj->literal #\space))     ;; 32
(assert-equal? "char literal" "#\\!"         (obj->literal #\!))         ;; 33
(assert-equal? "char literal" "#\\\""        (obj->literal #\"))         ;; 34
(assert-equal? "char literal" "#\\#"         (obj->literal #\#))         ;; 35
(assert-equal? "char literal" "#\\$"         (obj->literal #\$))         ;; 36
(assert-equal? "char literal" "#\\%"         (obj->literal #\%))         ;; 37
(assert-equal? "char literal" "#\\&"         (obj->literal #\&))         ;; 38
(assert-equal? "char literal" "#\\'"         (obj->literal #\'))         ;; 39
(assert-equal? "char literal" "#\\("         (obj->literal #\())         ;; 40
(assert-equal? "char literal" "#\\)"         (obj->literal #\)))         ;; 41
(assert-equal? "char literal" "#\\*"         (obj->literal #\*))         ;; 42
(assert-equal? "char literal" "#\\+"         (obj->literal #\+))         ;; 43
(assert-equal? "char literal" "#\\,"         (obj->literal #\,))         ;; 44
(assert-equal? "char literal" "#\\-"         (obj->literal #\-))         ;; 45
(assert-equal? "char literal" "#\\."         (obj->literal #\.))         ;; 46
(assert-equal? "char literal" "#\\/"         (obj->literal #\/))         ;; 47
(assert-equal? "char literal" "#\\0"         (obj->literal #\0))         ;; 48
(assert-equal? "char literal" "#\\1"         (obj->literal #\1))         ;; 49
(assert-equal? "char literal" "#\\2"         (obj->literal #\2))         ;; 50
(assert-equal? "char literal" "#\\3"         (obj->literal #\3))         ;; 51
(assert-equal? "char literal" "#\\4"         (obj->literal #\4))         ;; 52
(assert-equal? "char literal" "#\\5"         (obj->literal #\5))         ;; 53
(assert-equal? "char literal" "#\\6"         (obj->literal #\6))         ;; 54
(assert-equal? "char literal" "#\\7"         (obj->literal #\7))         ;; 55
(assert-equal? "char literal" "#\\8"         (obj->literal #\8))         ;; 56
(assert-equal? "char literal" "#\\9"         (obj->literal #\9))         ;; 57
(assert-equal? "char literal" "#\\:"         (obj->literal #\:))         ;; 58
(assert-equal? "char literal" "#\\;"         (obj->literal #\;))         ;; 59
(assert-equal? "char literal" "#\\<"         (obj->literal #\<))         ;; 60
(assert-equal? "char literal" "#\\="         (obj->literal #\=))         ;; 61
(assert-equal? "char literal" "#\\>"         (obj->literal #\>))         ;; 62
(assert-equal? "char literal" "#\\?"         (obj->literal #\?))         ;; 63
(assert-equal? "char literal" "#\\@"         (obj->literal #\@))         ;; 64
(assert-equal? "char literal" "#\\A"         (obj->literal #\A))         ;; 65
(assert-equal? "char literal" "#\\B"         (obj->literal #\B))         ;; 66
(assert-equal? "char literal" "#\\C"         (obj->literal #\C))         ;; 67
(assert-equal? "char literal" "#\\D"         (obj->literal #\D))         ;; 68
(assert-equal? "char literal" "#\\E"         (obj->literal #\E))         ;; 69
(assert-equal? "char literal" "#\\F"         (obj->literal #\F))         ;; 70
(assert-equal? "char literal" "#\\G"         (obj->literal #\G))         ;; 71
(assert-equal? "char literal" "#\\H"         (obj->literal #\H))         ;; 72
(assert-equal? "char literal" "#\\I"         (obj->literal #\I))         ;; 73
(assert-equal? "char literal" "#\\J"         (obj->literal #\J))         ;; 74
(assert-equal? "char literal" "#\\K"         (obj->literal #\K))         ;; 75
(assert-equal? "char literal" "#\\L"         (obj->literal #\L))         ;; 76
(assert-equal? "char literal" "#\\M"         (obj->literal #\M))         ;; 77
(assert-equal? "char literal" "#\\N"         (obj->literal #\N))         ;; 78
(assert-equal? "char literal" "#\\O"         (obj->literal #\O))         ;; 79
(assert-equal? "char literal" "#\\P"         (obj->literal #\P))         ;; 80
(assert-equal? "char literal" "#\\Q"         (obj->literal #\Q))         ;; 81
(assert-equal? "char literal" "#\\R"         (obj->literal #\R))         ;; 82
(assert-equal? "char literal" "#\\S"         (obj->literal #\S))         ;; 83
(assert-equal? "char literal" "#\\T"         (obj->literal #\T))         ;; 84
(assert-equal? "char literal" "#\\U"         (obj->literal #\U))         ;; 85
(assert-equal? "char literal" "#\\V"         (obj->literal #\V))         ;; 86
(assert-equal? "char literal" "#\\W"         (obj->literal #\W))         ;; 87
(assert-equal? "char literal" "#\\X"         (obj->literal #\X))         ;; 88
(assert-equal? "char literal" "#\\Y"         (obj->literal #\Y))         ;; 89
(assert-equal? "char literal" "#\\Z"         (obj->literal #\Z))         ;; 90
(assert-equal? "char literal" "#\\["         (obj->literal #\[))         ;; 91
(assert-equal? "char literal" "#\\\\"        (obj->literal #\\))         ;; 92
(assert-equal? "char literal" "#\\]"         (obj->literal #\]))         ;; 93
(assert-equal? "char literal" "#\\^"         (obj->literal #\^))         ;; 94
(assert-equal? "char literal" "#\\_"         (obj->literal #\_))         ;; 95
(assert-equal? "char literal" "#\\`"         (obj->literal #\`))         ;; 96
(assert-equal? "char literal" "#\\a"         (obj->literal #\a))         ;; 97
(assert-equal? "char literal" "#\\b"         (obj->literal #\b))         ;; 98
(assert-equal? "char literal" "#\\c"         (obj->literal #\c))         ;; 99
(assert-equal? "char literal" "#\\d"         (obj->literal #\d))         ;; 100
(assert-equal? "char literal" "#\\e"         (obj->literal #\e))         ;; 101
(assert-equal? "char literal" "#\\f"         (obj->literal #\f))         ;; 102
(assert-equal? "char literal" "#\\g"         (obj->literal #\g))         ;; 103
(assert-equal? "char literal" "#\\h"         (obj->literal #\h))         ;; 104
(assert-equal? "char literal" "#\\i"         (obj->literal #\i))         ;; 105
(assert-equal? "char literal" "#\\j"         (obj->literal #\j))         ;; 106
(assert-equal? "char literal" "#\\k"         (obj->literal #\k))         ;; 107
(assert-equal? "char literal" "#\\l"         (obj->literal #\l))         ;; 108
(assert-equal? "char literal" "#\\m"         (obj->literal #\m))         ;; 109
(assert-equal? "char literal" "#\\n"         (obj->literal #\n))         ;; 110
(assert-equal? "char literal" "#\\o"         (obj->literal #\o))         ;; 111
(assert-equal? "char literal" "#\\p"         (obj->literal #\p))         ;; 112
(assert-equal? "char literal" "#\\q"         (obj->literal #\q))         ;; 113
(assert-equal? "char literal" "#\\r"         (obj->literal #\r))         ;; 114
(assert-equal? "char literal" "#\\s"         (obj->literal #\s))         ;; 115
(assert-equal? "char literal" "#\\t"         (obj->literal #\t))         ;; 116
(assert-equal? "char literal" "#\\u"         (obj->literal #\u))         ;; 117
(assert-equal? "char literal" "#\\v"         (obj->literal #\v))         ;; 118
(assert-equal? "char literal" "#\\w"         (obj->literal #\w))         ;; 119
(assert-equal? "char literal" "#\\x"         (obj->literal #\x))         ;; 120
(assert-equal? "char literal" "#\\y"         (obj->literal #\y))         ;; 121
(assert-equal? "char literal" "#\\z"         (obj->literal #\z))         ;; 122
(assert-equal? "char literal" "#\\{"         (obj->literal #\{))         ;; 123
(assert-equal? "char literal" "#\\|"         (obj->literal #\|))         ;; 124
(assert-equal? "char literal" "#\\}"         (obj->literal #\}))         ;; 125
(assert-equal? "char literal" "#\\~"         (obj->literal #\~))         ;; 126
(assert-equal? "char literal" "#\\delete"    (obj->literal #\delete))    ;; 127

;; R6RS(SRFI-75) hexadecimal character literal 
(assert-equal? "R6RS hexadecimal char literal" #\nul       #\x00)    ;; 0
(assert-equal? "R6RS hexadecimal char literal" #\x01       #\x01)    ;; 1
(assert-equal? "R6RS hexadecimal char literal" #\x02       #\x02)    ;; 2
(assert-equal? "R6RS hexadecimal char literal" #\x03       #\x03)    ;; 3
(assert-equal? "R6RS hexadecimal char literal" #\x04       #\x04)    ;; 4
(assert-equal? "R6RS hexadecimal char literal" #\x05       #\x05)    ;; 5
(assert-equal? "R6RS hexadecimal char literal" #\x06       #\x06)    ;; 6
(assert-equal? "R6RS hexadecimal char literal" #\alarm     #\x07)    ;; 7
(assert-equal? "R6RS hexadecimal char literal" #\backspace #\x08)    ;; 8
(assert-equal? "R6RS hexadecimal char literal" #\tab       #\x09)    ;; 9
(assert-equal? "R6RS hexadecimal char literal" #\newline   #\x0a)   ;; 10
(assert-equal? "R6RS hexadecimal char literal" #\vtab      #\x0b)   ;; 11
(assert-equal? "R6RS hexadecimal char literal" #\page      #\x0c)   ;; 12
(assert-equal? "R6RS hexadecimal char literal" #\return    #\x0d)   ;; 13
(assert-equal? "R6RS hexadecimal char literal" #\x0e       #\x0e)   ;; 14
(assert-equal? "R6RS hexadecimal char literal" #\x0f       #\x0f)   ;; 15
(assert-equal? "R6RS hexadecimal char literal" #\x10       #\x10)   ;; 16
(assert-equal? "R6RS hexadecimal char literal" #\x11       #\x11)   ;; 17
(assert-equal? "R6RS hexadecimal char literal" #\x12       #\x12)   ;; 18
(assert-equal? "R6RS hexadecimal char literal" #\x13       #\x13)   ;; 19
(assert-equal? "R6RS hexadecimal char literal" #\x14       #\x14)   ;; 20
(assert-equal? "R6RS hexadecimal char literal" #\x15       #\x15)   ;; 21
(assert-equal? "R6RS hexadecimal char literal" #\x16       #\x16)   ;; 22
(assert-equal? "R6RS hexadecimal char literal" #\x17       #\x17)   ;; 23
(assert-equal? "R6RS hexadecimal char literal" #\x18       #\x18)   ;; 24
(assert-equal? "R6RS hexadecimal char literal" #\x19       #\x19)   ;; 25
(assert-equal? "R6RS hexadecimal char literal" #\x1a       #\x1a)   ;; 26
(assert-equal? "R6RS hexadecimal char literal" #\esc       #\x1b)   ;; 27
(assert-equal? "R6RS hexadecimal char literal" #\x1c       #\x1c)   ;; 28
(assert-equal? "R6RS hexadecimal char literal" #\x1d       #\x1d)   ;; 29
(assert-equal? "R6RS hexadecimal char literal" #\x1e       #\x1e)   ;; 30
(assert-equal? "R6RS hexadecimal char literal" #\x1f       #\x1f)   ;; 31
(assert-equal? "R6RS hexadecimal char literal" #\space     #\x20)   ;; 32
(assert-equal? "R6RS hexadecimal char literal" #\!         #\x21)   ;; 33
(assert-equal? "R6RS hexadecimal char literal" #\"         #\x22)   ;; 34
(assert-equal? "R6RS hexadecimal char literal" #\#         #\x23)   ;; 35
(assert-equal? "R6RS hexadecimal char literal" #\$         #\x24)   ;; 36
(assert-equal? "R6RS hexadecimal char literal" #\%         #\x25)   ;; 37
(assert-equal? "R6RS hexadecimal char literal" #\&         #\x26)   ;; 38
(assert-equal? "R6RS hexadecimal char literal" #\'         #\x27)   ;; 39
(assert-equal? "R6RS hexadecimal char literal" #\(         #\x28)   ;; 40
(assert-equal? "R6RS hexadecimal char literal" #\)         #\x29)   ;; 41
(assert-equal? "R6RS hexadecimal char literal" #\*         #\x2a)   ;; 42
(assert-equal? "R6RS hexadecimal char literal" #\+         #\x2b)   ;; 43
(assert-equal? "R6RS hexadecimal char literal" #\,         #\x2c)   ;; 44
(assert-equal? "R6RS hexadecimal char literal" #\-         #\x2d)   ;; 45
(assert-equal? "R6RS hexadecimal char literal" #\.         #\x2e)   ;; 46
(assert-equal? "R6RS hexadecimal char literal" #\/         #\x2f)   ;; 47
(assert-equal? "R6RS hexadecimal char literal" #\0         #\x30)   ;; 48
(assert-equal? "R6RS hexadecimal char literal" #\1         #\x31)   ;; 49
(assert-equal? "R6RS hexadecimal char literal" #\2         #\x32)   ;; 50
(assert-equal? "R6RS hexadecimal char literal" #\3         #\x33)   ;; 51
(assert-equal? "R6RS hexadecimal char literal" #\4         #\x34)   ;; 52
(assert-equal? "R6RS hexadecimal char literal" #\5         #\x35)   ;; 53
(assert-equal? "R6RS hexadecimal char literal" #\6         #\x36)   ;; 54
(assert-equal? "R6RS hexadecimal char literal" #\7         #\x37)   ;; 55
(assert-equal? "R6RS hexadecimal char literal" #\8         #\x38)   ;; 56
(assert-equal? "R6RS hexadecimal char literal" #\9         #\x39)   ;; 57
(assert-equal? "R6RS hexadecimal char literal" #\:         #\x3a)   ;; 58
(assert-equal? "R6RS hexadecimal char literal" #\;         #\x3b)   ;; 59
(assert-equal? "R6RS hexadecimal char literal" #\<         #\x3c)   ;; 60
(assert-equal? "R6RS hexadecimal char literal" #\=         #\x3d)   ;; 61
(assert-equal? "R6RS hexadecimal char literal" #\>         #\x3e)   ;; 62
(assert-equal? "R6RS hexadecimal char literal" #\?         #\x3f)   ;; 63
(assert-equal? "R6RS hexadecimal char literal" #\@         #\x40)   ;; 64
(assert-equal? "R6RS hexadecimal char literal" #\A         #\x41)   ;; 65
(assert-equal? "R6RS hexadecimal char literal" #\B         #\x42)   ;; 66
(assert-equal? "R6RS hexadecimal char literal" #\C         #\x43)   ;; 67
(assert-equal? "R6RS hexadecimal char literal" #\D         #\x44)   ;; 68
(assert-equal? "R6RS hexadecimal char literal" #\E         #\x45)   ;; 69
(assert-equal? "R6RS hexadecimal char literal" #\F         #\x46)   ;; 70
(assert-equal? "R6RS hexadecimal char literal" #\G         #\x47)   ;; 71
(assert-equal? "R6RS hexadecimal char literal" #\H         #\x48)   ;; 72
(assert-equal? "R6RS hexadecimal char literal" #\I         #\x49)   ;; 73
(assert-equal? "R6RS hexadecimal char literal" #\J         #\x4a)   ;; 74
(assert-equal? "R6RS hexadecimal char literal" #\K         #\x4b)   ;; 75
(assert-equal? "R6RS hexadecimal char literal" #\L         #\x4c)   ;; 76
(assert-equal? "R6RS hexadecimal char literal" #\M         #\x4d)   ;; 77
(assert-equal? "R6RS hexadecimal char literal" #\N         #\x4e)   ;; 78
(assert-equal? "R6RS hexadecimal char literal" #\O         #\x4f)   ;; 79
(assert-equal? "R6RS hexadecimal char literal" #\P         #\x50)   ;; 80
(assert-equal? "R6RS hexadecimal char literal" #\Q         #\x51)   ;; 81
(assert-equal? "R6RS hexadecimal char literal" #\R         #\x52)   ;; 82
(assert-equal? "R6RS hexadecimal char literal" #\S         #\x53)   ;; 83
(assert-equal? "R6RS hexadecimal char literal" #\T         #\x54)   ;; 84
(assert-equal? "R6RS hexadecimal char literal" #\U         #\x55)   ;; 85
(assert-equal? "R6RS hexadecimal char literal" #\V         #\x56)   ;; 86
(assert-equal? "R6RS hexadecimal char literal" #\W         #\x57)   ;; 87
(assert-equal? "R6RS hexadecimal char literal" #\X         #\x58)   ;; 88
(assert-equal? "R6RS hexadecimal char literal" #\Y         #\x59)   ;; 89
(assert-equal? "R6RS hexadecimal char literal" #\Z         #\x5a)   ;; 90
(assert-equal? "R6RS hexadecimal char literal" #\[         #\x5b)   ;; 91
(assert-equal? "R6RS hexadecimal char literal" #\\         #\x5c)   ;; 92
(assert-equal? "R6RS hexadecimal char literal" #\]         #\x5d)   ;; 93
(assert-equal? "R6RS hexadecimal char literal" #\^         #\x5e)   ;; 94
(assert-equal? "R6RS hexadecimal char literal" #\_         #\x5f)   ;; 95
(assert-equal? "R6RS hexadecimal char literal" #\`         #\x60)   ;; 96
(assert-equal? "R6RS hexadecimal char literal" #\a         #\x61)   ;; 97
(assert-equal? "R6RS hexadecimal char literal" #\b         #\x62)   ;; 98
(assert-equal? "R6RS hexadecimal char literal" #\c         #\x63)   ;; 99
(assert-equal? "R6RS hexadecimal char literal" #\d         #\x64)  ;; 100
(assert-equal? "R6RS hexadecimal char literal" #\e         #\x65)  ;; 101
(assert-equal? "R6RS hexadecimal char literal" #\f         #\x66)  ;; 102
(assert-equal? "R6RS hexadecimal char literal" #\g         #\x67)  ;; 103
(assert-equal? "R6RS hexadecimal char literal" #\h         #\x68)  ;; 104
(assert-equal? "R6RS hexadecimal char literal" #\i         #\x69)  ;; 105
(assert-equal? "R6RS hexadecimal char literal" #\j         #\x6a)  ;; 106
(assert-equal? "R6RS hexadecimal char literal" #\k         #\x6b)  ;; 107
(assert-equal? "R6RS hexadecimal char literal" #\l         #\x6c)  ;; 108
(assert-equal? "R6RS hexadecimal char literal" #\m         #\x6d)  ;; 109
(assert-equal? "R6RS hexadecimal char literal" #\n         #\x6e)  ;; 110
(assert-equal? "R6RS hexadecimal char literal" #\o         #\x6f)  ;; 111
(assert-equal? "R6RS hexadecimal char literal" #\p         #\x70)  ;; 112
(assert-equal? "R6RS hexadecimal char literal" #\q         #\x71)  ;; 113
(assert-equal? "R6RS hexadecimal char literal" #\r         #\x72)  ;; 114
(assert-equal? "R6RS hexadecimal char literal" #\s         #\x73)  ;; 115
(assert-equal? "R6RS hexadecimal char literal" #\t         #\x74)  ;; 116
(assert-equal? "R6RS hexadecimal char literal" #\u         #\x75)  ;; 117
(assert-equal? "R6RS hexadecimal char literal" #\v         #\x76)  ;; 118
(assert-equal? "R6RS hexadecimal char literal" #\w         #\x77)  ;; 119
(assert-equal? "R6RS hexadecimal char literal" #\x         #\x78)  ;; 120
(assert-equal? "R6RS hexadecimal char literal" #\y         #\x79)  ;; 121
(assert-equal? "R6RS hexadecimal char literal" #\z         #\x7a)  ;; 122
(assert-equal? "R6RS hexadecimal char literal" #\{         #\x7b)  ;; 123
(assert-equal? "R6RS hexadecimal char literal" #\|         #\x7c)  ;; 124
(assert-equal? "R6RS hexadecimal char literal" #\}         #\x7d)  ;; 125
(assert-equal? "R6RS hexadecimal char literal" #\~         #\x7e)  ;; 126
(assert-equal? "R6RS hexadecimal char literal" #\delete    #\x7f)  ;; 127

(assert-equal? "R6RS hexadecimal char literal #2" "#\\nul"       (obj->literal #\x00))  ;; 0
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x01"       (obj->literal #\x01))  ;; 1
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x02"       (obj->literal #\x02))  ;; 2
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x03"       (obj->literal #\x03))  ;; 3
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x04"       (obj->literal #\x04))  ;; 4
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x05"       (obj->literal #\x05))  ;; 5
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x06"       (obj->literal #\x06))  ;; 6
(assert-equal? "R6RS hexadecimal char literal #2" "#\\alarm"     (obj->literal #\x07))  ;; 7
(assert-equal? "R6RS hexadecimal char literal #2" "#\\backspace" (obj->literal #\x08))  ;; 8
(assert-equal? "R6RS hexadecimal char literal #2" "#\\tab"       (obj->literal #\x09))  ;; 9
(assert-equal? "R6RS hexadecimal char literal #2" "#\\newline"   (obj->literal #\x0a))  ;; 10
(assert-equal? "R6RS hexadecimal char literal #2" "#\\vtab"      (obj->literal #\x0b))  ;; 11
(assert-equal? "R6RS hexadecimal char literal #2" "#\\page"      (obj->literal #\x0c))  ;; 12
(assert-equal? "R6RS hexadecimal char literal #2" "#\\return"    (obj->literal #\x0d))  ;; 13
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x0e"       (obj->literal #\x0e))  ;; 14
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x0f"       (obj->literal #\x0f))  ;; 15
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x10"       (obj->literal #\x10))  ;; 16
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x11"       (obj->literal #\x11))  ;; 17
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x12"       (obj->literal #\x12))  ;; 18
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x13"       (obj->literal #\x13))  ;; 19
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x14"       (obj->literal #\x14))  ;; 20
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x15"       (obj->literal #\x15))  ;; 21
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x16"       (obj->literal #\x16))  ;; 22
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x17"       (obj->literal #\x17))  ;; 23
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x18"       (obj->literal #\x18))  ;; 24
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x19"       (obj->literal #\x19))  ;; 25
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x1a"       (obj->literal #\x1a))  ;; 26
(assert-equal? "R6RS hexadecimal char literal #2" "#\\esc"       (obj->literal #\x1b))  ;; 27
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x1c"       (obj->literal #\x1c))  ;; 28
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x1d"       (obj->literal #\x1d))  ;; 29
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x1e"       (obj->literal #\x1e))  ;; 30
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x1f"       (obj->literal #\x1f))  ;; 31
(assert-equal? "R6RS hexadecimal char literal #2" "#\\space"     (obj->literal #\x20))  ;; 32
(assert-equal? "R6RS hexadecimal char literal #2" "#\\!"         (obj->literal #\x21))  ;; 33
(assert-equal? "R6RS hexadecimal char literal #2" "#\\\""        (obj->literal #\x22))  ;; 34
(assert-equal? "R6RS hexadecimal char literal #2" "#\\#"         (obj->literal #\x23))  ;; 35
(assert-equal? "R6RS hexadecimal char literal #2" "#\\$"         (obj->literal #\x24))  ;; 36
(assert-equal? "R6RS hexadecimal char literal #2" "#\\%"         (obj->literal #\x25))  ;; 37
(assert-equal? "R6RS hexadecimal char literal #2" "#\\&"         (obj->literal #\x26))  ;; 38
(assert-equal? "R6RS hexadecimal char literal #2" "#\\'"         (obj->literal #\x27))  ;; 39
(assert-equal? "R6RS hexadecimal char literal #2" "#\\("         (obj->literal #\x28))  ;; 40
(assert-equal? "R6RS hexadecimal char literal #2" "#\\)"         (obj->literal #\x29))  ;; 41
(assert-equal? "R6RS hexadecimal char literal #2" "#\\*"         (obj->literal #\x2a))  ;; 42
(assert-equal? "R6RS hexadecimal char literal #2" "#\\+"         (obj->literal #\x2b))  ;; 43
(assert-equal? "R6RS hexadecimal char literal #2" "#\\,"         (obj->literal #\x2c))  ;; 44
(assert-equal? "R6RS hexadecimal char literal #2" "#\\-"         (obj->literal #\x2d))  ;; 45
(assert-equal? "R6RS hexadecimal char literal #2" "#\\."         (obj->literal #\x2e))  ;; 46
(assert-equal? "R6RS hexadecimal char literal #2" "#\\/"         (obj->literal #\x2f))  ;; 47
(assert-equal? "R6RS hexadecimal char literal #2" "#\\0"         (obj->literal #\x30))  ;; 48
(assert-equal? "R6RS hexadecimal char literal #2" "#\\1"         (obj->literal #\x31))  ;; 49
(assert-equal? "R6RS hexadecimal char literal #2" "#\\2"         (obj->literal #\x32))  ;; 50
(assert-equal? "R6RS hexadecimal char literal #2" "#\\3"         (obj->literal #\x33))  ;; 51
(assert-equal? "R6RS hexadecimal char literal #2" "#\\4"         (obj->literal #\x34))  ;; 52
(assert-equal? "R6RS hexadecimal char literal #2" "#\\5"         (obj->literal #\x35))  ;; 53
(assert-equal? "R6RS hexadecimal char literal #2" "#\\6"         (obj->literal #\x36))  ;; 54
(assert-equal? "R6RS hexadecimal char literal #2" "#\\7"         (obj->literal #\x37))  ;; 55
(assert-equal? "R6RS hexadecimal char literal #2" "#\\8"         (obj->literal #\x38))  ;; 56
(assert-equal? "R6RS hexadecimal char literal #2" "#\\9"         (obj->literal #\x39))  ;; 57
(assert-equal? "R6RS hexadecimal char literal #2" "#\\:"         (obj->literal #\x3a))  ;; 58
(assert-equal? "R6RS hexadecimal char literal #2" "#\\;"         (obj->literal #\x3b))  ;; 59
(assert-equal? "R6RS hexadecimal char literal #2" "#\\<"         (obj->literal #\x3c))  ;; 60
(assert-equal? "R6RS hexadecimal char literal #2" "#\\="         (obj->literal #\x3d))  ;; 61
(assert-equal? "R6RS hexadecimal char literal #2" "#\\>"         (obj->literal #\x3e))  ;; 62
(assert-equal? "R6RS hexadecimal char literal #2" "#\\?"         (obj->literal #\x3f))  ;; 63
(assert-equal? "R6RS hexadecimal char literal #2" "#\\@"         (obj->literal #\x40))  ;; 64
(assert-equal? "R6RS hexadecimal char literal #2" "#\\A"         (obj->literal #\x41))  ;; 65
(assert-equal? "R6RS hexadecimal char literal #2" "#\\B"         (obj->literal #\x42))  ;; 66
(assert-equal? "R6RS hexadecimal char literal #2" "#\\C"         (obj->literal #\x43))  ;; 67
(assert-equal? "R6RS hexadecimal char literal #2" "#\\D"         (obj->literal #\x44))  ;; 68
(assert-equal? "R6RS hexadecimal char literal #2" "#\\E"         (obj->literal #\x45))  ;; 69
(assert-equal? "R6RS hexadecimal char literal #2" "#\\F"         (obj->literal #\x46))  ;; 70
(assert-equal? "R6RS hexadecimal char literal #2" "#\\G"         (obj->literal #\x47))  ;; 71
(assert-equal? "R6RS hexadecimal char literal #2" "#\\H"         (obj->literal #\x48))  ;; 72
(assert-equal? "R6RS hexadecimal char literal #2" "#\\I"         (obj->literal #\x49))  ;; 73
(assert-equal? "R6RS hexadecimal char literal #2" "#\\J"         (obj->literal #\x4a))  ;; 74
(assert-equal? "R6RS hexadecimal char literal #2" "#\\K"         (obj->literal #\x4b))  ;; 75
(assert-equal? "R6RS hexadecimal char literal #2" "#\\L"         (obj->literal #\x4c))  ;; 76
(assert-equal? "R6RS hexadecimal char literal #2" "#\\M"         (obj->literal #\x4d))  ;; 77
(assert-equal? "R6RS hexadecimal char literal #2" "#\\N"         (obj->literal #\x4e))  ;; 78
(assert-equal? "R6RS hexadecimal char literal #2" "#\\O"         (obj->literal #\x4f))  ;; 79
(assert-equal? "R6RS hexadecimal char literal #2" "#\\P"         (obj->literal #\x50))  ;; 80
(assert-equal? "R6RS hexadecimal char literal #2" "#\\Q"         (obj->literal #\x51))  ;; 81
(assert-equal? "R6RS hexadecimal char literal #2" "#\\R"         (obj->literal #\x52))  ;; 82
(assert-equal? "R6RS hexadecimal char literal #2" "#\\S"         (obj->literal #\x53))  ;; 83
(assert-equal? "R6RS hexadecimal char literal #2" "#\\T"         (obj->literal #\x54))  ;; 84
(assert-equal? "R6RS hexadecimal char literal #2" "#\\U"         (obj->literal #\x55))  ;; 85
(assert-equal? "R6RS hexadecimal char literal #2" "#\\V"         (obj->literal #\x56))  ;; 86
(assert-equal? "R6RS hexadecimal char literal #2" "#\\W"         (obj->literal #\x57))  ;; 87
(assert-equal? "R6RS hexadecimal char literal #2" "#\\X"         (obj->literal #\x58))  ;; 88
(assert-equal? "R6RS hexadecimal char literal #2" "#\\Y"         (obj->literal #\x59))  ;; 89
(assert-equal? "R6RS hexadecimal char literal #2" "#\\Z"         (obj->literal #\x5a))  ;; 90
(assert-equal? "R6RS hexadecimal char literal #2" "#\\["         (obj->literal #\x5b))  ;; 91
(assert-equal? "R6RS hexadecimal char literal #2" "#\\\\"        (obj->literal #\x5c))  ;; 92
(assert-equal? "R6RS hexadecimal char literal #2" "#\\]"         (obj->literal #\x5d))  ;; 93
(assert-equal? "R6RS hexadecimal char literal #2" "#\\^"         (obj->literal #\x5e))  ;; 94
(assert-equal? "R6RS hexadecimal char literal #2" "#\\_"         (obj->literal #\x5f))  ;; 95
(assert-equal? "R6RS hexadecimal char literal #2" "#\\`"         (obj->literal #\x60))  ;; 96
(assert-equal? "R6RS hexadecimal char literal #2" "#\\a"         (obj->literal #\x61))  ;; 97
(assert-equal? "R6RS hexadecimal char literal #2" "#\\b"         (obj->literal #\x62))  ;; 98
(assert-equal? "R6RS hexadecimal char literal #2" "#\\c"         (obj->literal #\x63))  ;; 99
(assert-equal? "R6RS hexadecimal char literal #2" "#\\d"         (obj->literal #\x64))  ;; 100
(assert-equal? "R6RS hexadecimal char literal #2" "#\\e"         (obj->literal #\x65))  ;; 101
(assert-equal? "R6RS hexadecimal char literal #2" "#\\f"         (obj->literal #\x66))  ;; 102
(assert-equal? "R6RS hexadecimal char literal #2" "#\\g"         (obj->literal #\x67))  ;; 103
(assert-equal? "R6RS hexadecimal char literal #2" "#\\h"         (obj->literal #\x68))  ;; 104
(assert-equal? "R6RS hexadecimal char literal #2" "#\\i"         (obj->literal #\x69))  ;; 105
(assert-equal? "R6RS hexadecimal char literal #2" "#\\j"         (obj->literal #\x6a))  ;; 106
(assert-equal? "R6RS hexadecimal char literal #2" "#\\k"         (obj->literal #\x6b))  ;; 107
(assert-equal? "R6RS hexadecimal char literal #2" "#\\l"         (obj->literal #\x6c))  ;; 108
(assert-equal? "R6RS hexadecimal char literal #2" "#\\m"         (obj->literal #\x6d))  ;; 109
(assert-equal? "R6RS hexadecimal char literal #2" "#\\n"         (obj->literal #\x6e))  ;; 110
(assert-equal? "R6RS hexadecimal char literal #2" "#\\o"         (obj->literal #\x6f))  ;; 111
(assert-equal? "R6RS hexadecimal char literal #2" "#\\p"         (obj->literal #\x70))  ;; 112
(assert-equal? "R6RS hexadecimal char literal #2" "#\\q"         (obj->literal #\x71))  ;; 113
(assert-equal? "R6RS hexadecimal char literal #2" "#\\r"         (obj->literal #\x72))  ;; 114
(assert-equal? "R6RS hexadecimal char literal #2" "#\\s"         (obj->literal #\x73))  ;; 115
(assert-equal? "R6RS hexadecimal char literal #2" "#\\t"         (obj->literal #\x74))  ;; 116
(assert-equal? "R6RS hexadecimal char literal #2" "#\\u"         (obj->literal #\x75))  ;; 117
(assert-equal? "R6RS hexadecimal char literal #2" "#\\v"         (obj->literal #\x76))  ;; 118
(assert-equal? "R6RS hexadecimal char literal #2" "#\\w"         (obj->literal #\x77))  ;; 119
(assert-equal? "R6RS hexadecimal char literal #2" "#\\x"         (obj->literal #\x78))  ;; 120
(assert-equal? "R6RS hexadecimal char literal #2" "#\\y"         (obj->literal #\x79))  ;; 121
(assert-equal? "R6RS hexadecimal char literal #2" "#\\z"         (obj->literal #\x7a))  ;; 122
(assert-equal? "R6RS hexadecimal char literal #2" "#\\{"         (obj->literal #\x7b))  ;; 123
(assert-equal? "R6RS hexadecimal char literal #2" "#\\|"         (obj->literal #\x7c))  ;; 124
(assert-equal? "R6RS hexadecimal char literal #2" "#\\}"         (obj->literal #\x7d))  ;; 125
(assert-equal? "R6RS hexadecimal char literal #2" "#\\~"         (obj->literal #\x7e))  ;; 126
(assert-equal? "R6RS hexadecimal char literal #2" "#\\delete"    (obj->literal #\x7f))  ;; 127

(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   0) #\x00)  ;; 0
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   1) #\x01)  ;; 1
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   2) #\x02)  ;; 2
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   3) #\x03)  ;; 3
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   4) #\x04)  ;; 4
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   5) #\x05)  ;; 5
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   6) #\x06)  ;; 6
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   7) #\x07)  ;; 7
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   8) #\x08)  ;; 8
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char   9) #\x09)  ;; 9
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  10) #\x0a)  ;; 10
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  11) #\x0b)  ;; 11
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  12) #\x0c)  ;; 12
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  13) #\x0d)  ;; 13
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  14) #\x0e)  ;; 14
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  15) #\x0f)  ;; 15
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  16) #\x10)  ;; 16
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  17) #\x11)  ;; 17
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  18) #\x12)  ;; 18
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  19) #\x13)  ;; 19
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  20) #\x14)  ;; 20
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  21) #\x15)  ;; 21
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  22) #\x16)  ;; 22
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  23) #\x17)  ;; 23
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  24) #\x18)  ;; 24
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  25) #\x19)  ;; 25
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  26) #\x1a)  ;; 26
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  27) #\x1b)  ;; 27
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  28) #\x1c)  ;; 28
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  29) #\x1d)  ;; 29
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  30) #\x1e)  ;; 30
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  31) #\x1f)  ;; 31
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  32) #\x20)  ;; 32
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  33) #\x21)  ;; 33
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  34) #\x22)  ;; 34
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  35) #\x23)  ;; 35
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  36) #\x24)  ;; 36
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  37) #\x25)  ;; 37
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  38) #\x26)  ;; 38
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  39) #\x27)  ;; 39
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  40) #\x28)  ;; 40
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  41) #\x29)  ;; 41
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  42) #\x2a)  ;; 42
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  43) #\x2b)  ;; 43
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  44) #\x2c)  ;; 44
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  45) #\x2d)  ;; 45
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  46) #\x2e)  ;; 46
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  47) #\x2f)  ;; 47
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  48) #\x30)  ;; 48
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  49) #\x31)  ;; 49
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  50) #\x32)  ;; 50
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  51) #\x33)  ;; 51
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  52) #\x34)  ;; 52
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  53) #\x35)  ;; 53
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  54) #\x36)  ;; 54
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  55) #\x37)  ;; 55
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  56) #\x38)  ;; 56
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  57) #\x39)  ;; 57
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  58) #\x3a)  ;; 58
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  59) #\x3b)  ;; 59
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  60) #\x3c)  ;; 60
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  61) #\x3d)  ;; 61
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  62) #\x3e)  ;; 62
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  63) #\x3f)  ;; 63
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  64) #\x40)  ;; 64
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  65) #\x41)  ;; 65
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  66) #\x42)  ;; 66
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  67) #\x43)  ;; 67
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  68) #\x44)  ;; 68
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  69) #\x45)  ;; 69
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  70) #\x46)  ;; 70
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  71) #\x47)  ;; 71
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  72) #\x48)  ;; 72
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  73) #\x49)  ;; 73
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  74) #\x4a)  ;; 74
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  75) #\x4b)  ;; 75
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  76) #\x4c)  ;; 76
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  77) #\x4d)  ;; 77
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  78) #\x4e)  ;; 78
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  79) #\x4f)  ;; 79
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  80) #\x50)  ;; 80
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  81) #\x51)  ;; 81
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  82) #\x52)  ;; 82
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  83) #\x53)  ;; 83
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  84) #\x54)  ;; 84
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  85) #\x55)  ;; 85
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  86) #\x56)  ;; 86
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  87) #\x57)  ;; 87
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  88) #\x58)  ;; 88
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  89) #\x59)  ;; 89
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  90) #\x5a)  ;; 90
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  91) #\x5b)  ;; 91
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  92) #\x5c)  ;; 92
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  93) #\x5d)  ;; 93
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  94) #\x5e)  ;; 94
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  95) #\x5f)  ;; 95
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  96) #\x60)  ;; 96
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  97) #\x61)  ;; 97
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  98) #\x62)  ;; 98
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char  99) #\x63)  ;; 99
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 100) #\x64)  ;; 100
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 101) #\x65)  ;; 101
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 102) #\x66)  ;; 102
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 103) #\x67)  ;; 103
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 104) #\x68)  ;; 104
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 105) #\x69)  ;; 105
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 106) #\x6a)  ;; 106
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 107) #\x6b)  ;; 107
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 108) #\x6c)  ;; 108
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 109) #\x6d)  ;; 109
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 110) #\x6e)  ;; 110
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 111) #\x6f)  ;; 111
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 112) #\x70)  ;; 112
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 113) #\x71)  ;; 113
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 114) #\x72)  ;; 114
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 115) #\x73)  ;; 115
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 116) #\x74)  ;; 116
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 117) #\x75)  ;; 117
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 118) #\x76)  ;; 118
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 119) #\x77)  ;; 119
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 120) #\x78)  ;; 120
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 121) #\x79)  ;; 121
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 122) #\x7a)  ;; 122
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 123) #\x7b)  ;; 123
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 124) #\x7c)  ;; 124
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 125) #\x7d)  ;; 125
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 126) #\x7e)  ;; 126
(assert-equal? "R6RS hexadecimal char literal #3" (integer->char 127) #\x7f)  ;; 127

;; capitalized hexadecimal
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   0) #\x00)  ;; 0
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   1) #\x01)  ;; 1
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   2) #\x02)  ;; 2
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   3) #\x03)  ;; 3
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   4) #\x04)  ;; 4
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   5) #\x05)  ;; 5
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   6) #\x06)  ;; 6
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   7) #\x07)  ;; 7
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   8) #\x08)  ;; 8
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char   9) #\x09)  ;; 9
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  10) #\x0A)  ;; 10
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  11) #\x0B)  ;; 11
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  12) #\x0C)  ;; 12
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  13) #\x0D)  ;; 13
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  14) #\x0E)  ;; 14
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  15) #\x0F)  ;; 15
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  16) #\x10)  ;; 16
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  17) #\x11)  ;; 17
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  18) #\x12)  ;; 18
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  19) #\x13)  ;; 19
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  20) #\x14)  ;; 20
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  21) #\x15)  ;; 21
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  22) #\x16)  ;; 22
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  23) #\x17)  ;; 23
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  24) #\x18)  ;; 24
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  25) #\x19)  ;; 25
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  26) #\x1A)  ;; 26
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  27) #\x1B)  ;; 27
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  28) #\x1C)  ;; 28
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  29) #\x1D)  ;; 29
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  30) #\x1E)  ;; 30
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  31) #\x1F)  ;; 31
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  32) #\x20)  ;; 32
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  33) #\x21)  ;; 33
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  34) #\x22)  ;; 34
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  35) #\x23)  ;; 35
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  36) #\x24)  ;; 36
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  37) #\x25)  ;; 37
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  38) #\x26)  ;; 38
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  39) #\x27)  ;; 39
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  40) #\x28)  ;; 40
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  41) #\x29)  ;; 41
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  42) #\x2A)  ;; 42
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  43) #\x2B)  ;; 43
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  44) #\x2C)  ;; 44
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  45) #\x2D)  ;; 45
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  46) #\x2E)  ;; 46
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  47) #\x2F)  ;; 47
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  48) #\x30)  ;; 48
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  49) #\x31)  ;; 49
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  50) #\x32)  ;; 50
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  51) #\x33)  ;; 51
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  52) #\x34)  ;; 52
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  53) #\x35)  ;; 53
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  54) #\x36)  ;; 54
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  55) #\x37)  ;; 55
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  56) #\x38)  ;; 56
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  57) #\x39)  ;; 57
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  58) #\x3A)  ;; 58
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  59) #\x3B)  ;; 59
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  60) #\x3C)  ;; 60
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  61) #\x3D)  ;; 61
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  62) #\x3E)  ;; 62
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  63) #\x3F)  ;; 63
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  64) #\x40)  ;; 64
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  65) #\x41)  ;; 65
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  66) #\x42)  ;; 66
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  67) #\x43)  ;; 67
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  68) #\x44)  ;; 68
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  69) #\x45)  ;; 69
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  70) #\x46)  ;; 70
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  71) #\x47)  ;; 71
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  72) #\x48)  ;; 72
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  73) #\x49)  ;; 73
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  74) #\x4A)  ;; 74
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  75) #\x4B)  ;; 75
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  76) #\x4C)  ;; 76
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  77) #\x4D)  ;; 77
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  78) #\x4E)  ;; 78
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  79) #\x4F)  ;; 79
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  80) #\x50)  ;; 80
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  81) #\x51)  ;; 81
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  82) #\x52)  ;; 82
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  83) #\x53)  ;; 83
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  84) #\x54)  ;; 84
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  85) #\x55)  ;; 85
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  86) #\x56)  ;; 86
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  87) #\x57)  ;; 87
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  88) #\x58)  ;; 88
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  89) #\x59)  ;; 89
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  90) #\x5A)  ;; 90
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  91) #\x5B)  ;; 91
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  92) #\x5C)  ;; 92
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  93) #\x5D)  ;; 93
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  94) #\x5E)  ;; 94
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  95) #\x5F)  ;; 95
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  96) #\x60)  ;; 96
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  97) #\x61)  ;; 97
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  98) #\x62)  ;; 98
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char  99) #\x63)  ;; 99
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 100) #\x64)  ;; 100
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 101) #\x65)  ;; 101
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 102) #\x66)  ;; 102
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 103) #\x67)  ;; 103
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 104) #\x68)  ;; 104
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 105) #\x69)  ;; 105
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 106) #\x6A)  ;; 106
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 107) #\x6B)  ;; 107
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 108) #\x6C)  ;; 108
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 109) #\x6D)  ;; 109
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 110) #\x6E)  ;; 110
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 111) #\x6F)  ;; 111
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 112) #\x70)  ;; 112
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 113) #\x71)  ;; 113
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 114) #\x72)  ;; 114
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 115) #\x73)  ;; 115
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 116) #\x74)  ;; 116
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 117) #\x75)  ;; 117
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 118) #\x76)  ;; 118
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 119) #\x77)  ;; 119
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 120) #\x78)  ;; 120
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 121) #\x79)  ;; 121
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 122) #\x7A)  ;; 122
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 123) #\x7B)  ;; 123
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 124) #\x7C)  ;; 124
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 125) #\x7D)  ;; 125
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 126) #\x7E)  ;; 126
(assert-equal? "R6RS hexadecimal char literal #4" (integer->char 127) #\x7F)  ;; 127

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

(assert-equal? "integer->char #2" "#\\nul"       (i->chlit 0))    ;; 0
(assert-equal? "integer->char #2" "#\\x01"       (i->chlit 1))    ;; 1
(assert-equal? "integer->char #2" "#\\x02"       (i->chlit 2))    ;; 2
(assert-equal? "integer->char #2" "#\\x03"       (i->chlit 3))    ;; 3
(assert-equal? "integer->char #2" "#\\x04"       (i->chlit 4))    ;; 4
(assert-equal? "integer->char #2" "#\\x05"       (i->chlit 5))    ;; 5
(assert-equal? "integer->char #2" "#\\x06"       (i->chlit 6))    ;; 6
(assert-equal? "integer->char #2" "#\\alarm"     (i->chlit 7))    ;; 7
(assert-equal? "integer->char #2" "#\\backspace" (i->chlit 8))    ;; 8
(assert-equal? "integer->char #2" "#\\tab"       (i->chlit 9))    ;; 9
(assert-equal? "integer->char #2" "#\\newline"   (i->chlit 10))   ;; 10
(assert-equal? "integer->char #2" "#\\vtab"      (i->chlit 11))   ;; 11
(assert-equal? "integer->char #2" "#\\page"      (i->chlit 12))   ;; 12
(assert-equal? "integer->char #2" "#\\return"    (i->chlit 13))   ;; 13
(assert-equal? "integer->char #2" "#\\x0e"       (i->chlit 14))   ;; 14
(assert-equal? "integer->char #2" "#\\x0f"       (i->chlit 15))   ;; 15
(assert-equal? "integer->char #2" "#\\x10"       (i->chlit 16))   ;; 16
(assert-equal? "integer->char #2" "#\\x11"       (i->chlit 17))   ;; 17
(assert-equal? "integer->char #2" "#\\x12"       (i->chlit 18))   ;; 18
(assert-equal? "integer->char #2" "#\\x13"       (i->chlit 19))   ;; 19
(assert-equal? "integer->char #2" "#\\x14"       (i->chlit 20))   ;; 20
(assert-equal? "integer->char #2" "#\\x15"       (i->chlit 21))   ;; 21
(assert-equal? "integer->char #2" "#\\x16"       (i->chlit 22))   ;; 22
(assert-equal? "integer->char #2" "#\\x17"       (i->chlit 23))   ;; 23
(assert-equal? "integer->char #2" "#\\x18"       (i->chlit 24))   ;; 24
(assert-equal? "integer->char #2" "#\\x19"       (i->chlit 25))   ;; 25
(assert-equal? "integer->char #2" "#\\x1a"       (i->chlit 26))   ;; 26
(assert-equal? "integer->char #2" "#\\esc"       (i->chlit 27))   ;; 27
(assert-equal? "integer->char #2" "#\\x1c"       (i->chlit 28))   ;; 28
(assert-equal? "integer->char #2" "#\\x1d"       (i->chlit 29))   ;; 29
(assert-equal? "integer->char #2" "#\\x1e"       (i->chlit 30))   ;; 30
(assert-equal? "integer->char #2" "#\\x1f"       (i->chlit 31))   ;; 31
(assert-equal? "integer->char #2" "#\\space"     (i->chlit 32))   ;; 32
(assert-equal? "integer->char #2" "#\\!"         (i->chlit 33))   ;; 33
(assert-equal? "integer->char #2" "#\\\""        (i->chlit 34))   ;; 34
(assert-equal? "integer->char #2" "#\\#"         (i->chlit 35))   ;; 35
(assert-equal? "integer->char #2" "#\\$"         (i->chlit 36))   ;; 36
(assert-equal? "integer->char #2" "#\\%"         (i->chlit 37))   ;; 37
(assert-equal? "integer->char #2" "#\\&"         (i->chlit 38))   ;; 38
(assert-equal? "integer->char #2" "#\\'"         (i->chlit 39))   ;; 39
(assert-equal? "integer->char #2" "#\\("         (i->chlit 40))   ;; 40
(assert-equal? "integer->char #2" "#\\)"         (i->chlit 41))   ;; 41
(assert-equal? "integer->char #2" "#\\*"         (i->chlit 42))   ;; 42
(assert-equal? "integer->char #2" "#\\+"         (i->chlit 43))   ;; 43
(assert-equal? "integer->char #2" "#\\,"         (i->chlit 44))   ;; 44
(assert-equal? "integer->char #2" "#\\-"         (i->chlit 45))   ;; 45
(assert-equal? "integer->char #2" "#\\."         (i->chlit 46))   ;; 46
(assert-equal? "integer->char #2" "#\\/"         (i->chlit 47))   ;; 47
(assert-equal? "integer->char #2" "#\\0"         (i->chlit 48))   ;; 48
(assert-equal? "integer->char #2" "#\\1"         (i->chlit 49))   ;; 49
(assert-equal? "integer->char #2" "#\\2"         (i->chlit 50))   ;; 50
(assert-equal? "integer->char #2" "#\\3"         (i->chlit 51))   ;; 51
(assert-equal? "integer->char #2" "#\\4"         (i->chlit 52))   ;; 52
(assert-equal? "integer->char #2" "#\\5"         (i->chlit 53))   ;; 53
(assert-equal? "integer->char #2" "#\\6"         (i->chlit 54))   ;; 54
(assert-equal? "integer->char #2" "#\\7"         (i->chlit 55))   ;; 55
(assert-equal? "integer->char #2" "#\\8"         (i->chlit 56))   ;; 56
(assert-equal? "integer->char #2" "#\\9"         (i->chlit 57))   ;; 57
(assert-equal? "integer->char #2" "#\\:"         (i->chlit 58))   ;; 58
(assert-equal? "integer->char #2" "#\\;"         (i->chlit 59))   ;; 59
(assert-equal? "integer->char #2" "#\\<"         (i->chlit 60))   ;; 60
(assert-equal? "integer->char #2" "#\\="         (i->chlit 61))   ;; 61
(assert-equal? "integer->char #2" "#\\>"         (i->chlit 62))   ;; 62
(assert-equal? "integer->char #2" "#\\?"         (i->chlit 63))   ;; 63
(assert-equal? "integer->char #2" "#\\@"         (i->chlit 64))   ;; 64
(assert-equal? "integer->char #2" "#\\A"         (i->chlit 65))   ;; 65
(assert-equal? "integer->char #2" "#\\B"         (i->chlit 66))   ;; 66
(assert-equal? "integer->char #2" "#\\C"         (i->chlit 67))   ;; 67
(assert-equal? "integer->char #2" "#\\D"         (i->chlit 68))   ;; 68
(assert-equal? "integer->char #2" "#\\E"         (i->chlit 69))   ;; 69
(assert-equal? "integer->char #2" "#\\F"         (i->chlit 70))   ;; 70
(assert-equal? "integer->char #2" "#\\G"         (i->chlit 71))   ;; 71
(assert-equal? "integer->char #2" "#\\H"         (i->chlit 72))   ;; 72
(assert-equal? "integer->char #2" "#\\I"         (i->chlit 73))   ;; 73
(assert-equal? "integer->char #2" "#\\J"         (i->chlit 74))   ;; 74
(assert-equal? "integer->char #2" "#\\K"         (i->chlit 75))   ;; 75
(assert-equal? "integer->char #2" "#\\L"         (i->chlit 76))   ;; 76
(assert-equal? "integer->char #2" "#\\M"         (i->chlit 77))   ;; 77
(assert-equal? "integer->char #2" "#\\N"         (i->chlit 78))   ;; 78
(assert-equal? "integer->char #2" "#\\O"         (i->chlit 79))   ;; 79
(assert-equal? "integer->char #2" "#\\P"         (i->chlit 80))   ;; 80
(assert-equal? "integer->char #2" "#\\Q"         (i->chlit 81))   ;; 81
(assert-equal? "integer->char #2" "#\\R"         (i->chlit 82))   ;; 82
(assert-equal? "integer->char #2" "#\\S"         (i->chlit 83))   ;; 83
(assert-equal? "integer->char #2" "#\\T"         (i->chlit 84))   ;; 84
(assert-equal? "integer->char #2" "#\\U"         (i->chlit 85))   ;; 85
(assert-equal? "integer->char #2" "#\\V"         (i->chlit 86))   ;; 86
(assert-equal? "integer->char #2" "#\\W"         (i->chlit 87))   ;; 87
(assert-equal? "integer->char #2" "#\\X"         (i->chlit 88))   ;; 88
(assert-equal? "integer->char #2" "#\\Y"         (i->chlit 89))   ;; 89
(assert-equal? "integer->char #2" "#\\Z"         (i->chlit 90))   ;; 90
(assert-equal? "integer->char #2" "#\\["         (i->chlit 91))   ;; 91
(assert-equal? "integer->char #2" "#\\\\"        (i->chlit 92))   ;; 92
(assert-equal? "integer->char #2" "#\\]"         (i->chlit 93))   ;; 93
(assert-equal? "integer->char #2" "#\\^"         (i->chlit 94))   ;; 94
(assert-equal? "integer->char #2" "#\\_"         (i->chlit 95))   ;; 95
(assert-equal? "integer->char #2" "#\\`"         (i->chlit 96))   ;; 96
(assert-equal? "integer->char #2" "#\\a"         (i->chlit 97))   ;; 97
(assert-equal? "integer->char #2" "#\\b"         (i->chlit 98))   ;; 98
(assert-equal? "integer->char #2" "#\\c"         (i->chlit 99))   ;; 99
(assert-equal? "integer->char #2" "#\\d"         (i->chlit 100))  ;; 100
(assert-equal? "integer->char #2" "#\\e"         (i->chlit 101))  ;; 101
(assert-equal? "integer->char #2" "#\\f"         (i->chlit 102))  ;; 102
(assert-equal? "integer->char #2" "#\\g"         (i->chlit 103))  ;; 103
(assert-equal? "integer->char #2" "#\\h"         (i->chlit 104))  ;; 104
(assert-equal? "integer->char #2" "#\\i"         (i->chlit 105))  ;; 105
(assert-equal? "integer->char #2" "#\\j"         (i->chlit 106))  ;; 106
(assert-equal? "integer->char #2" "#\\k"         (i->chlit 107))  ;; 107
(assert-equal? "integer->char #2" "#\\l"         (i->chlit 108))  ;; 108
(assert-equal? "integer->char #2" "#\\m"         (i->chlit 109))  ;; 109
(assert-equal? "integer->char #2" "#\\n"         (i->chlit 110))  ;; 110
(assert-equal? "integer->char #2" "#\\o"         (i->chlit 111))  ;; 111
(assert-equal? "integer->char #2" "#\\p"         (i->chlit 112))  ;; 112
(assert-equal? "integer->char #2" "#\\q"         (i->chlit 113))  ;; 113
(assert-equal? "integer->char #2" "#\\r"         (i->chlit 114))  ;; 114
(assert-equal? "integer->char #2" "#\\s"         (i->chlit 115))  ;; 115
(assert-equal? "integer->char #2" "#\\t"         (i->chlit 116))  ;; 116
(assert-equal? "integer->char #2" "#\\u"         (i->chlit 117))  ;; 117
(assert-equal? "integer->char #2" "#\\v"         (i->chlit 118))  ;; 118
(assert-equal? "integer->char #2" "#\\w"         (i->chlit 119))  ;; 119
(assert-equal? "integer->char #2" "#\\x"         (i->chlit 120))  ;; 120
(assert-equal? "integer->char #2" "#\\y"         (i->chlit 121))  ;; 121
(assert-equal? "integer->char #2" "#\\z"         (i->chlit 122))  ;; 122
(assert-equal? "integer->char #2" "#\\{"         (i->chlit 123))  ;; 123
(assert-equal? "integer->char #2" "#\\|"         (i->chlit 124))  ;; 124
(assert-equal? "integer->char #2" "#\\}"         (i->chlit 125))  ;; 125
(assert-equal? "integer->char #2" "#\\~"         (i->chlit 126))  ;; 126
(assert-equal? "integer->char #2" "#\\delete"    (i->chlit 127))  ;; 127

(total-report)
