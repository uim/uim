#! /usr/bin/env sscm -C EUC-CN
;; -*- euc-cn -*-

;;  FileName : test-enc-eucgeneric.scm
;;  About    : unit test for EUC string
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

;; This file provides a fallback test unit for all EUC systems.  It's
;; just a copy of test-enc-eucjp.scm with EUCJP-specific character
;; sequences removed, so some characters may be undefined in other EUC
;; systems.
(assert-equal? "string 1" "Èþ¿Í¤Ë¤Ï" (string #\Èþ #\¿Í #\¤Ë #\¤Ï))
(assert-equal? "list->string 1" "3Æü¤Ç" (list->string '(#\3 #\Æü #\¤Ç)))
(assert-equal? "string->list 1" '(#\¤¡ #\¤­ #\¤ë) (string->list "¤¡¤­¤ë"))

(assert-equal? "string-ref 1" #\Êâ  (string-ref "»õhiÊâŽÍÊâ" 3))
(assert-equal? "make-string 1" "ÊâÊâÊâÊâÊâ"   (make-string 5 #\Êâ))
(assert-equal? "string-copy 1"     "¶â¶ä¹á"   (string-copy "¶â¶ä¹á"))
(assert-equal? "string-set! 1"     "¶â·Ë¶Ì"   (string-set!
                                               (string-copy "¶â·Ë¤È")
                                               2
                                               #\¶Ì))

(define str1 "¤¢¥ãahË½\\Ë½n!¡ù¡ý!")
(define str1-list '(#\¤¢ #\¥ã #\a #\h #\Ë½ #\\ #\Ë½ #\n #\! #\¡ù #\¡ý #\!))

(assert-equal? "string 2" str1 (apply string str1-list))
(assert-equal? "list->string 2" str1-list (string->list str1))

(total-report)
