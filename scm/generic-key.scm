;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(define-key generic-begin-conv-key? " ")
(define-key generic-next-candidate-key? '(" " "down" "<Control>n" "<Control>N"))
(define-key generic-prev-candidate-key? '("up" "<Control>p" "<Control>P"))
(define-key generic-next-page-key? "next")
(define-key generic-prev-page-key? "prior")
(define-key generic-return-key? '("return" "<Control>m" "<Control>M"))
(define-key generic-commit-key? '("<Control>j" "<Control>J" generic-return-key?))
(define-key generic-cancel-key? '("escape" "<Control>g" "<Control>G"))
(define-key generic-backspace-key? '("backspace" "<Control>h" "<Control>H"))
(define-key generic-delete-key? '("delete" "<Control>d" "<Control>D"))
(define-key generic-kill-key? '("<Control>k" "<Control>K"))
(define-key generic-kill-backward-key? '("<Control>u" "<Control>U"))
(define-key generic-go-left-key? '(left-key? "<Control>b" "<Control>B"))
(define-key generic-go-right-key? '(right-key? "<Control>f" "<Control>F"))
(define-key generic-beginning-of-preedit-key? '("home" "<Control>a" "<Control>A"))
(define-key generic-end-of-preedit-key? '("end" "<Control>e" "<Control>E"))
;;
(define-key generic-on-key? '("zenkaku-hankaku" "<Shift> "))
(define-key generic-off-key? '("zenkaku-hankaku" "<Shift> "))
