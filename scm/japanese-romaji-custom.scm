;;; japanese-romaji-custom.scm: Customization variables for japanese-romaji
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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

(require "i18n.scm")


(define-custom-group 'ja-romaji
                     (_ "Japanese romaji")
                     (_ "Japanese romaji"))

;; subgroup
(define-custom-group 'ja-romaji-symbols
                     (_ "Symbolic characters for fullwidth kana modes")
                     (_ "Symbolic characters for fullwidth kana modes"))

;; subgroup
(define-custom-group 'ja-romaji-punctuations
                     (_ "Punctuation marks for fullwidth kana modes")
                     (_ "Punctuation marks for fullwidth kana modes"))

;;
;; Alphanumeric and symbolic characters
;;

(define-custom 'ja-romaji-fullwidth-space-ruleset
  'ja-fullwidth-space-ruleset
  '(ja-romaji ja-romaji-symbols)
  (list 'choice
	(list 'ja-halfwidth-space-ruleset
	      (_ "Halfwidth space")
	      (_ "Halfwidth space"))
	(list 'ja-fullwidth-space-ruleset
	      (_ "Fullwidth space")
	      (_ "Fullwidth space")))
  (_ "Space character")
  (_ "long description will be here."))

(define-custom 'ja-romaji-fullwidth-basic-symbol-ruleset
  'ja-fullwidth-basic-symbol-ruleset
  '(ja-romaji ja-romaji-symbols)
  (list 'choice
	(list 'ja-halfwidth-basic-symbol-ruleset
	      (_ "Halfwidth symbols")
	      (_ "Halfwidth symbols"))
	(list 'ja-fullwidth-basic-symbol-ruleset
	      (_ "Fullwidth symbols")
	      (_ "Fullwidth symbols")))
  (_ "Symbolic characters")
  (_ "long description will be here."))

(define-custom 'ja-romaji-fullwidth-number-ruleset
  'ja-fullwidth-number-ruleset
  '(ja-romaji ja-romaji-symbols)
  (list 'choice
	(list 'ja-halfwidth-number-ruleset
	      (_ "Halfwidth numbers")
	      (_ "Halfwidth numbers"))
	(list 'ja-fullwidth-number-ruleset
	      (_ "Fullwidth numbers")
	      (_ "Fullwidth numbers")))
  (_ "Numeric characters")
  (_ "long description will be here."))

;;
;; Punctuation marks
;;

(define-custom 'ja-romaji-fullwidth-kana-comma-ruleset
  'ja-fullwidth-kana-comma-ruleset
  '(ja-romaji ja-romaji-punctuations)
  (list 'choice
	(list 'ja-halfwidth-comma-ruleset
	      (_ "Halfwidth comma")
	      (_ "Halfwidth comma"))
	(list 'ja-fullwidth-comma-ruleset
	      (_ "Fullwidth comma")
	      (_ "Fullwidth comma"))
	(list 'ja-fullwidth-kana-comma-ruleset
	      (_ "Fullwidth kana comma")
	      (_ "Fullwidth kana comma")))
  (_ "Comma character")
  (_ "long description will be here."))

(define-custom 'ja-romaji-fullwidth-kana-period-ruleset
  'ja-fullwidth-kana-period-ruleset
  '(ja-romaji ja-romaji-punctuations)
  (list 'choice
	(list 'ja-halfwidth-period-ruleset
	      (_ "Halfwidth period")
	      (_ "Halfwidth period"))
	(list 'ja-fullwidth-period-ruleset
	      (_ "Fullwidth period")
	      (_ "Fullwidth period"))
	(list 'ja-fullwidth-kana-period-ruleset
	      (_ "Fullwidth kana full stop")
	      (_ "Fullwidth kana full stop")))
  (_ "Period character")
  (_ "long description will be here."))
