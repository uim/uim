;;; tutcode-rule-custom.scm: Customization variables for tutcode-rule.scm
;;;
;;; Copyright (c) 2012-2013 uim Project https://github.com/uim/uim
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require "i18n.scm")


;;; tutcode.scmでは、tutcode-rule.scm以外(tcode.scm等)を使う可能性があるので、
;;; tutcode-rule.scm固有の設定はtutcode-rule-custom.scmで行う。
;;; (tutcode-custom.scmではなく)
(define-custom-group 'tutcode-rule
                     (N_ "tutcode-rule")
                     (N_ "Settings for tutcode-rule.scm"))

(define-custom 'tutcode-rule-use-tutplus? #f
  '(tutcode-rule)
  '(boolean)
  (N_ "Use TUT+ Code which supports shin joyo kanji")
  (N_ "long description will be here."))

(define-custom 'tutcode-rule-uppercase-as-opposite-kana? #f
  '(tutcode-rule)
  '(boolean)
  (N_ "Use uppercase rule to input opposite kana")
  (N_ "long description will be here."))

(define-custom 'tutcode-rule-exclude-uppercase-for-katakana? #f
  '(tutcode-rule)
  '(boolean)
  (N_ "Exclude uppercase katakana rule")
  (N_ "long description will be here."))

(define-custom 'tutcode-rule-exclude-uppercase-for-kigou-in-katakana? #f
  '(tutcode-rule)
  '(boolean)
  (N_ "Exclude uppercase kigou in katakana rule")
  (N_ "long description will be here."))

(custom-add-hook 'tutcode-rule-exclude-uppercase-for-kigou-in-katakana?
		 'custom-activity-hooks
		 (lambda ()
		   (not tutcode-rule-exclude-uppercase-for-katakana?)))

(custom-add-hook 'tutcode-rule-uppercase-as-opposite-kana?
		 'custom-activity-hooks
		 (lambda ()
		   (not tutcode-rule-exclude-uppercase-for-katakana?)))
