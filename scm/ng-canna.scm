;;; ng-canna.scm: A Japanese multi-segment converter Canna (next generation)
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
(require "ustr.scm")
(require "utext.scm")
(require "segmental-converter.scm")

(define canna-lib-initialized? #f)
(define canna-default-locale (locale-new "ja_JP.EUC-JP"))
(define canna-default-utext-props (list (cons 'locale anthy-default-locale)))
(define canna-intrinsic-transposition-hiragana? #f)    ;; RK_XFER
(define canna-intrinsic-transposition-katakana? #f)    ;; RK_KFER
;;(define canna-intrinsic-transposition-halfkana? #f)
(define canna-intrinsic-transposition-half-alnum? #f)  ;; RK_HFER
(define canna-intrinsic-transposition-full-alnum? #f)  ;; RK_ZFER


;;
;; canna-engine
;;

(define canna-engine-finalize!
  (lambda (self)
    (canna-lib-release-context (canna-engine-cc-id self))
    (canna-engine-set-cc-id! -1)))

(define canna-engine-reset!
  (lambda (self)
    (canna-lib-reset-context (canna-engine-cc-id self))))

(define canna-engine-set-source-str!
  (lambda (self utext)
    (canna-lib-begin-conversion (canna-engine-cc-id self)
				(utext->eucjp-string utext))))

(define canna-engine-commit!
  (lambda (self)
    (canna-lib-commit-segment (canna-engine-cc-id self) 'dummy 'dummy)))

(define canna-engine-nr-segments
  (lambda (self)
    (canna-lib-get-nr-segments (canna-engine-cc-id self))))

(define canna-engine-segment-source-length
  (lambda (self seg-idx)
    ;; TODO: implement canna-lib-get-segment-source-length using RkGetYomi()
    ;;(canna-lib-get-segment-source-length (canna-engine-cc-id self) seg-idx)))
    0))

;; TODO: support other than -1 and 1 for offset
(define canna-engine-resize-segment!
  (lambda (self seg-idx offset)
    (canna-lib-resize-segment (canna-engine-cc-id self) seg-idx offset)
    seg-idx))

(define canna-engine-nr-candidates
  (lambda (self seg-idx)
    (canna-lib-get-nr-candidates (canna-engine-cc-id self) seg-idx)))

;; TODO: get proper cand-idx by RkGetStat()
(define canna-engine-candidate-index
  (lambda (self seg-idx)
    0))

;; TODO: support partial commit
(define canna-engine-set-candidate-index!
  (lambda (self seg-idx cand-idx commit)
    (if commit
	(canna-lib-commit-segment (canna-engine-cc-id self) seg-idx cand-idx)
	;; TODO: set cand-idx by RkXfer()
	)))

(define canna-engine-candidate
  (lambda (self seg-idx cand-idx)
    (let ((cc-id (canna-engine-cc-id self))
	  (str (canna-lib-get-nth-candidate cc-id seg-idx cand-idx)))
      (eucjp-string->utext str))))


(define canna-engine-method-table
  (segconv-engine-method-table-new
   canna-engine-finalize!
   canna-engine-reset!
   canna-engine-set-source-str!
   canna-engine-commit!
   canna-engine-nr-segments
   canna-engine-segment-source-length
   canna-engine-resize-segment!
   canna-engine-nr-candidates
   canna-engine-candidate-index
   canna-engine-set-candidate-index!
   canna-engine-candidate))

(define canna-engine-rec-spec
  (append
   segconv-engine-rec-spec
   '((cc-id -1))))  ;; canna-context-id
(define-record 'canna-engine canna-engine-rec-spec)
(define canna-engine-new-internal canna-engine-new)

(define canna-engine-new
  (lambda ()
    (if (not canna-lib-initialized?)
	(set! canna-lib-initialized? (and (symbol-bound? 'canna-lib-init)
					  (canna-lib-init canna-server-name))))
    (let ((cc-id (and canna-lib-initialized?
		      (canna-lib-alloc-context))))
      (and cc-id
	   (canna-engine-new canna-engine-methods cc-id)))))
