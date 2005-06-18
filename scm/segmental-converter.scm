;;; segmental-converter.scm: Abstraction for Japanese multi-segment conversion
;;; engines
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

(require "util.scm")
(require "composer.scm")


;;
;; segmental-converter
;;

;;
;; segconv-segment
;;

;;
;; segconv-engine
;;

(define segconv-engine-method-table-spec
  '((finalize!             #f)
    (reset!                #f)
    (set-source-str!       #f)
    (nr-segments           #f)
    (segment-source-length #f)
    (resize-segment!       #f)
    (commit-segment!       #f)
    (nr-candidates         #f)
    (nth-candidate         #f)))
(define-record 'segconv-engine-method-table segconv-engine-method-table-spec)

(define segconv-engine-spec
  '((methods #f)))

(define segconv-engine-finalize!
  (lambda (self)
    ((segconv-engine-method-table-finalize! self) self)))

(define segconv-engine-reset!
  (lambda (self)
    ((segconv-engine-method-table-reset! self) self)))

(define segconv-engine-set-source-str!
  (lambda (self str)
    ((segconv-engine-method-table-set-source-str! self) self str)))

(define segconv-engine-nr-segments
  (lambda (self)
    ((segconv-engine-method-table-nr-segments self) self)))

;; segment length counted in source string
(define segconv-engine-segment-source-length
  (lambda (self seg-idx)
    ((segconv-engine-method-table-segment-source-length self) self seg-idx)))

(define segconv-engine-resize-segment!
  (lambda (self seg-idx offset)
    ((segconv-engine-method-table-resize-segment! self) self seg-idx offset)))

(define segconv-engine-commit-segment!
  (lambda (self seg-idx cand-idx)
    ((segconv-engine-method-table-commit-segment! self) self seg-idx cand-idx)))

(define segconv-engine-nr-candidates
  (lambda (self seg-idx)
    ((segconv-engine-method-table-nr-candidates self) self seg-idx)))

(define segconv-engine-nth-candidate
  (lambda (self seg-idx cand-idx)
    ((segconv-engine-method-table-nth-candidate self) self seg-idx cand-idx)))


;;
;; anthy-engine
;;

;; TODO: move into anthy.scm

(define anthy-lib-initialized? #f)

(define anthy-engine-finalize!
  (lambda (self)
    (anthy-lib-free-context (anthy-engine-ac-id self))
    (anthy-engine-set-ac-id! -1)))

(define anthy-engine-reset!
  (lambda (self)
    (anthy-lib-free-context (anthy-engine-ac-id self))
    (anthy-engine-set-ac-id! (or (anthy-lib-alloc-context)
				 -1))))

(define anthy-engine-set-source-str!
  (lambda (self str)
    (anthy-lib-set-string (anthy-engine-ac-id self) str)))

(define anthy-engine-nr-segments
  (lambda (self)
    (anthy-lib-get-nr-segments (anthy-engine-ac-id self))))

(define anthy-engine-segment-source-length
  (lambda (self seg-idx)
    (anthy-lib-get-segment-length (anthy-engine-ac-id self) seg-idx)))

(define anthy-engine-resize-segment!
  (lambda (self seg-idx offset)
    (anthy-lib-resize-segment (anthy-engine-ac-id self) seg-idx offset)))

(define anthy-engine-commit-segment!
  (lambda (self seg-idx cand-idx)
    (anthy-lib-commit-segment (anthy-engine-ac-id self) seg-idx cand-idx)))

(define anthy-engine-nr-candidates
  (lambda (self seg-idx)
    (anthy-lib-get-nr-candidates (anthy-engine-ac-id self) seg-idx)))

(define anthy-engine-nth-candidate
  (lambda (self seg-idx cand-idx)
    (anthy-lib-get-nth-candidate (anthy-engine-ac-id self) seg-idx cand-idx)))

(define anthy-engine-method-table
  (segconv-engine-method-table-new
   anthy-engine-finalize!
   anthy-engine-reset!
   anthy-engine-set-source-str!
   anthy-engine-nr-segments
   anthy-engine-segment-source-length
   anthy-engine-resize-segment!
   anthy-engine-commit-segment!
   anthy-engine-nr-candidates
   anthy-engine-nth-candidate))

(define anthy-engine-spec
  (append
   segconv-engine-spec
   '((ac-id -1))))
(define-record 'anthy-engine anthy-engine-spec)
(define anthy-engine-new-internal anthy-engine-new)

(define anthy-engine-new
  (lambda ()
    (if (not anthy-lib-initialized?)
	(set! anthy-lib-initialized? (and (symbol-bound? 'anthy-lib-init)
					  (anthy-lib-init))))
    (let ((ac-id (and anthy-lib-initialized?
		      (anthy-lib-alloc-context))))
      (and ac-id
	   (anthy-engine-new anthy-engine-methods ac-id)))))


;;
;; canna-engine
;;

;; TODO: move into canna.scm

(define canna-lib-initialized? #f)

(define canna-engine-finalize!
  (lambda (self)
    (canna-lib-release-context (canna-engine-cc-id self))
    (canna-engine-set-cc-id! -1)))

(define canna-engine-reset!
  (lambda (self)
    (canna-lib-reset-context (canna-engine-cc-id self))))

(define canna-engine-set-source-str!
  (lambda (self str)
    (canna-lib-begin-conversion (canna-engine-cc-id self) str)))

(define canna-engine-nr-segments
  (lambda (self)
    (canna-lib-get-nr-segments (canna-engine-cc-id self))))

(define canna-engine-segment-source-length
  (lambda (self seg-idx)
    ;; TODO: implement canna-lib-get-segment-source-length using RkGetYomi()
    ;;(canna-lib-get-segment-source-length (canna-engine-cc-id self) seg-idx)))
    0))

(define canna-engine-resize-segment!
  (lambda (self seg-idx offset)
    (canna-lib-resize-segment (canna-engine-cc-id self) seg-idx offset)))

;; commit all segments even if called for one segment
(define canna-engine-commit-segment!
  (lambda (self seg-idx cand-idx)
    (canna-lib-commit-segment (canna-engine-cc-id self) seg-idx cand-idx)))

(define canna-engine-nr-candidates
  (lambda (self seg-idx)
    (canna-lib-get-nr-candidates (canna-engine-cc-id self) seg-idx)))

(define canna-engine-nth-candidate
  (lambda (self seg-idx cand-idx)
    (canna-lib-get-nth-candidate (canna-engine-cc-id self) seg-idx cand-idx)))

(define canna-engine-method-table
  (segconv-engine-method-table-new
   canna-engine-finalize!
   canna-engine-reset!
   canna-engine-set-source-str!
   canna-engine-nr-segments
   canna-engine-segment-source-length
   canna-engine-resize-segment!
   canna-engine-commit-segment!
   canna-engine-nr-candidates
   canna-engine-nth-candidate))

(define canna-engine-spec
  (append
   segconv-engine-spec
   '((cc-id -1))))
(define-record 'canna-engine canna-engine-spec)
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
