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

(define segconv-engine-method-table-rec-spec
  '((finalize!             #f)
    (reset!                #f)
    (set-source-str!       #f)
    (commit!               #f)
    (nr-segments           #f)
    (segment-source-length #f)
    (resize-segment!       #f)
    (nr-candidates         #f)
    (candidate-index       #f)
    (set-candidate-index!  #f)
    (candidate             #f)))
(define-record 'segconv-engine-method-table segconv-engine-method-table-rec-spec)

(define segconv-engine-rec-spec
  '((methods #f)))

(define segconv-engine-finalize!
  (lambda (self)
    ((segconv-engine-method-table-finalize! self) self)))

(define segconv-engine-reset!
  (lambda (self)
    ((segconv-engine-method-table-reset! self) self)))

(define segconv-engine-set-source-str!
  (lambda (self utexts)
    ((segconv-engine-method-table-set-source-str! self) self utexts)))

;; .returns Commit string as utext-list
(define segconv-engine-commit!
  (lambda (self)
    ((segconv-engine-method-table-commit! self) self)))

(define segconv-engine-nr-segments
  (lambda (self)
    ((segconv-engine-method-table-nr-segments self) self)))

;; segment length counted in source string
(define segconv-engine-segment-source-length
  (lambda (self seg-idx)
    ((segconv-engine-method-table-segment-source-length self) self seg-idx)))

;; side effect: invalidates nr-segments and all segment info
(define segconv-engine-resize-segment!
  (lambda (self seg-idx offset)
    ((segconv-engine-method-table-resize-segment! self) self seg-idx offset)))

(define segconv-engine-nr-candidates
  (lambda (self seg-idx)
    ((segconv-engine-method-table-nr-candidates self) self seg-idx)))

(define segconv-engine-candidate-index
  (lambda (self seg-idx)
    ((segconv-engine-method-table-candidate-index self) self seg-idx)))

;; side effect: invalidates nr-segments and all segment info
;; .parameter commit Instructs partial (sequencial) commit if #t
;; .returns Commit string as utext-list if commit is #t
(define segconv-engine-set-candidate-index!
  (lambda (self seg-idx cand-idx commit)
    ((segconv-engine-method-table-set-candidate-index! self) self seg-idx cand-idx commit)))

;; .returns Converted segment string as utext-list
(define segconv-engine-candidate
  (lambda (self seg-idx cand-idx)
    ((segconv-engine-method-table-candidate self) self seg-idx cand-idx)))
