;;; ng-anthy.scm: A Japanese multi-segment converter Anthy (next generation)
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

(define anthy-lib-initialized? #f)
(define anthy-default-locale (locale-new "ja_JP.EUC-JP"))
(define anthy-default-utext-props (list (cons 'locale anthy-default-locale)))
(define anthy-intrinsic-transposition-hiragana? #f) ;; NTH_UNCONVERTED_CANDIDATE
(define anthy-intrinsic-transposition-katakana? #f)
(define anthy-intrinsic-transposition-halfkana? #f)
(define anthy-intrinsic-transposition-half-alnum? #f)
(define anthy-intrinsic-transposition-full-alnum? #f)

;;
;; anthy-engine
;;

(define anthy-engine-finalize!
  (lambda (self)
    (anthy-lib-free-context (anthy-engine-ac-id self))
    (anthy-engine-set-ac-id! -1)
    (ustr-clear! (anthy-engine-cand-indices self))))

;; TODO: rewrite with anthy_reset_context()
(define anthy-engine-reset!
  (lambda (self)
    ;;(anthy-lib-reset-context (anthy-engine-ac-id self))
    (anthy-lib-free-context (anthy-engine-ac-id self))
    (anthy-engine-set-ac-id! (or (anthy-lib-alloc-context)
				 -1))
    (ustr-clear! (anthy-engine-cand-indices self))))

(define anthy-engine-set-source-str!
  (lambda (self utext)
    (anthy-lib-set-string (anthy-engine-ac-id self)
			  (utext->eucjp-string utext))
    (let ((nsegs (segconv-engine-nr-segments self))
	  (cands (anthy-engine-cand-indices self)))
      (ustr-clear! cands)
      (ustr-set-latter-seq! cands (make-list nsegs 0)))))

(define anthy-engine-commit!
  (lambda (self)
    (append-map
     (lambda (seg-idx)
       (let ((cand-idx (segconv-engine-candidate-index seg-idx)))
	 (segconv-engine-set-candidate-index! self seg-idx cand-idx #t)))
     (iota (segconv-engine-nr-segments self)))))

(define anthy-engine-nr-segments
  (lambda (self)
    (- (anthy-lib-get-nr-segments (anthy-engine-ac-id self))
       (anthy-engine-nr-committed-segments self))))

(define anthy-engine-segment-source-length
  (lambda (self seg-idx)
    (let ((iseg-idx (anthy-engine-internal-seg-idx self seg-idx)))
      (anthy-lib-get-segment-length (anthy-engine-ac-id self) iseg-idx))))

(define anthy-engine-resize-segment!
  (lambda (self seg-idx offset)
    (let ((iseg-idx (anthy-engine-internal-seg-idx self seg-idx)))
      (anthy-lib-resize-segment (anthy-engine-ac-id self) iseg-idx offset)
      (let* ((new-nseg (anthy-lib-get-nr-segments ac-id))
	     (latter-nseg (- new-nseg iseg-idx))
	     (cands (anthy-engine-cand-indices self))
	     (orig-pos (ustr-cursor-pos cands)))
	(ustr-set-cursor-pos! cands iseg-idx)
	(ustr-set-latter-seq! cands (make-list latter-nseg 0))
	(ustr-set-cursor-pos! cands orig-pos)
	seg-idx))))  ;; seg-idx..last-idx have been invalidated

(define anthy-engine-nr-candidates
  (lambda (self seg-idx)
    (let ((iseg-idx (anthy-engine-internal-seg-idx self seg-idx)))
      (anthy-lib-get-nr-candidates (anthy-engine-ac-id self) iseg-idx))))

(define anthy-engine-candidate-index
  (lambda (self seg-idx)
    (ustr-nth (anthy-engine-internal-seg-idx self seg-idx)
	      (anthy-engine-cand-indices self))))

(define anthy-engine-set-candidate-index!
  (lambda (self seg-idx cand-idx commit)
    (let ((ac-id (anthy-engine-ac-id self))
	  (iseg-idx (anthy-engine-internal-seg-idx self seg-idx))
	  (cands (anthy-engine-cand-indices self)))
      (ustr-set-nth! cands iseg-idx cand-idx)
      (and commit
	   (let ((committer
		  (lambda (i)
		    (let ((iseg-idx (anthy-engine-internal-seg-idx self i))
			  (cand-idx (anthy-engine-candidate-index self i)))
		      (anthy-lib-commit-segment ac-id iseg-idx cand-idx)
		      (segconv-engine-candidate self i cand-idx))))
		 (utext (append-map committer (iota (+ seg-idx 1)))))
	     (ustr-set-cursor-pos! cands iseg-idx)
	     (and (ustr-cursor-at-end? cands)
		  (ustr-clear! cands))
	     utext)))))

(define anthy-engine-candidate
  (lambda (self seg-idx cand-idx)
    (let ((ac-id (anthy-engine-ac-id self))
	  (iseg-idx (anthy-engine-internal-seg-idx self seg-idx))
	  (str (anthy-lib-get-nth-candidate ac-id iseg-idx cand-idx)))
      (eucjp-string->utext str))))

;; for partial commission feature
(define anthy-engine-nr-committed-segments
  (lambda (self)
    (ustr-cursor-pos (anthy-engine-cand-indices self))))

;; for partial commission feature
(define anthy-engine-internal-seg-idx
  (lambda (self seg-idx)
    (+ (anthy-engine-nr-committed-segments self)
       seg-idx)))

(define anthy-engine-method-table
  (segconv-engine-mtbl-new
   anthy-engine-finalize!
   anthy-engine-reset!
   anthy-engine-set-source-str!
   anthy-engine-commit!
   anthy-engine-nr-segments
   anthy-engine-segment-source-length
   anthy-engine-resize-segment!
   anthy-engine-nr-candidates
   anthy-engine-candidate-index
   anthy-engine-set-candidate-index!
   anthy-engine-candidate))

(define anthy-engine-rec-spec
  (append
   segconv-engine-rec-spec
   '((ac-id        -1)  ;; anthy-context-id
     (cand-indices #f))))
(define-record 'anthy-engine anthy-engine-rec-spec)
(define anthy-engine-new-internal anthy-engine-new)

(define anthy-engine-new
  (lambda ()
    (if (not anthy-lib-initialized?)
	(set! anthy-lib-initialized? (and (symbol-bound? 'anthy-lib-init)
					  (anthy-lib-init))))
    (let ((ac-id (and anthy-lib-initialized?
		      (anthy-lib-alloc-context))))
      (and ac-id
	   (anthy-engine-new anthy-engine-methods ac-id (ustr-new))))))
