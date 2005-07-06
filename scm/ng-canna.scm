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


;;
;; anthy-engine
;;

;; TODO: move into ng-anthy.scm

(require "i18n.scm")
(require "ustr.scm")

(define anthy-lib-initialized? #f)
(define anthy-default-locale (locale-new "ja_JP.EUC-JP"))
(define anthy-intrinsic-transposition-hiragana? #f) ;; NTH_UNCONVERTED_CANDIDATE
(define anthy-intrinsic-transposition-katakana? #f)
(define anthy-intrinsic-transposition-halfkana? #f)
(define anthy-intrinsic-transposition-half-alnum? #f)
(define anthy-intrinsic-transposition-full-alnum? #f)

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
  (lambda (self utexts)
    (anthy-lib-set-string (anthy-engine-ac-id self)
			  (string-append-map utext-str
					     utexts))
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
	(ustr-set-cursor-pos! cands orig-pos)))))

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
		 (utexts (append-map committer (iota (+ seg-idx 1)))))
	     (ustr-set-cursor-pos! cands iseg-idx)
	     (and (ustr-cursor-at-end? cands)
		  (ustr-clear! cands))
	     utexts)))))

(define anthy-engine-candidate
  (lambda (self seg-idx cand-idx)
    (let ((ac-id (anthy-engine-ac-id self))
	  (iseg-idx (anthy-engine-internal-seg-idx self seg-idx))
	  (str (anthy-lib-get-nth-candidate ac-id iseg-idx cand-idx)))
      (list (utext-new str anthy-default-locale ;;'((ruby . "ふりがな"))
		       )))))

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
  (segconv-engine-method-table-new
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


;;
;; canna-engine
;;

;; TODO: move into ng-canna.scm

(require "i18n.scm")

(define canna-lib-initialized? #f)
(define canna-default-locale (locale-new "ja_JP.EUC-JP"))
(define canna-intrinsic-transposition-hiragana? #f)    ;; RK_XFER
(define canna-intrinsic-transposition-katakana? #f)    ;; RK_KFER
;;(define canna-intrinsic-transposition-halfkana? #f)
(define canna-intrinsic-transposition-half-alnum? #f)  ;; RK_HFER
(define canna-intrinsic-transposition-full-alnum? #f)  ;; RK_ZFER

(define canna-engine-finalize!
  (lambda (self)
    (canna-lib-release-context (canna-engine-cc-id self))
    (canna-engine-set-cc-id! -1)))

(define canna-engine-reset!
  (lambda (self)
    (canna-lib-reset-context (canna-engine-cc-id self))))

(define canna-engine-set-source-str!
  (lambda (self utexts)
    (canna-lib-begin-conversion (canna-engine-cc-id self)
				(string-append-map utext-str utexts))))

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
    (canna-lib-resize-segment (canna-engine-cc-id self) seg-idx offset)))

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
      (list (utext-new str canna-default-locale)))))


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
