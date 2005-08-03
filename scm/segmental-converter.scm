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

;; TODO: write test

(require "util.scm")
(require "utext.scm")
(require "event.scm")
(require "composer.scm")
(require "ng-action.scm")
(require "choosable.scm")


(define segmental-converter-styles
  '((segment        . ((underline . #t)))
    (active-segment . ((underline . #t)
		       (reverse   . #t)
		       (cursor    . #t)))))

;;
;; segconv-engine
;;

(define segconv-engine-mtbl-rec-spec
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
(define-record 'segconv-engine-mtbl segconv-engine-mtbl-rec-spec)

(define segconv-engine-rec-spec
  '((methods #f)))

(define segconv-engine-finalize!
  (lambda (self)
    ((segconv-engine-mtbl-finalize! self) self)))

(define segconv-engine-reset!
  (lambda (self)
    ((segconv-engine-mtbl-reset! self) self)))

(define segconv-engine-set-source-str!
  (lambda (self utext)
    ((segconv-engine-mtbl-set-source-str! self) self utext)))

;; .returns Commit string as utext
(define segconv-engine-commit!
  (lambda (self)
    ((segconv-engine-mtbl-commit! self) self)))

(define segconv-engine-nr-segments
  (lambda (self)
    ((segconv-engine-mtbl-nr-segments self) self)))

;; segment length counted in source string
(define segconv-engine-segment-source-length
  (lambda (self seg-idx)
    ((segconv-engine-mtbl-segment-source-length self) self seg-idx)))

;; side effect: invalidates nr-segments and segment info
;; .returns first segment index to be invalidated. ret..last-idx have
;; been invalidated
(define segconv-engine-resize-segment!
  (lambda (self seg-idx offset)
    ((segconv-engine-mtbl-resize-segment! self) self seg-idx offset)))

(define segconv-engine-nr-candidates
  (lambda (self seg-idx)
    ((segconv-engine-mtbl-nr-candidates self) self seg-idx)))

(define segconv-engine-candidate-index
  (lambda (self seg-idx)
    ((segconv-engine-mtbl-candidate-index self) self seg-idx)))

;; side effect: invalidates nr-segments and all segment info
;; .parameter commit Instructs partial (sequencial) commit if #t
;; .returns Commit string as utext if commit is #t
(define segconv-engine-set-candidate-index!
  (lambda (self seg-idx cand-idx commit)
    ((segconv-engine-mtbl-set-candidate-index! self) self seg-idx cand-idx commit)))

;; .returns Converted segment string as utext
(define segconv-engine-candidate
  (lambda (self seg-idx cand-idx)
    ((segconv-engine-mtbl-candidate self) self seg-idx cand-idx)))


;;
;; segmental-converter
;;

(define segmental-converter-rec-spec
  (append
   composer-base-rec-spec
   '(;;(preconv          #f)  ;; stored as opaque0 of composer-base
     (engine           #f)
     (converting?      #f)
     (overlay-new-proc #f))))
(define-record 'segmental-converter segmental-converter-rec-spec)
(define segmental-converter-preconv composer-base-opaque0)
(define segmental-converter-set-preconv! composer-base-set-opaque0!)
(define segmental-converter-new-internal segmental-converter-new)

(define segmental-converter-new
  (lambda (actset preconv engine overlay-new-proc)
    (let ((obj (segmental-converter-new-internal)))
      (composer-base-initialize! obj
				 segmental-converter-method-table
				 (ustr-new (list preconv))  ;; children
				 actset)
      (segmental-converter-initialize! obj preconv engine overlay-new-proc)
      obj)))

(define segmental-converter-initialize!
  (lambda (self preconv engine overlay-new-proc)
    (segmental-converter-set-preconv! self preconv)
    (segmental-converter-set-engine! self engine)
    (segmental-converter-set-overlay-new-proc! self overlay-new-proc)))

(define segmental-converter-clear-segments! composer-base-finalize!)

(define segmental-converter-finalize!
  (lambda (self)
    (segmental-converter-clear-segments! self)
    (composer-finalize! (segmental-converter-preconv self))
    (segmental-converter-set-preconv! self #f)
    (segconv-engine-finalize! (segmental-converter-engine self))
    (segmental-converter-set-engine! self #f)))

(define segmental-converter-idname
  (lambda (self)
    'segmental-converter))

(define segmental-converter-indication
  (lambda (self)
    (internal-construct-indication-new '("segmental-converter"))))

(define segmental-converter-filter-event!
  (lambda (self ev)
    (case (event-type ev)
      ((reset)
       (segmental-converter-reset! self))

      (else
       (composer-base-filter-event! self)))))

(define segmental-converter-text
  (lambda (self start len)
    (if (segmental-converter-converting? self)
	(let* ((sty-inact (assq-cdr 'segment segmental-converter-styles))
	       (sty-act (assq-cdr 'active-segment segmental-converter-styles))
	       (sur-texts (composer-inactive-texts self))
	       (former (map-ustr-former uchar-add-props sur-texts sty-inact))
	       (latter (map-ustr-latter uchar-add-props sur-texts sty-inact))
	       (act-seg (utext-add-props (composer-active-text self) sty-act)))
	  (sublist-rel (append former act-seg latter) start len))
	(composer-base-text self start len))))  ;; preconv

(define segmental-converter-reset!
  (lambda (self)
    (segmental-converter-clear-segments! self)
    (composer-base-filter-event! (segmental-converter-preconv self)
				 (reset-event-new))
    (segconv-engine-reset! (segmental-converter-engine self))
    (ustr-insert-elem! (composer-base-children self) preconv)
    (segmental-converter-set-converting?! #f)))

(define segmental-converter-begin-conv!
  (lambda (self)
    (let ((preconv (segmental-converter-preconv self)))
      (if (positive? (composer-text-length preconv))
	  (let ((engine (segmental-converter-engine self))
		(preconv-str (composer-text preconv)))
	    (ustr-clear! (segmental-converter-children self))
	    (segconv-engine-set-source-str! engine preconv-str)
	    (segmental-converter-refresh-segments! self 0)
	    (segmental-converter-set-converting?! self #t)
	    (composer-raise-event self (preedit-updated-event-new)))))))

(define segmental-converter-cancel-conv!
  (lambda (self)
    (segmental-converter-clear-segments! self)
    (ustr-insert-elem! (composer-children self)
		       (segmental-converter-preconv self))
    (segmental-converter-set-converting?! self #f)
    
    (segmental-converter-deactivate-choosable-cand! self)
    (composer-raise-event self (preedit-updated-event-new))))

(define segmental-converter-commit!
  (lambda (self)
    (let ((commit-str (composer-text self 0 -1)))  ;; reflect overlays
      (segconv-engine-commit! self)
      (segmental-converter-reset! self)
      (composer-raise-event self (commit-event-new commit-str)))))

(define segmental-converter-deactivate-choosable-cand!
  (lambda (self)
    (and (segmental-converter-converting? self)
	 (let ((chbl (composer-choosable self 'chbl_candidates)))
	   (and chbl
		(choosable-raise-deactivated-event chbl))))))

(define segmental-converter-cursor-correct-pos!
  (lambda (self)
    (let ((segments (composer-children self)))
      (if (and (ustr-cursor-at-beginning? segments)
	       (not (ustr-empty? segments)))
	  (ustr-cursor-move-forward! segments)))))

(define segmental-converter-cursor-set-pos!
  (lambda (self seg-idx)
    (segmental-converter-deactivate-choosable-cand! self)
    (ustr-set-cursor-pos! (composer-children self) seg-idx)
    (segmental-converter-cursor-correct-pos! self)))

(define segmental-converter-cursor-move!
  (lambda (self offset)
    (segmental-converter-deactivate-choosable-cand! self)
    (ustr-cursor-move! (composer-children self) offset)
    (segmental-converter-cursor-correct-pos! self)))

(define segmental-converter-cursor-go-last!
  (lambda (self)
    (segmental-converter-deactivate-choosable-cand! self)
    (ustr-cursor-move-end! (composer-children self))
    (segmental-converter-cursor-correct-pos! self)))

(define segmental-converter-refresh-segments!
  (lambda (self from)
    (let ((engine (segmental-converter-engine self))
	  (nr-segments (segconv-engine-nr-segments engine))
	  (new-latter (map (lambda (idx)
			     (segmental-converter-segment-new self idx))
			   (iota nr-segments from)))
	  (segments (composer-children self)))
      (map-ustr-latter composer-finalize! segments)
      (ustr-set-latter-seq! segments new-latter))))

(define segmental-converter-resize-segment!
  (lambda (self offset)
    (let* ((engine (segmental-converter-engine self))
	   (segments (composer-children self))
	   (cur-seg (ustr-cursor-pos segments))
	   (inval-from (segconv-engine-resize-segment! engine cur-seg offset)))
      (segmental-converter-refresh-segments! self inval-from)
      (segmental-converter-deactivate-choosable-cand self)
      (composer-raise-event self (preedit-updated-event-new)))))

(define segmental-converter-segment-start-pos
  (lambda (self seg-idx)
    (let ((engine (segmental-converter-engine self)))
      (apply + (map (lambda (idx)
		      (segconv-engine-segment-source-length engine idx))
		    (iota seg-idx))))))

(define segmental-converter-segment-new
  (lambda (self seg-idx)
    (let* ((segment (segconv-segment-new self seg-idx
					 segconv-segment-actionset))
	   (overlay-new (segmental-converter-overlay-new-proc self)))
      (composer-set-parent! (if overlay-new
				(overlay-new segment)
				segment)
			    self))))

(define segmental-converter-method-table
  (let ((m (copy-list composer-base-method-table)))
    (composer-mtbl-set-finalize!!     m segmental-converter-finalize!)
    (composer-mtbl-set-idname!        m segmental-converter-idname)
    (composer-mtbl-set-indication!    m segmental-converter-indication)
    (composer-mtbl-set-filter-event!! m segmental-converter-filter-event!)
    (composer-mtbl-set-text!          m segmental-converter-text)
    m))


;;
;; segconv-segment
;;

;; choosable-cand does not belong to segmental-converter but does to
;; segconv-segment, to allow extensible simultaneous candidate
;; selection
(define segconv-segment-rec-spec
  (append
   composer-base-rec-spec
   '(;;(controller #f)  ;; stored as opaque0 of composer-base
     (seg-idx    -1)
     (chbl-cand   #f))))
(define-record 'segconv-segment segconv-segment-rec-spec)
(define segconv-segment-controller composer-base-opaque0)
(define segconv-segment-set-controller! composer-base-set-opaque0!)
(define segconv-segment-new-internal segconv-segment-new)

(define segconv-segment-new
  (lambda (controller seg-idx actset)
    (let ((obj (segconv-segment-new-internal)))
      (composer-base-initialize! obj
				 segconv-segment-method-table
				 (ustr-new)  ;; children
				 actset)
      (segconv-segment-initialize! obj controller seg-idx)
      obj)))

(define segconv-segment-initialize!
  (lambda (self controller seg-idx)
    (segconv-segment-set-controller! self controller)
    (segconv-segment-set-seg-idx! self seg-idx)))

(define segconv-segment-idname
  (lambda (self)
    'segconv-segment))

(define segconv-segment-indication
  (lambda (self)
    (internal-construct-indication-new '("segconv-segment"))))

(define segconv-segment-filter-event!
  (lambda (self ev)
    (case (event-type ev)
      ((reset)
       (segconv-segment-set-cand-idx! self 0))

      ((focus-in)
       (choosable-raise-updated-event (segconv-segment-choosable-cand self))
       #f)  ;; pass through

      (else
       (or (choosable-handle-event (segconv-segment-choosable-cand self) ev)
	   (composer-base-filter-event! self ev))))))

(define segconv-segment-text-length
  (lambda (self)
    (utext-length (composer-text self 0 -1))))

;; TODO:
;; - acquire raw preconv text "ぶんせつ" instead of converted text "文節"
;; - provide switching method between above two style texts
(define segconv-segment-text
  (lambda (self start len)
    (let* ((cand-idx (segconv-segment-cand-idx self))
	   (cand (segconv-segment-candidate self cand-idx)))
      (sublist-rel cand start len))))

(define segconv-segment-preconv-text-length
  (lambda (self)
    (let ((engine (segconv-segment-engine self))
	  (seg-idx (segconv-segment-seg-idx self)))
      (segconv-engine-segment-source-length engine seg-idx))))

;; FIXME: clamp len
(define segconv-segment-preconv-text
  (lambda (self start len)
    (let* ((controller (segconv-segment-controller self))
	   (seg-idx (segconv-segment-seg-idx self))
	   (seg-start (+ (segmental-converter-segment-start-pos controller
								seg-idx)
			 start)))
      (composer-text (segconv-segment-preconv self) seg-start len))))

(define segconv-segment-held-events
  (lambda (self start len)
    (let* ((controller (segconv-segment-controller self))
	   (seg-idx (segconv-segment-seg-idx self))
	   (seg-start (+ (segmental-converter-segment-start-pos controller
								seg-idx)
			 start)))
      (composer-held-events (segconv-segment-preconv self) seg-start len))))

(define segconv-segment-choosable
  (lambda (self cho-id)
    (or (choosable-identify (segconv-segment-choosable-cand self) cho-id)
	(composer-base-choosable self))))

(define segconv-segment-engine
  (compose segmental-converter-engine segconv-segment-controller))

(define segconv-segment-preconv
  (compose segmental-converter-preconv segconv-segment-controller))

(define segconv-segment-nr-candidates
  (lambda (self)
    (let ((engine (segconv-segment-engine self))
	  (seg-idx (segconv-segment-seg-idx self)))
      (segconv-engine-nr-candidates engine seg-idx))))

(define segconv-segment-cand-idx
  (lambda (self)
    (segconv-engine-candidate-index (segconv-segment-engine self)
				    (segconv-segment-seg-idx self))))

(define segconv-segment-candidate
  (lambda (self cand-idx)
    (let ((engine (segconv-segment-engine self))
	  (seg-idx (segconv-segment-seg-idx self)))
      (segconv-engine-candidate engine seg-idx cand-idx))))

;; create on demand
(define segconv-segment-choosable-cand
  (lambda (self)
    (or (segconv-segment-chbl-cand self)
	(let ((chbl-cand (segconv-choosable-cand-new 'chbl_candidates self)))
	  (segconv-segment-set-choosable-cand! self chbl-cand)
	  chbl-cand))))

(define segconv-segment-method-table
  (let ((m (copy-list composer-base-method-table)))
    (composer-mtbl-set-idname!        m segconv-segment-idname)
    (composer-mtbl-set-indication!    m segconv-segment-indication)
    (composer-mtbl-set-filter-event!! m segconv-segment-filter-event!)
    (composer-mtbl-set-text-length!   m segconv-segment-text-length)
    (composer-mtbl-set-text!          m segconv-segment-text)
    (composer-mtbl-set-held-events!   m segconv-segment-held-events)
    (composer-mtbl-set-choosable!     m segconv-segment-choosable)
    m))


;;
;; segconv-choosable-cand
;;

(define segconv-choosable-cand-new
  (lambda (id owner)
    (choosable-new id owner segconv-choosable-cand-method-table)))

(define segconv-choosable-cand-nr-items
  (compose segconv-segment-nr-candidates choosable-owner))

(define segconv-choosable-cand-chosen
  (compose segconv-segment-cand-idx choosable-owner))

(define segconv-choosable-cand-choose!
  (lambda (self item-idx)
    (segconv-segment-set-cand-idx! (choosable-owner self) item-idx)))

(define segconv-choosable-cand-item-indicate
  (lambda (self item-idx)
    (let ((cand (segconv-segment-candidate (choosable-owner self) item-idx)))
      (label-indication-new cand))))

(define segconv-choosable-cand-method-table
  (let ((m (copy-list choosable-base-method-table)))
    (choosable-mtbl-set-nr-items!      m segconv-choosable-cand-nr-items)
    (choosable-mtbl-set-chosen!        m segconv-choosable-cand-chosen)
    (choosable-mtbl-set-choose!!       m segconv-choosable-cand-choose!)
    (choosable-mtbl-set-item-indicate! m segconv-choosable-cand-item-indicate)
    m))


;;
;; predefined actions
;;

(define segmental-converter-conv-action-ready?
  (compose segmental-converter-converting? action-owner))

(define segconv-converting-action-skeleton-new
  (lambda (act-id label short-desc activate! ready?)
    (std-action-skeleton-new act-id label short-desc activate!
			     segmental-converter-conv-action-ready?)))

(define segconv-segment-action-skeleton-new
  (lambda (act-id label short-desc activate! ready?)
    (std-action-skeleton-new act-id label short-desc activate! #f)))

(define segmental-converter-actionset
  (list
   assq
   (std-action-skeleton-new
    'act_std_begin_conv
    (N_ "Begin conversion")
    (N_ "Begin conversion")
    (compose segmental-converter-begin-conv! action-owner)
    (compose not segmental-converter-converting? action-owner))

   (segconv-converting-action-skeleton-new
    'act_std_cancel_conv
    (N_ "Cancel conversion")
    (N_ "Cancel conversion")
    (compose segmental-converter-cancel-conv! action-owner))

   (segconv-converting-action-skeleton-new
    'act_std_commit
    (N_ "Commit")
    (N_ "Commit")
    (compose segmental-converter-commit! action-owner))

   (segconv-converting-action-skeleton-new
    'act_segconv_extend_segment
    (N_ "Extend segment")
    (N_ "Extend segment")
    (lambda (act)
      (segmental-converter-resize-segment! (action-owner act) 1)))

   (segconv-converting-action-skeleton-new
    'act_segconv_shrink_segment
    (N_ "Shrink segment")
    (N_ "Shrink segment")
    (lambda (act)
      (segmental-converter-resize-segment! (action-owner act) -1)))

   (segconv-converting-action-skeleton-new
    'act_segconv_next_segment
    (N_ "Next segment")
    (N_ "Go to next segment")
    (lambda (act)
      (segmental-converter-cursor-move! (action-owner act) 1)))

   (segconv-converting-action-skeleton-new
    'act_segconv_prev_segment
    (N_ "Previous segment")
    (N_ "Go to previous segment")
    (lambda (act)
      (segmental-converter-cursor-move! (action-owner act) -1)))

   (segconv-converting-action-skeleton-new
    'act_segconv_first_segment
    (N_ "First segment")
    (N_ "Go to first segment")
    (lambda (act)
      (segmental-converter-cursor-set-pos! (action-owner act) 0)))

   (segconv-converting-action-skeleton-new
    'act_segconv_last_segment
    (N_ "Last segment")
    (N_ "Go to last segment")
    (compose segmental-converter-cursor-go-last! action-owner))))

(define segconv-segment-actionset
  (list
   assq
   (segconv-segment-action-skeleton-new
    'act_segconv_confirm_segment
    (N_ "Confirm segment")
    (N_ "Confirm segment")
    (compose segconv-segment-confirm! action-owner))

   (segconv-segment-action-skeleton-new
    'act_segconv_commit_segment
    (N_ "Commit segment")
    (N_ "Commit segment")
    (compose segconv-segment-commit! action-owner))

   (segconv-segment-action-skeleton-new
    'act_segconv_next_candidate
    (N_ "Next candidate")
    (N_ "Move to next candidate")
    ;;'act_chooser_next_item
    (lambda (act)
      (composer-action-activate! (action-owner act) 'act_chooser_next_item)))

   (segconv-segment-action-skeleton-new
    'act_segconv_prev_candidate
    (N_ "Previous candidate")
    (N_ "Move to previous candidate")
    ;;'act_chooser_prev_item
    (lambda (act)
      (composer-action-activate! (action-owner act) 'act_chooser_prev_item)))))
