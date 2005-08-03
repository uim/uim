;;; chooser.scm: An abstraction of user interaction about choosing something
;;; (controller part of a MVC)
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

;; TODO:
;; - describe standard choosable classes such as chbl_candidates,
;;   chbl_input_modes, chbl_input_methods and chbl_preconv_input_methods
;; - write test
;;
;; - specify chooser widget position flexibly by utext properties of preedit
;; - support nested chooser
;; - support scope of nested chooser

(require "util.scm")
(require "i18n.scm")
(require "event.scm")
(require "ng-action.scm")
(require "composer.scm")
(require "choosable.scm")


;;
;; chooser
;;

(define chooser-item-heading-info-rec-spec
  '((heading-figure-id #f)    ;; figure-id (i.e. icon name) for heading area
    (heading-label     ())))  ;; utext for heading area (used if !figure-id)
(define-record 'chooser-item-heading-info chooser-item-heading-info-rec-spec)

;; indication-rec is placed at tail to reduce copying list through
;; a construction of chooser-item-indication
(define chooser-item-indication-rec-spec
  (append
   '((ready  #t)   ;; ready to be chosen
     (status #f))  ;; 'selected 'checked #f
   chooser-item-heading-info-rec-spec
   indication-rec-spec))
(define-record 'chooser-item-indication chooser-item-indication-rec-spec)

(define chooser-mtbl-rec-spec
  (append
   composer-mtbl-rec-spec
   '((reset!             #f)
     (choose!            #f)
     (item-head          #f)
     (indicate-status    #f)
     (indicate-title     #f)
     (scope-top          #f)
     (scope-size         #f)
     (set-scope!         #f)
     (shift-scope!       #f)
     (activate-widget!   #f)
     (deactivate-widget! #f)
     (widget-active?     #f))))
(define-record 'chooser-mtbl chooser-mtbl-rec-spec)

(define chooser-rec-spec
  (append
   composer-base-rec-spec
   '(;;(bound-choosable #f)  ;; stored as opaque0 of composer-base
     (tied-choosables ())
     (tied-widget-id  #f))))
(define-record 'chooser chooser-rec-spec)
(define chooser-bound-choosable composer-base-opaque0)
(define chooser-set-bound-choosable! composer-base-set-opaque0!)
(define chooser-new-internal chooser-new)

;; .parameter methods
;; .parameter children
;; .parameter actset (optional)
(define chooser-new
  (lambda args
    (let ((obj (chooser-new-internal)))
      (apply chooser-initialize! (cons obj args)))))

;; .parameter self A chooser object
;; .parameter methods
;; .parameter children
;; .parameter actset (optional)
;; .returns Initialized self
(define chooser-initialize!
  (lambda args
    (let ((self (car args)))
      (apply composer-base-initialize! args)
      (chooser-reset! self)
      self)))

(define chooser-finalize!
  (lambda (self)
    (chooser-finish-choice! self)
    (composer-base-finalize! self)))

(define chooser-idname
  (lambda (self)
    'chooser))

(define chooser-indication
  (lambda (self)
    (internal-construct-indication-new '("chooser"))))

(define chooser-filter-event!
  (lambda (self ev)
    (or (chooser-handle-event! self ev)
	(composer-base-filter-event! self ev))))

(define chooser-filter-upward-event!
  (lambda (self sender ev)
    (or (chooser-handle-event! self ev)
        (composer-raise-event self ev))))

;; Subclass implementation must reset widget-suppression-count,
;; scope-size and so on
(define chooser-reset!
  (lambda (self)
    ((chooser-mtbl-reset! (chooser-methods self)) self)))

;; .parameter idx Index to choose (over-range value must be clamped). -1
;; indicates no spot.
;; .returns A bool indicates succeeded or not
(define chooser-choose!
  (lambda (self idx)
    ((chooser-mtbl-choose! (chooser-methods self)) self idx)))

(define chooser-item-head
  (lambda (self item-idx)
    ((chooser-mtbl-item-head (chooser-methods self)) self item-idx)))

;; .returns Indication for title bar of chooser-widget. #f indicates
;; no title.
(define chooser-indicate-title
  (lambda (self)
    ((chooser-mtbl-indicate-title (chooser-methods self)) self)))

;; .returns Indication for status line of chooser-widget. #f indicates
;; no status.
(define chooser-indicate-status
  (lambda (self)
    ((chooser-mtbl-indicate-status (chooser-methods self)) self)))

(define chooser-scope-top
  (lambda (self)
    ((chooser-mtbl-scope-top (chooser-methods self)) self)))

(define chooser-scope-size
  (lambda (self)
    ((chooser-mtbl-scope-size (chooser-methods self)) self)))

;; Update scope state
;; .parameter self A chooser object
;; .parameter top Index number of top of the scope. Negative value instructs
;; that keep current value
;; .parameter size Number of items displayable in the scope at a
;; time. Negative value or 0 instructs that keep current value
(define chooser-set-scope!
  (lambda (self top size)
    ((chooser-mtbl-set-scope! (chooser-methods self)) self top size)))

;; Shift scope
;; .parameter self A chooser object
;; .parameter direction An integer value instructs shift direction. Subclass
;; method will be passed clamped value -1, 0 or 1.
(define chooser-shift-scope!
  (lambda (self direction)
    ((chooser-mtbl-shift-scope! (chooser-methods self)) self (clamp direction -1 1))))

;; .returns #f if already active
(define chooser-activate-widget!
  (lambda (self)
    ((chooser-mtbl-activate-widget! (chooser-methods self)) self)))

;; .returns #f if already inactive
(define chooser-deactivate-widget!
  (lambda (self)
    ((chooser-mtbl-deactivate-widget! (chooser-methods self)) self)))

(define chooser-widget-active?
  (lambda (self)
    ((chooser-mtbl-widget-active? (chooser-methods self)) self)))

;; .pre-condition Choosable must be bound
(define chooser-nr-items
  (compose choosable-nr-items chooser-bound-choosable))

;; .pre-condition Choosable must be bound
(define chooser-chosen
  (compose choosable-chosen chooser-bound-choosable))

(define chooser-move-chosen!
  (lambda (self offset)
    (and (chooser-bound-choosable self)
	 (let* ((chosen (chooser-chosen self))
		(idx (if (negative? chosen)
                         (chooser-scope-top self)  ;; no spot
                         chosen))
                (new-idx (chooser-compensate-index self (+ idx offset))))
           (if (not (chooser-scope-relative-index new-idx))
               (chooser-shift-scope! self offset))
	   (chooser-choose! self new-idx)))))

(define chooser-finish-choice!
  (lambda (self)
    (begin
      (chooser-set-bound-choosable! self #f)
      (chooser-deactivate-widget! self))))

;; .returns #t
(define chooser-refresh-widget!
  (lambda (self)
    (let ((top (chooser-scope-top self))
	  (size (chooser-actual-scope-size self)))
      (chooser-update-widget! self #t top size))))

;; .returns #t
(define chooser-update-widget!
  (lambda (self init? items-top nr-items)
    (if (chooser-bound-choosable self)
	(let ((trans (if (and init?
			      (chooser-widget-active? self))
			 'activate
			 'update)))
	  (chooser-raise-update-event self init? trans items-top nr-items)
	  #t)
	(chooser-deactivate-widget! self))))

(define chooser-handle-event!
  (lambda (self ev)
    (case (event-type ev)
      ((reset)
       (chooser-finish-choice! self)
       #f)  ;; pass through

      ((focus-in)
       (chooser-refresh-widget! self)
       #f)  ;; pass through

      ((focus-out)
       (chooser-deactivate-widget! self)
       #f)  ;; pass through

      ((chooser)
       (chooser-handle-chooser-event! self ev))

      ((chooser-update-req)
       (chooser-handle-chooser-update-req-event! self ev))

      ((choosable-updated)
       (chooser-handle-choosable-updated-event! self ev))

      ((choosable-deactivated)
       (chooser-handle-choosable-deactivated-event! self ev))

      (else
       #f))))

;; .pre-condition ev is a chooser-event
(define chooser-handle-chooser-event!
  (lambda (self ev)
    (and (eq? (chooser-tied-widget-id self)
	      (chooser-event-widget-id ev))
	 (chooser-bound-choosable self)
	 (begin
	   (chooser-set-scope! self (chooser-event-scope-top ev) -1)
	   (let ((updated (chooser-choose! self (chooser-event-chosen ev))))
	     (or (and (chooser-event-finish ev)
		      (chooser-finish-choice! self))
		 updated
		 (chooser-update-widget! self #f -1 -1)))))))

;; .pre-condition ev is a chooser-update-req-event
(define chooser-handle-chooser-update-req-event!
  (lambda (self ev)
    (and (eq? (chooser-tied-widget-id self)
	      (chooser-update-req-event-widget-id ev))
	 (chooser-bound-choosable self)
	 (chooser-update-widget! self
				 (chooser-update-req-event-initialize ev)
				 (chooser-update-req-event-items-top ev)
				 (chooser-update-req-event-nr-items ev)))))

;; .pre-condition ev is a choosable-updated-event
(define chooser-handle-choosable-updated-event!
  (lambda (self ev)
    (and (chooser-tied-choosable? self (choosable-updated-event-choosable-id ev))
	 (let ((sender (choosable-updated-event-sender ev)))
	   (if (eq? (chooser-bound-choosable self)
		    sender)
	       (chooser-update-widget! self #f -1 -1)
	       (begin
		 (chooser-set-bound-choosable! self sender)
		 (chooser-reset! self)
		 (chooser-refresh-widget! self)))))))

;; .pre-condition ev is a choosable-deactivated-event
(define chooser-handle-choosable-deactivated-event!
  (lambda (self ev)
    (and (chooser-tied-choosable? self (choosable-deactivated-event-choosable-id ev))
	 (eq? (chooser-bound-choosable self)
	      (choosable-updated-event-sender ev))
	 (chooser-finish-choice! self))))

(define chooser-raise-update-event
  (lambda (self init? trans items-top nr-items)
    (let ((ev (chooser-update-event-new self init? trans items-top nr-items)))
      (composer-raise-event (choosable-owner self) ev))))

(define chooser-update-event-new
  (lambda (self init? trans items-top nr-items)
    (let ((choosable (chooser-bound-choosable self)))
      (chooser-update-event-new (chooser-tied-widget-id self)
				init?
				trans
				(choosable-nr-items choosable)
				(choosable-chosen choosable)
				(chooser-scope-top self)
				(chooser-scope-size self)
				(chooser-title self)
				(chooser-status self)
				items-top
				(map (lambda (idx)
				       (chooser-item-indicate self idx))
				     (iota nr-items items-top))))))

;; .pre-condition Choosable must be bound
(define chooser-item-indicate
  (lambda (self idx)
    (let ((choosable (chooser-bound-choosable self)))
      (append
       (list
	(choosable-item-ready? choosable)
	(choosable-item-status choosable))
       (chooser-item-head self idx)
       (choosable-item-indicate choosable idx)))))

;; Tests if the choosable-class is tied with this chooser
;; .returns Passed choosable-id or #f
(define chooser-tied-choosable?
  (lambda (self choosable-id)
    (safe-car (memq choosable-id (chooser-tied-choosables self)))))

;; .pre-condition Choosable must be bound
;; .parameter self A chooser object
;; .parameter idx Item index. Negative value instructs (abs idx)
;; items before from bottom of the scope
(define chooser-compensate-index
  (lambda (self idx)
    (compensate-index idx (chooser-nr-items self))))

;; scope management

(define chooser-scope-bottom
  (lambda (self)
    (chooser-compensate-index self -1)))

(define chooser-scope-nr-segments
  (lambda (self)
    (inc (/ (chooser-nr-items self)
	    (chooser-scope-size self)))))

(define chooser-scope-segment-index
  (lambda (self)
    (/ (chooser-chosen self)
       (chooser-scope-size self))))

(define chooser-scope-relative-index
  (lambda (self item-idx)
    (let ((rel-idx (- item-idx (chooser-scope-top self))))
      (and (<= 0 rel-idx)
	   (< rel-idx (chooser-scope-size self))
	   rel-idx))))

(define chooser-choose-scope-relative-item!
  (lambda (self rel-idx)
    (and (< rel-idx (chooser-scope-size self))
         (let ((abs-idx (+ (chooser-scope-top self)
                           rel-idx)))
           (chooser-choose! self abs-idx)))))

(define chooser-actual-scope-size
  (lambda (self)
    (let ((scope-size (chooser-scope-size self))
          (tail-size (- (chooser-nr-items self)
                        (chooser-chosen self))))
      (min scope-size tail-size))))

(define chooser-resize-scope!
  (lambda (self offset)
    (let* ((size (chooser-scope-size self))
           (new-size (max 1 (+ size offset))))
      (chooser-set-scope! self -1 new-size))))

;; Move scope by specifying absolute position
;; .parameter self A chooser object
;; .parameter top Item index. Negative value instructs (abs top)
;; items before from bottom of the scope
;; .returns New index as absolute value
(define chooser-move-scope!
  (lambda (self top)
    (let ((compensated-top (chooser-compensate-index self top)))
      (chooser-set-scope! self compensated-top -1)
      compensated-top)))

;; .parameter self A chooser object
;; .parameter n Scope index. Negative value instructs (last - n)th
;; segment of scope
(define chooser-scope-go-nth-segment!
  (lambda (self n)
    (let* ((compensated-n (compensate-index n (chooser-scope-nr-segments self)))
           (top (* n (chooser-scope-size self))))
      (chooser-move-scope! self top))))

;;
;; chooser-base
;;

(define chooser-base-rec-spec
  (append
   chooser-rec-spec
   '((scope-top  0)
     (scope-size 10))))
(define-record 'chooser-base chooser-base-rec-spec)
(define chooser-base-new-internal chooser-base-new)

;; .parameter methods
;; .parameter children
;; .parameter actset (optional)
(define chooser-base-new
  (lambda args
    (let ((obj (chooser-base-new-internal)))
      (apply chooser-initialize! (cons obj args)))))

(define chooser-base-choose!
  (lambda (self idx)
    (let ((choosable (chooser-bound-choosable self)))
      (and choosable
	   (let ((new-idx (clamp idx -1 (dec (chooser-nr-items self)))))
	     (choosable-choose! choosable new-idx)
	     (or (chooser-activate-widget! self)
		 (chooser-update-widget! self #f -1 -1))
	     #t)))))

(define chooser-base-set-scope!
  (lambda (self top size)
    (if (not (negative? top))
	(chooser-set-scope-top! self top))
    (if (positive? size)
	(chooser-set-scope-size! self (min size (chooser-nr-items self))))))

(define chooser-base-activate-widget!
  (lambda (self)
    (chooser-refresh-widget! self)))

(define chooser-base-deactivate-widget!
  (lambda (self)
    (chooser-reset! self)
    (chooser-raise-update-event self #f 'deactivate -1 -1)))

(define chooser-base-method-table
  (let ((m (chooser-mtbl-new composer-base-method-table)))
    (chooser-mtbl-set-finalize!!            m chooser-finalize!)
    (chooser-mtbl-set-idname!               m chooser-idname)
    (chooser-mtbl-set-indication!           m chooser-indication)
    (chooser-mtbl-set-filter-event!!        m chooser-filter-event!)
    (chooser-mtbl-set-filter-upward-event!! m chooser-filter-upward-event!)
    (chooser-mtbl-set-reset!!               m #f)
    (chooser-mtbl-set-choose!!              m chooser-base-choose!)
    (chooser-mtbl-set-indicate-status!      m #f)
    (chooser-mtbl-set-indicate-title!       m #f)
    (chooser-mtbl-set-item-head!            m #f)
    (chooser-mtbl-set-scope-top!            m chooser-base-scope-top)
    (chooser-mtbl-set-scope-size!           m chooser-base-scope-size)
    (chooser-mtbl-set-set-scope!!           m chooser-base-set-scope!)
    (chooser-mtbl-set-shift-scope!!         m #f)
    (chooser-mtbl-set-activate-widget!!     m chooser-base-activate-widget!)
    (chooser-mtbl-set-deactivate-widget!!   m chooser-base-deactivate-widget!)
    (chooser-mtbl-set-widget-active?!       m #f)
    m))


;;
;; replaceable chooser parts
;;

(define chooser-item-heading-info-none (chooser-item-heading-info-new))

(define chooser-heading-info-tbl-num10
  '((lkey_1 ("1")) (lkey_2 ("2")) (lkey_3 ("3")) (lkey_4 ("4"))
    (lkey_5 ("5")) (lkey_6 ("6")) (lkey_7 ("7")) (lkey_8 ("8"))
    (lkey_9 ("9")) (lkey_0 ("0"))))

(define chooser-heading-info-tbl-asdf7
  '((lkey_a ("a")) (lkey_s ("s")) (lkey_d ("d")) (lkey_f ("f"))
    (lkey_j ("j")) (lkey_k ("k")) (lkey_l ("l"))))

(define chooser-heading-info-tbl-asdf9
  '((lkey_a ("a")) (lkey_s ("s")) (lkey_d ("d")) (lkey_f ("f"))
    (lkey_g ("g")) (lkey_h ("h")) (lkey_j ("j")) (lkey_k ("k"))
    (lkey_l ("l"))))

(define chooser-heading-info-tbl-asdf10
  (append chooser-heading-info-tbl-asdf9 '((lkey_semicolon (";")))))


(define chooser-scope-rel-head
  (lambda (self item-idx tbl)
    (let ((scope-idx (chooser-scope-relative-index self item-idx)))
    (if (and (integer? scope-idx)
	     (not (negative? scope-idx))
	     (< item-idx (length tbl)))
	(nth scope-idx tbl)
	chooser-item-heading-info-none))))

;; Generates a evmap-ruleset to activate scope-relative choice actions. This
;; assumes that heading-info-figure-id of tbl element is a logical key.
;; .parameter tbl A list of chooser-item-heading-info
(define chooser-scope-rel-choice-action-ruleset
  (lambda (tbl)
    (map (lambda (i head)
           (list (list (chooser-item-heading-info-figure-id head))
                 (list (chooser-scope-rel-choice-action-id i))))
         (iota (length tbl))
         tbl)))

(define chooser-item-head-none
  (lambda (self item-idx)
    chooser-item-heading-info-none))

(define chooser-item-head-abs-num
  (lambda (self item-idx)
    (chooser-item-heading-info-new #f (list (number->string item-idx)))))

(define chooser-item-head-scope-num10
  (lambda (self item-idx)
    (chooser-scope-rel-head self item-idx chooser-heading-info-tbl-num10)))

(define chooser-item-head-scope-asdf7
  (lambda (self item-idx)
    (chooser-scope-rel-head self item-idx chooser-heading-info-tbl-asdf7)))

(define chooser-item-head-scope-asdf9
  (lambda (self item-idx)
    (chooser-scope-rel-head self item-idx chooser-heading-info-tbl-asdf9)))

(define chooser-item-head-scope-asdf10
  (lambda (self item-idx)
    (chooser-scope-rel-head self item-idx chooser-heading-info-tbl-asdf10)))

(define chooser-indicate-title-none
  (lambda (self)
    #f))

(define chooser-indicate-status-cur-idx
  (lambda (self)
    (let* ((chbl (chooser-bound-choosable self))
	   (label (string-append (number->string (choosable-chosen chbl))
				 " / "
				 (number->string (choosable-nr-items chbl)))))
      (indication-new 'none
		      ""
		      (list label)
		      (N_ "currently chosen / number of items")))))


;; Shift scope by spefifying relative item position
;; .parameter self A chooser object
;; .parameter offset Offset for item index
(define chooser-shift-scope-as-linear!
  (lambda (self offset)
    (chooser-move-scope! self (+ top offset))))

;; Shift scope by spefifying relative segment position
;; .parameter self A chooser object
;; .parameter offset Offset for relative segment index
(define chooser-shift-scope-as-relatively-segmented!
  (lambda (self offset)
    (let ((new-top (* offset (chooser-scope-size self))))
      (chooser-shift-scope-as-linear! self new-top))))

;; Shift scope by spefifying relative segment position
;; .parameter self A chooser object
;; .parameter offset Offset for segment index
(define chooser-shift-scope-as-segmented!
  (lambda (self offset)
    (let (new-idx (+ (chooser-scope-segment-index self)
                     offset))
      (chooser-scope-go-nth-segment! self new-idx))))


;;
;; std-chooser
;;

(define std-chooser-rec-spec
  (append
   chooser-base-rec-spec
   '((widget-suppression-count 0))))
(define-record 'std-chooser std-chooser-rec-spec)
(define std-chooser-new-internal std-chooser-new)

;; .parameter children
(define std-chooser-new
  (lambda (children)
    (let ((obj (std-chooser-new-internal)))
      (chooser-initialize! obj std-chooser-method-table children chooser-actionset))))

(define std-chooser-reset!
  (lambda (self)
    (chooser-base-set-scope-top! self 0)
    (chooser-base-set-scope-size! self 10)
    (std-chooser-set-widget-suppression-count! self 3)))

(define std-chooser-activate-widget!
  (lambda (self)
    (or (std-chooser-decrement-widget-suppression-count! self)
        (chooser-base-activate-widget! self))))

(define std-chooser-force-activate-widget!
  (lambda (self)
    (std-chooser-set-widget-suppression-count! self 0)
    (chooser-base-activate-widget! self)))

(define std-chooser-widget-active?
  (lambda (self)
    (zero? (std-chooser-widget-suppression-count self))))

;; .returns A bool indicates chooser-widget has been handled or not
(define std-chooser-decrement-widget-suppression-count!
  (lambda (self)
    (let* ((count (std-chooser-widget-suppression-count self))
	   (new-count (max 0 (- count 1))))
      (std-chooser-set-widget-suppression-count! self new-count)
      (and (positive? count)
	   (begin
	     (if (zero? new-count)
		 (chooser-base-activate-widget! self))
	     #t)))))

(define std-chooser-method-table
  (let ((m (copy-list chooser-base-method-table)))
    (chooser-mtbl-set-reset!!           m std-chooser-reset!)
    (chooser-mtbl-set-indicate-status!  m chooser-indicate-status-cur-idx)
    (chooser-mtbl-set-indicate-title!   m chooser-indicate-title-none)
    (chooser-mtbl-set-item-head!        m chooser-item-head-abs-num)
    (chooser-mtbl-set-shift-scope!!     m chooser-shift-scope-as-segmented!)
    (chooser-mtbl-set-activate-widget!! m std-chooser-activate-widget!)
    (chooser-mtbl-set-widget-active?!   m std-chooser-widget-active?)
    m))


;;
;; predefined actions
;;

(define chooser-action-skeleton-new
  (let ((ready? (compose chooser-bound-choosable action-owner)))
    (lambda (act-id label short-desc activate!)
      (std-action-skeleton-new act-id label short-desc activate! ready?))))

(define chooser-widget-action-skeleton-new
  (let ((ready? (compose chooser-widget-active? action-owner)))
    (lambda (act-id label short-desc activate!)
      (std-action-skeleton-new act-id label short-desc activate! ready?))))

(define chooser-scope-rel-choice-action-id
  (lambda (idx)
    (symbolconc 'act_chsr_choose_scope_rel_ (number->symbol idx))))

(define chooser-scope-rel-choice-action-labels
  '(((N_ "1st item of page")  . (N_ "Choose 1st item of page and finish"))
    ((N_ "2nd item of page")  . (N_ "Choose 2nd item of page and finish"))
    ((N_ "3rd item of page")  . (N_ "Choose 3rd item of page and finish"))
    ((N_ "4th item of page")  . (N_ "Choose 4th item of page and finish"))
    ((N_ "5th item of page")  . (N_ "Choose 5th item of page and finish"))
    ((N_ "6th item of page")  . (N_ "Choose 6th item of page and finish"))
    ((N_ "7th item of page")  . (N_ "Choose 7th item of page and finish"))
    ((N_ "8th item of page")  . (N_ "Choose 8th item of page and finish"))
    ((N_ "9th item of page")  . (N_ "Choose 9th item of page and finish"))
    ((N_ "10th item of page") . (N_ "Choose 10th item of page and finish"))))

(define chooser-actions
  (list
   assq
   (chooser-action-skeleton-new
    'act_chsr_next_item
    (N_ "Next item")
    (N_ "Choose next item")
    (lambda (act)
      (chooser-move-chosen! (action-owner act) 1)))

   (chooser-action-skeleton-new
    'act_chsr_prev_item
    (N_ "Previous item")
    (N_ "Choose previous item")
    (lambda (act)
      (chooser-move-chosen! (action-owner act) -1)))

   (chooser-action-skeleton-new
    'act_chsr_first_item
    (N_ "First item")
    (N_ "Choose first item")
    (lambda (act)
      (chooser-choose! (action-owner act) 0)))

   (chooser-action-skeleton-new
    'act_chsr_last_item
    (N_ "Last item")
    (N_ "Choose last item")
    (lambda (act)
      (let ((chsr (action-owner act)))
	(chooser-choose! chsr (chooser-nr-items chsr))))

   (chooser-action-skeleton-new
    'act_chsr_activate_widget
    (N_ "Show chooser")
    (N_ "Explicitly show chooser")
    (compose chooser-activate-widget! action-owner))

   (chooser-action-skeleton-new
    'act_chsr_deactivate_widget
    (N_ "Hide chooser")
    (N_ "Explicitly hide chooser")
    (compose chooser-deactivate-widget! action-owner))

   (chooser-action-skeleton-new
    'act_chsr_finish
    (N_ "Finish")
    (N_ "Finish choice with current item")
    (compose chooser-finish-choice! action-owner)))))

(define chooser-scope-actions
  (list
   (chooser-widget-action-skeleton-new
    'act_chsr_next_scope
    (N_ "Next page")
    (N_ "Go to next page of items")
    (lambda (act)
      (chooser-shift-scope-as-segmented! (action-owner act) 1)))

   (chooser-widget-action-skeleton-new
    'act_chsr_prev_scope
    (N_ "Previous page")
    (N_ "Go to previous page of items")
    (lambda (act)
      (chooser-shift-scope-as-segmented! (action-owner act) -1)))

   (chooser-widget-action-skeleton-new
    'act_chsr_first_scope
    (N_ "First page")
    (N_ "Go to first page of items")
    (lambda (act)
      (chooser-go-nth-scope! (action-owner act) 0)))

   (chooser-widget-action-skeleton-new
    'act_chsr_last_scope
    (N_ "Last page")
    (N_ "Go to last page of items")
    (lambda (act)
      (chooser-go-nth-scope! (action-owner act) -1)))))

(define chooser-scope-rel-choice-actions
  (map (lambda (i label-desc)
         (chooser-widget-action-skeleton-new
          (chooser-scope-rel-choice-action-id i)
          (car label-desc)
          (cdr label-desc)
          (lambda (act)
            (and (chooser-choose-scope-relative-item! (action-owner act) i)
                 (chooser-finish-choice! self)))))
       (iota (length chooser-scope-rel-choice-action-labels))
       chooser-scope-rel-choice-action-labels))

(define chooser-actionset
  (cons assq
        (append chooser-actions
                chooser-scope-actions
                chooser-scope-rel-choice-actions)))
