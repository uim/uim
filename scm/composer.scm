;;; composer.scm: The composer framework for uim
;;;
;;; Copyright (c) 2005-2008 uim Project http://code.google.com/p/uim/
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
(require "i18n.scm")
(require "event.scm")
(require "ustr.scm")
;;(require "utext.scm")
(require "ng-action.scm")


;;
;; composer
;;

;; To minimize memory consumption, composer record has a special field layout.
;; The method table is not stored at index 0 but 4. And index 0-3 is reserved
;; for subclass to hold context specific information. Arbitrary length tail of
;; composer instance can be shared between multiple instances as follows.
;;
;; (define foo-composer-shared-tail (list f2 f3 foo-methods))
;; (define foo-composer-new
;;   (lambda (f0 f1)
;;     (cons f0 (cons f1 foo-composer-shared-tail))))
;;
;; The primary variable 'parent' is relocateable to arbitrary field to support
;; per class different requirements about tail sharing.
;;
;; See comoser-base definitions as actual example.

(define composer-rec-spec
  '((opaque0        #f)    ;; reserved field for subclass (least common)
    (opaque1        #f)    ;; reserved field for subclass
    (opaque2        #f)    ;; reserved field for subclass
    (opaque3        #f)    ;; reserved field for subclass (most common)
    (methods        #f)))  ;; the position must be kept in subclass
(define-record 'composer composer-rec-spec)

;; Get a composer as parent of the composer for internal use
;;
;; This method is used by composer internally. And must not be invoked by user
;; defined composer method. Only defining actual getter method and storing it
;; into method table of subclass is allowed.
;;
;; .parameter self Abstract composer object
;; .returns Parent composer
(define composer-parent-internal
  (lambda (self)
    ((composer-mtbl-parent-internal (composer-methods self)) self)))

;; Set a composer as parent of the composer
;; .parameter self Abstract composer object
;; .parameter parent A composer
;; .returns self
(define composer-set-parent!
  (lambda (self)
    ((composer-mtbl-set-parent! (composer-methods self)) self) self))

;; Finalize composer object
;;
;; Finalize the object appropriately including constructs outside Scheme. All
;; chidlren must also be finalized and should be orphaned (i.e. make
;; (composer-children self) empty). reset-event is not issued before invoking
;; this.
;;
;; .parameter self Abstract composer object
(define composer-finalize!
  (lambda (self)
    ((composer-mtbl-finalize! (composer-methods self)) self)))

;; FIXME: describe naming rule
;; Get a symbol for programs that uniquely identifies the class of composer
;; .parameter self Abstract composer object
;; .returns A symbol such as 'anthy
(define composer-idname
  (lambda (self)
    ((composer-mtbl-idname (composer-methods self)) self)))

;; Get a indication object providing information about the composer for human
;; .parameter self Abstract composer object
;; .returns An indication object
(define composer-indication
  (lambda (self)
    ((composer-mtbl-indication (composer-methods self)) self)))

;; Get a locale object that indicates what is currently composing
;;
;; This method is intended to be used for locale-aware treatment of parent.
;;
;; .parameter self Abstract composer object
;; .returns A locale object
(define composer-current-locale
  (lambda (self)
    ((composer-mtbl-current-locale (composer-methods self)) self)))

;; Get an ustr of child composers
;;
;; Result contains position information about active child. The position is
;; (- (ustr-cursor-pos result) 1). composer-active-child-idx and
;; composer-active-child are also provided to handle active child
;; conveniently.
;;
;; .parameter self Abstract composer object
;; .returns An ustr of child composers. empty ustr if no children
(define composer-children
  (lambda (self)
    ((composer-mtbl-children (composer-methods self)) self)))

;; Filter an event sent from parent
;;
;; This method filters an event sent from parent (downward event) and reflect
;; it to state of the composer and descendants.
;;
;; .parameter self Abstract composer object
;; .parameter ev An downward event
;; .returns #f if not filtered. Otherwise filtered
(define composer-filter-event!
  (lambda (self ev)
    ((composer-mtbl-filter-event! (composer-methods self)) self ev)))

;; Filter an event sent from a child
;;
;; This method filters an event sent from a child (upward event) and reflect
;; it to state of the composer and descendants.
;;
;; .parameter self Abstract composer object
;; .parameter sender The child composer that sent the ev directly. sender is
;; not originated composer but adjacent child who has relayed it
;; .parameter ev An upward event
;; .returns #f if not filtered. Otherwise filtered
(define composer-filter-upward-event!
  (lambda (self sender ev)
    ((composer-mtbl-filter-upward-event! (composer-methods self)) self sender ev)))

;; Get length of preedit text of composer
;;
;; This method is expected as efficient than (length (composer-text self)).
;;
;; .parameter self Abstract composer object
;; .returns Preedit length counted in logical char
(define composer-text-length
  (lambda (self)
    ((composer-mtbl-text-length (composer-methods self)) self)))

;; Get preedit text of composer
;; .parameter self Abstract composer object
;; .parameter start Start position counted in logical char
;; .parameter len Length of text to get counted in logical char. -1 means end
;; of text
;; .returns utext
(define composer-text
  (lambda (self start len)
    ((composer-mtbl-text (composer-methods self)) self start len)))

;; Returns surrounding text in response to request from a child
;;
;; This procedure is intended to be used as returning surrounding text in
;; response to request from a child triggered by composer-surrounding-text. If
;; the composer does not have the serving capability about surrounding text,
;; the invocation should be forwarded to parent. This procedure should not be
;; invoked directly. Use composer-surrounding-text instead.
;;
;; .parameter self Abstract composer object
;; .parameter former-len Length of former part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .parameter latter-len Length of latter part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .returns ustr of uchar (editable utext)
(define composer-supply-surrounding-text
  (lambda (self former-len latter-len)
    ((composer-mtbl-supply-surrounding-text (composer-methods self)) self former-len latter-len)))

;; Get source event sequence of preedit text of composer
;;
;; This method is used to transfer original event sequence to another composer
;; such as transposer or fallback composers.
;;
;; The args start and len specifies the range of composed text counted in
;; logical character, then source events of the partial text returns. Note
;; that start and len does not specify event count.
;;
;; Lacking some fragments or containing fabricated events is acceptable
;; although this method should returns original event list. And returning null
;; list is also allowed as compromise.
;;
;; .parameter self Abstract composer object
;; .parameter start Start position in logical char
;; .parameter len Length of logical char list to get. -1 means end of text
;; .returns List of event
(define composer-held-events
  (lambda (self start len)
    ((composer-mtbl-held-events (composer-methods self)) self start len)))

;; Fetch a blessed action of the composer or descendants
;; .parameter self Abstract composer object
;; .parameter act-id A symbol as action ID
;; .returns concrete action object or #f if not found
(define composer-action
  (lambda (self act-id)
    ((composer-mtbl-action (composer-methods self)) self act-id)))

;; Fetch a choosable object of the composer or descendants
;; .parameter self Abstract composer object
;; .parameter cho-id A symbol as choosable ID
;; .returns choosable object or #f if not found
(define composer-choosable
  (lambda (self cho-id)
    ((composer-mtbl-choosable (composer-methods self)) self cho-id)))

;; non-polymorphic methods

;; Send an upward event to parent
;; .parameter self Abstract composer object
;; .parameter ev An upward event
;; .returns Result of composer-filter-upward-event!
(define composer-raise-event
  (lambda (self ev)
    (composer-filter-upward-event! (composer-parent-internal self) self ev)))

;; Get active child composer
;; .parameter self Abstract composer object
;; .returns A child composer
(define composer-active-child
  (lambda (self)
    (let ((children (composer-children self)))
      (and (not (ustr-cursor-at-beginning? children))
	   (ustr-cursor-backside children)))))

;; Get index of active child composer
;; .parameter self Abstract composer object
;; .returns index of active child
(define composer-active-child-idx
  (lambda (self)
    (let ((children (composer-children self)))
      (and (not (ustr-cursor-at-beginning? children))
	   (- (ustr-cursor-pos children) 1)))))

;; Get entire text of active child
;; .parameter self Abstract composer object
;; .returns utext
(define composer-active-text
  (lambda (self)
    (composer-text (composer-active-child self) 0 -1)))

;; Get texts around active child
;;
;; Since the interface is compatible with supply-surrounding-text, this method
;; can be set as supply-surrounding-text of composer method table. It makes
;; terminating upward forwarding of the method and returns the texts of this
;; composer as surrounding text.
;;
;; .parameter self Abstract composer object
;; .parameter former-len Length of former part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .parameter latter-len Length of latter part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .returns ustr of uchar (editable utext). ownership of the ustr is
;; transferred to caller and freely mutable
;; FIXME: add range clamping
;; TODO: make efficient
(define composer-inactive-texts
  (lambda (self former-len latter-len)
    (let* ((children (ustr-dup (composer-children self)))
	   (sur (ustr-cursor-delete-backside! children)))  ;; remove active
      (ustr-new (take-right (map-ustr-former sur) former-len)
		(list-head (map-ustr-latter composer-text sur) latter-len)))))

;; Retrieve surrounding text from parent composer
;;
;; This procedure retrieves surrounding text of this composer from
;; parent. Result does not contain text of the self. If the parent does not
;; have the serving capability about surrounding text, it will be forwarded
;; ascending until such composer has been found. Finally, application bridge
;; will supply the text if it has the capability.
;;
;; .parameter self Abstract composer object
;; .parameter former-len Length of former part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .parameter latter-len Length of latter part of the text counted in logical
;; character. -1 or longer than available text indicates that as long as
;; possible
;; .returns ustr of uchar (editable utext)
(define composer-surrounding-text
  (lambda (self former-len latter-len)
    (composer-supply-surrounding-text (composer-parent-internal self) former-len latter-len)))

;; Activate an action by name
;; .parameter self Abstract composer object
;; .parameter act-id A symbol as action ID
(define composer-action-activate!
  (lambda (self act-id)
    (composer-filter-event! self (action-event-new act-id))))

;; Forward a method invocation to active child
;; .parameter method A polymorphic method of composer
;; .parameter self Abstract composer object
;; .parameter ... args for the method
;; .returns Result of actual invocation of the forwarded method
(define composer-delegate-method
  (lambda args
    (let* ((method (car args))
	   (self (cadr args))
	   (method-args (cddr args))
	   (child (composer-active-child self))))
      (and child
	   (apply method (cons child method-args)))))

;; CAUTION: The field order WILL sometimes be changed without notification to
;; be optimized. Don't rely on current layout. See also the comment at end of
;; this file.
(define composer-mtbl-rec-spec
  '(;; object structure maintenance
    (parent-internal         #f)    ;; don't invoke this directly
    (set-parent!             #f)    ;; store parent by composer specific way
    (finalize!               #f)    ;; finalize composer object
    ;; information about the composer
    (idname                  #f)    ;; symbol that identifies the composer
    (indication              #f)    ;; visual information about the composer
    (current-locale          #f)    ;; indicates what is currently composing
    ;; core methods
    (children                #f)    ;; ustr of child composers
    (filter-event!           #f)    ;; filter an event sent from parent
    (filter-upward-event!    #f)    ;; filter an event sent from a child
    (text-length             #f)    ;; text length counted in logical character
    (text                    #f)    ;; preedit text (as utext)
    (supply-surrounding-text #f)    ;; returns surrounding text to a child
    (held-events             #f)    ;; source of composed text
    ;; manipulator exportations
    (action                  #f)    ;; fetch a blessed action
    (choosable               #f)))  ;; fetch a choosable object
(define-record 'composer-mtbl composer-mtbl-rec-spec)


;;
;; Useful base implementation of composer
;;

(define composer-base-rec-spec
  '((opaque0          #f)    ;; reserved field to hold context for subclass
    (private-children #f)    ;; don't touch this directly
    (private-parent   #f)    ;; don't touch this directly
    (actset           #f)    ;; actionset
    (methods          #f)))  ;; must be placed at same position of composer rec
(define-record 'composer-base composer-base-rec-spec)
(define composer-base-new-internal composer-base-new)

(define composer-base-new
  (lambda args
    (let ((methods (car args))
	  (children (cadr args))
	  (actset (and (not (null? (cddr args)))
		       (car (cddr args)))))
      (composer-base-new-internal methods #f children actset))))

;; (Re)initialize composer-base part of derived object
;; Parent is kept untouched
(define composer-base-initialize!
  (lambda args
    (let ((self (car args))
	  (methods (cadr args))
	  (children (car (cddr args)))
	  (actset (and (not (null? (cdr (cddr args))))
		       (cadr (cddr args)))))
      (composer-base-set-methods! self methods)
      (composer-base-set-private-children! self children)
      (composer-base-set-actset! self actset)
      self)))

(define composer-base-parent-internal composer-base-private-parent)
(define composer-base-set-parent! composer-base-set-private-parent!)

(define composer-base-finalize!
  (lambda (self)
    (map-ustr-whole composer-finalize! (composer-children self))
    (ustr-clear! (composer-base-children self))))

(define composer-base-idname
  (lambda (self)
    'composer-base))

(define composer-base-indication
  (lambda (self)
    (indication-new 'null
		    ""
		    "composer-base"
		    (N_ "An internal composer"))))

(define composer-base-current-locale
  (lambda (self)
    (composer-delegate-method composer-current-locale self)))

(define composer-base-children composer-base-private-children)
(define composer-base-set-children! composer-base-set-private-children!)

(define composer-base-filter-event!
  (lambda (self ev)
    (or (actionset-handle-event (composer-base-actset self) self ev)
	(composer-delegate-method composer-filter-event! self ev))))

(define composer-base-filter-upward-event!
  (lambda (self sender ev)
    (or (actionset-handle-event (composer-base-actset self) self ev)
	(composer-raise-event self ev))))

(define composer-base-text-length
  (lambda (self)
    (apply + (map-ustr-whole composer-text-length
			     (composer-children self)))))

(define composer-base-text
  (lambda (self start len)
    (let ((whole (append-map composer-text (composer-children self))))
      (sublist-rel whole start len))))

;; delegates to parent
(define composer-base-supply-surrounding-text composer-surrounding-text)

;; FIXME: don't count in events but in characters
(define composer-base-held-events
  (lambda (self start len)
    (let ((whole (append-map composer-held-events (composer-children self))))
      (sublist-rel whole start len))))

(define composer-base-action
  (lambda (self act-id)
      (or (actionset-fetch-action (composer-base-actset self) self act-id)
	  (composer-delegate-method composer-action self act-id))))

(define composer-base-choosable
  (lambda (self cho-id)
    (composer-delegate-method composer-choosable self cho-id)))

(define composer-base-method-table
  (composer-mtbl-new
   composer-base-finalize!
   composer-base-idname
   composer-base-indication
   composer-base-current-locale
   composer-base-children
   composer-base-filter-event!
   composer-base-filter-upward-event!
   composer-base-text-length
   composer-base-text
   composer-base-supply-surrounding-text
   composer-base-held-events
   composer-base-action
   composer-base-choosable))

;; To make your own method table, perform as follows. The copy-list
;; and setter combination is recommended to avoid being affected by
;; change of composer-mtbl definition about ordering or new
;; member.
;;
;;(define foo-composer-method-table
;;  (let ((m (copy-list composer-base-method-table)))
;;    (composer-mtbl-set-text!         m foo-composer-text)
;;    (composer-mtbl-set-filter-event! m foo-composer-filter-event!)
;;    m))
;;
;;(composer-base-new foo-composer-method-table ())
