;;; composer.scm: The composer framework for uim
;;;
;;; Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
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
(require "wlos.scm")
(require "i18n.scm")
(require "event.scm")
(require "ustr.scm")
;;(require "utext.scm")
(require "ng-action.scm")


;;
;; composer
;;
(define-class composer object
  '()
  '(;; object structure maintenance
    %parent                  ;; returns parent composer
    set-parent!              ;; store parent by composer-specific way
    finalize!                ;; finalize composer object
    ;; information about the composer
    idname                   ;; symbol that identifies the composer
    indication               ;; visual information about the composer
    current-locale           ;; indicates what is currently composing
    ;; core methods
    children                 ;; ustr of child composers
    filter-event!            ;; filter an event sent from parent
    filter-upward-event!     ;; filter an event sent from a child
    text-length              ;; text length counted in logical character
    text                     ;; preedit text (as utext)
    supply-surrounding-text  ;; returns surrounding text to a child
    held-events              ;; source of composed text
    ;; manipulator exportations
    action                   ;; fetch a blessed action
    choosable))              ;; fetch a choosable object

(define %composer-undefined-method
  (lambda (self . args)
    (error "composer: undefined method")))

;; Get a composer as parent of the composer for internal use
;;
;; This method is used by composer internally. And must not be invoked by user
;; defined composer method. Only defining actual getter method and storing it
;; into method table of subclass is allowed.
;;
;; .parameter self Abstract composer object
;; .returns Parent composer
(class-set-method! composer %parent
  (lambda (self)
    (%composer-undefined-method)))

;; Set a composer as parent of the composer
;; .parameter self Abstract composer object
;; .parameter parent A composer
;; .returns self
(class-set-method! composer set-parent!
  (lambda (self parent)
    (%composer-undefined-method)))

;; Finalize composer object
;;
;; Finalize the object appropriately including constructs outside Scheme. All
;; chidlren must also be finalized and should be orphaned (i.e. make
;; (composer-children self) empty). reset-event is not issued before invoking
;; this.
;;
;; .parameter self Abstract composer object
(class-set-method! composer finalize!
  (lambda (self)
    (%composer-undefined-method)))

;; FIXME: describe naming rule
;; Get a symbol for programs that uniquely identifies the class of composer
;; .parameter self Abstract composer object
;; .returns A symbol such as 'anthy
(class-set-method! composer idname
  (lambda (self)
    (%composer-undefined-method)))

;; Get a indication object providing information about the composer for human
;; .parameter self Abstract composer object
;; .returns An indication object
(class-set-method! composer indication
  (lambda (self)
    (%composer-undefined-method)))

;; Get a locale object that indicates what is currently composing
;;
;; This method is intended to be used for locale-aware treatment of parent.
;;
;; .parameter self Abstract composer object
;; .returns A locale object
(class-set-method! composer current-locale
  (lambda (self)
    (%composer-undefined-method)))

;; Get an ustr of child composers
;;
;; Result contains position information about active child. The position is
;; (- (ustr-cursor-pos result) 1). composer-active-child-idx and
;; composer-active-child are also provided to handle active child
;; conveniently.
;;
;; .parameter self Abstract composer object
;; .returns An ustr of child composers. empty ustr if no children
(class-set-method! composer children
  (lambda (self)
    (%composer-undefined-method)))

;; Filter an event sent from parent
;;
;; This method filters an event sent from parent (downward event) and reflect
;; it to state of the composer and descendants.
;;
;; .parameter self Abstract composer object
;; .parameter ev An downward event
;; .returns #f if not filtered. Otherwise filtered
(class-set-method! composer filter-event!
  (lambda (self ev)
    (%composer-undefined-method)))

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
(class-set-method! composer filter-upward-event!
  (lambda (self sender ev)
    (%composer-undefined-method)))

;; Get length of preedit text of composer
;;
;; This method is expected as efficient than (length (composer-text self)).
;;
;; .parameter self Abstract composer object
;; .returns Preedit length counted in logical char
(class-set-method! composer text-length
  (lambda (self)
    (%composer-undefined-method)))

;; Get preedit text of composer
;; .parameter self Abstract composer object
;; .parameter start Start position counted in logical char
;; .parameter len Length of text to get counted in logical char. -1 means end
;; of text
;; .returns utext
(class-set-method! composer text
  (lambda (self start len)
    (%composer-undefined-method)))

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
(class-set-method! composer supply-surrounding-text
  (lambda (self former-len latter-len)
    (%composer-undefined-method)))

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
(class-set-method! composer held-events
  (lambda (self start len)
    (%composer-undefined-method)))

;; Fetch a blessed action of the composer or descendants
;; .parameter self Abstract composer object
;; .parameter act-id A symbol as action ID
;; .returns concrete action object or #f if not found
(class-set-method! composer action
  (lambda (self act-id)
    (%composer-undefined-method)))

;; Fetch a choosable object of the composer or descendants
;; .parameter self Abstract composer object
;; .parameter cho-id A symbol as choosable ID
;; .returns choosable object or #f if not found
(class-set-method! composer choosable
  (lambda (self cho-id)
    (%composer-undefined-method)))

;; non-polymorphic methods

;; Send an upward event to parent
;; .parameter self Abstract composer object
;; .parameter ev An upward event
;; .returns Result of composer-filter-upward-event!
(define composer-raise-event
  (lambda (self ev)
    (composer-filter-upward-event! (composer-%parent self) self ev)))

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
    (composer-supply-surrounding-text (composer-%parent self) former-len latter-len)))

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
  (lambda (method self . method-args)
    (let ((child (composer-active-child self)))
      (and child
	   (apply method (cons child method-args))))))


;;
;; composer-base: Base implementation for ordinary composers
;;
(define-class composer-base composer
  '((%actset     #f)    ;; actionset
    (%parent-var #f)
    (%children   #f))
  '())

(define %make-vector-based-composer-base.orig make-vector-based-composer-base)
(define %make-list-based-composer-base.orig make-list-based-composer-base)

(define %make-make-composer-base
  (lambda (make.orig)
    (lambda (children . rest)
      (let-optionals* rest ((actset #f))
	(make.orig actset #f children)))))

(define make-vector-based-composer-base
  (%make-make-composer-base %make-vector-based-composer-base.orig))
(define make-list-based-composer-base
  (%make-make-composer-base %make-list-based-composer-base.orig))
(define make-composer-base make-vector-based-composer-base)

;; (Re)initialize composer-base part of derived object
;; Parent is kept untouched
(define composer-base-initialize!
  (lambda (self children . rest)
    (let-optionals* rest ((actset #f))
      (composer-base-set-%children! self children)
      (composer-base-set-%actset! self actset)
      self)))

(class-set-method! composer-base %parent
  (lambda (self)
    (composer-base-%parent-var self)))

(class-set-method! composer-base set-parent!
  (lambda (self parent)
    (composer-base-set-%parent-var! self parent)))

(class-set-method! composer-base finalize!
  (lambda (self)
    (map-ustr-whole composer-finalize! (composer-children self))
    (ustr-clear! (composer-base-children self))))

(class-set-method! composer-base idname
  (lambda (self)
    'composer-base))

(class-set-method! composer-base indication
  (lambda (self)
    (indication-new 'null
		    ""
		    "composer-base"
		    (N_ "An internal composer"))))

(class-set-method! composer-base current-locale
  (lambda (self)
    (composer-delegate-method composer-current-locale self)))

(class-set-method! composer-base children
  composer-base-%children)

(define composer-base-set-children! composer-base-set-%children!)

(class-set-method! composer-base filter-event!
  (lambda (self ev)
    (or (actionset-handle-event (composer-base-%actset self) self ev)
	(composer-delegate-method composer-filter-event! self ev))))

(class-set-method! composer-base filter-upward-event!
  (lambda (self sender ev)
    (or (actionset-handle-event (composer-base-%actset self) self ev)
	(composer-raise-event self ev))))

(class-set-method! composer-base text-length
  (lambda (self)
    (apply + (map-ustr-whole composer-text-length
			     (composer-children self)))))

(class-set-method! composer-base text
  (lambda (self start len)
    (let ((whole (append-map composer-text (composer-children self))))
      (sublist-rel whole start len))))

;; delegates to parent
(class-set-method! composer-base supply-surrounding-text
  composer-surrounding-text)

;; FIXME: don't count in events but in characters
(class-set-method! composer-base held-events
  (lambda (self start len)
    (let ((whole (append-map composer-held-events (composer-children self))))
      (sublist-rel whole start len))))

(class-set-method! composer-base action
  (lambda (self act-id)
      (or (actionset-fetch-action (composer-base-%actset self) self act-id)
	  (composer-delegate-method composer-action self act-id))))

(class-set-method! composer-base choosable
  (lambda (self cho-id)
    (composer-delegate-method composer-choosable self cho-id)))
