;;; choosable.scm: An abstraction of user interaction about choosing something
;;; (model part of a MVC)
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
(require "event.scm")
(require "composer.scm")


;;
;; choosable
;;

(define choosable-mtbl-rec-spec
  '((nr-items      #f)
    (chosen        #f)
    (choose!       #f)
    (item-indicate #f)
    (item-status   #f)
    (item-ready?   #f)))
(define-record 'choosable-mtbl choosable-mtbl-rec-spec)

(define choosable-rec-spec
  '((id      #f)
    (owner   #f)    ;; must be a composer
    (methods #f)))
(define-record 'choosable choosable-rec-spec)

;; .returns Number of items. Must be positive or zero
(define choosable-nr-items
  (lambda (self)
    ((choosable-mtbl-nr-items (choosable-methods self)) self)))

;; .returns Item index currently chosen
(define choosable-chosen
  (lambda (self)
    ((choosable-mtbl-chosen (choosable-methods self)) self)))

;; .pre-condition idx is included in [-1,nr-items)
;; .parameter idx Index to choose. -1 indicates that nothing is chosen (!=
;; keep currently chosen).
(define choosable-choose!
  (lambda (self idx)
    (and (choosable-item-ready? self idx)
         ((choosable-mtbl-choose! (choosable-methods self)) self idx))))

;; .returns indication of nth item
(define choosable-item-indicate
  (lambda (self idx)
    ((choosable-mtbl-item-indicate (choosable-methods self)) self idx)))

;; .returns A status symbol 'selected 'checked or #f. See also action-status
(define choosable-item-status
  (lambda (self idx)
    ((choosable-mtbl-item-status (choosable-methods self)) self idx)))

(define choosable-item-ready?
  (lambda (self idx)
    ((choosable-mtbl-item-ready? (choosable-methods self)) self idx)))

(define choosable-identify
  (lambda (self chbl-id)
    (and self
	 (eq? chbl-id (choosable-id self))
	 self)))

(define choosable-raise-updated-event
  (lambda (self)
    (let ((ev (choosable-updated-event-new (choosable-id self) self)))
      (composer-raise-event (choosable-owner self) ev))))

(define choosable-raise-deactivated-event
  (lambda (self)
    (let ((ev (choosable-deactivated-event-new (choosable-id self) self)))
      (composer-raise-event (choosable-owner self) ev))))


;;
;; choosable-base
;;

(define choosable-base-item-ready-t
  (lambda (self item-idx)
    #t))

(define choosable-base-item-status-none
  (lambda (self item-idx)
    #f))

(define choosable-base-method-table
  (choosable-mtbl-new
   #f    ;; nr-items
   #f    ;; chosen
   #f    ;; choose!
   #f    ;; item-indicate
   choosable-base-item-status-none
   choosable-base-item-ready-t))
