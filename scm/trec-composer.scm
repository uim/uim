;;; trec-composer.scm: trec-based character composer
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(require "trec.scm")


(define trec-composer-matcher-new
  (lambda (event-match?)
    (lambda (ev-exp ev)
      (if (procedure? ev-exp)
	  (and-let* ((matched (ev-exp ev-exp ev)))  ;; vkey
	    (if (implicit-press-release? ev-exp)
		()))  ;; FIXME
	  (and (match? ev-exp ev)
	       TREC-VKEY-FIN)))))

(define trec-context-fetch-ref-time-new
  (lambda (tc)
    (lambda (key)
      (let ((route (trec-context-route tc)))
	(event-timestamp (trec-route-last-node route))))))

(define trec-vkey-interval-new
  (lambda (fetch-ref-time interval duration)
    (lambda (dummy-key-exp key)
      (and-let* ((ref (fetch-ref-time key))
		 (now (event-timestamp key))
		 ((uphase-within? now ref interval duration)))
	;; recycle when the key is not a dedicated timer event
	(if (eq? (event-type key)
		 'timer)
	    TREC-VKEY-FIN
	    TREC-VKEY-RECYCLE)))))

;; TODO: move to event.scm

;; approx. 1 hour 9 min in msec
(if (not (symbol-bound? 'UPHASE-MAX))  ;; allows platform-specific config
    (define UPHASE-MAX #x3fffff))

(define uphase-normalize
  (lambda (x)
    (bitwise-and UPHASE-MAX x)))

;; Judge whether now is a time between A and B.
;; Both now and reference are a cyclic-timestamp. To save memory consumption,
;; uim uses the cyclic-timestamp instead of absolute 64bit timestamps. now
;; and reference must not be distant UPHASE-MAX or more. Otherwise the result
;; will be invalid.
;;
;;   time-->
;;     |                  |              |
;;     |----------------->|------------->|
;;     |     interval     |   duration   |
;; reference              A              B
;;
;; .pre-condition now must be a timestamp after or equals to reference
;; .pre-condition interval must be positive or zero
;; .pre-condition duration must be positive or zero
(define uphase-within?
  (lambda (now reference interval . duration)
    (let* ((compensated-now (if (< now reference)
				(+ UPHASE-MAX 1 (uphase-normalize now))
				(uphase-normalize now)))
	   (from (+ (uphase-normalize reference)
		    interval))
	   (to (if (null? duration)
		   compensated-now
		   (+ from (car duration)))))
      (and (<= from compensated-now)
	   (<= compensated-now to)))))
