#!/usr/bin/env gosh

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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;; This file is tested with revision 732 of new repository

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase evmap-csv"
  (setup
   (lambda ()
     (uim '(require "evmap-csv.scm"))))

  ("test evmap-csv-next-state"
   ;; elem state
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'elem "\n"))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'elem ""))))
   (assert-equal 'error
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'elem "\""))))
   (assert-equal 'list-elem
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'elem " "))))
   (assert-equal 'elem
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'elem "a"))))
   ;; string state
   (assert-equal 'neutral
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string "\""))))
   (assert-equal 'string-escaping
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string "\\"))))
   (assert-equal 'string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string "a"))))
   ;; string-escaping state
   (assert-equal 'string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string-escaping "a"))))
   (assert-equal 'string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string-escaping "\\"))))
   (assert-equal 'string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'string-escaping "\""))))
   ;; list-elem state
   (assert-equal 'neutral
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-elem ","))))
   (assert-equal 'list-elem
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-elem "a"))))
   (assert-equal 'error
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-elem "\""))))
   (assert-equal 'list-neutral
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-elem " "))))
   ;; string state
   (assert-equal 'list-neutral
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string "\""))))
   (assert-equal 'list-string-escaping
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string "\\"))))
   (assert-equal 'list-string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string "a"))))
   ;; string-escaping state
   (assert-equal 'list-string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string-escaping "a"))))
   (assert-equal 'list-string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string-escaping "\\"))))
   (assert-equal 'list-string
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'list-string-escaping "\""))))
   ;; fin state
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin "a"))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin ""))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin "\n"))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin "\""))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin " "))))
   (assert-equal 'fin
		 (uim '(evmap-csv-state-next-state (evmap-csv-next-state 'fin ","))))))
