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

;; This file is tested with revision 734 of new repository

(use test.unit)

(require "test/uim-test-utils")

(define-uim-test-case "testcase legacy-api-bridge"
  (setup
   (lambda ()
     (uim '(require "legacy-api-bridge.scm"))))

  ("test legacy-modifier->modifier"
   ;; no modifier
   (assert-equal (uim 'mod_None)
		 (uim '(legacy-modifier->modifier 0)))
   ;; single modifier
   (assert-equal (uim 'mod_Shift)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Shift_key key-state-alist))))
   (assert-equal (uim 'mod_Control)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Control_key key-state-alist))))
   (assert-equal (uim 'mod_Alt)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Alt_key key-state-alist))))
   (assert-equal (uim 'mod_Meta)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Meta_key key-state-alist))))
   (assert-equal (uim 'mod_Super)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Super_key key-state-alist))))
   (assert-equal (uim 'mod_Hyper)
		 (uim '(legacy-modifier->modifier
			(assq-cdr 'Hyper_key key-state-alist))))
   ;; duplexed modifier
   (assert-equal (uim '(bitwise-or mod_Shift mod_Control))
		 (uim '(legacy-modifier->modifier
			(bitwise-or (assq-cdr 'Shift_key key-state-alist)
				    (assq-cdr 'Control_key key-state-alist)))))
   (assert-equal (uim '(bitwise-or mod_Shift mod_Control mod_Alt))
		 (uim '(legacy-modifier->modifier
			(bitwise-or (assq-cdr 'Shift_key key-state-alist)
				    (assq-cdr 'Control_key key-state-alist)
				    (assq-cdr 'Alt_key key-state-alist)))))
   (assert-equal (uim '(bitwise-or mod_Shift mod_Control mod_Alt
				   mod_Super mod_Hyper))
		 (uim '(legacy-modifier->modifier
			(bitwise-or (assq-cdr 'Shift_key key-state-alist)
				    (assq-cdr 'Control_key key-state-alist)
				    (assq-cdr 'Alt_key key-state-alist)
				    (assq-cdr 'Super_key key-state-alist)
				    (assq-cdr 'Hyper_key key-state-alist))))))

  ("test legacy-key->key-event"
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_a
			     #f
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "a") 0 #t)))
   ;; enable physical-key mapping
   (uim '(require "physical-key.scm"))
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_a
			     'pkey_jp106_a
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "a") 0 #t)))
   ;; press/release
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_a
			     'pkey_jp106_a
			     mod_None
			     #f
			     #f))
		 (uim '(legacy-key->key-event (string->char "a") 0 #f)))
   ;; with single modifier
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_a
			     'pkey_jp106_a
			     mod_Shift
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "a")
					      (assq-cdr 'Shift_key
							key-state-alist)
					      #t)))
   ;; with duplexed modifier
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "a"
			     'lkey_a
			     'pkey_jp106_a
			     (bitwise-or mod_Shift mod_Control)
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "a")
					      (bitwise-or
					       (assq-cdr 'Shift_key
							 key-state-alist)
					       (assq-cdr 'Control_key
							 key-state-alist))
					      #t)))
   ;; capitalized alphabet
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "A"
			     'lkey_A
			     'pkey_jp106_a
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "A") 0 #t)))
   ;; ASCII symbol
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     "("
			     'lkey_parenleft
			     'pkey_jp106_8
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "(") 0 #t)))
   ;; ASCII space
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     " "
			     'lkey_space
			     'pkey_jp106_space
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char " ") 0 #t)))
   ;; symbolic key
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     #f
			     'lkey_BackSpace
			     'pkey_jp106_BackSpace
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event 'backspace 0 #t)))
   ;; invalid key
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     #f
			     #f
			     #f
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event 'nonexistent 0 #t)))
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     #f
			     #f
			     #f
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "\n") 0 #t)))
   (assert-equal (uim '(list 'key
			     #f
			     #f
			     -1
			     #f
			     #f
			     #f
			     mod_None
			     #t
			     #f))
		 (uim '(legacy-key->key-event (string->char "") 0 #t)))))
