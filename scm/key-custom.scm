;;; key-custom.scm: Customization variables for key inputs
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

(require "i18n.scm")


(define-custom-group 'keyboard
		     (_ "Keyboard settings")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'keyboard-env
		     (_ "System configuration of your keyboard")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'keyboard-translation
		     (_ "Key-input translation for uim input methods")
		     (_ "long description will be here."))

(define-custom 'system-physical-keyboard-type 'jp106
  '(keyboard keyboard-env)
  (list 'choice
	(list 'qwerty
	      (_ "Standard QWERTY")
	      (_ "Standard QWERTY"))
;;	(list 'latin-qwerty
;;	      (_ "Latin QWERTY")
;;	      (_ "Latin QWERTY which has dead-keys and multi key"))
	(list 'jp106
	      (_ "Japanese JP106 (QWERTY)")
	      (_ "Japanese JP106 (QWERTY)"))
;;	(list 'jp109
;;	      (_ "Japanese JP109 (QWERTY)")
;;	      (_ "Japanese JP109 (QWERTY)"))
;;	(list 'kinesis
;;	      (_ "KINESIS Contoured (QWERTY)")
;;	      (_ "KINESIS Contoured (QWERTY)"))
	)
  (_ "Physical keyboard type")
  (_ "long description will be here."))

(define-custom 'system-logical-key-mapping 'jp106-qwerty
  '(keyboard keyboard-env)
  (list 'choice
	(list 'qwerty
	      (_ "QWERTY")
	      (_ "QWERTY"))
	(list 'dvorak
	      (_ "Dvorak")
	      (_ "Dvorak"))
	(list 'jp106-qwerty
	      (_ "QWERTY (JP106)")
	      (_ "QWERTY (JP106"))
	(list 'jp106-dvorak
	      (_ "Dvorak (JP106)")
	      (_ "Dvorak (JP106")))
  (_ "Logical key mapping")
  (_ "long description will be here."))

;;(define-custom 'key-event-basic-translator 'jp106-qwerty
;;  '(keyboard keyboard-env)
;;  (list 'choice
;;	(list 'qwerty
;;	      (_ "QWERTY")
;;	      (_ "QWERTY"))
;;	(list 'dvorak
;;	      (_ "Dvorak")
;;	      (_ "Dvorak"))
;;	(list 'jp106-qwerty
;;	      (_ "QWERTY (JP106)")
;;	      (_ "QWERTY (JP106"))
;;	(list 'jp106-dvorak
;;	      (_ "Dvorak (JP106)")
;;	      (_ "Dvorak (JP106")))
;;  (_ "Basic key-input translation")
;;  (_ "long description will be here."))

(define-custom 'enable-jp106-henkan-muhenkan-shift? #f
  '(keyboard keyboard-translation)
  '(boolean)
  (_ "Use Henkan and Muhenkan as ordinary shift key")
  (_ "long description will be here."))

(define-custom 'enable-ja-nicola-jp106-pseudo-thumb-shift? #f
  '(keyboard keyboard-translation)
  '(boolean)
  (_ "Use Henkan and Muhenkan as NICOLA thumb shift keys")
  (_ "long description will be here."))

(define-custom 'enable-qwerty-shift->space? #f
  '(keyboard keyboard-translation)
  '(boolean)
  (_ "Use shift keys as space key (testing purpose)")
  (_ "long description will be here."))

(define-custom 'inspect-key-event-translation? #f
  '(keyboard keyboard-translation)
  '(boolean)
  (_ "Inspect key-event translation (for developer, use with LIBUIM_VERBOSE=1)")
  (_ "long description will be here."))

;;
;; modifier settings
;;

(define-custom-group 'modifier1
		     (_ "Key modifier settings 1")
		     (_ "Key modifier state translation settings for uim input methods (1)"))

(define-custom-group 'modifier2
		     (_ "Key modifier settings 2")
		     (_ "Key modifier state translation settings for uim input methods (2)"))

;; subgroup
(define-custom-group 'modifier-sticky
		     (_ "Sticky modifiers")
		     (_ "long description will be here."))

;; subgroup
(define-custom-group 'modifier-lock
		     (_ "Lockable modifiers")
		     (_ "long description will be here."))

(define-custom 'enable-modifier-translation? #f
  '(modifier1)
  '(boolean)
  (_ "Enable modifier state translation based on uim's own rule")
  (_ "long description will be here."))

(define-custom 'use-sticky-shift? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Shift key")
  (_ "long description will be here."))

(define-custom 'use-sticky-control? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Control key")
  (_ "long description will be here."))

(define-custom 'use-sticky-alt? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Alt key")
  (_ "long description will be here."))

(define-custom 'use-sticky-meta? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Meta key")
  (_ "long description will be here."))

(define-custom 'use-sticky-super? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Super key")
  (_ "long description will be here."))

(define-custom 'use-sticky-hyper? #f
  '(modifier1 modifier-sticky)
  '(boolean)
  (_ "Hyper key")
  (_ "long description will be here."))

(define-custom 'use-combinational-shift? #f
  '(modifier1)
  '(boolean)
  (_ "Use combinational shift")
  (_ "long description will be here."))


(define-custom 'use-shift-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use shift key lock")
  (_ "long description will be here."))

(define-custom 'shift-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Shift key press count to lock/unlock shift state")
  (_ "long description will be here."))

(define-custom 'use-control-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use control key lock")
  (_ "long description will be here."))

(define-custom 'control-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Control key press count to lock/unlock control state")
  (_ "long description will be here."))

(define-custom 'use-alt-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use alt key lock")
  (_ "long description will be here."))

(define-custom 'alt-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Alt key press count to lock/unlock alt state")
  (_ "long description will be here."))

(define-custom 'use-meta-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use meta key lock")
  (_ "long description will be here."))

(define-custom 'meta-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Meta key press count to lock/unlock meta state")
  (_ "long description will be here."))

(define-custom 'use-super-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use super key lock")
  (_ "long description will be here."))

(define-custom 'super-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Super key press count to lock/unlock super state")
  (_ "long description will be here."))

(define-custom 'use-hyper-lock? #f
  '(modifier2 modifier-lock)
  '(boolean)
  (_ "Use hyper key lock")
  (_ "long description will be here."))

(define-custom 'hyper-lock-count 2
  '(modifier2 modifier-lock)
  '(integer 1 10)
  (_ "Hyper key press count to lock/unlock hyper state")
  (_ "long description will be here."))

