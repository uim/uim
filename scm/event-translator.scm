;;; event-translator.scm: Event translator
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


(require "util.scm")
(require "event.scm")
(require "ng-key.scm")
(require "evmap.scm")

;;
;; key-event translator
;;

;; should be moved into appropriate file

(define combinational-shift-ruleset
  '((((char-alphabet press) (char-alphabet press))
     ((($1 char-upcase mod_shift loopback)) (($2 loopback))))))

;; TODO:
;;  - support arbitrary set of modifiers
;;  - support non-alphabet chars
(define sticky-shift-ruleset
  '(((lkey_Shift_L char-alphabet) (($3 char-upcase mod_Shift_L loopback)
				   ($4 char-upcase mod_Shift_L loopback)))
    ((lkey_Shift_R char-alphabet) (($3 char-upcase mod_Shift_R loopback)
				   ($4 char-upcase mod_Shift_R loopback)))))

(define shift-lock-ruleset
  '(((lkey_Shift_L lkey_Shift_L) (action_toggle_shift_lock_l))
    ((lkey_Shift_R lkey_Shift_R) (action_toggle_shift_lock_r))))

(define control-lock-ruleset
  '(((lkey_Control_L lkey_Control_L) (action_toggle_control_lock_l))
    ((lkey_Control_R lkey_Control_R) (action_toggle_control_lock_r))))

(define alt-lock-ruleset
  '(((lkey_Alt_L lkey_Alt_L) (action_toggle_alt_lock_l))
    ((lkey_Alt_R lkey_Alt_R) (action_toggle_alt_lock_r))))

(define meta-lock-ruleset
  '(((lkey_Meta_L lkey_Meta_L) (action_toggle_meta_lock_l))
    ((lkey_Meta_R lkey_Meta_R) (action_toggle_meta_lock_r))))

(define super-lock-ruleset
  '(((lkey_Super_L lkey_Super_L) (action_toggle_super_lock_l))
    ((lkey_Super_R lkey_Super_R) (action_toggle_super_lock_r))))

(define hyper-lock-ruleset
  '(((lkey_Hyper_L lkey_Hyper_L) (action_toggle_hyper_lock_l))
    ((lkey_Hyper_R lkey_Hyper_R) (action_toggle_hyper_lock_r))))

;; for functional test and demonstration
(define qwerty-shift->space-ruleset
  '((((lkey_Shift_L press))   (($1 " " lkey_space)))
    (((lkey_Shift_L release)) (($1 " " lkey_space)))
    (((lkey_Shift_R press))   (($1 " " lkey_space)))
    (((lkey_Shift_R release)) (($1 " " lkey_space)))))

(define jp106-henkan-muhenkan->shift-ruleset
  '((((lkey_Henkan   press))   (($1 lkey_Shift_R)
				action_set_shift_r_state))
    (((lkey_Henkan   release)) (($1 lkey_Shift_R mod_Shift_R)
				action_reset_shift_r_state))
    (((lkey_Muhenkan press))   (($1 lkey_Shift_L)
				action_set_shift_l_state))
    (((lkey_Muhenkan release)) (($1 lkey_Shift_L mod_Shift_L)
				action_reset_shift_l_state))))

;; temporary workaround for dedicated key-event translator
(define ja-nicola-jp106-pseudo-thumb-shift-ruleset
  '((((lkey_Henkan   press))   (($1 lkey_Thumb_Shift_R)))
    (((lkey_Henkan   release)) (($1 lkey_Thumb_Shift_R)))
    (((lkey_Muhenkan press))   (($1 lkey_Thumb_Shift_L)))
    (((lkey_Muhenkan release)) (($1 lkey_Thumb_Shift_L)))))


(define key-event-translator-ruleset
  (append
   ;;qwerty->dvorak-ruleset
   ;;combinational-shift-ruleset
   ;;sticky-shift-ruleset
   ;;shift-lock-ruleset
   ;;(if qwerty-enable-pseudo-multi-key?
   ;;    qwerty-enable-pseudo-multi-key-ruleset
   ;;    ())
   ;;(if qwerty-enable-pseudo-dead-keys?
   ;;    qwerty-enable-pseudo-dead-keys-ruleset
   ;;    ())
   (if enable-jp106-henkan-muhenkan-shift?
       jp106-henkan-muhenkan->shift-ruleset
       ())
   (if enable-ja-nicola-jp106-pseudo-thumb-shift?
       ja-nicola-jp106-pseudo-thumb-shift-ruleset
       ())
   (if enable-qwerty-shift->space?
       qwerty-shift->space-ruleset
       ())
   ))

(define key-event-translator-ruletree
  (evmap-parse-ruleset key-event-translator-ruleset))

(define key-event-translator-new
  (lambda ()
    (evmap-context-new key-event-translator-ruletree)))

;; TODO: write test, replace with composer framework
;; returns rest actions
(define key-event-translator-translate!
  (lambda (emc ev)
    (if enable-modifier-translation?
	(key-event-set-modifier! ev mod_None))
    (if (evmap-context-input! emc ev)
	(begin
	  (event-set-consumed! ev #f)
	  (event-set-loopback! ev #f)))
    (if (evmap-context-complete? emc)
	(let* ((action-seq (evmap-context-action-seq emc))
	       (translated (safe-car action-seq)))
	  (evmap-context-flush! emc)
	  (if (pair? translated)
	       (begin
		 (list-copy! ev translated)
		 (cdr action-seq))
	       action-seq)))))
