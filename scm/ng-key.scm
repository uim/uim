;;; ng-key.scm: Key definitions and utilities (next generation)
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

(require "util.scm")
;;(require-custom "key-custom.scm")  ;; FIXME: temporarily disabled

;;
;; modifiers
;;

(define valid-modifiers
  '(mod_None

    mod_Shift
    mod_Shift_R
    mod_Shift_L
    mod_Control
    mod_Control_R
    mod_Control_L
    mod_Alt
    mod_Alt_R
    mod_Alt_L
    mod_Meta
    mod_Meta_R
    mod_Meta_L
    mod_Super
    mod_Super_R
    mod_Super_L
    mod_Hyper
    mod_Hyper_R
    mod_Hyper_L

    mod_Caps_Lock
    ;;mod_Shift_Lock
    ;;mod_Num_Lock

    ;; pseudo modifiers for meta-event
    mod_ignore_Shift
    mod_ignore_Control
    mod_ignore_Alt
    mod_ignore_Meta
    mod_ignore_Super
    mod_ignore_Hyper))

(define mod_None           #x00000000)
(define mod_Shift_L        #x00000001)
(define mod_Shift_R        #x00000002)
(define mod_Shift          #x00000004)
(define mod_Control_L      #x00000008)
(define mod_Control_R      #x00000010)
(define mod_Control        #x00000020)
(define mod_Alt_L          #x00000040)
(define mod_Alt_R          #x00000080)
(define mod_Alt            #x00000100)
(define mod_Meta_L         #x00000200)
(define mod_Meta_R         #x00000400)
(define mod_Meta           #x00000800)
(define mod_Super_L        #x00001000)
(define mod_Super_R        #x00002000)
(define mod_Super          #x00004000)
(define mod_Hyper_L        #x00008000)
(define mod_Hyper_R        #x00010000)
(define mod_Hyper          #x00020000)
(define mod_Caps_Lock      #x00040000)
;;(define  #x00080000)
;;(define  #x00100000)
(define mod_ignore_Shift   #x00200000)
(define mod_ignore_Control #x00400000)
(define mod_ignore_Alt     #x00800000)
(define mod_ignore_Meta    #x01000000)
(define mod_ignore_Super   #x02000000)
(define mod_ignore_Hyper   #x04000000)
;;(define  #x08000000)  ;; incapable by storage-compact
;;(define  #x10000000)  ;; incapable by storage-compact
;;(define  #x20000000)  ;; incapable by storage-compact
;;(define  #x40000000)  ;; incapable by storage-compact

(define modifier-shift-mask
  (bitwise-ior mod_Shift_L   mod_Shift_R   mod_Shift))
(define modifier-control-mask
  (bitwise-ior mod_Control_L mod_Control_R mod_Control))
(define modifier-alt-mask
  (bitwise-ior mod_Alt_L     mod_Alt_R     mod_Alt))
(define modifier-meta-mask
  (bitwise-ior mod_Meta_L    mod_Meta_R    mod_Meta))
(define modifier-super-mask
  (bitwise-ior mod_Super_L   mod_Super_R   mod_Super))
(define modifier-hyper-mask
  (bitwise-ior mod_Hyper_L   mod_Hyper_R   mod_Hyper))


;; API
(define modifier-symbol?
  (lambda (sym)
    (and (symbol? sym)
	 (memq sym valid-modifiers))))

;; API
(define modifier-has?
  (lambda (self other)
    (= (bitwise-and self other)
       other)))

(define modifier-aggregate
  (lambda (self flags)
    (let ((aggregate-mod-group (lambda (self flags mod mod-ignore mod-mask)
				 (let ((self-mods (bitwise-and self mod-mask)))
				   (if (modifier-has? flags mod-ignore)
				       mod-ignore
				       (if (and (modifier-has? flags mod)
						(not (= self-mods 0)))
					   mod
					   self-mods))))))
      (bitwise-ior (aggregate-mod-group self flags mod_Shift mod_ignore_Shift
				       modifier-shift-mask)
		  (aggregate-mod-group self flags mod_Control mod_ignore_Control
				       modifier-control-mask)
		  (aggregate-mod-group self flags mod_Alt mod_ignore_Alt
				       modifier-alt-mask)
		  (aggregate-mod-group self flags mod_Meta mod_ignore_Meta
				       modifier-meta-mask)
		  (aggregate-mod-group self flags mod_Super mod_ignore_Super
				       modifier-super-mask)
		  (aggregate-mod-group self flags mod_Hyper mod_ignore_Hyper
				       modifier-hyper-mask)
		  (bitwise-and self mod_Caps_Lock)))))

;; API
(define modifier-match?
  (lambda (self other)
    (let* ((aggregated-self (modifier-aggregate self self))
	   (aggregated-other (modifier-aggregate other aggregated-self)))
      (= aggregated-self
	 aggregated-other))))

;;
;; logical keys
;;

(define valid-logical-keys
  '(lkey_VoidSymbol

    lkey_BackSpace
    lkey_Tab
    lkey_Return
    lkey_Escape
    lkey_Delete
    lkey_Home
    lkey_Left
    lkey_Up
    lkey_Right
    lkey_Down
    lkey_Page_Up
    lkey_Page_Down
    lkey_End
    lkey_Insert

    lkey_Shift_L
    lkey_Shift_R
    lkey_Control_L
    lkey_Control_R
    lkey_Caps_Lock
    lkey_Meta_L
    lkey_Meta_R
    lkey_Alt_L
    lkey_Alt_R
    lkey_Super_L
    lkey_Super_R
    lkey_Hyper_L
    lkey_Hyper_R

    lkey_Multi_key    ;; Multi-key character compose
    lkey_Mode_switch  ;; Character set switch

    ;; Japanese keyboard support
    lkey_Kanji             ;; Kanji, Kanji convert
    lkey_Muhenkan          ;; Cancel Conversion
    lkey_Henkan            ;; Henkan_Mode
    lkey_Hiragana_Katakana ;; Hiragana/Katakana toggle
    lkey_Zenkaku_Hankaku   ;; Zenkaku/Hankaku toggle

    ;; NICOLA keys
    lkey_Thumb_Shift_L
    lkey_Thumb_Shift_R

    lkey_F1
    lkey_F2
    lkey_F3
    lkey_F4
    lkey_F5
    lkey_F6
    lkey_F7
    lkey_F8
    lkey_F9
    lkey_F10
    lkey_F11
    lkey_F12
    lkey_F13
    lkey_F14
    lkey_F15
    lkey_F16
    lkey_F17
    lkey_F18
    lkey_F19
    lkey_F20
    lkey_F21
    lkey_F22
    lkey_F23
    lkey_F24
    lkey_F25
    lkey_F26
    lkey_F27
    lkey_F28
    lkey_F29
    lkey_F30
    lkey_F31
    lkey_F32
    lkey_F33
    lkey_F34
    lkey_F35

    ;; ASCII keys
    lkey_space
    lkey_exclam
    lkey_quotedbl
    lkey_numbersign
    lkey_dollar
    lkey_percent
    lkey_ampersand
    lkey_apostrophe
    lkey_parenleft
    lkey_parenright
    lkey_asterisk
    lkey_plus
    lkey_comma
    lkey_minus
    lkey_period
    lkey_slash
    lkey_0
    lkey_1
    lkey_2
    lkey_3
    lkey_4
    lkey_5
    lkey_6
    lkey_7
    lkey_8
    lkey_9
    lkey_colon
    lkey_semicolon
    lkey_less
    lkey_equal
    lkey_greater
    lkey_question
    lkey_at
    lkey_A
    lkey_B
    lkey_C
    lkey_D
    lkey_E
    lkey_F
    lkey_G
    lkey_H
    lkey_I
    lkey_J
    lkey_K
    lkey_L
    lkey_M
    lkey_N
    lkey_O
    lkey_P
    lkey_Q
    lkey_R
    lkey_S
    lkey_T
    lkey_U
    lkey_V
    lkey_W
    lkey_X
    lkey_Y
    lkey_Z
    lkey_bracketleft
    lkey_backslash
    lkey_bracketright
    lkey_asciicircum
    lkey_underscore
    lkey_grave
    lkey_a
    lkey_b
    lkey_c
    lkey_d
    lkey_e
    lkey_f
    lkey_g
    lkey_h
    lkey_i
    lkey_j
    lkey_k
    lkey_l
    lkey_m
    lkey_n
    lkey_o
    lkey_p
    lkey_q
    lkey_r
    lkey_s
    lkey_t
    lkey_u
    lkey_v
    lkey_w
    lkey_x
    lkey_y
    lkey_z
    lkey_braceleft
    lkey_bar
    lkey_braceright
    lkey_asciitilde

    ;; extended keys
    lkey_yen

    ;; dead keys
    lkey_dead_grave
    lkey_dead_acute
    lkey_dead_circumflex
    lkey_dead_tilde
    lkey_dead_macron
    lkey_dead_breve
    lkey_dead_abovedot
    lkey_dead_diaeresis
    lkey_dead_abovering
    lkey_dead_doubleacute
    lkey_dead_caron
    lkey_dead_cedilla
    lkey_dead_ogonek
    lkey_dead_iota
    lkey_dead_voiced_sound
    lkey_dead_semivoiced_sound
    lkey_dead_belowdot
    lkey_dead_hook
    lkey_dead_horn))

;; API
(define logical-key?
  (lambda (key)
    (and (symbol? key)
	 (memq key valid-logical-keys))))

;;
;; physical key
;;

;; added on demand
(define valid-physical-keys '(pkey_VoidSymbol))

;; API
(define physical-key?
  (lambda (key)
    (and (symbol? key)
	 (memq key valid-physical-keys))))

;; API
;; will be replaced with actual one when physical-key.scm loaded
(define lkey->pkey
  (lambda (lkey)
    #f))
