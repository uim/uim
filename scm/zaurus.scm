;; zaurus.scm: platform-specific support for Sharp Zaurus PDA
;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

;;
;; This file is intended to use with IMKit-uim on Qtopia platform
;; See following documents for Zaurus specific key definitions
;;
;; SL-A300
;; http://developer.ezaurus.com/sl_j/doc/software/keycode_qt_a300_20021213.pdf
;;
;; SL-B500 and SL-C700
;; http://developer.ezaurus.com/sl_j/doc/software/keycode_b500c700.pdf

(require "generic-key.scm")

(define-key zaurus-calendar-key? "F9")

(define-key zaurus-addressbook-key? "F10")

(define-key zaurus-menu-key? "F11")

;; application key to show "Home" tab of the launcher, not ordinary
;; "Home" key in desktop environment
(define-key zaurus-home-key? "F12")

(define-key zaurus-mail-key? "F13")

;; "Mail" being pressed long
(define-key zaurus-mail-pressed-long-key? "F14")

(define-key zaurus-zenkaku-hankaku-key? "F21")

;; "Fn" modifier key
;; modified keys are translated to another unmodified key by Qt/Embedded
(define-key zaurus-fn-key? "F22")

;; Fn + Space ("OnKun")
(define-key zaurus-onkun-key? "F23")

;; Fn + "Zenkaku/Hankaku" ("Kanji")
(define-key zaurus-kanji-key? "F25")

;; Katakana/Hiragana toggle
(define-key zaurus-katakana-hiragana-key? "F26")

;; "SELECT" on SL-5500 (center of round cursor keys)
(define-key zaurus-select-key? "F30")
(define-key zaurus-shift-select-key? "<Shift>F30")

;; Fn + "Katakana/Hiragana" ("Kigou")
(define-key zaurus-kigou-key? "F31")

;; "Sync start" button on cradle
(define-key zaurus-sync-start-key? "F32")

(define-key zaurus-ok-key? "F33")

;; Power switch (don't use this key)
(define-key zaurus-power-key? "F34")

;; "Calendar" being pressed long (as "Sync" on SL-C700)
(define-key zaurus-sync-key? "F35")

;;; following keysyms are locally assigned by IMKit-uim from raw integer
;; Fn + 1 ("Zoom out")
(define-key zaurus-zoom-out-key? "Private1")

;; Fn + 2 ("Zoom in")
(define-key zaurus-zoom-in-key? "Private2")

;; Fn + 3 ("Decrease contrast")
(define-key zaurus-decrease-contrast-key? "Private3")

;; Fn + 4 ("Increase contrast")
(define-key zaurus-increase-contrast-key? "Private4")

;; Fn + 5
(define-key zaurus-fn-5-key? "Private5")

;; Fn + 6 ("Hiragana")
(define-key zaurus-hiragana-mode-key? "Private6")

;; Fn + 7 ("Katakana")
(define-key zaurus-katakana-mode-key? "Private7")

;; Fn + 8 ("half-width Katakana")
(define-key zaurus-half-katakana-mode-key? "Private8")

;; Fn + 9 ("fullwidth Alphanumeric")
(define-key zaurus-wide-latin-mode-key? "Private9")

;; Fn + 0 ("Alphanumeric")
(define-key zaurus-latin-mode-key? "Private10")

;; Fn + Q
(define-key zaurus-fn-q-key? "Private11")

;; Fn + O
(define-key zaurus-fn-o-key? "Private12")

;; Fn + P
(define-key zaurus-fn-p-key? "Private13")

;; Fn + A
(define-key zaurus-fn-a-key? "Private14")

;; Fn + S
(define-key zaurus-fn-s-key? "Private15")

;; Fn + K
(define-key zaurus-fn-k-key? "Private16")

;; Fn + N
(define-key zaurus-fn-n-key? "Private17")

;; Fn + M
(define-key zaurus-fn-m-key? "Private18")

;;; SL-6000 specific keys
;; "Backlight"
(define-key zaurus-backlight-key? "F35")

;; "Backlight" being pressed long ("Rotate screen on the fly")
(define-key zaurus-rotate-screen-key? "Private19")

;; "Rec" being pressed long
(define-key zaurus-rec-key? "Private20")

;; user defined key (looks like "(  *  )")
(define-key zaurus-user-defined-key? "Private21")


;; save original definitions
(define zaurus-orig-modifier-key? modifier-key?)
(define zaurus-orig-generic-begin-conv-key? generic-begin-conv-key?)
(define zaurus-orig-generic-next-candidate-key? generic-next-candidate-key?)
(define zaurus-orig-generic-commit-key? generic-commit-key?)
(define zaurus-orig-generic-cancel-key? generic-cancel-key?)

(define-key zaurus-begin-conv-key?
  '(zaurus-orig-generic-begin-conv-key? zaurus-select-key?))
(define-key zaurus-next-candidate-key?
  '(zaurus-orig-generic-next-candidate-key? zaurus-select-key?))

;; "OK" key is not included to avoid accidential termination of application
(define-key zaurus-commit-key? 'zaurus-orig-generic-commit-key?)

;; "CANCEL" key is already bound as 'escape("CANCEL" key in Zaurus) in
;; generic-cancel-key?, so ignore it to avoid accidential termination
;; of application
(define zaurus-cancel-key?
  (lambda (key key-state)
    (or
     (and
      (not (eq? key 'escape))
      (zaurus-orig-generic-cancel-key? key key-state))
     (zaurus-shift-select-key? key key-state))))

;; additionally bind "OK" key for commit
(define-key zaurus-barbarous-commit-key? '(zaurus-commit-key? zaurus-ok-key?))

;; additionally bind "CANCEL" key for cancel operation
(define-key zaurus-barbarous-cancel-key? '(zaurus-cancel-key? "escape"))

;; placeholder for future use
(define zaurus-translate-key
  (lambda (key key-state)
    (cond
     ((zaurus-zenkaku-hankaku-key? key key-state)
      ('zenkaku-hankaku key-state))
     ((zaurus-katakana-hiragana-key? key key-state)
      ('Mode_switch key-state)))))

;; ignore Fn key as modifier key
(define-key modifier-key? '(zaurus-orig-modifier-key? zaurus-fn-key?))

;; replace the default keybindings to fit Zaurus hardware
(define generic-begin-conv-key? zaurus-begin-conv-key?)
(define generic-next-candidate-key? zaurus-next-candidate-key?)
(define generic-commit-key? zaurus-commit-key?)
(define generic-cancel-key? zaurus-cancel-key?)
;; you can use the barbarous bindings with careful key operation
;(define generic-commit-key? zaurus-barbarous-commit-key?)
;(define generic-cancel-key? zaurus-barbarous-cancel-key?)
