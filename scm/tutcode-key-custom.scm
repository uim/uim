;;; tutcode-key-custom.scm: Customization variables for tutcode key bindings
;;;
;;; Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/
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

(require "i18n.scm")

;; key defs

;;; TUT-Codeモードに入るキー(CTRL-\)かどうかを判定する。
;;; @param key 判定するキー
;;; @param key-state コントロールキー等の状態
;;; @return #t: keyがTUT-Codeモードに入るキーの場合。
;;;	    #f: そうでない場合。 
;(define-key tutcode-on-key? "<Control>\\")

;;; TUT-Codeモードを抜けるキー(CTRL-\)かどうかを判定する。
;;; @param key 判定するキー
;;; @param key-state コントロールキー等の状態
;;; @return #t: keyがTUT-Codeモードを抜けるキーの場合。
;;;	    #f: そうでない場合。 
;(define-key tutcode-off-key? "<Control>\\")

;;; ひらがな/カタカナモードの切り替えを行うキー(')かどうかを判定する。
;;; @param key 判定するキー
;;; @param key-state コントロールキー等の状態
;;; @return #t: keyがひらがな/カタカナモードの切り替えキーの場合。
;;;	    #f: そうでない場合。 
;(define-key tutcode-kana-toggle-key? "'")

;;; Backspaceキーかどうかを判定する。
;;; @param key 判定するキー
;;; @param key-state コントロールキー等の状態
;;; @return #t: keyがBackspaceキーの場合。
;;;	    #f: そうでない場合。 
;(define-key tutcode-backspace-key? 'generic-backspace-key?)

(define-custom-group 'tutcode-keys1
		     (_ "TUT-Code key bindings 1")
		     (_ "long description will be here."))

(define-custom-group 'tutcode-keys2
		     (_ "TUT-Code key bindings 2")
		     (_ "long description will be here."))

(define-custom 'tutcode-on-key '("<Control>\\" generic-on-key)
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (_ "[TUT-Code] on")
	       (_ "long description will be here"))

(define-custom 'tutcode-off-key '("<Control>\\" generic-off-key)
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (_ "[TUT-Code] off")
	       (_ "long description will be here"))

(define-custom 'tutcode-kana-toggle-key '("<IgnoreShift>'")
               '(tutcode-keys1 mode-transition)
	       '(key)
	       (_ "[TUT-Code] toggle hiragana/katakana mode")
	       (_ "long description will be here"))


(define-custom 'tutcode-backspace-key '(generic-backspace-key)
               '(tutcode-keys2)
	       '(key)
	       (_ "[TUT-Code] backspace")
	       (_ "long description will be here"))
