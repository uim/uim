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

(define look-im-name-label (N_ "Look"))
(define look-im-short-desc (N_ "Tiny predictive input method"))

(define-custom-group 'look
  look-im-name-label
  look-im-short-desc)

(define-custom-group 'look-keys
  (N_ "Look key bindings")
  (N_ "long description will be here."))


(define-custom 'look-dict "/usr/share/dict/words"
  '(look)
  '(pathname regular-file)
  (N_ "[Look] Use UNIX look dictionary file")
  (N_ "long description will be here."))

(define-custom 'look-personal-dict-filename
  (string-append (or (home-directory (user-name)) "") "/.look-uim-dict")
  '(look)
  '(pathname regular-file)
  (N_ "[Look] Personal dictionary file")
  (N_ "long description will be here."))

(define-custom 'look-use-annotation? #f
  '(look)
  '(boolean)
  (N_ "[Look] Use annotations")
  (N_ "long description will be here."))

(define-custom 'look-annotation-show-lines 2
  '(look)
  '(integer 1 80)
  (N_ "[Look] Show annotation of lines")
  (N_ "long description will be here."))

(custom-add-hook 'look-annotation-show-lines
                 'custom-activity-hooks
                 (lambda ()
                   look-use-annotation?))

(define-custom 'look-beginning-character-length 1
  '(look)
  '(integer 1 65535)
  (N_ "[Look] beginning character length of predicting")
  (N_ "long description will be here."))

(define-custom 'look-candidates-max 20
  '(look)
  '(integer 1 65535)
  (N_ "[Look] max words of candidates")
  (N_ "long description will be here."))

(define-custom 'look-prepared-words 0
  '(look)
  '(integer 0 65535)
  (N_ "[Look] Prepared words for prediction")
  (N_ "long description will be here."))

(define-custom 'look-fence-left "{"
  '(look)
  '(string ".*")
  (N_ "[Look] left fence character of candidate")
  (N_ "long description will be here"))

(define-custom 'look-fence-right "}"
  '(look)
  '(string ".*")
  (N_ "[Look] right fence character of candidate")
  (N_ "long description will be here"))

(define-custom 'look-on-key generic-on-key
  '(look-keys)
  '(key)
  (N_ "[Look] on")
  (N_ "long description will be here"))

(define-custom 'look-off-key generic-off-key
  '(look-keys)
  '(key)
  (N_ "[Look] off")
  (N_ "long description will be here"))

(define-custom 'look-completion-key '("tab" generic-end-of-preedit-key)
  '(look-keys)
  '(key)
  (N_ "[Look] completion character")
  (N_ "long description will be here"))

(define-custom 'look-next-char-key generic-go-right-key
  '(look-keys)
  '(key)
  (N_ "[Look] next character")
  (N_ "long description will be here"))

(define-custom 'look-prev-char-key '(generic-backspace-key generic-go-left-key)
  '(look-keys)
  '(key)
  (N_ "[Look] previous character")
  (N_ "long description will be here"))

(define-custom 'look-next-candidate-key '("down" "<IgnoreCase><Control>n") ;; generic-next-candidate-key
  '(look-keys)
  '(key)
  (N_ "[Look] next candidate")
  (N_ "long description will be here"))

(define-custom 'look-prev-candidate-key '("up" "<IgnoreCase><Control>p") ;; generic-prev-candidate-key
  '(look-keys)
  '(key)
  (N_ "[Look] previous candidate")
  (N_ "long description will be here"))

(define-custom 'look-save-dict-key '("<IgnoreCase><Control>s" "return")
  '(look-keys)
  '(key)
  (N_ "[Look] save dictionary")
  (N_ "long description will be here"))

(define-custom 'look-load-dict-key '("<IgnoreCase><Control>l")
  '(look-keys)
  '(key)
  (N_ "[Look] load dictionary")
  (N_ "long description will be here"))
