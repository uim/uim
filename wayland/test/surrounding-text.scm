;;; Copyright (c) 2026 uim Project https://github.com/uim/uim
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
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

;;; An input method that commits what it can see of the text around the
;;; cursor, so that the uim-wayland test can check the bridge against
;;; the values the compositor sent. Nothing else loads this.
;;;
;;;   a  commit the whole surrounding text
;;;   j  commit the line before the cursor
;;;   k  commit the selected text
;;;   l  delete the character before the cursor

;; im-acquire-text hands its result back as a ustr.
(require "ustr.scm")

(define surrounding-text-context-rec-spec context-rec-spec)
(define-record 'surrounding-text-context surrounding-text-context-rec-spec)

(define surrounding-text-init-handler
  (lambda (id im arg)
    (surrounding-text-context-new id im)))

(define surrounding-text-release-handler
  (lambda (c)
    #f))

;; "[former|latter]", or "[]" when the bridge has nothing to give.
(define surrounding-text-format
  (lambda (ustr)
    (if ustr
        (string-append "["
                       (apply string-append (ustr-former-seq ustr))
                       "|"
                       (apply string-append (ustr-latter-seq ustr))
                       "]")
        "[]")))

(define surrounding-text-press-key-handler
  (lambda (c key state)
    (cond
     ((= key 97)
      (im-commit c (surrounding-text-format
                    (im-acquire-text c 'primary 'cursor 'full 'full))))
     ((= key 106)
      (im-commit c (surrounding-text-format
                    (im-acquire-text c 'primary 'cursor 'line 0))))
     ((= key 107)
      (im-commit c (surrounding-text-format
                    (im-acquire-text c 'selection 'beginning 0 'full))))
     ((= key 108)
      (im-commit c (if (im-delete-text c 'primary 'cursor 1 0)
                       "[deleted]"
                       "[]")))
     (else
      (im-commit-raw c)))))

(define surrounding-text-release-key-handler
  (lambda (c key state)
    #f))

(define surrounding-text-reset-handler
  (lambda (c)
    #f))

(define surrounding-text-get-candidate-handler
  (lambda (c idx accel-enum-hint)
    #f))

(define surrounding-text-set-candidate-index-handler
  (lambda (c idx)
    #f))

;; register-im ignores an input method the user hasn't enabled.
(if (not (memq 'surrounding-text enabled-im-list))
    (set! enabled-im-list (cons 'surrounding-text enabled-im-list)))

(register-im
 'surrounding-text
 "*"
 "UTF-8"
 "surrounding text"
 "Commits the text around the cursor, for the uim-wayland test"
 #f
 surrounding-text-init-handler
 surrounding-text-release-handler
 context-mode-handler
 surrounding-text-press-key-handler
 surrounding-text-release-key-handler
 surrounding-text-reset-handler
 surrounding-text-get-candidate-handler
 surrounding-text-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
