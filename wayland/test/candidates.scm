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

;;; Shows candidates and commits the selected index, for the uim-wayland
;;; test.
;;;
;;;   a  show 12 candidates, 5 to a page

(define candidates-count 12)
(define candidates-page-size 5)

(define candidates-context-rec-spec context-rec-spec)
(define-record 'candidates-context candidates-context-rec-spec)

(define candidates-init-handler
  (lambda (id im arg)
    (candidates-context-new id im)))

(define candidates-release-handler
  (lambda (c)
    #f))

(define candidates-press-key-handler
  (lambda (c key state)
    (if (= key 97)
        (im-activate-candidate-selector c
                                        candidates-count
                                        candidates-page-size)
        (im-commit-raw c))))

(define candidates-release-key-handler
  (lambda (c key state)
    #f))

(define candidates-reset-handler
  (lambda (c)
    #f))

(define candidates-get-candidate-handler
  (lambda (c idx accel-enum-hint)
    (list (string-append "candidate " (number->string idx))
          (number->string (+ (remainder idx candidates-page-size) 1))
          "")))

;; "[index]"
(define candidates-set-candidate-index-handler
  (lambda (c idx)
    (im-commit c (string-append "[" (number->string idx) "]"))))

;; register-im ignores an input method the user hasn't enabled.
(if (not (memq 'candidates enabled-im-list))
    (set! enabled-im-list (cons 'candidates enabled-im-list)))

(register-im
 'candidates
 "*"
 "UTF-8"
 "candidates"
 "Shows candidates, for the uim-wayland test"
 #f
 candidates-init-handler
 candidates-release-handler
 context-mode-handler
 candidates-press-key-handler
 candidates-release-key-handler
 candidates-reset-handler
 candidates-get-candidate-handler
 candidates-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )
