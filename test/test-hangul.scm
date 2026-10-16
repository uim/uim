;;; Copyright (c) 2026 uim Project https://github.com/uim/uim
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
;;;

(require-extension (unittest))

;; Record what the IMs output instead of sending it to a client.
(define test-output '())
(define test-preedit "")
(define test-record!
  (lambda (x)
    (set! test-output (append test-output (list x)))))
(define im-commit
  (lambda (pc str)
    (test-record! str)))
(define im-commit-raw
  (lambda (pc)
    (test-record! 'raw)))
(define im-clear-preedit
  (lambda (pc)
    (set! test-preedit "")))
(define im-pushback-preedit
  (lambda (pc attr str)
    (set! test-preedit (string-append test-preedit str))))
(define im-update-preedit (lambda (pc) #f))
(define im-activate-candidate-selector (lambda args #f))
(define im-deactivate-candidate-selector (lambda args #f))
(define im-select-candidate (lambda args #f))
(define im-shift-page-candidate (lambda args #f))

;; Only the IMs in enabled-im-list are registered. It's defined in
;; loader.scm but loader.scm isn't loaded with LIBUIM_VANILLA=1.
(set! enabled-im-list '(hangul2 hangul3 romaja))
(require-module "hangul")

;; Returns the committed strings ('raw for a key passed through) and the
;; last preedit after typing KEYS in the on mode of the IM. A string in
;; KEYS is typed character by character.
(define test-type
  (lambda (name keys)
    (let* ((im (retrieve-im name))
           (pc ((im-init-handler im) 0 im (im-init-arg im))))
      (set! test-output '())
      (set! test-preedit "")
      (generic-context-set-on! pc #t)
      (for-each (lambda (key)
                  ((im-key-press-handler im) pc key 0))
                (apply append
                       (map (lambda (key)
                              (if (string? key)
                                  (map char->integer (string->list key))
                                  (list key)))
                            keys)))
      (list test-output test-preedit))))

;; hangul2 and romaja process space and return as input while there is
;; preedit. hangul3 uses them to select and commit a candidate.
(test-begin "space")
(test-equal '(("한" raw) "")
            (test-type 'hangul2 '("gks ")))
(test-equal '(("킴" raw) "")
            (test-type 'romaja '("kim ")))
(test-equal '(("ㅡ" "ㄱ") "ㄵ")
            (test-type 'hangul3 '("gks ")))
(test-end)

(test-begin "return")
(test-equal '(("한" raw "글") "")
            (test-type 'hangul2 '("gks" return "rmf" return)))
(test-equal '(("킴" raw) "")
            (test-type 'romaja '("kim" return)))
(test-equal '(("ㅡ" "ㄱ" "ㄴ" "ㅐ" "하") "")
            (test-type 'hangul3 '("gks" return "rmf" return)))
(test-end)

;; The candidate keys are enabled while selecting a candidate.
(test-begin "candidates")
(test-equal '(() "았")
            (test-type 'hangul2 '("dk" down)))
(test-equal '(() "앝")
            (test-type 'hangul2 '("dk" up)))
(test-end)

(test-report-result)
