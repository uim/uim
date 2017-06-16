;;; socket.scm: socket library for uim.
;;;
;;; Copyright (c) 2009-2013 uim Project https://github.com/uim/uim
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

(require-extension (srfi 9 48))
(require "process.scm")

(define uim-help-branch #f)

(define toolbar-help-url "https://github.com/uim/uim/wiki")

(define toolbar-help-url-locale-alist
  '(("ja" . "https://github.com/uim/uim-doc-ja/wiki")))

(define (uim-help-set-branch! fd)
  (let ((port (open-file-port fd)))
    (let loop ((line (file-read-line port)))
      (let ((ret (string-split line "\t")))
        (if (string=? (car ret) "branch")
            (set! uim-help-branch (string->symbol (list-ref ret 1)))
            (loop (file-read-line port)))))))

(define (make-wikiname im)
  (apply string-append
         (map (lambda (s)
                (let ((sym (string->list s)))
                  (list->string
                   (cons (char-upcase (car sym))
                         (cdr sym)))))
              (string-split (symbol->string im) "-"))))

(define (select-url-from-im im)
  (or (and-let* ((i (retrieve-im im))
                 (ret (assoc (im-lang i) toolbar-help-url-locale-alist)))
         (format "~aUim~a" (cdr ret) (make-wikiname im)))
      toolbar-help-url))

(define (uim-help args)
  (let ((cmd (cond ((eq? toolbar-help-browser 'system)
                    "xdg-open")
                   ((eq? toolbar-help-browser 'manual)
                    toolbar-help-browser-name)
                   (else
                    #f))))
    (if (and cmd uim-help-branch)
        (process-execute cmd (list cmd (select-url-from-im uim-help-branch)))
        255)))
