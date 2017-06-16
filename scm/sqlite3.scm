;;; sqlite3.scm: sqlite3 for uim.
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

(guard (err (else #f))
       (require-dynlib "sqlite3"))

(require-extension (srfi 1))

(define (sqlite3-escape-string str)
  (string-join (string-split str "'") "''"))

(define (sqlite3-run-statement *statement* fun . binds)
  (for-each (lambda (nth-bind)
              (let ((nth (car nth-bind))
                    (bind (cadr nth-bind)))
                (cond ((string? bind)
                       (sqlite3-bind-text *statement* nth
                                          (sqlite3-escape-string bind)
                                          -1))
                      ((number? bind)
                       (sqlite3-bind-int *statement* nth
                                          bind))
                      ((null? bind)
                       (sqlite3-bind-null *statement* nth))
                      (else
                       (error (format "unknown binding '~a'" bind))))))
            (zip (iota (length binds) 1) binds))
  (let loop ((ret (sqlite3-step *statement*))
             (rest '()))
    (if (= ret (assq-cdr '$SQLITE_DONE (sqlite3-results)))
        (begin
          (sqlite3-clear-bindings *statement*)
          (sqlite3-reset *statement*)
          (reverse rest))
        (let ((result (fun *statement*)))
          (if result
              (loop (sqlite3-step *statement*) (cons result rest))
              (begin
                (sqlite3-clear-bindings *statement*)
                (sqlite3-reset *statement*)
                '()))))))
