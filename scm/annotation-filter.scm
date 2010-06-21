;;; annotation-filter.scm: generic filter for uim
;;;
;;; Copyright (c) 2010 uim Project http://code.google.com/p/uim/
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

(require-extension (srfi 1))
(require "process.scm")

;;
;; annotation-filter format
;;
;; query_message = "GET\t" query "\t" charset "\n"
;; result_messages = ( result_messages result_message | result_message ) ".\n"
;; result_message = <any characters> "\n"
;; quit_message = "QUIT\n"
;;

(define annotation-filter-pipe-pair #f)

(define (annotation-filter-init)
  (and (not (string=? "" annotation-filter-command))
       (let ((fds (process-io annotation-filter-command)))
         (set! annotation-filter-pipe-pair (cons (open-file-port (car fds))
                                                 (open-file-port (cdr fds))))
         #t)))

(define (annotation-filter-read-message iport)
  (let loop ((line (file-read-line iport))
             (rest ""))
    (if (string=? "." line)
        rest
        (loop (file-read-line iport) (string-append rest line)))))

(define (annotation-filter-get-text text enc)
  (or (and annotation-filter-pipe-pair
           (let ((iport (car annotation-filter-pipe-pair))
                 (oport (cdr annotation-filter-pipe-pair)))
             (file-display (format "GET\t~a\t~a\n" text enc) oport)
             (annotation-filter-read-message iport)))
      ""))

(define (annotation-filter-release)
  (if annotation-filter-pipe-pair
      (file-display "QUIT\n" (cdr annotation-filter-pipe-pair)))
  #t)
