;;; predict-google-suggest.scm: google suggest prediction module
;;;
;;; Copyright (c) 2011- uim Project https://github.com/uim/uim
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
(require "http-client.scm")
(require "util.scm")
(require "wlos.scm")

(require-dynlib "expat")

(define-class predict-google-suggest predict
  '((use-ssl #t)
    (language 'en)
    (internal-charset "UTF-8")
    (limit 5))
  '(parse
    suggest
    search))

(define google-suggest-charset-alist
  '((ja . "Shift-JIS")))

(class-set-method! predict-google-suggest parse
  (lambda (self xml-str)
    (let ((parser (xml-parser-create "UTF-8"))
          (path '())
          (data '()))
      (define (elem-start name atts)
        (if (and (equal? name "suggestion")
                 (equal? path '("toplevel" "CompleteSuggestion")))
            (set! data (append data
                               (map cdr
                                    (filter (lambda (x) (equal? (car x) "data")) atts)))))
        (set! path (append path (list name))))
      (define (elem-end name)
      (set! path (drop-right path 1)))
      (if xml-str
          (begin
            (xml-element-handler-set! parser elem-start elem-end)
            (xml-parse parser xml-str 1)
            data)
          '()))))

(class-set-method! predict-google-suggest suggest
  (lambda (self str)
    (define google-suggest-server
      (if (predict-google-suggest-use-ssl self)
          "encrypted.google.com"
          "google.com"))
    (define lang-query
      (if (assq (predict-google-suggest-language self)
                google-suggest-charset-alist)
          (format "&hl=~a" (symbol->string (predict-google-suggest-language self)))
          ""))
    (define (string->lang str)
      (if (assq (predict-google-suggest-language self)
                google-suggest-charset-alist)
          (iconv-convert "UTF-8"
                         (assq-cdr (predict-google-suggest-language self)
                                   google-suggest-charset-alist)
                         str)
          str))
    (and-let* ((uri-string (predict->internal-charset self str)))
      (let* ((proxy (make-http-proxy-from-custom))
             (ssl (and (predict-google-suggest-use-ssl self)
                       (make-http-ssl (SSLv3-client-method) 443)))
             (result (http:get google-suggest-server
                               (format "/complete/search?output=toolbar&q=~a~a"
                                       uri-string
                                       lang-query)
                               80
                               proxy
                               ssl))
             (parsed (predict-google-suggest-parse self (string->lang result))))
        (map (lambda (s)
               (predict->external-charset self s))
             parsed)))))

(class-set-method! predict-google-suggest search
  (lambda (self str)
    (let* ((suggest (predict-google-suggest-suggest self
                                                    str))
           (ret (if (< (predict-google-suggest-limit self) (length suggest))
                    (take suggest (predict-google-suggest-limit self))
                    suggest)))
      (make-predict-result
       ret
       ret
       (map (lambda (x) "") (iota (length ret)))))))


(define (make-predict-google-suggest-with-custom)
  (let ((obj (make-predict-google-suggest)))
    (predict-google-suggest-set-limit! obj predict-custom-google-suggest-candidates-max)
    (predict-google-suggest-set-language! obj predict-custom-google-suggest-language)
    (predict-google-suggest-set-use-ssl! obj predict-custom-google-suggest-use-ssl)
    obj))

