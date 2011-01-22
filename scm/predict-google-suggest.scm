;; Copyright (c) Iwata <iwata@quasiquote.org>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
    (define (iconv-convert to-code from-code from-str)
      (if (equal? to-code from-code)
          from-str
          (and-let* ((ic (iconv-open to-code from-code))
                     (to-str (iconv-code-conv ic from-str)))
                    (iconv-release ic)
                    to-str)))
    (define google-suggest-server
      (if (predict-google-suggest-use-ssl self)
          "encrypted.google.com"
          "google.com"))
    (define (make-lang-query)
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
                                       (make-lang-query))
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

