;;; generic-predict.scm: generic prediction base class
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

(require-extension (srfi 1 48))

(require "util.scm")
(require "wlos.scm")
(require "light-record.scm")

(require-custom "predict-custom.scm")


(define-list-record predict-result
  '((word "")
    (cands ())      ;; list of string
    (appendix ()))) ;; list of string

(define-class predict object
  '((limit 10)
    (internal-charset "UTF-8")
    (external-charset "UTF-8"))
  '(open
    close
    search
    commit
    convert-charset
    >internal-charset
    >external-charset))

(class-set-method! predict open
  (lambda (self im-name)
    #t))

(class-set-method! predict close
  (lambda (self)
    #t))

(class-set-method! predict search
  (lambda (self str)
    (make-predict-result str str '())))

(class-set-method! predict commit
  (lambda (self word cand appendix)
    #t))

(class-set-method! predict convert-charset
  (lambda (self str tocode fromcode)
    (iconv-convert tocode fromcode str)))

(class-set-method! predict >internal-charset
  (lambda (self str)
    (predict-convert-charset
     self
     str
     (predict-internal-charset self)
     (predict-external-charset self))))
(class-set-method! predict >external-charset
  (lambda (self str)
    (predict-convert-charset
     self
     str
     (predict-external-charset self)
     (predict-internal-charset self))))

(for-each try-load
          '("predict-look.scm"
            "predict-look-skk.scm"
            "predict-sqlite3.scm"
            "predict-google-suggest.scm"))

;;
;; uim-custom specific settings
;;
(define-macro (make-predict-make-meta-search methods)
  `(if predict-custom-enable?
       (map-in-order (lambda (m)
                       (let ((method (find (lambda (x)
                                             (eq? m x))
                                          ,methods)))
                         (if method
                             (string->symbol
                              (format "make-predict-~a-with-custom" method))
                             (error (N_ "unknown prediction method")))))
                     predict-custom-methods)
      '()))

(define (predict-make-meta-search)
  (map-in-order (lambda (m)
                  (eval (list m) (interaction-environment)))
                (make-predict-make-meta-search '(look look-skk sqlite3 google-suggest))))

(define (predict-meta-open methods im-name)
  (for-each (lambda (obj)
              (predict-open obj im-name))
            methods))

(define (predict-meta-search methods str)
  (map-in-order (lambda (obj)
                  (predict-search obj str))
                methods))

(define (predict-meta-select-result results thunk)
  (apply append
         (filter (lambda (x)
                   (not (null? x)))
                 (map thunk results))))

(define (predict-meta-word? results)
  (predict-meta-select-result results predict-result-word))

(define (predict-meta-candidates? results)
  (predict-meta-select-result results predict-result-cands))

(define (predict-meta-appendix? results)
  (predict-meta-select-result results predict-result-appendix))

(define (predict-meta-set-external-charset! methods external-charset)
  (for-each (lambda (obj)
              (predict-set-external-charset! obj external-charset))
            methods))

(define (predict-meta-set-internal-charset! methods external-charset)
  (for-each (lambda (obj)
              (predict-set-internal-charset! obj external-charset))
            methods))

(define (predict-meta-commit methods word cands appendix)
  (for-each (lambda (obj)
              (predict-commit obj word cands appendix))
            methods))



