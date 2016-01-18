;;;
;;; Copyright (c) 2010-2013 uim Project https://github.com/uim/uim
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

(require "sxml-tools.scm")
(require "sxpathlib.scm")
(require "http-server.scm")
(require "i18n.scm")
(require "util.scm")

(require-dynlib "custom-enabler")

(define (write-tree tree out)
  (let loop ((tree tree))
    (cond ((null? tree))
          ((pair? tree) (write-tree (car tree) out) (loop (cdr tree)))
          (else (display tree out)))))

(define (tree->string tree)
  (call-with-output-string
   (lambda (port)
     (write-tree tree port))))

(define (uim-pref-http:make-primary-group-sxml)
  `(div (% (id "menu"))
        (ul
         ,@(map (lambda (g)
                  `(li
                    (a (% (href ,(format "/~a" (symbol->string g))))
                       ,(ugettext (custom-group-label g)))))
                (custom-list-primary-groups)))))


(define uim-pref-http:menu
   (uim-pref-http:make-primary-group-sxml))

(define (uim-pref-http:top-page resource req-header req-body)
  (tree->string
   (sxml:sxml->xml
    `(html
      (title "uim setup")
      (body
       (h1 "uim setup")
       ,uim-pref-http:menu)))))

(define (uim-pref-http:make-item-boolean group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (active? (custom-active? csym))
         (input-attribute `(% (type "checkbox")
                              (name ,(symbol->string csym))
                              (value ,(symbol->string csym))
                              ,(if (custom-value csym)
                                   '(checked "checked")
                                   '(id "nochecked")))))
    `(input ,(if active?
                 input-attribute
                 (append input-attribute '((disabled "disabled"))))
            ,(ugettext (custom-rec-label rec)))))

(define (uim-pref-http:integer-length num)
  (+ (string-length (number->string num)) 1))
(define (uim-pref-http:make-item-integer group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (active? (custom-active? csym))
         (input-attribute `(% (type "text")
                              (name ,(symbol->string csym))
                              (value ,(number->string (custom-value csym)))
                              (size ,(number->string
                                      (uim-pref-http:integer-length (list-ref types 2)))))))
    `(,(ugettext (custom-rec-label rec))  ": "
      (input ,(if active?
                  input-attribute
                  (append input-attribute '((disabled "disabled"))))))))

(define (uim-pref-http:make-item-string group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (active? (custom-active? csym))
         (input-attribute `(% (type "text")
                              (name ,(symbol->string csym))
                              (value ,(custom-value csym)))))
    `(,(ugettext (custom-rec-label rec))  ": "
      (input ,(if active?
                  input-attribute
                  (append input-attribute '((disabled "disabled"))))))))

(define (uim-pref-http:make-item-pathname group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (active? (custom-active? csym))
         (input-attribute `(% (type "text")
                              (name ,(symbol->string csym))
                              (value ,(custom-value csym)))))
    `(,(ugettext (custom-rec-label rec)) ": "
      (input ,(if active?
                  input-attribute
                  (append input-attribute '((disabled "disabled"))))))))

(define (uim-pref-http:make-item-choice group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (choiced (custom-value csym))
         (active? (custom-active? csym))
         (select-attribute `(% (name ,(symbol->string csym)))))
    `(,(ugettext (custom-rec-label rec))  ": "
      (select ,(if active?
                   select-attribute
                   (append select-attribute '((disabled "disabled"))))
              ,(map (lambda (opt)
                      `(option ,(if (eq? choiced (list-ref opt 0))
                                    '(% (selected "selected"))
                                    "")
                               ,(ugettext (list-ref opt 1))))
                    (cdr types))))))

(define (uim-pref-http:make-item-ordered-list group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (text (apply string-append
                      (map (lambda (x)
                             (string-append (write-to-string (symbol->string x)) " "))
                           (custom-value csym))))
         (active? (custom-active? csym))
         (input-attribute `(% (type "text")
                              (name ,(symbol->string csym))
                              (value ,text))))
    `(,(ugettext (custom-rec-label rec)) ": "
      (input ,(if active?
                  input-attribute
                  (append input-attribute '((disabled "disabled"))))))))

(define (uim-pref-http:make-item-key group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types))
         (text (apply string-append
                      (map (lambda (x)
                             (string-append (write-to-string x) " "))
                           (custom-value csym))))
         (active? (custom-active? csym))
         (input-attribute `(% (type "text")
                              (name ,(symbol->string csym))
                              (value ,text))))
    `(,(ugettext (custom-rec-label rec)) ": "
      (input ,(if active?
                  input-attribute
                  (append input-attribute '((disabled "disabled"))))))))

(define (uim-pref-http:make-item group sub csym)
  (let* ((rec (custom-rec csym))
         (types (custom-rec-type rec))
         (type (car types)))
    `(li
      ,(cond ((eq? type 'boolean)
              (uim-pref-http:make-item-boolean group sub csym))
             ((eq? type 'integer)
              (uim-pref-http:make-item-integer group sub csym))
             ((eq? type 'string)
              (uim-pref-http:make-item-string group sub csym))
             ((eq? type 'pathname)
              (uim-pref-http:make-item-pathname group sub csym))
             ((eq? type 'choice)
              (uim-pref-http:make-item-choice group sub csym))
             ((eq? type 'ordered-list)
              (uim-pref-http:make-item-ordered-list group sub csym))
             ((eq? type 'key)
              (uim-pref-http:make-item-key group sub csym))
             (else
              (format "~a: not supported widget"
                      (ugettext (custom-rec-label rec))))))))


(define (uim-pref-http:make-contents-page group sub)
  `(div (% (id "entry"))
        (h2 ,(ugettext (custom-group-label sub)))
        (form (% (method "post") (action ,(symbol->string group)))
              (ul
               ,(map (lambda (csym)
                       (uim-pref-http:make-item group sub csym))
                     (custom-collect-by-group group sub)))
              (input (% (type "submit") (value "set"))))))

(define (uim-pref-http:regist-pages server)
  (for-each (lambda (group)
              (http-server-regist-resource!
               server
               (format "/~a" (symbol->string group))
               (lambda (resource req-header req-body)
                 ;; (print (format "resource:~a header:~a body:~a" resource req-header req-body))
                 (tree->string
                  (sxml:sxml->xml
                   `(html
                     (title ,(format "uim setup: ~a" (ugettext (custom-group-label group))))
                     (body
                      (h1 (format "uim setup: " ,(ugettext (custom-group-label group))))
                      ,uim-pref-http:menu
                      (div (% (id "contents"))
                           ,(map (lambda (sub)
                                   (uim-pref-http:make-contents-page group sub))
                                 (custom-group-subgroups group))))))))))
            (custom-list-primary-groups)))


(define (uim-http-server:start args)
  (let ((server (make-http-server)))
    (http-server-regist-resource! server "/" uim-pref-http:top-page)
    (uim-pref-http:regist-pages server)
    (http-server-resource server)
    (http-server-start server "localhost" 8569)
    1))
