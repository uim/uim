;;; predict-look-skk.scm: predicion using look with SKK-JISYO
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

(require-extension (srfi 1))

(require-dynlib "look")

(require "wlos.scm")

(define-class predict-look-skk predict
  '((limit 10)
    (jisyo "/usr/share/skk/SKK-JISYO.L")) ;; SKK-JISYO
  '(search))

(class-set-method! predict-look-skk search
  (lambda (self str)
    (let* ((looked (look-lib-look #f #f
                                  (predict-look-skk-limit self)
                                  (predict-look-skk-jisyo self)
                                  str))
           (ret (map (lambda (x)
                       (string-split x " "))
                     (if looked
                         looked
                         '())))
           (yomi/kanji/appendix
            (apply append
                   (map (lambda (x)
                          (map (lambda (l)
                                 (cons
                                  (string-append str (car x))
                                  (let ((s (string-split l ";")))
                                    (if (= 2 (length s))
                                            s
                                            (list (car s) "")))))
                               (filter (lambda (s)
                                         (not (string=? "" s)))
                                       (string-split (cadr x) "/"))))
                        ret)))
           (yomi     (map (lambda (x) (list-ref x 0)) yomi/kanji/appendix))
           (kanji    (map (lambda (x) (list-ref x 1)) yomi/kanji/appendix))
           (appendix (map (lambda (x) (list-ref x 2)) yomi/kanji/appendix)))
      (make-predict-result
       (if (< (length yomi/kanji/appendix) (predict-look-skk-limit self))
           yomi
           (take yomi (predict-look-skk-limit self)))
       (if (< (length yomi/kanji/appendix) (predict-look-skk-limit self))
           kanji
           (take kanji (predict-look-skk-limit self)))
       (if (< (length yomi/kanji/appendix) (predict-look-skk-limit self))
           appendix
           (take appendix (predict-look-skk-limit self)))))))

(define (make-predict-look-skk-with-custom)
  (let ((obj (make-predict-look-skk)))
    (predict-look-skk-set-jisyo! obj predict-custom-look-skk-jisyo)
    (predict-look-skk-set-limit! obj predict-custom-look-skk-candidates-max)
    obj))
