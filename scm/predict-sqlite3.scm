;;; predict-sqlite3.scm: sqlite3 prediction module
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

(require-extension (srfi 1 2))

(require "sqlite3.scm")
(require "wlos.scm")
(require "i18n.scm")

(define (predict-sqlite3-make-prepare-table *db*)
  (sqlite3-prepare
   *db*
   "CREATE TABLE predict (word TEXT, date DATE, cand TEXT, appendix TEXT);"
   -1))
(define (predict-sqlite3-make-prepare-have-word *db*)
  (sqlite3-prepare
   *db*
   "SELECT * FROM predict WHERE word = ? AND cand = ? AND appendix = ? LIMIT 1;"
   -1))
(define (predict-sqlite3-make-prepare-prefix-search *db*)
  (sqlite3-prepare
   *db*
   "SELECT word, cand, appendix FROM predict WHERE word LIKE ? ORDER BY date DESC LIMIT ?;"
   -1))
(define (predict-sqlite3-make-prepare-insert *db*)
  (sqlite3-prepare
   *db*
   "INSERT INTO predict VALUES(?, datetime('now', 'localtime'), ?, ?);"
   -1))
(define (predict-sqlite3-make-prepare-update *db*)
  (sqlite3-prepare
   *db*
   "UPDATE predict SET date = datetime('now', 'localtime') WHERE word = ? AND cand = ? AND appendix = ?;"
   -1))

(define-class predict-sqlite3 predict
  '((limit 5)
    (db-filename #f)
    (db #f)
    (internal-charset "UTF-8")
    (external-charset "UTF-8")
    (*insert-statement* #f)
    (*have-word-statement* #f)
    (*prefix-search-statement* #f)
    (*update-statement* #f))
  '(create-db-path!
    open
    close
    search
    commit))

(class-set-method! predict-sqlite3 create-db-path!
  (lambda (self im-name)
    (let ((config-path (get-config-path! #f)))
      (if (create/check-directory! (format "~a/dict" config-path))
          (format "~a/dict/predict-~a.sqlite3" config-path im-name)
          (begin
            (uim-notify-fatal (N_ "cannot create dictionary directory"))
            #f)))))

(class-set-method! predict-sqlite3 open
  (lambda (self im-name)
    (let* ((db-filename (predict-sqlite3-create-db-path! self im-name))
           (create-new? (not (file-readable? db-filename)))
           (*db* (sqlite3-open db-filename)))
      (if create-new?
          (let ((*statement*
                 (car (predict-sqlite3-make-prepare-table *db*))))
            (sqlite3-step *statement*)
            (sqlite3-reset *statement*)))
      (predict-sqlite3-set-db-filename! self db-filename)
      (predict-sqlite3-set-db! self *db*)
      (if (not (predict-sqlite3-*insert-statement* self))
          (predict-sqlite3-set-*insert-statement*! self (car (predict-sqlite3-make-prepare-insert *db*))))
      (if (not (predict-sqlite3-*have-word-statement* self))
          (predict-sqlite3-set-*have-word-statement*! self (car (predict-sqlite3-make-prepare-have-word *db*))))
      (if (not (predict-sqlite3-*prefix-search-statement* self))
          (predict-sqlite3-set-*prefix-search-statement*! self (car (predict-sqlite3-make-prepare-prefix-search *db*))))
      (if (not (predict-sqlite3-*update-statement* self))
          (predict-sqlite3-set-*update-statement*! self (car (predict-sqlite3-make-prepare-update *db*))))
      #t)))

(class-set-method! predict-sqlite3 close
  (lambda (self)
    (sqlite3-close (predict-sqlite3-db self))
    (predict-sqlite3-set-db-filename! self #f)))

(class-set-method! predict-sqlite3 search
  (lambda (self str)
    (let ((ret (sqlite3-run-statement (predict-sqlite3-*prefix-search-statement* self)
                                      (lambda (*statement*)
                                        (list
                                         (predict->external-charset
                                          self
                                          (sqlite3-column-text *statement* 0))
                                         (predict->external-charset
                                          self
                                          (sqlite3-column-text *statement* 1))
                                         (predict->external-charset
                                          self
                                          (sqlite3-column-text *statement* 2))))
                                      (string-append (predict->internal-charset self str)
                                                     "%")
                                      (predict-sqlite3-limit self))))
      (if ret
          (make-predict-result
           (map (lambda (x) (list-ref x 0)) ret)
           (map (lambda (x) (list-ref x 1)) ret)
           (map (lambda (x) (list-ref x 2)) ret))
          '()))))

(class-set-method! predict-sqlite3 commit
  (lambda (self word cand appendix)
    (let ((intern-word     (predict->internal-charset self word))
          (intern-cand     (predict->internal-charset self cand))
          (intern-appendix (predict->internal-charset self appendix)))
      (if (null? (sqlite3-run-statement (predict-sqlite3-*have-word-statement* self)
                                        (lambda (*statement*)
                                          (sqlite3-column-text *statement* 0))
                                        intern-word intern-cand intern-appendix))
          (sqlite3-run-statement (predict-sqlite3-*insert-statement* self)
                                 (lambda (*statement*) #t)
                                 intern-word intern-cand intern-appendix))
          (sqlite3-run-statement (predict-sqlite3-*update-statement* self)
                                 (lambda (*statement*) #t)
                                 intern-word intern-cand intern-appendix))))

(define (make-predict-sqlite3-with-custom)
  (if (not (provided? "sqlite3"))
      (begin
        (uim-notify-info "uim-sqlite3 is not installed.")
        (make-predict))
      (let ((obj (make-predict-sqlite3)))
        (predict-sqlite3-set-limit! obj predict-custom-sqlite3-candidates-max)
        obj)))
