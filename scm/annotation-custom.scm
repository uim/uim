;;; annotation-custom.scm: Customization variables for annotation.scm
;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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

(require "i18n.scm")

;;
;; Annotation
;;

(define-custom 'enable-annotation? #t
  '(annotation candwin)
  '(boolean)
  (N_ "Enable annotation")
  (N_ "long description will be here."))

(define annotation-agent-list
  (lambda ()
    (let ((has-eb? (require-dynlib "eb"))
          (has-osx-dcs? (require-dynlib "osx-dcs")))
      (if (not custom-full-featured?)
        (begin
          (dynlib-unload "eb")
          (dynlib-unload "osx-dcs")))
      (filter (lambda (x) x)
              (list 'choice
                    (if has-eb?
                      (list 'eb
                            (N_ "EB library")
                            (N_ "EB library"))
                      #f)
                    (list 'dict
                          (N_ "dict server")
                          (N_ "dict server"))
                    (list 'filter
                          (N_ "Custom filter")
                          (N_ "Custom filter"))
                    (if has-osx-dcs?
                      (list 'osx-dcs
                            (N_ "OS X dictionary services")
                            (N_ "OS X dictionary services"))
                      #f)
                    (list 'im
                          (N_ "Input method itself")
                          (N_ "long description will be here.")))))))

(define-custom 'annotation-agent 'eb
  '(annotation candwin)
  (annotation-agent-list)
  (N_ "Annotation agent name")
  (N_ "long description will be here."))

(custom-add-hook 'annotation-agent
  'custom-activity-hooks
  (lambda ()
    enable-annotation?))

(custom-add-hook 'annotation-agent
                 'custom-set-hooks
                 (lambda ()
                   (annotation-unload)
                   (annotation-load
                     (if enable-annotation?
                       (symbol->string annotation-agent)
                       #f))))

;; EB Library support
(define-custom-group 'eb
		     (N_ "EB library")
		     (N_ "long description will be here."))

;; eb-enable-for-annotation? exists only for compatibility.
;; You shouldn't add similar variables to other annotation agents.
(define-custom 'eb-enable-for-annotation? #f
  '(annotation eb)
  '(boolean)
  (N_ "Use EB library to search annotations")
  (N_ "long description will be here."))

(custom-add-hook 'eb-enable-for-annotation?
  'custom-activity-hooks
  (lambda ()
    (and enable-annotation?
      (eq? annotation-agent 'eb))))

(define-custom 'annotation-eb-dic-path
  (string-append (sys-datadir) "/dict")
  '(annotation eb)
  '(pathname directory)
  (N_ "The directory which contains EB dictionary file")
  (N_ "long description will be here."))

(custom-add-hook 'annotation-eb-dic-path
  'custom-activity-hooks
  (lambda ()
    (and enable-annotation?
         eb-enable-for-annotation?
         (eq? annotation-agent 'eb))))

;; dict server support
(define-custom-group 'dict
		     (N_ "dict server")
		     (N_ "long description will be here."))

(define-custom 'annotation-dict-server
  "dict.org"
  '(annotation dict)
  '(string ".*")
  (N_ "Server address of dict")
  (N_ "long description will be here."))

(define-custom 'annotation-dict-servname
  2628
  '(annotation dict)
  '(integer 0 65535)
  (N_ "Server port of dict")
  (N_ "long description will be here."))

(define-custom 'annotation-dict-database
  "web1913"
  '(annotation dict)
  '(string ".*")
  (N_ "Database name of dict")
  (N_ "long description will be here."))

(define-custom 'annotation-dict-cache-words
  256
  '(annotation dict)
  '(integer 0 65535)
  (N_ "Number of cache of annotation")
  (N_ "long description will be here."))

(custom-add-hook 'annotation-dict-server
		 'custom-activity-hooks
                 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'dict))))

(custom-add-hook 'annotation-dict-servname
		 'custom-activity-hooks
                 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'dict))))

(custom-add-hook 'annotation-dict-database
		 'custom-activity-hooks
                 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'dict))))

(custom-add-hook 'annotation-dict-cache-words
		 'custom-activity-hooks
                 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'dict))))

(define-custom-group 'filter
		     (N_ "Custom filter")
		     (N_ "long description will be here."))

(define-custom 'annotation-filter-server-setting? 'pipe
  '(annotation filter)
  (list 'choice
        (list 'unixdomain
              (N_ "Unix domain")
              (N_ "Use UNIX domain socket to communicate with custom filter."))
        (list 'tcpserver
              (N_ "TCP server")
              (N_ "Use tcp server to communicate with custom filter"))
        (list 'pipe
              (N_ "Pipe")
              (N_ "Use pipe. spawn new annotation-filter process to communicate with custom filter")))
  (N_ "Custom filter connection setting")
  (N_ "long description will be here."))

(custom-add-hook 'annotation-filter-server-setting?
  'custom-activity-hooks
  (lambda ()
    (and enable-annotation?
         (eq? annotation-agent 'filter))))


(define-custom 'annotation-filter-unix-domain-socket-path "/path/of/socket"
  '(annotation filter)
  '(pathname regular-file)
  (N_ "Path of custom filter socket")
  (N_ "long description will be here."))

(define-custom 'annotation-filter-tcpserver-name "localhost"
  '(annotation filter)
  '(string ".*")
  (N_ "Custom filter server address")
  (N_ "long description will be here."))

(define-custom 'annotation-filter-tcpserver-port 6789
  '(annotation filter)
  '(integer 0 65535)
  (N_ "Custom filter server port")
  (N_ "long description will be here."))

(define-custom 'annotation-filter-command "/path/of/filter-program"
  '(annotation filter)
  '(pathname regular-file)
  (N_ "Path of custom filter")
  (N_ "long description will be here."))

(custom-add-hook 'annotation-filter-unix-domain-socket-path
		 'custom-activity-hooks
		 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'filter)
                        (eq? annotation-filter-server-setting?
                             'unixdomain))))

(custom-add-hook 'annotation-filter-tcpserver-name
		 'custom-activity-hooks
		 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'filter)
                        (eq? annotation-filter-server-setting?
                             'tcpserver))))

(custom-add-hook 'annotation-filter-tcpserver-port
		 'custom-activity-hooks
		 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'filter)
                        (eq? annotation-filter-server-setting?
                             'tcpserver))))

(custom-add-hook 'annotation-filter-command
		 'custom-activity-hooks
                 (lambda ()
                   (and enable-annotation?
                        (eq? annotation-agent 'filter)
                        (eq? annotation-filter-server-setting?
                             'pipe))))
