;;;
;;; $Id:$
;;;
;;; plugin.scm: Plugin for uim.
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(define uim-plugin-lib-load-path
  (list (string-append (getenv "HOME") "/.uim.d/plugin")
	(string-append (sys-pkglibdir) "/plugin")))
(define uim-plugin-scm-load-path
  (list (string-append (getenv "HOME") "/.uim.d/plugin")
	(sys-pkgdatadir)))

(if (getenv "LIBUIM_PLUGIN_DIR")
    (set! uim-plugin-lib-load-path
	  (append (list (getenv "LIBUIM_PLUGIN_DIR"))
		  uim-plugin-lib-load-path)))
(if (getenv "LIBUIM_SCM_FILES")
    (set! uim-plugin-scm-load-path
	  (append (list (getenv "LIBUIM_SCM_FILES"))
		  uim-plugin-scm-load-path)))

;;; XXX
(if (getenv "LD_LIBRARY_PATH")
    (set! uim-plugin-lib-load-path
	  (append uim-plugin-lib-load-path
		  (string-split (getenv "LD_LIBRARY_PATH") ":"))))

;; 'print' prevents testing framework from normal run.
;;(print uim-plugin-lib-load-path)
;;(print uim-plugin-scm-load-path)
