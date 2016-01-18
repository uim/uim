;; 
;;  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or
;;  without modification, are permitted provided that the
;;  following conditions are met:
;;
;;  1. Redistributions of source code must retain the above
;;     copyright notice, this list of conditions and the
;;     following disclaimer.
;;  2. Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the
;;     following disclaimer in the documentation and/or other
;;     materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its
;;     contributors may be used to endorse or promote products
;;     derived from this software without specific prior written
;;     permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(defvar uim-debug-enable nil) 
(defvar uim-debug-buffer nil)
(defconst uim-debug-buffer-name "uim-debug")

(defvar uim-timestamp-list nil)

(defun uim-debug (msg)
  (if uim-debug-enable
      (progn
	(if (not uim-debug-buffer)
	    (setq uim-debug-buffer (get-buffer-create uim-debug-buffer-name)))
	(save-current-buffer
	  (set-buffer uim-debug-buffer)
	  (goto-char 0)
	  (insert msg)
	  (insert "\n")))))

(defun uim-timestamp (name)
  (setq uim-timestamp-list (append uim-timestamp-list
				   (list (list (current-time) name)))))

(defun uim-reset-timestamp ()
  (setq uim-timestamp-list nil))

(defun uim-show-timestamp ()
  (let (sec1 sec2 microsec current (start 0))
    (while uim-timestamp-list
      (setq sec1 (nth 0 (caar uim-timestamp-list)))
      (setq sec2 (nth 1 (caar uim-timestamp-list)))
      (setq microsec (nth 2 (caar uim-timestamp-list)))
      (setq current (+ (* (+ (lsh sec1 16) sec2) 1000) (/ microsec 1000)))

      (if (= start 0)
	  (setq start current))

      (uim-debug (format "%5d :%s (%d)"
			 (- current start)
			 (car (cdar uim-timestamp-list))
			 (memory-limit)
			 ))

      (setq uim-timestamp-list (cdr uim-timestamp-list)))))

(provide 'uim-debug)

