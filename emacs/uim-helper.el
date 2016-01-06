;; 
;;  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim
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

(defun uim-helper-send-message (helperstr)
  (mapcar
   '(lambda (x)
      (process-send-string uim-el-helper-agent-process (concat x "\n"))
      )
   helperstr)
  )


(defun uim-helper-handler (msg)
  (let* ((message (substring msg 0 (- (length msg) 1)))
	 (cmd (format "%d HELPER %s" uim-context-id message)))
    (cond ((or (eq (string-match "prop_activate" message) 0)
	       (eq (string-match "im_change_whole_desktop" message) 0)
	       (eq (string-match "im_change_this_text_area_only" message) 0))
	   (uim-do-send-recv-cmd cmd)
	   ;; update all buffer
	   (save-current-buffer
	     (mapcar
	      '(lambda (x)
		 (set-buffer x)
		 (if (and (boundp 'uim-mode) uim-mode)
		     (uim-do-send-recv-cmd (format "%d NOP" uim-context-id)))
		 )
	      (buffer-list)))
	   )
	  (t
	   (uim-do-send-recv-cmd cmd))))
  )


(defun uim-helper-message-processor ()
    (let (eom msg)
      (while (setq eom (string-match "\n" uim-helper-message))
	(setq msg (substring uim-helper-message 0 (+ eom 1)))
	(setq uim-helper-message (substring uim-helper-message (+ eom 1)))
	(if (> (length msg) 0)
	    (uim-helper-handler msg))
	)
      )
    )

(defun uim-helper-filter (process output)
  (let ((inhibit-quit t))
    (setq uim-helper-message (concat uim-helper-message output))
    (uim-helper-message-processor)
    )
  )


(defun uim-helper-process-sentinel (proc stat)
  (message "uim-helper.el: %s" stat)
  (setq uim-el-helper-agent-process nil)
  )



(defun uim-el-helper-agent-start ()
  (let (proc
	(buffer (get-buffer-create uim-el-helper-agent-buffer-name)) )

    (save-current-buffer
      (set-buffer buffer) (erase-buffer))

    (message "uim.el: starting uim-el-helper-agent...")

    (setq proc (start-process "uim-el-helper-agent" 
			      buffer uim-el-helper-agent))

    (if (not proc)
	(error "uim.el: Couldn't invoke uim-el-helper-agent."))

    (process-kill-without-query proc)

    ;; wait "OK"
    (let ((patience uim-startup-timeout) (ok nil))
      (save-current-buffer
	(set-buffer buffer)
	(while (not ok)
	  (accept-process-output proc 0 uim-el-agent-accept-timeout)
	  (goto-char 1)
	  (end-of-line)
	  (if (string= "OK" (buffer-substring 1 (point)))
	      (setq ok t)
	    (setq patience (- patience (/ uim-el-agent-accept-timeout 1000)))
	    (if (<= patience 0)
		(progn
		  (kill-process (process-name proc))
		  (error "uim.el: uim-el-helper-agent is unreponsive.  Giving up.")))))
	(erase-buffer)))
	

    (set-process-filter proc 'uim-helper-filter)

    (setq uim-el-helper-agent-buffer buffer)

    (set-process-sentinel proc 'uim-helper-process-sentinel)

    (message "uim.el: starting uim-el-helper-agent... done")

    proc 
    ))


(provide 'uim-helper)
