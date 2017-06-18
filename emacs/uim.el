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

(require 'uim-var)
(require 'uim-keymap)
(when uim-xemacs
  (require 'overlay)
  (load "mule-util"))

(require 'uim-debug)
(require 'uim-util)
(require 'uim-key)

(require 'uim-preedit)
(require 'uim-candidate)
(require 'uim-helper)


;;
;; Add uim-mode to minor modes
;;
(defun uim-init-minor-mode ()
  ;; register uim-mode to minor-mode alist
  (if (not (assq 'uim-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons
	     (cons 'uim-mode '(uim-mode-line-string))
	     minor-mode-alist)))

  ;; register keymap of uim.el to minor-mode keymap alist
  (if (not (assq 'uim-mode minor-mode-map-alist))
      (setq minor-mode-map-alist
	    (cons
	     (cons 'uim-mode nil)
	     minor-mode-map-alist))))


;;
;; Get context ID
;;   If there are any used IDs, use one of them preferentially
;;
(defun uim-get-context-id ()
  (let (id)
    (if (not (null uim-context-id-recycle))
	(progn
	  (setq id (car uim-context-id-recycle))
	  (setq uim-context-id-recycle (cdr uim-context-id-recycle)))
      (setq id uim-context-id-max)
      (setq uim-context-id-max (+ uim-context-id-max 1)))
    id))

;;
;; Release context ID
;;   The released ID is added to the used-ID's list
;;
(defun uim-release-context-id (id)
  (setq uim-context-id-recycle (cons id uim-context-id-recycle)))

;;
;; Validation IM
;;
(defun uim-check-im (im)
  (if (assoc im uim-im-alist)
      t
    (message (format "uim.el: invalid IM engine: %s" im))))

;;
;; Get default IM engine name.
;;
(defun uim-get-default-engine ()
  (if (and uim-default-im-engine
	   (uim-check-im uim-default-im-engine))
      uim-default-im-engine
    nil))


;;
;; Update default IM name of the uim-el-agent.
;;   (for uim-im-switcher 
;;       "Change whole desktop" and "Change this text area only")
;;
(defun uim-update-default-engine (engine)

  (when (not (equal uim-default-im-engine engine))
    (setq uim-default-im-engine engine)
    (run-hooks 'uim-update-default-engine-hook)
    ))

;;
;; Update IM name currently used in this buffer.
;;   (for uim-im-switcher "Change this text area only")
;;
(defun uim-update-current-engine (engine)

  (setq engine (car engine))

  (when (not (equal uim-current-im-engine engine))
    (setq uim-current-im-engine engine) 
    (run-hooks 'uim-update-current-engine-hook))

  ;; update current decoding code   
  (let ((newcode (uim-get-emacs-encoding uim-current-im-engine)))
    (when (not (string= newcode uim-decoding-code))
      (setq uim-decoding-code newcode)
      (uim-change-process-encoding uim-decoding-code)))
  )


;;	
;; Create new context
;;
(defun uim-context-create ()
  (let (id)
    ;; get new context ID
    (setq id (uim-get-context-id))

    ;;    Default context's encoding is determined by 
    ;;  the name of default IM 
    (uim-do-send-recv-cmd 
     (format "%d NEW %s" id (uim-get-uim-encoding (uim-get-default-engine))))
    id))


;;
;; Delete context
;;   The context is kept until the buffer is killed
;;
(defun uim-delete-context ()
  (uim-do-send-recv-cmd (format "%d RELEASE" uim-context-id))
  (uim-release-context-id uim-context-id))


;;
;; Reset context
;;
(defun uim-reset-context ()
  (uim-do-send-recv-cmd (format "%d RESET" uim-context-id)))


;;
;; Get serial number for the next message
;;
(defun uim-get-communication-serial-number ()
  (if (= uim-communication-serial-number 65535)
      (setq uim-communication-serial-number 0))
  (setq uim-communication-serial-number
	(+ uim-communication-serial-number 1)))

;;
;; Focused 
;;
(defun uim-focused ()
  (if uim-preedit-keymap-enabled
      (progn
	;; If preedit strings and/or candidates are displayed,
	;; other keymaps of minor-mode-map-alist should be disabled.
	;; Since minor-mode-map-alist is not a buffer local variable, 
	;; we must re-enable the other keymaps of minor-mode-map-alist
	;; when the focus has moved to other buffer.
	;; So, we also need to re-disable other keymaps of minor-mode-map-alist
	;; when the focus has move to a buffer which have
	;; preedit strings and/or candidates.
	(uim-disable-other-minor-mode-map)
	(uim-enable-preedit-keymap))
    (uim-enable-mode-keymap))

  (uim-change-process-encoding uim-decoding-code)
  (setq uim-focused-buffer (current-buffer))
  (uim-do-send-recv-cmd (format "%d FOCUSED" uim-context-id))
  )

;;
;; Unfocused
;;
(defun uim-unfocused ()
  (setq uim-focused-buffer nil)
  ;; don't send a message to uim-el-agent if it has been dead
  (if uim-el-agent-process
      (uim-do-send-recv-cmd (format "%d UNFOCUSED" uim-context-id)))
  ;; Enable other keymaps of minor-mode-map-alist.  See uim-focused.
  (if uim-preedit-keymap-enabled
      (uim-enable-other-minor-mode-map))
  )


;;
;; Force disable uim-mode
;;
(defun uim-force-off ()
  ;; cleanup displayed preedit and candidate
  (setq uim-last-key-vector nil)
  (uim-process-agent-output '(("e")))
;  (uim-reset-context)
  (uim-release-context-id uim-context-id)
  (run-hooks 'uim-force-inactivate-hook)
  (if uim-mode
      (uim-mode-off))
  )

;;
;; Detect switching of current focused buffer
;;   This function is hooked on 'post-command-hook and 
;;  called after every operation
;;
(defun uim-post-command ()

  ;; detect switching of current buffer
  (when (not (eq uim-recent-buffer (current-buffer)))

    ;; update recent bufferto detect next change
    (setq uim-recent-buffer (current-buffer))
    
    ;; remove Uim focus from previous buffer
    (when (and (bufferp uim-focused-buffer)
	     (buffer-name uim-focused-buffer))
	(save-current-buffer
	  (set-buffer uim-focused-buffer)
	(when (and uim-initialized uim-mode)
	    (uim-unfocused)
	    )))
	    
    ;; set Uim focus to current buffer if it has Uim context
    (when (and uim-initialized uim-mode)
	  (uim-focused)
	  )
    )
  )


;;
;; Update IM label
;;
(defun uim-update-im-label ()
  (uim-do-send-recv-cmd (format "%d LABEL" uim-context-id)))


;;
;; Overwrite properties of current context
;;
(defun uim-prop-activate (proplist)
  (mapcar 
   '(lambda (x)
      (uim-do-send-recv-cmd (format "%d PROP %s" uim-context-id x)))
   proplist))

;;
;; Update process encoding 
;; 
(defun uim-change-process-encoding (outcode)
  (set-process-coding-system uim-el-agent-process outcode 'iso-8859-1)
  )

;; 
;; Get IM information from uim-im-alist
;;
(defun uim-get-im-info (im field)
  (nth field (or (assoc im uim-im-alist)
		 (assoc "direct" uim-im-alist))))

(defun uim-get-uim-lang (im)
  (uim-get-im-info im 1))

(defun uim-get-emacs-lang (im)
  (uim-get-im-info im 2))

(defun uim-get-emacs-encoding (im)
  (uim-get-im-info im 3))

(defun uim-get-uim-encoding (im)
  (uim-get-im-info im 4))


;;
;; Change IM engine.
;; 
(defun uim-change-im (im)
  ;; change decoding method temporarily to receive encoded IM label etc.
  (uim-change-process-encoding (uim-get-emacs-encoding im))
  (uim-do-send-recv-cmd 
   (format "%d CHANGE %s" uim-context-id im))

  (if uim-default-im-prop
      (uim-prop-activate uim-default-im-prop)))


;;
;; Set each IM's output encoding 
;;
(defun uim-set-encoding (im encoding)
  (uim-do-send-recv-cmd 
   (format "0 SETENC %s %s" im encoding)))



;;
;; Initialize each buffer 
;;   Called when a context has been created
;;
(defun uim-init-buffer ()

  ;; if buffer has no context
  (setq uim-context-id (uim-context-create))

  ;; redraw Uim related objects when window has been resized
  (when uim-xemacs
    (make-local-hook 'window-configuration-change-hook))
  (add-hook 'window-configuration-change-hook 'uim-window-changed nil 'local)

  (when uim-xemacs
    (make-local-hook 'kill-buffer-hook))
  (add-hook 'kill-buffer-hook 'uim-kill-buffer nil 'local)

  ;; hide candidate/preedit when buffer-save has been called
  (add-hook 'local-write-file-hooks 
	    '(lambda ()
	       (if (or uim-preedit-displayed uim-candidate-displayed)
		   (uim-process-agent-output '(("e"))))))

  ;; change IM engine to uim-default-im-engine
  (if (and uim-default-im-engine
	   (uim-check-im uim-default-im-engine))
      (uim-change-im uim-default-im-engine))

  ;; set properties
  (if uim-default-im-prop
      (uim-prop-activate uim-default-im-prop))

  (run-hooks 'uim-buffer-init-hook)

  (setq uim-initialized t)
  )


(defun uim-im-init ()
  (when (not uim-im-initialized)
    (uim-init-im-alist)
    (uim-init-im-encoding)
    (setq uim-im-initialized t)
    ))


;;
;; Activate uim
;;
(defun uim-mode-on ()

  (if uim-mode
      (message "uim.el: uim-mode is already activated. (buffer %s)" 
	       (current-buffer)))

  ;; Initialize IM encoding
  (uim-im-init)

  ;; override some functions
  (uim-this-command-keys-override)
  (if (= emacs-major-version 21)
      (uim-read-char-exclusive-override))

  (if (not buffer-read-only)
      (progn 

	;; initialize context and buffer
	(when (not uim-initialized)
	  (uim-init-buffer))

	;; enable and update keymap
	(uim-enable-mode-keymap)

	;; focus to current context
	(uim-focused)

	;; update mode-line
	(force-mode-line-update)

	;; really enable minor-mode
	(setq uim-mode t))

    ;; don't activate if the current buffer is read-only
    (message "uim.el: This buffer is read-only.")
    )
  )


;;
;; Deactivate Uim
;;
(defun uim-mode-off ()
  
  (if (not uim-mode)
      (message "uim.el: uim-mode is already inactivated."))

  (uim-unfocused)

  (uim-process-agent-output '(("e")))

  ;; update mode-line
  (force-mode-line-update)

  ;; Disable uim-mode
  (setq uim-mode nil)

  ;; restore original functions
  (uim-this-command-keys-restore)
  (if (= emacs-major-version 21)
      (uim-read-char-exclusive-restore))

  )


;;
;; Release context when buffer is killed or major-mode changed
;;   This function is called by uim-initialized buffer only
;;
(defun uim-kill-buffer ()

  ;; unfocus
  (if (eq uim-focused-buffer (current-buffer))
      (uim-unfocused))

  ;; delete context
  (uim-delete-context)
  )


;;
;; Reset context
;;
(defun uim-change-major-mode ()

  (when uim-initialized
       (uim-unfocused)
       (uim-delete-context))
  )


;;
;; start uim-el-agent process
;;
(defun uim-el-agent-start ()
  (let (proc
	(buffer (get-buffer-create uim-el-agent-buffer-name)) )

    (if (not uim-el-helper-agent-process)
	(setq uim-el-helper-agent-process (uim-el-helper-agent-start)))

    ;; erase buffer
    (save-current-buffer
      (set-buffer buffer) (erase-buffer))

    (message "uim.el: starting uim-el-agent...")

    (setq proc (start-process "uim-el-agent" buffer uim-el-agent))

    (if (not proc)
	(error "uim.el: Couldn't invoke uim-el-agent."))
    
    ;; don't ask kill
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
		  (error "uim.el: uim-el-agent is unreponsive.  Giving up.")))))
	(erase-buffer)))
	
    (setq uim-el-agent-buffer buffer)

    (set-process-sentinel proc 'uim-process-sentinel)

    (message "uim.el: starting uim-el-agent... done")

    proc 
    ))


;;
;; Stop uim-el-agent
;;
(defun uim-el-agent-stop (proc)
  (if (and proc
	   (process-status proc))
      (interrupt-process proc)))


;;
;; Restore undo list
;;
(defun uim-restore-undo ()
  (when uim-buffer-undo-list-saved
    (buffer-enable-undo)
    (setq buffer-undo-list uim-buffer-undo-list)
    (setq uim-buffer-undo-list-saved nil))
  )


;;
;; Save undo list
;;
(defun uim-save-undo ()
  (when (not uim-buffer-undo-list-saved)
    (setq uim-buffer-undo-list buffer-undo-list)
    (buffer-disable-undo)
    (setq uim-buffer-undo-list-saved 't))
  )


;;
;; Update IM list
;;
(defun uim-update-imlist (imlist)
  (setq uim-im-alist 
	(mapcar '(lambda (x) 
		   (let ((im (nth 0 x))
			 (lang-uim (nth 2 x)))
		     (cons im
			   (or (assoc lang-uim uim-lang-code-alist)
			       (assoc "Other" uim-lang-code-alist)))))
		imlist))
  )


;;
;; Update property label
;;
(defun uim-update-label (label)
  (let ((mode-str ""))
    (mapcar
     '(lambda (x)
	(cond ((string= (nth 0 x) "im-mode")
	       (setq mode-str (concat mode-str (nth 2 x))))
	      ((string= (nth 0 x) "im-name")
	       (setq uim-im-indication-id (nth 1 x))
	       (setq uim-im-name-str (nth 3 x)))
	      )
	)
     label)

    (setq uim-im-mode-str mode-str)

    (setq uim-mode-line-string
	  (concat (if (or uim-show-im-name uim-show-im-mode) " ")
		  (if uim-show-im-name
		      uim-im-name-str "")
		  (if uim-show-im-mode
		      (format "[%s]" uim-im-mode-str) "")))
    )

  (run-hooks 'uim-update-label-hook)  
  )



;;
;; Check lock status of buffer file
;; 
(defun uim-buffer-locked-p ()
  (if (buffer-file-name)
      (let ((locked (file-locked-p (buffer-file-name))))
	(not (or (not locked)     ;; nil when unlocked
		 (eq locked t))))))  ;; locked by this process

	    

;;
;; Cehck the buffer is editable or not
;;
(defun uim-buffer-not-editable ()
  (or (uim-buffer-locked-p)
      (not (verify-visited-file-modtime (current-buffer)))
      buffer-read-only))


;; 
;; Called when the window has been resized
;;
(defun uim-window-changed ()
  (let (candidate-displayed)
  
    (when (and (not uim-candidate-in-echo-region)
	       uim-candidate-displayed
	       uim-candidate-display-inline)
      (setq candidate-displayed uim-candidate-displayed)

      (uim-do-send-recv-cmd (format "%d HIDE" uim-context-id))

      (when (and candidate-displayed
		 (uim-check-candidate-space))
	(uim-do-send-recv-cmd (format "%d SHOW" uim-context-id))))))


;;
;; Reset all uim.el buffer local variables
;;
(defun uim-init-all-local-var ()
   (mapcar
    '(lambda (x)
       (set (car x) (cdr x)))
    uim-local-var))

;; 
;; Sentinel of uim-el-agent 
;;   STOP state is not supported yet
;; 
(defun uim-process-sentinel (proc stat)

  (message "uim.el: %s" stat)

  (setq uim-el-agent-process nil)

  (let ((old-buffer (current-buffer))
	(bufs (buffer-list)))
    (while bufs
      (save-current-buffer
	(set-buffer (car bufs))
	(when uim-initialized
	  ;; disable uim
	  (uim-force-off)
	  ;; reset all local variables
	  (uim-init-all-local-var)
	  (setq uim-im-initialized nil)
	  ))
      (setq bufs (cdr bufs)))
    ))


;;
;; Send command to uim-el-agent
;; 
(defun uim-send-cmd (cmd serial)

  ;; check process status and start if it's not running
  (if (not uim-el-agent-process)
      (setq uim-el-agent-process (uim-el-agent-start)))

  (if uim-el-agent-process
      (progn
	(save-current-buffer
	  (set-buffer uim-el-agent-buffer)
	  (erase-buffer))

        (uim-debug (format "%s"
                           (concat (format "%s " serial) cmd)))
	
	(process-send-string uim-el-agent-process
			     (concat (format "%s " serial) cmd "\n"))
	t)
    nil))


;;
;; Delete the first line from buffer
;;
(defun uim-delete-oneline-from-buffer (buf)
  (save-current-buffer
    (set-buffer buf)
    (beginning-of-buffer)
    (delete-region 1 (progn (end-of-line) (point)))
    (if (= (following-char) ?\n)
	(delete-char 1))))


;;
;; Get 1st one line from buffer
;;   skip non S-expression line
;;
(defun uim-get-oneline-from-buffer (buf)
  (save-current-buffer
    (set-buffer buf)
    (when (> (buffer-size) 0)
      (goto-char 1)
      (skip-chars-forward " ")
      ;; check the first line is beginning with bracket
      (if (= (following-char) ?\( )
	  (progn
	    (end-of-line)
	    (if (looking-at "\n")
		(buffer-substring 1 (point))
	      nil))
	;; if the first line isn't S-expression, skip and return nil
	(uim-delete-oneline-from-buffer buf)
	nil))))


;; 
;; Wait reply from uim-el-agent
;;   Check serial number 
;;
(defun uim-wait-recv (serial)

  (let (rstr
	(helper-filter (process-filter uim-el-helper-agent-process)))

    (set-process-filter uim-el-helper-agent-process nil)

    (let ((start (nth 1 (current-time)))
	  (accept uim-el-agent-accept-timeout))
      (catch 'read-loop
	(while (and (process-status uim-el-agent-process)
		    (< (abs (- start (nth 1 (current-time))))
		      uim-el-agent-timeout))
	  
	  (accept-process-output uim-el-agent-process 0 accept)

	  (if (setq rstr (uim-get-oneline-from-buffer uim-el-agent-buffer))
	      (if (= (car (read rstr)) serial)
		  (throw 'read-loop t)
		(uim-delete-oneline-from-buffer uim-el-agent-buffer)))
	  ;; double the accepting time if waiting again
	  (setq accept (* accept 2)))))

    (save-current-buffer
      (set-buffer uim-el-helper-agent-buffer)
      (when (> (buffer-size) 0)
	(setq uim-helper-message 
	      (concat uim-helper-message
		      (buffer-substring 1 (point-max))))
	(erase-buffer)))
    (set-process-filter uim-el-helper-agent-process helper-filter)

    (unwind-protect
    (cond ((not (process-status uim-el-agent-process))
	   ;; uim-el-agent is dead
	   (message "uim.el: state of uim-el-agent has been changed (died?)"))
	  
	  ((not rstr)
	   (message "uim.el: ***TIMEOUT*** no response from uim-el-agent")
	   ;; stop uim-el-agent (uim-el-agent will be restarting on-demand)
	   (uim-el-agent-stop uim-el-agent-process))

	  (t
	   ;; received
	   ;;   rstr: ( serial# context-id (...) )
	   (uim-process-agent-output (cddr (read rstr)))
	   )
	      )
      (uim-helper-message-processor)
      )
    )
  )


;;
;; Send message to uim-el-agent and wait return
;;
(defun uim-do-send-recv-cmd (cmd)
  (setq uim-last-cmd cmd) ;; keep last command
  (let ((serial (uim-get-communication-serial-number)))
    (if (uim-send-cmd cmd serial)
	(uim-wait-recv serial))))


;; for XEmacs
(defun uim-overwrite-font-face (start end)
  (let ((facelist '()) tail face)
    (catch 'face-loop
      (while t
	(setq tail
	      (next-single-property-change start 'face (current-buffer) end))
	(if (setq face (get-text-property start 'face))
	    (if (atom face)
		(put-text-property start tail 'face 
				   (cons face nil))))
	(setq start tail)
	(if (= start end)
	    (throw 'face-loop t))))
    )
  )


(defun uim-freeze-buffer ()
  (when (not uim-buffer-frozen)
    (setq uim-after-change-functions after-change-functions)
    (setq after-change-functions nil)

    (when (boundp 'timer-idle-list)
      (setq uim-timer-idle-list timer-idle-list)
      (setq timer-idle-list nil))

    (when (boundp 'fontification-functions)
      (setq uim-fontification-functions fontification-functions)
      (setq fontification-functions nil))

    (setq uim-buffer-frozen t))
  )


(defun uim-unfreeze-buffer ()
  (when uim-buffer-frozen ;; avoid multiple unfreeze 
    (setq after-change-functions uim-after-change-functions)
    (setq uim-after-change-functions nil)

    (when (boundp 'timer-idle-list)
      (setq timer-idle-list uim-timer-idle-list)
      (setq uim-timer-idle-list nil))

    (when (boundp 'fontification-functions)
      (setq fontification-functions uim-fontification-functions)
      (setq uim-fontification-functions nil))

    (setq uim-buffer-frozen nil)))

 

;;
;; Reset some global parameters for inputted key processing
;;
(defun uim-reset-input-parameter ()
  (setq uim-translated-key-vector nil)
  (setq uim-untranslated-key-vector nil)
  (setq uim-prefix-arg nil)
  (setq uim-prefix-arg-vector nil)
  (setq uim-merge-next nil))


;;
;; Process inputted key
;;
(defun uim-process-input (&optional arg event reg1 reg2)
  (interactive (list current-prefix-arg 
		     last-input-event
		     ))

  (let (new-key-vector send-vector send-vector-raw issue-vector
        send issue mouse wait discard
	(critical t))

    (unwind-protect
	(progn

	  (if uim-xemacs
	      (setq zmacs-region-stays nil))

	  (if (not (or uim-translated-key-vector uim-untranslated-key-vector))
	      (setq uim-keystroke-displaying nil))

	  (setq new-key-vector (uim-this-command-keys-vector))

	  (if (or current-prefix-arg
		  uim-merge-next)
	      (let (vector-list)
		(setq vector-list (uim-separate-prefix-vector new-key-vector))
		(setq new-key-vector (car vector-list))
		(setq uim-prefix-arg-vector (nth 1 vector-list))

		;; Workaround for FSF Emacs-20/21
		;;  Key event beginning with C-u and terminating with 
		;;  ESC-something invokes uim-process-input with twice 
		;;  at a time.
		;;  In such a case, uim.el merges these two inputs.
		(if (not uim-merge-next)
		    (progn
		      ;; normal
		      (setq uim-prefix-arg current-prefix-arg)
		      (if (and uim-emacs
			       (not window-system)
			       (<= emacs-major-version 21)
			       (>= (length new-key-vector) 2))
			  ;; workaround
			  (setq uim-merge-next t)))

		  ;; uim-merge-next is valid
		  (setq uim-merge-next nil)
		  (setq uim-translated-key-vector nil)
		  (setq uim-untranslated-key-vector nil))
		
		))

	  (if uim-xemacs
	      (setq uim-original-input-event (copy-event last-input-event)))

	  (setq uim-untranslated-key-vector
		(vconcat uim-untranslated-key-vector new-key-vector))
	  
	  (let (translated-list translated map)
	    
	    (setq translated-list 
		  (uim-translate-key uim-untranslated-key-vector))
	    (setq translated (nth 0 translated-list))
	    (setq map (nth 1 translated-list))

	    (cond ((stringp translated)
		   (setq issue t)
		   (setq uim-translated-key-vector translated)
		   (setq uim-untranslated-key-vector nil))

		  ((vectorp translated)
		   (setq send t) 
		   (setq uim-translated-key-vector 
			 (vconcat uim-translated-key-vector translated))
		   (setq uim-untranslated-key-vector nil))

		  (map
		   (setq wait t))

		  (t
		   (setq send t))
		  ))

	  (when send

	    (setq send-vector-raw (vconcat uim-translated-key-vector
					   uim-untranslated-key-vector))

	    (setq send-vector 
		  (if uim-emacs 
		      send-vector-raw
		    (if uim-xemacs
			(uim-convert-keystr-to-uimagent-vector (key-description send-vector-raw))
		      nil)))

	    (cond (uim-merge-next
		   (setq send nil))

		  ((or (and uim-emacs 
			    (eventp event)
			    (memq (event-basic-type event) 
				  '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5)))
		       (and uim-xemacs 
			    (string-match "button\\(1\\|2\\|3\\|4\\|5\\)" 
					  (key-description send-vector-raw))))
		   (setq send nil)
		   (setq mouse t))

		  ((or (and uim-preedit-keymap-enabled
			    (not window-system)
			    (not uim-use-single-escape-on-terminal)
			    (uim-is-single-escape send-vector-raw)))
		   (setq send nil)
		   (setq wait t))

		  ((or current-prefix-arg
		       uim-prefix-arg)
		   (setq send nil)
		   (setq issue t))
		  
		  ((or (eq (car-safe (aref send-vector 0)) 'menu-bar)
		       (eq (car-safe (aref send-vector 0)) 'tool-bar))
		   (setq send nil)
		   (if (not uim-preedit-keymap-enabled)
		       (setq issue t)
		     (if (and uim-preedit-keymap-enabled
			      (keymapp (uim-key-binding send-vector)))
			 (setq wait t)
		       (setq discard t))))

		  ((and (or (>= (length send-vector) 3)
			    (and (= (length send-vector) 2)
				 (not (or (eq (aref send-vector 0) 27)
					  (eq (aref send-vector 0) 'escape))))))
		   (setq send nil)
		   (if (not uim-preedit-keymap-enabled)
		       (setq issue t)))

		  ))


	  (when send

	    (setq uim-last-key-vector send-vector-raw)

	    (when uim-emacs
	      (cond ((equal send-vector [127])   (setq send-vector [backspace]))
		    ((equal send-vector [27 27]) (setq send-vector [27]))
		    ((equal send-vector [28])    (setq send-vector [C-\\]))
		    ((equal send-vector [29])    (setq send-vector [C-\]]))
		    ((equal send-vector [30])    (setq send-vector [C-~]))
		    ((equal send-vector [31])    (setq send-vector [C-_]))
		    ))
	    
	    (when uim-xemacs 

	      (if (equal (make-vector 2 (uim-xemacs-make-event [(escape)]))
			 send-vector-raw)
		  (setq send-vector-raw
			(vector (uim-xemacs-make-event [(escape)]))))

	      (setq send-vector-raw (uim-translate-escape-meta send-vector-raw))

	      (setq send-vector 
		    (uim-convert-keystr-to-uimagent-vector 
		     (key-description send-vector-raw))))

	    (setq uim-wait-next-key nil)
	    (uim-do-send-recv-cmd (format "%d %s" 
					  uim-context-id send-vector))
	    (setq wait uim-wait-next-key)
		
	    (when (not uim-wait-next-key)
	      (uim-reset-input-parameter)))
	    
	  (when mouse
	    (uim-process-mouse-event event)
	    (uim-reset-input-parameter))

	  (when issue
	    (let (issue-vector-raw)
	      (setq issue-vector-raw (vconcat uim-translated-key-vector
					      uim-untranslated-key-vector))
	      
	      (setq issue-vector 
		    (if uim-emacs 
			issue-vector-raw
		      (if uim-xemacs
			  (uim-convert-char-to-symbolvector 
			   (key-description issue-vector-raw)))))
	      
	      (setq uim-last-key-vector issue-vector-raw)
      
	      (setq wait
		    (uim-process-key-vector issue-vector-raw uim-prefix-arg))  

	      (when (not wait)
		(uim-reset-input-parameter))))

	  (when wait

	    (if (and (memq last-command (list 'universal-argument
					      'digit-argument
					      'negative-argument))
		     (current-message))
		(setq uim-keystroke-displaying t))

	    (if (not uim-keystroke-displaying)
		(cond (uim-emacs
		       (let (key)
			 (when (setq key (with-timeout (echo-keystrokes nil)
					   (read-key-sequence-vector nil)))
			   (setq unread-command-events
				 (nconc (listify-key-sequence key) 
					unread-command-events)))
			 
			 (setq uim-keystroke-displaying (not key))))
		      (uim-xemacs
		       (setq uim-keystroke-displaying (sit-for echo-keystrokes)))))

	    ;; display "ESC-" or something
	    (if uim-keystroke-displaying
		(let (message-log-max)
		  (message (concat (key-description 
				    (vconcat uim-prefix-arg-vector
					     (if uim-xemacs
						 (uim-translate-escape-meta uim-translated-key-vector)
					       uim-translated-key-vector)
					     (if uim-xemacs
						 (uim-translate-escape-meta uim-untranslated-key-vector)
					       uim-untranslated-key-vector)))
				   (if uim-xemacs " ")
				   "-"))))

	    (if uim-emacs
		(setq uim-deactivate-mark nil))

	    (if uim-xemacs
		(setq zmacs-region-stays t))
	    )

	  (when discard
	    (uim-reset-input-parameter))


	  ;; display "ESC ESC ESC" or something
	  (when (and (or send issue)
		     (not wait)
		     uim-keystroke-displaying)

	    (let (message-log-max msg)
	      (setq msg (if uim-prefix-arg-vector
			    (concat " " (key-description uim-prefix-arg-vector) " ")
			  ""))

	      (mapcar '(lambda (x)
			 (setq msg (concat msg
					   (key-description (vector x)) " ")))
		 
		      (append (if uim-xemacs
				  (uim-translate-escape-meta send-vector-raw)
				uim-last-key-vector) nil))
	      (message msg)))

	  (if uim-emacs
	      (setq deactivate-mark uim-deactivate-mark))

	  (setq critical nil))

      (when critical
	(uim-reset-input-parameter)))
    )
  )



(defun uim-enter-preedit-mode ()
  ;; change keymap and freeze faces at first time
  (uim-enable-preedit-keymap)
  (uim-disable-other-minor-mode-map)
  (when (= (minibuffer-depth) 0)
    (uim-freeze-buffer)
    (setq uim-buffer-read-only buffer-read-only)
    (setq buffer-read-only t))
  )

(defun uim-leave-preedit-mode ()
  (uim-disable-preedit-keymap)
  (setq buffer-read-only uim-buffer-read-only)
  (uim-unfreeze-buffer)
  )



;; process return expression from uim-el-agent 
(defun uim-process-agent-output (str)
  (catch 'process-agent-output
    (let ((inhibit-quit t)
	  preedit-existed
	  candidate-existed
	  key commit preedit candidate default im label imlist helpermsg
	  )

      (uim-debug (format "%s" str))
      
      (let ((modified (buffer-modified-p)))

	;; remove candidate anyway
	(when uim-candidate-displayed
	  (setq candidate-existed t)
	  (let ((inhibit-read-only t))
	    (uim-remove-candidate))
	  (setq uim-candidate-displayed nil)
	  (uim-goto-char uim-preedit-start))

	;; remove preedit anyway
	(when uim-preedit-displayed
	  (setq preedit-existed t)
	  (let ((inhibit-read-only t))
	    (uim-remove-preedit))
	  (setq uim-preedit-displayed nil))

	;; restore cursor point
	(when (and uim-preedit-keymap-enabled uim-original-cursor)
	  (goto-char uim-original-cursor))

	;; restore modified flag
	(set-buffer-modified-p modified))

      (mapcar 
       '(lambda (x) 
	  (let ((rcode (car x))
		(rval (cdr x)))
	
	    (cond ((string= rcode "n") ;; uim returns key code
		   (if uim-last-key-vector
		       (setq key uim-last-key-vector)
		     (setq key (car rval)))
		   )
		  ((string= rcode "s") ;; commit string
		   (setq commit (append commit (list (car rval))))
		   )
		  ((string= rcode "c") ;; candidate data
		   (setq candidate rval)
		   )
		  ((string= rcode "p") ;; preedit
		   (setq preedit rval)
		   )
		  ((string= rcode "d") ;; default engine
		   (setq default (car rval))
		   )
		  ((string= rcode "i") ;; current im
		   (setq im rval)
		   )
		  ((string= rcode "l") ;; label
		   (setq label rval)
		   )
		  ((string= rcode "h") ;; helper message
		   (setq helpermsg (append helpermsg rval))
		   )
		  ((string= rcode "L") ;; IM list
		   (setq imlist rval)
		   )
		  )
	    )) 
       str)

      (when helpermsg
	(uim-helper-send-message helpermsg))

      (when (and default (not (uim-get-default-engine)))
	(uim-update-default-engine default))

      (when im
	(uim-update-current-engine im))

      (when label
	(uim-update-label label))

      (when imlist
	(uim-update-imlist imlist))


      (if commit

	  ;; insert committed strings
	  (let ((inhibit-read-only t)
		(buffer-frozen uim-buffer-frozen))
	    (if uim-buffer-frozen
		(uim-unfreeze-buffer))

	    (mapcar
	     '(lambda (x)
		;; enable buffer-undo temporarily
		(when uim-buffer-undo-list-saved
		  (setq buffer-undo-list nil)
		  (buffer-enable-undo))

		(let ((buffer-undo-list-tmp buffer-undo-list))
		  (unwind-protect
		      (progn
			(setq buffer-undo-list nil)
			(insert x))
		    (when buffer-undo-list
		      (setq buffer-undo-list
 			    (append (cons nil 
					  (uim-delete-atom buffer-undo-list))
				    buffer-undo-list-tmp))

		      )))

		;; disable buffer-undo temporarily
		(when uim-buffer-undo-list-saved
		    
		  (setq uim-buffer-undo-list
			(append buffer-undo-list uim-buffer-undo-list))
		  (setq buffer-undo-list nil)
		  (buffer-disable-undo)))
	     commit)

	    (if auto-fill-function
		(funcall auto-fill-function))

	    (if buffer-frozen
		(uim-freeze-buffer))
	    )
	)


      (if key
	  (let (keyproc-done
		(buffer-undo-list-saved uim-buffer-undo-list-saved))
	    (when buffer-undo-list-saved
	      (uim-restore-undo))
	    ;; process raw key
	    ;;  C-o is also processed here ... orz
	    (let ((inhibit-read-only t))
	      (unwind-protect
		  (progn

		    ;; remove shift if possible
		    (if (and (uim-check-shift key)
			     (not (uim-key-binding key)))
			;; with shift key but no key bind
			(let* ((keytmp (uim-remove-shift key))
			       (translated (nth 0 (uim-translate-key keytmp))))
			  ;; lookup function-key-map
			  (if translated
			      (setq key translated)
			    (if (uim-key-binding keytmp)
				(setq key keytmp)))))

		    (if (uim-process-key-vector key uim-prefix-arg)
			(setq uim-wait-next-key t))
		    (setq keyproc-done t))
		(when (not keyproc-done)
		  (uim-leave-preedit-mode))
		)
	      ;; following expressions will not be evaluated
	      ;; when an error occurs in this function...
	      )
	    (if (not uim-mode)
		(throw 'process-agent-output t))
	    ;; save undo hisotry again
	    (when buffer-undo-list-saved
	      (uim-save-undo))))

      (setq uim-original-cursor (point))


      (if (or preedit candidate)
	  ;; process preedit/candidate
	  (let ((modified (buffer-modified-p)))

	    ;; save undo list if not saved
	    (when (not uim-buffer-undo-list-saved)
	      (uim-flush-concat-undo)
	      (uim-save-undo))

	    ;; change keymap and freeze faces at first time
	    (when (not uim-preedit-keymap-enabled)
	      (uim-enter-preedit-mode))

	    (setq uim-preedit-start uim-original-cursor)
	    (setq uim-preedit-overlap 0)
	    (setq uim-preedit-current-sentence-start uim-preedit-start)
	    (setq uim-preedit-cursor uim-preedit-start)

	    ;; show preedit
	    (when preedit
	      (setq uim-preedit-displayed t)
	      (let ((inhibit-read-only t))
		(uim-insert-preedit preedit)
		))

	    (uim-goto-char uim-preedit-cursor)

	    (setq uim-candidate-cursor (point))

	    ;; show candidate
	    (when (and candidate
		       (uim-check-candidate-space)
		       (eq uim-focused-buffer (current-buffer)))
	      (setq uim-candidate-displayed t)
	      (setq uim-candidate-start uim-preedit-current-sentence-start)
	      (setq uim-candidate-vofs uim-preedit-overlap)
	      (let ((inhibit-read-only t))
		(uim-show-candidate candidate)))
	  
	    (uim-goto-char uim-candidate-cursor)

	    (set-buffer-modified-p modified)
	    )

	;; no preedit/candidate

	;; restore undo-list if saved
	(when uim-buffer-undo-list-saved
	  (uim-restore-undo))

	;; no raw-key and no preedit/candidate
	(when uim-preedit-keymap-enabled
	  (uim-leave-preedit-mode))

	)

      ;; scroll buffer after candidate removed
      ;;  recenter if buffer is force scrolled at candidate displaying
      (when (and candidate-existed
		 (not uim-candidate-displayed) 
		 uim-window-force-scrolled) 
	(setq uim-window-force-scrolled nil)
	(save-excursion
	  (goto-char uim-window-force-scrolled-original)
	  (recenter 0))
	(setq uim-window-force-scrolled-original nil))

      (if (not uim-send-recv-again)
	  (when label 
	    (setq uim-send-recv-again t)
	    (uim-update-im-label))
	(setq uim-send-recv-again nil))
      ))
  )





;;
;; Get available IM list from uim-el-agent
;;
(defun uim-get-im-list ()
  (uim-do-send-recv-cmd (format "0 LIST")))

(defun uim-init-im-alist ()
  (uim-get-im-list))

;;
;; Initialize IM list and encoding
;;
(defun uim-init-im-encoding ()

  ;; set Uim side encoding to agent
  (mapcar 
   '(lambda (x)
      (let ((im (car x)))
	(uim-set-encoding im (uim-get-uim-encoding im))))
   uim-im-alist))

;;
;; Called when entering into the mini-buffer
;;
(defun uim-minibuffer-enter ()
  nil)

;;
;; Called when leaving from the mini-buffer
;;
(defun uim-minibuffer-exit ()
  (if (and uim-initialized
	   uim-mode)
	;;(uim-mode-off)
      (uim-force-off)
    ))



;;
;; Initialize uim
;;
(defun uim-init ()

  ;; initialize minor-mode
  (uim-init-minor-mode)
  (uim-enable-mode-keymap)

  ;; initialize keymap
  (uim-init-keymap)

  ;; add hook to detect status change of buffer
  (add-hook 'post-command-hook 'uim-post-command)

  ;; disable uim when major-mode has changed
  (add-hook 'change-major-mode-hook 'uim-change-major-mode)

  ;; add hooks to detect minibuffer entering and leaving
  (add-hook 'minibuffer-setup-hook 'uim-minibuffer-enter)
  (add-hook 'minibuffer-exit-hook 'uim-minibuffer-exit)

  ;; advice to support kbd macro
  (defadvice call-last-kbd-macro (around uim-call-last-kbd-macro activate)
    (let ((current-uim-mode uim-mode))
      (setq uim-mode nil)
      (unwind-protect
	  ad-do-it
	(setq uim-mode current-uim-mode))))
  )



;;
;; Toggle uim
;;

(defun uim-mode (&optional arg)
  "Toggle uim mode.
With argument ARG, turn uim mode on if ARG > 0.
uim mode facilitates internationalized input through the uim library."
  (interactive)
  (if arg
      (if (> arg 0)
	  (if (not uim-mode) (uim-mode-on))
	(if uim-mode (uim-mode-off)))
    (if uim-mode
	(uim-mode-off)
      (uim-mode-on))))

;; compat
(defun uim-mode-switch ()
  (interactive)
  (if uim-mode
      (uim-mode-off)
    (uim-mode-on)))

;;
;; Toggle candidate display style
;;
(defun uim-switch-candidate-display-mode (&optional arg)
  (interactive)

  (if (not (local-variable-p 'uim-candidate-display-inline (current-buffer)))
      (make-variable-buffer-local 'uim-candidate-display-inline))
  
  (if (not uim-candidate-displayed)
      (if (and arg (integerp arg))
	  (if (> arg 0)
	      (setq uim-candidate-display-inline t)
	    (setq uim-candidate-display-inline nil))

	;; interactive
	(if uim-candidate-display-inline
	    (progn
	      (message "uim.el: mini-buffer candidate display mode")
	      (setq uim-candidate-display-inline nil))
	  (message "uim.el: inline candidate display mode")
	  (setq uim-candidate-display-inline t))))
  )


;;
;; Switch IM engine
;;
(defun uim-im-switch (&optional im)
  (interactive)

  (if (not uim-mode)
      (uim-mode-on))

  (when uim-initialized
    (if (not im)
	(let (alist)
	  (setq alist (mapcar '(lambda (x) 
				 (cons (car x) (car x))) 
			      uim-im-alist))
	  (save-window-excursion
	    (setq im (cdr (assoc
			   (completing-read (concat "IM Engine: ") alist nil t)
			   alist) )))))
    (if (uim-check-im im)
	(uim-change-im im))))

;;
;; Show uim.el version
;;
(defun uim-el-version ()
  (interactive)
  (message uim-el-version))



;; Initialize uim.el 
(uim-init)

(run-hooks 'uim-load-hook)

(provide 'uim)

