;; 
;;  Copyright (c) 2005-2006 uim Project http://uim.freedesktop.org/
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
	     (cons 'uim-mode uim-mode-map)
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

;  (uim-debug (format "update-default-engine: %s" engine))

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

  (uim-debug (format "update-current-engine: %s" engine))

  (when (not (equal uim-current-im-engine engine))
    (setq uim-current-im-engine engine) 
    (run-hooks 'uim-update-current-engine-hook))

  ;; update current decoding code   
  (let ((newcode (uim-get-emacs-encoding uim-current-im-engine)))
    (when (not (string= newcode uim-decoding-code))
      (setq uim-decoding-code newcode)
      (uim-change-process-encoding uim-decoding-code)))
  )


;; Update mode-line string to "label1 label2".
(defun uim-update-mode-line (label1 &optional label2)
  (setq uim-current-prop-label (format "%s" (car label1)))
  (if label2
      (setq uim-current-prop-label 
	    (concat uim-current-prop-label (format "%s" (car label2)))))

  (setq uim-mode-line-string 
	(format " U %s[%s]"
		uim-current-im-engine uim-current-prop-label))

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

    (uim-debug (format "current-buffer changed (from %s to %s)"
		       uim-focused-buffer
		       (current-buffer)))

    ;; update recent bufferto detect next change
    (setq uim-recent-buffer (current-buffer))
    
    ;; remove Uim focus from previous buffer
    (when (and (bufferp uim-focused-buffer)
	     (buffer-name uim-focused-buffer))
	(save-current-buffer
	  (set-buffer uim-focused-buffer)
	(when (and uim-initialized uim-mode)
	  (uim-debug (format "unfocused %s" (buffer-name (current-buffer))))
	    (uim-unfocused)
	    )))
	    
    ;; set Uim focus to current buffer if it has Uim context
    (when (and uim-initialized uim-mode)
      (uim-debug (format "focused %s" (buffer-name (current-buffer))))
	  (uim-focused)
	  )

    (uim-debug (format "current-buffer %s / uim-focused-buffer %s" 
		       (current-buffer) uim-focused-buffer ))
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
  (uim-debug (format "set-process-encoding to %s" outcode))
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

  (uim-debug (format "initialize buffer: %s" (current-buffer)))

  ;; if buffer has no context
  (setq uim-context-id (uim-context-create))

  ;; redraw Uim related objects when window has been resized
  (make-local-hook 'window-configuration-change-hook)
  (add-hook 'window-configuration-change-hook 'uim-window-changed nil t)

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'uim-kill-buffer nil t)

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


;;
;; Activate uim
;;
(defun uim-mode-on ()

  (uim-debug "uim-mode-on")

  (if uim-mode
      (message "uim.el: uim-mode is already activated. (buffer %s)" 
	       (current-buffer)))

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

  (uim-debug (format "uim-mode: %s" uim-mode))

  )


;;
;; Release context when buffer is killed or major-mode changed
;;   This function is called by uim-initialized buffer only
;;
(defun uim-kill-buffer ()

  (uim-debug (format "kill-bufer %s"
		     (buffer-name (current-buffer))))

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

  (uim-debug "uim-change-major-mode")

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
    (uim-debug "restore undo list")
    (buffer-enable-undo)
    (setq buffer-undo-list uim-buffer-undo-list)
    (setq uim-buffer-undo-list-saved nil))
  )


;;
;; Save undo list
;;
(defun uim-save-undo ()
  (when (not uim-buffer-undo-list-saved)
    (uim-debug "save undo list")
    (setq uim-buffer-undo-list buffer-undo-list)
    (buffer-disable-undo)
    (setq uim-buffer-undo-list-saved 't))
  )


;;
;; Update IM list
;;
(defun uim-update-imlist (imlist)
  ;; imlist: (engine lang_code Language explanation encoding) ...

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
  ;; label: ( ("label_abbr"  "label") ... )
  (let (label1 label2 mode-str)
    ;; current IM engine name
    (setq label1 (car label))
    (setq label (cdr label))

    (when label
      (setq label2 (car label)))

    ;; Update mode-line string to "label1 label2".
    (setq mode-str (format "%s" (car label1)))

    (if label2 
	(setq mode-str (concat mode-str (format "%s" (car label2)))))

    (setq uim-mode-line-string 
	  (format " U %s[%s]" uim-current-im-engine mode-str))


    )
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
  
    (when (and uim-candidate-displayed
	       uim-candidate-display-inline)
      (setq candidate-displayed uim-candidate-displayed)

      (uim-do-send-recv-cmd (format "%d HIDE" uim-context-id))

      (when (and candidate-displayed
		 (uim-check-candidate-space))
	(uim-do-send-recv-cmd (format "%d SHOW" uim-context-id))))))


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
	  (kill-all-local-variables)
	  ))
      (setq bufs (cdr bufs)))
    ))


;;
;; Send command to uim-el-agent
;; 
(defun uim-send-cmd (cmd serial)

  (uim-debug (concat (format "%s " serial) cmd ))

  ;; check process status and start if it's not running
  (if (not uim-el-agent-process)
      (setq uim-el-agent-process (uim-el-agent-start)))

  (if uim-el-agent-process
      (progn
	(save-current-buffer
	  (set-buffer uim-el-agent-buffer)
	  (erase-buffer))
	
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
(defun uim-wait-recv(serial)

  (let (rstr)

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
	  )))


;;
;; Send message to uim-el-agent and wait return
;;
(defun uim-do-send-recv-cmd (cmd)
  (let ((serial (uim-get-communication-serial-number)))
    (if (uim-send-cmd cmd serial)
	(uim-wait-recv serial))))


;;
;; Save face property
;;
(defun uim-get-face-list (start end)
  (let ((facelist '()) tail face)
    (catch 'face-loop
      (while t
	(setq tail
	      (next-single-property-change start 'face (current-buffer) end))
	(if (setq face (get-text-property start 'face))
	    (setq uim-facelist (cons (list face start tail) uim-facelist)))
	(setq start tail)
	(if (= start end)
	    (throw 'face-loop t))))
    )
  )



;;
;; Save face property of the window
;;   Save only displayed region
(defun uim-get-facelist-window (win)
  (select-window win)
  (let (end)
    (save-excursion
      (goto-char (window-end nil t))
      (vertical-motion (window-height))
      (setq end (point)))
    (save-excursion
      (uim-get-face-list (window-start) end)
      )
    ))


;;
;; Save face property of windows which are displaying the current buffer.
;;
(defun uim-get-facelist-buffer ()
  (save-selected-window
    (mapcar 'uim-get-facelist-window
	    (get-buffer-window-list (current-buffer) t 'visible))
    uim-facelist))


;;
;; Disable font-lock mode
;;    This function should be called only when the font-lock-mode is 
;;   already in use
;;
(defun uim-disable-font-lock-mode ()
  ;; save and disable font-lock-mode
  (setq uim-font-lock-mode font-lock-mode)
  (setq uim-font-lock-verbose font-lock-verbose)
  (when font-lock-mode
    (font-lock-mode 0)
    (setq font-lock-verbose nil)))


;;
;; Eable font-lock mode
;;
(defun uim-enable-font-lock-mode ()
  (when uim-font-lock-mode
    (font-lock-mode uim-font-lock-mode)
    (setq font-lock-verbose uim-font-lock-verbose)))


;;
;; Restore face property
;;
(defun uim-put-face (facelist)
  (put-text-property (nth 1 facelist) (nth 2 facelist)
		     'face (car facelist)))


;;
;; Update font 
;;
(defun uim-update-font ()
  (mapcar 
   '(lambda (x)
      (let ((font (or (cdr (assq 'font (frame-parameters)))
		      (face-font 'default))))
	(set-face-font x font)))
		     
   '(uim-preedit-face 
     uim-preedit-underline-face 
     uim-preedit-highlight-face
     uim-preedit-highlight-underline-face
     uim-separator-face
     uim-candidate-odd-face
     uim-candidate-even-face
     uim-candidate-selected-face
     uim-candidate-nth-face)))

;;
;; Lock appearance of buffer
;;
(defun uim-freeze-buffer ()
  (let (facelists)

    (setq uim-facelist nil)
    (uim-get-facelist-buffer)
    ;(setq facelists (uim-get-face-list 1 (point-max)))
    (if (memq 'font-lock features)	     
	(progn
	  (uim-disable-font-lock-mode)
	  (mapcar 'uim-put-face uim-facelist)))
    )
  (setq uim-buffer-frozen t))



;;
;; Unlock appearance of buffer
;;
(defun uim-unfreeze-buffer ()
  (let (facelists)
    (if (memq 'font-lock features)
	(uim-enable-font-lock-mode))
    )
  (setq uim-buffer-frozen nil))

 



;;
;; Process inputted key
;;
(defun uim-process-input (&optional arg event reg1 reg2)
  (interactive (list current-prefix-arg 
		     last-input-event
		     ))

  (if uim-emacs
      (setq uim-deactivate-mark t))

  (if (not uim-stacked-key-vector)
      (setq uim-show-keystrokes nil))

  ;; send an input key code to uim-el-agent
  (let ((keyvec (uim-this-command-keys))
	sendkey newvec bypass count mouse)

    (if keyvec
	(progn
	  (if uim-show-keystrokes
	      (let (message-log-max)
		(message (key-description keyvec))))

	  (setq uim-last-key-vector keyvec)

	  (cond (uim-xemacs 
		 ;; XEmacs ;;
		 (setq sendkey 
		       (uim-convert-keystr-to-uimagent-vector (key-description keyvec)))

		 (cond ((string-match "button\\(1\\|2\\|3\\|4\\|5\\)" 
				      (key-description keyvec))
			;; through mouse event
			(setq mouse t)
			(setq bypass t))
		       
		       (current-prefix-arg
			(uim-debug "with prefix arg")
			(setq bypass t)
			(setq count (prefix-numeric-value arg)))
		       
		       ((and (not (or uim-preedit-displayed 
				      uim-candidate-displayed))
			     (uim-buffer-not-editable))
			;; through uim-el-agent when buffer is uneditable
			(setq bypass t)
			))
		 
		 (if bypass
		     (setq sendkey 
			   (uim-convert-char-to-symbolvector 
			    (key-description keyvec))))
		 )
		
		(uim-emacs 
		 ;; Emacs ;;
		 (setq sendkey keyvec)
		 
		 (cond (current-prefix-arg
			;; (uim-debug (format "with prefix arg %s"
			;; current-prefix-arg))
			(setq bypass t)
			(setq count (prefix-numeric-value arg)))
		       ((and (eventp event)
			     (memq (event-basic-type event) 
				   '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5)))
			;; detect mouse event
			;;(uim-debug "mouse event")
			(setq bypass t)
			(setq mouse t)
			)
		       ((and (not (or uim-preedit-displayed
				      uim-candidate-displayed))
			     (uim-buffer-not-editable))
			(setq bypass t))
		       (t 
			;; overwrite special keyvector
			(cond ((equal sendkey [127])   (setq sendkey [backspace]))
			      ((equal sendkey [27 27]) (setq sendkey [27]))
			      ((equal sendkey [28])    (setq sendkey [C-\\]))
			      ((equal sendkey [29])    (setq sendkey [C-\]]))
			      ((equal sendkey [30])    (setq sendkey [C-~]))
			      ((equal sendkey [31])    (setq sendkey [C-_]))
			      ))
		       )
		 ))
	  
	  ;; pass only Uim supported keys
	  (when (or (>= (length sendkey) 3)
		    (and (= (length sendkey) 2)
			 (not (or (eq (aref sendkey 0) 27)
				  (eq (aref sendkey 0) 'escape)))))
	    (uim-debug "bypass")
	    (setq bypass t))
		 

	  (if (not bypass)
	      (uim-do-send-recv-cmd (format "%d %s" uim-context-id sendkey))
	    (if mouse
		(uim-process-mouse-event event)
	      (uim-process-keyvec uim-last-key-vector count)))
	  )

      ;; if keyvec is nil
      (setq uim-deactivate-mark nil)

      (if (not uim-show-keystrokes)
	  (if (sit-for echo-keystrokes)
	      (setq uim-show-keystrokes t)))

      ;; display "ESC-" or something
      (if uim-show-keystrokes
	  (let (message-log-max)
	    (message (concat (key-description uim-stacked-key-vector) "-"))))
      )
    )

  (if uim-emacs
      (setq deactivate-mark uim-deactivate-mark))
  )




(defun uim-enter-preedit-mode ()
  ;; change keymap and freeze faces at first time
  (uim-enable-preedit-keymap)
  (when (= (minibuffer-depth) 0)
    (uim-freeze-buffer)
    (setq uim-buffer-read-only buffer-read-only)
    (setq buffer-read-only t))
  )

(defun uim-leave-preedit-mode ()
  (uim-disable-preedit-keymap)
  (uim-unfreeze-buffer)
  (setq buffer-read-only uim-buffer-read-only)
  )



;; process return expression from uim-el-agent 
(defun uim-process-agent-output (str)
  (catch 'process-agent-output
    (let (;;(inhibit-quit t)
	  preedit-existed
	candidate-existed
	  key commit preedit candidate default im label imlist
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
	(setq uim-preedit-displayed nil)
	  (uim-goto-char uim-original-cursor))

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
		     (progn
		       (setq key uim-last-key-vector)
		       (uim-debug (format "last-key: %s" key)))
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
		((string= rcode "L") ;; IM list
		 (setq imlist rval)
		 )
		)
	  )) 
     str)


    (when default
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
		(let ((start (point)))
	      (insert x)
	      (uim-debug (format "insert %s" x))

		  ;; append undo info to saved buffer-undo-list
		  (if uim-buffer-undo-list-saved
		      (setq uim-buffer-undo-list
			    (cons nil
				  (cons (cons start (point))
					uim-buffer-undo-list))))
		  )
		)
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
	      (uim-debug "call restore undo 2")
	      (uim-restore-undo))
	    ;; process raw key
	    ;;  C-o is also processed here ... orz
	    (let ((inhibit-read-only t))
	      (unwind-protect
		  (progn
		    (uim-process-keyvec key)
		    (setq keyproc-done t))
		(when (not keyproc-done)
		  (uim-leave-preedit-mode)
		  (uim-debug "leave preedit mode forcibly")
		  )
		)
	      ;; following expressions will not be evaluated
	      ;; when an error occurs in this function...
      )
	    (if (not uim-mode)
		(throw 'process-agent-output t))
	    ;; save undo hisotry again
	    (when buffer-undo-list-saved
	      (uim-debug "call save undo 2")
	      (uim-save-undo))))

      (setq uim-original-cursor (uim-point))


    (if (or preedit candidate)
	;; process preedit/candidate
	(let ((modified (buffer-modified-p)))

	  ;; save undo list if not saved
	  (when (not uim-buffer-undo-list-saved)
	    (uim-flush-concat-undo)
	      (uim-debug "call save undo 3")
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

	    (setq uim-candidate-cursor (uim-point))

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
	  (uim-debug "call restore undo 3")
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
      (uim-debug "recenter: window-force-scrolled is true")
      (setq uim-window-force-scrolled nil)
      (recenter))


    (if (not uim-send-recv-again)
	(when label 
	  (setq uim-send-recv-again t)
	  (uim-update-im-label))
      (setq uim-send-recv-again nil))

      (uim-debug (format "uim-original-cursor %s" uim-original-cursor))
      (uim-debug "process-agent-output done")
    ))
  )





;;
;; Get available IM list from uim-el-agent
;;
(defun uim-get-im-list ()
  (uim-do-send-recv-cmd (format "0 LIST")))

(defun uim-initialize-im-alist ()
  (uim-get-im-list))

;;
;; Initialize IM list and encoding
;;
(defun uim-init-im-encoding ()

  (uim-initialize-im-alist)

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
      ;(uim-mode-off)
      (uim-force-off)
      ))



;;
;; Initialize uim
;;
(defun uim-init ()

  ;; initialize minor-mode
  (uim-init-minor-mode)

  ;; initialize encoding
  (uim-init-im-encoding)
  
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
; compat
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


(provide 'uim)

