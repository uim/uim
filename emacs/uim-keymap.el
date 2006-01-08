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

;;
;; Mouse event for GNU Emacs
;;
(defconst uim-mouse-modifiers
  '(
    ()     
    (down)   
    (drag)
    (double)  
    (double down)  
    (double drag)
    (triple)  
    (triple down)  
    (triple drag)
    ))

(defconst uim-generic-modifiers
  '(
    ()     
    (shift)   
    (control)
    (meta)
    (shift control)
    (shift meta)
    (control meta)
    (shift control meta)  
    ))


(defconst uim-emacs-prefix-keys
  '(help-command
    2C-command
    Control-X-prefix
    mode-specific-command-prefix
    ctl-x-4-prefix
    ctl-x-5-prefix
    vc-prefix-map
    facemenu-keymap
    ))


(defun uim-mouse-set-point (event)
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (when (windowp (posn-window posn))
      (select-window (posn-window posn))
      )))

;;
;; Bind all mouse event to nil (for GNU Emacs)
;; 
(defun uim-unbind-mouse-event ()
  (mapcar
   '(lambda (w)
      (mapcar 
       '(lambda (x)
	  (mapcar 
	   '(lambda (y)
	      (let ((event (vector (append w x (list y)))))

	    (define-key uim-mode-map event nil)
	    (define-key uim-preedit-map event nil)
	    ))
	   '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5)))
       uim-mouse-modifiers))
   uim-generic-modifiers)
  
  (define-key uim-mode-map [vertical-scroll-bar] nil)
  (define-key uim-mode-map [mode-line] nil)
  (define-key uim-mode-map [vertical-line] nil)
  (define-key uim-mode-map [menu-bar] nil)
  (define-key uim-mode-map [tool-bar] nil)

  (define-key uim-mode-map [mouse-movement] nil)
  (define-key uim-mode-map [scroll-bar-movement] nil)
  (define-key uim-mode-map [switch-frame] nil)
  (define-key uim-mode-map [delete-frame] nil)
  (define-key uim-mode-map [iconify-frame] nil)
  (define-key uim-mode-map [make-frame-visible] nil)
  

  ;; for uim-preedit-map
  ;;  menu-bar/tool-bar operation must be avoided
  (define-key uim-preedit-map [vertical-scroll-bar] 'ignore)
  (define-key uim-preedit-map [mode-line] nil)
  (define-key uim-preedit-map [vertical-line] nil) 
  (define-key uim-preedit-map [menu-bar] 'ignore) 
  (define-key uim-preedit-map [tool-bar] 'ignore) 

  (define-key uim-preedit-map [mouse-movement] nil)
  (define-key uim-preedit-map [scroll-bar-movement] nil)
  (define-key uim-preedit-map [switch-frame] nil)
  (define-key uim-preedit-map [delete-frame] nil)
  (define-key uim-preedit-map [iconify-frame] nil)
  (define-key uim-preedit-map [make-frame-visible] nil)

  )
		  


;;
;; Initialize keymap
;; 
(defun uim-init-keymap ()

  (setq uim-mode-map (make-sparse-keymap))
  (setq uim-preedit-map (make-sparse-keymap))


  ;; copy special keys
  (let ((prefixs uim-emacs-prefix-keys))
    (while prefixs
      (let ((prefix-keys (where-is-internal (car prefixs))))
	(while prefix-keys
	  (define-key uim-mode-map (car prefix-keys) (car prefixs))
	  (setq prefix-keys (cdr prefix-keys))))
      (setq prefixs (cdr prefixs))))

  
  (define-prefix-command 'uim-escape-map)

  ;; set default key-binds
  (cond (uim-xemacs ;; XEmacs
	 (set-keymap-default-binding uim-mode-map 'uim-process-input)
	 (set-keymap-default-binding uim-preedit-map 'uim-process-input)
	 (define-key uim-mode-map [escape] 'uim-process-input)
	 (define-key uim-mode-map [(control ?\[)] 'uim-process-input)
	 )

	((and uim-emacs (= emacs-major-version 20)) ;;; GNU Emacs-20.7
	 (define-key uim-mode-map [t] 'uim-process-input)
	 (define-key uim-preedit-map [t] 'uim-process-input)

	 (define-key uim-escape-map [t] 'uim-process-input)
	 (define-key uim-mode-map [27] 'uim-escape-map)
	 (define-key uim-preedit-map [27] 'uim-escape-map)

	 (define-key uim-mode-map [escape] nil)

	 )

	((and uim-emacs (= emacs-major-version 21)) ;;; GNU Emacs-21.x 
	 (define-key uim-mode-map (vector t) 'uim-process-input)
	 (define-key uim-preedit-map (vector t) 'uim-process-input)

	 (define-key uim-mode-map [escape] nil)
	 )

	((and uim-emacs (= emacs-major-version 22)) ;;; GNU Emacs-22.x
	 (define-key uim-mode-map [t] 'uim-process-input)
	 (define-key uim-preedit-map [t] 'uim-process-input)

	 (define-key uim-escape-map [t] 'uim-process-input)
	 (define-key uim-mode-map [27] 'uim-escape-map)
	 (define-key uim-preedit-map [27] 'uim-escape-map)
	 )

	)


  (when uim-emacs
    (uim-unbind-mouse-event)
    )

  (when uim-xemacs
    (setq uim-toolbar-map (make-sparse-keymap))
    (set-keymap-default-binding uim-toolbar-map 'ignore)
    )
  
  )


;;
;; Switch keymap
;;
(defun uim-enable-mode-keymap ()
  (setcdr (assq 'uim-mode minor-mode-map-alist) 
	  uim-mode-map))

(defun uim-disable-mode-keymap ()
  (setcdr (assq 'uim-mode minor-mode-map-alist) 
	  nil))

(defun uim-enable-preedit-keymap ()
  (when (not uim-preedit-keymap-enabled)

    (setq uim-preedit-keymap-enabled t)
    (uim-debug "enable preedit keymap")

    (setcdr (assq 'uim-mode minor-mode-map-alist) 
	    uim-preedit-map) 

    ;; disable other keymaps
    (when (not uim-minor-mode-map-alist)
      (setq uim-minor-mode-map-alist minor-mode-map-alist)
      (setq minor-mode-map-alist (list (assq 'uim-mode minor-mode-map-alist)))

      (when (and uim-emacs (= emacs-major-version 22)) 
	(setq uim-emulation-mode-map-alists emulation-mode-map-alists)
	(setq emulation-mode-map-alists nil)))

    (when uim-xemacs
      ;; disable toolbar and menubar
      (make-variable-buffer-local 'toolbar-map)
      (setq toolbar-map uim-toolbar-map)
      (uim-xemacs-save-menubar))
    )
  )

(defun uim-disable-preedit-keymap ()
  (when uim-preedit-keymap-enabled
    (setq uim-preedit-keymap-enabled nil)
    (uim-debug "disable preedit keymap")

    ;; enable other keymaps
    (when uim-minor-mode-map-alist
      (setq minor-mode-map-alist uim-minor-mode-map-alist)
      (setq uim-minor-mode-map-alist nil)

      (when (and uim-emacs (= emacs-major-version 22)) 
	(setq emulation-mode-map-alists uim-emulation-mode-map-alists)
	(setq uim-emulation-mode-map-alists nil)))

    (uim-enable-mode-keymap)

    (when uim-xemacs
      ;;enable toolbar and menubar
      (kill-local-variable 'toolbar-map)
      (uim-xemacs-restore-menubar))

    )
  )


;; (defun uim-mode-keymap-enabled ()
;;   (eq (cdr (assq 'uim-mode minor-mode-map-alist))
;;       uim-mode-map))

;; (defun uim-preedit-keymap-enabled ()
;;   (eq (cdr (assq 'uim-mode minor-mode-map-alist))
;;       uim-preedit-map))



(defun uim-reset-keymap ()
  (uim-init-keymap)
  (run-hooks 'uim-reset-keymap-hook))

(defun uim-disable-single-escape-on-terminal ()
  )

(defun uim-enable-single-escape-on-terminal ()

  )

(provide 'uim-keymap)


