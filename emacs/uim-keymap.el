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

  (define-key uim-mode-map [mouse-movement] nil)
  (define-key uim-mode-map [scroll-bar-movement] nil)
  (define-key uim-mode-map [switch-frame] nil)
  (define-key uim-mode-map [delete-frame] nil)
  (define-key uim-mode-map [iconify-frame] nil)
  (define-key uim-mode-map [make-frame-visible] nil)
  

  ;; for uim-preedit-map
  ;;  menu-bar/tool-bar operation must be avoided
  (define-key uim-preedit-map [vertical-scroll-bar] nil)
  (define-key uim-preedit-map [mode-line] nil)
  (define-key uim-preedit-map [vertical-line] nil) 

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

  (setq uim-dummy-map (make-sparse-keymap))

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

	 (if (not window-system)
	     (define-key uim-mode-map [27] 'uim-process-input))
	 )

	((and uim-emacs (= emacs-major-version 21)) ;;; GNU Emacs-21.x 
	 (define-key uim-mode-map [t] 'uim-process-input)
	 (define-key uim-preedit-map [t] 'uim-process-input)

	 (define-key uim-mode-map [27] 'uim-process-input)
	 )

        ;; GNU Emacs-22 and 23
	((and uim-emacs
              (= emacs-major-version 22)
              (= emacs-major-version 23))
	 (define-key uim-mode-map [t] 'uim-process-input)
	 (define-key uim-preedit-map [t] 'uim-process-input)

	 (define-key uim-mode-map [27] 'uim-process-input)
	 )

	((and uim-emacs (>= emacs-major-version 24)) ;;; GNU Emacs-24 or later
	 (define-key uim-mode-map [t] 'uim-process-input)
	 (define-key uim-preedit-map [t] 'uim-process-input)

	 (define-key uim-mode-map (kbd "ESC") 'uim-process-input)

         (define-key uim-mode-map (kbd "C-c") nil)
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
;; Disable other minor-mode keymaps while preedit strings or candidates 
;; are displayed.
;;
(defun uim-disable-other-minor-mode-map ()
  (when (not uim-minor-mode-map-alist)
    (setq uim-minor-mode-map-alist minor-mode-map-alist)
    (setq minor-mode-map-alist (list (assq 'uim-mode minor-mode-map-alist)))

    (when (and uim-emacs (>= emacs-major-version 22)) 
      (setq uim-emulation-mode-map-alists emulation-mode-map-alists)
      (setq emulation-mode-map-alists nil)))
  )

(defun uim-enable-other-minor-mode-map ()
  (when uim-minor-mode-map-alist
    (setq minor-mode-map-alist uim-minor-mode-map-alist)
    (setq uim-minor-mode-map-alist nil)

    (when (and uim-emacs (>= emacs-major-version 22)) 
      (setq emulation-mode-map-alists uim-emulation-mode-map-alists)
      (setq uim-emulation-mode-map-alists nil)))
  )


;;
;; Change keymap of uim-mode
;;
(defun uim-set-keymap (map)
  (setcdr (assq 'uim-mode minor-mode-map-alist) 
	  map))

;;
;; Return current keymap of uim-mode
;;
(defun uim-get-keymap ()
  (cdr (assq 'uim-mode minor-mode-map-alist)))

;;
;; Set normal keymap
;;
(defun uim-enable-mode-keymap ()
  (uim-set-keymap uim-mode-map))

;;
;; Disable and return current keymap
;;
(defun uim-disable-keymap ()
  (let (map)
    (setq map (uim-get-keymap))
    (uim-set-keymap uim-dummy-map)
    map)
  )

;;
;; Set preedit keymap
;;
(defun uim-enable-preedit-keymap ()
  (when (not uim-preedit-keymap-enabled)
    (setq uim-preedit-keymap-enabled t))

  (uim-set-keymap uim-preedit-map)

    (when uim-xemacs
      ;; disable toolbar and menubar
      (make-variable-buffer-local 'toolbar-map)
      (setq toolbar-map uim-toolbar-map)
      (uim-xemacs-save-menubar))
    )

;;
;; Unset preedit keymap
;;
(defun uim-disable-preedit-keymap ()

  (when uim-preedit-keymap-enabled
    (setq uim-preedit-keymap-enabled nil))

  (uim-enable-other-minor-mode-map)
    (uim-enable-mode-keymap)

    (when uim-xemacs
      ;;enable toolbar and menubar
      (kill-local-variable 'toolbar-map)
      (uim-xemacs-restore-menubar))
  )


(defun uim-reorder-minor-mode-map-alist ()
  (interactive)
  (let ((inhibit-quit t) member)
    (setq member (assq 'uim-mode minor-mode-map-alist))
    (if member
	(setq minor-mode-map-alist 
	      (cons member 
		    (delq member minor-mode-map-alist))))))


(defun uim-reset-keymap ()
  (uim-init-keymap)
  (run-hooks 'uim-reset-keymap-hook))

(provide 'uim-keymap)


