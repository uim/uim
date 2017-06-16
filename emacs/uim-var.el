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

(require 'uim-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables for Customizing

;; Handle single Escape key press during preedit when Emacs
;; is executed on the terminal (i.e. Emacs with -nw option).
;;
(defvar uim-use-single-escape-on-terminal nil
  "If the value is non-nil and preedits exist, 
escape key code is sent immediately to Uim every time the Escape-key is 
pressed. This value is available only when the Emacs has been executed 
with -nw option. If the value is valid, key-bindings with Alt key 
cannot be used and function keys, such as F10 or Delete, are not
recognized correctly.

If the value is nil and Emacs is running on the terminal, 
Escape key is mapped to ESC-ESC or M-Esc.

Anyway, non-nil is unrecommended.")

;; Name of default IM engine
;; 
;; Set this variable if you want to use different IM
;; from default IM which is defined at the Uim common
;; setting.
(defvar uim-default-im-engine nil
  "IM name used by default.

If you want to use different IM from default IM defined at the Uim 
common setting, set IM name you want to use.")


;; List of IM engine property
(defvar uim-default-im-prop nil
  "List of IM properties which overwrite the common settings of Uim.

Example:
(setq uim-default-im-prop '(\"action_anthy_hiragana\" \"action_anthy_kana\"))
")

;; Command name of uim-el-agent
(defvar uim-el-agent "uim-el-agent"
  "Overwrite this variable if uim-el-agent is not in command path.

Example:
(setq uim-el-agent \"~/uim/emacs/uim-el-agent\")
" )


(defvar uim-el-helper-agent "uim-el-helper-agent"
  "Overwrite this variable if uim-el-helper-agent is not in command path.

Example:
(setq uim-el-helper-agent \"~/uim/emacs/uim-el-helper-agent\")
" )

;; display fences on both edges of preedit
(defvar uim-preedit-display-fences nil
  "If the value is non-nil, fences are put on both edges of the preedits."
  )

;; display frame of candidate by ASCII characters
(defvar uim-candidate-display-frame nil
  "If the value is non-nil, frame is put around the candidate list."
  )

;; candidate display style of this buffer
(defvar uim-candidate-display-inline nil
  "If non-nil, a candidate list is displayed below the
preedit string in vertical direction.  Otherwise, it is 
displayed at the echo area.")

;; display appendix or not
(defvar uim-candidate-display-appendix t
  "If the value is non-nil, appendixes are displayed with candidates."
  )

;; allow resize of echo region
(defvar uim-allow-resize-echo-region t
  "If the value is nil, uim.el uses only the 1st line of the echo-region and
keeps the size of it when showing the candidates.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Global Variables

;; Type of Emacs
(defconst uim-xemacs (featurep 'xemacs))
(defconst uim-emacs (string-match "^GNU Emacs" (emacs-version)))

;; Supported languages and encodings
;; ("UIM-Language" "Emacs-Language" Emacs-encoding "UIM-Encoding")
(defvar uim-lang-code-alist
  (list 
   '("Japanese"              "Japanese"     euc-jp      "EUC-JP")
   '("Korean"                "Korean"       euc-kr      "EUC-KR")
   '("Chinese (Simplified)"  "Chinese-GB"   gb2312      "GB2312")
   '("Chinese (Traditional)" "Chinese-BIG5" big5        "BIG5")
   '("Chinese"               "Chinese-GB"   gb2312      "GB2312")
   '("Vietnamese"            "Vietnamese"   viscii      "VISCII")
   '("Latin"                 "Latin-1"      iso-8859-1  "ISO-8859-1")
   '("Thai"                  "Thai"         thai-tis620 "TIS-620")
   '("Greek"                 "Greek"        iso-8859-7  "ISO-8859-7")
   '("Hebrew"                "Hebrew"       iso-8859-8  "ISO-8859-8")
   '("Laothian"              "Lao"          lao         "MULELAO-1")
   '("Croatian"              "Latin-2"      iso-8859-2  "ISO-8859-2")
   '("Slovenian"             "Latin-2"      iso-8859-2  "ISO-8859-2")
   '("Serbian"               "Latin-2"      iso-8859-2  "ISO-8859-2")
   '("Russian"               "Cyrillic-ISO" iso-8859-5  "ISO-8859-5")
   (if (and uim-emacs (>= emacs-major-version 21))
       '("Other"            "ASCII"        utf-8       "UTF-8")
     '("Other"              "ASCII"        iso-8859-1  "ISO-8859-1"))
   ))


;; Keymaps for minor-mode
(defvar uim-mode-map nil
  "Keymap used in buffers that have uim-mode turned on.")
(defvar uim-preedit-map nil
  "Keymap used in uim-mode buffers with preedits.")

(defvar uim-escape-map nil
  "Empty keymap for escape key handling.")

(defvar uim-toolbar-map nil
  "Empty keymap for preventing toolbar action on XEmacs.")

(defvar uim-dummy-map nil
  "Dummy keymap to disable uim-mode keymap temporarily.")

;; uim-el-agent related variables
(defvar uim-el-agent-process nil
  "uim-el-agent process.")
(defvar uim-el-agent-buffer nil
  "The buffer for communication with uim-el-agent.")
(defconst uim-el-agent-buffer-name " *uim*"
  "Name of the buffer for communication with uim-el-agent.")

(defvar uim-el-helper-agent-process nil
  "uim-el-helper-agent process.")
(defvar uim-el-helper-agent-buffer nil
  "The buffer for communication with uim-el-helper-agent.")
(defconst uim-el-helper-agent-buffer-name " *uim-helper*"
  "Name of the buffer for communication with uim-el-helper-agent.")
(defvar uim-helper-message ""
  "Buffer to store message from uim-el-helper-agent.")

(defconst uim-el-candidates-buffer-name " *uim-candidates*"
  "Name of the buffer used to estimate candidates size.")

;; Timeout related variables (wait output from uim-el-agent)
(defvar uim-el-agent-timeout 3)
(defvar uim-el-agent-accept-timeout 5000
  "Timeout for communication with uim-el-agent.")

(defvar uim-startup-timeout 2000
  "How long startup can take before it gets to your nerves, in milliseconds.")


;; Minimum window size to display the candidate inline
(defconst uim-candidate-minimum-width 8
  "Minimum window size for displaying candidates inline.")


;; List of IM engine 
;; (looking up with IM name and obtain its language, encoding and so on)
(defvar uim-im-alist nil)

;; Current focused buffer
;;  if current-buffer has no Uim context, this should be nil
(defvar uim-focused-buffer nil)

;; Last buffer which is used to detect change of current-buffer
(defvar uim-recent-buffer nil)

;; Current maximum context ID
(defvar uim-context-id-max 1)

;; List of released context ID
(defvar uim-context-id-recycle '())

;; Face for preedit
(defvar uim-preedit-face nil
  "The face used to display preedit strings in uim-mode.")
(defvar uim-preedit-underline-face nil
  "The face used to display preedit strings with underline in uim-mode.")
(defvar uim-preedit-highlight-face nil
  "The face used to display highlighted preedit strings in uim-mode.")
(defvar uim-preedit-highlight-underline-face nil
  "The face used to display highlighted preedit strings with underline
 in uim-mode.")

;; face for preedit separator
(defvar uim-separator-face nil)

;; face for candidate
(defvar uim-candidate-odd-face nil)
(defvar uim-candidate-even-face nil)
(defvar uim-candidate-selected-face nil)
(defvar uim-candidate-nth-face nil)


;; current serial number which is added to message to uim-el-agent
(defvar uim-communication-serial-number 0)

(defvar uim-last-cmd ""
  "Command string passed to uim-el-agent at last time.")


;; hook called when the default IM engine has been changed by agent
(defvar uim-update-default-engine-hook nil)
;; hook called when the current IM engine has been changed by agent
(defvar uim-update-current-engine-hook nil)
;; hook called when uim is force inactivated
(defvar uim-force-inactivate-hook nil)
;; hook called when buffer has been initialized
(defvar uim-buffer-init-hook nil)
;; hook called after reset key map 
(defvar uim-reset-keymap-hook nil)

(defvar uim-update-label-hook nil)

(defvar uim-load-hook nil)

(defvar uim-send-recv-again nil)


;; if non-nil, pressed keys are displayed at echo region
(defvar uim-keystroke-displaying nil)


;; keep original last-input-event to process the event correctly after
;; removing Shift modifier
(defvar uim-original-input-event nil)

(defvar uim-local-var '())

;; Macro for setting up buffer-local variable
(defmacro uim-deflocalvar (var default &optional documentation)
  `(progn
     (defvar ,var ,default
       (format "%s (local\)" ,documentation))
     (setq uim-local-var (cons (cons ',var ,default) uim-local-var))
     (make-variable-buffer-local ',var)))


;; Encoding initialized flag
(defvar uim-im-initialized nil)

(defvar uim-show-im-mode t
  "If the value is non-nil, IM mode is displayed on mode-line.")

(defvar uim-show-im-name t
  "If the value is non-nil, IM name is displayed on mode-line.")

(defvar uim-this-command-keys-original nil)

(defvar uim-read-char-exclusive-original nil)

;;; Buffer Local Variables

(uim-deflocalvar uim-last-key-vector nil
		 "Recent key vector.")

;; workaround for FSF Emacs-20/21
(uim-deflocalvar uim-merge-next nil
"If the value is non-nil, uim-process-input merges next input.")

(uim-deflocalvar uim-prefix-arg nil "Recent current-prefix-arg value.")

;; unprocessed keys
(uim-deflocalvar uim-wait-next-key nil)

(uim-deflocalvar uim-translated-key-vector nil)
(uim-deflocalvar uim-untranslated-key-vector nil)

(uim-deflocalvar uim-prefix-arg-vector nil)


(uim-deflocalvar uim-mode-line-string " U"
		 "mode-line string of uim-mode.")

;; IM name label (may not equal to uim-current-im-engine)
(uim-deflocalvar uim-im-name-str "")
;; IM's indication ID 
(uim-deflocalvar uim-im-indication-id "")

;; IM mode indicator
(uim-deflocalvar uim-im-mode-str "")


(uim-deflocalvar uim-initialized nil)

;; context ID (0 means "no context")
(uim-deflocalvar uim-context-id 0)

;; IM name which is used in the buffer
(uim-deflocalvar uim-current-im-engine nil) 

;; minor-mode status
(uim-deflocalvar uim-mode nil)

;; code to decode output of uim-el-agent
(uim-deflocalvar uim-decoding-code nil)

(uim-deflocalvar uim-minor-mode-map-alist nil)

(uim-deflocalvar uim-emulation-mode-map-alists nil)


;;
(uim-deflocalvar uim-original-cursor nil)

;; preedit string is displayed or not
(uim-deflocalvar uim-preedit-displayed nil)

;; beginning point of preedit string
(uim-deflocalvar uim-preedit-start 0)
;; end point of preedit string 
(uim-deflocalvar uim-preedit-end 0)
;; cursor point of current preedit string
(uim-deflocalvar uim-preedit-cursor 0)

;; list of overlays of preedit string
(uim-deflocalvar uim-preedit-ol-list '())

(uim-deflocalvar uim-preedit-position-list '())

(uim-deflocalvar uim-preedit-current-sentence-start nil)

;; freeze flag (to save font property)
(uim-deflocalvar uim-buffer-frozen nil)

;; candidate is displayed or not
(uim-deflocalvar uim-candidate-displayed nil)

;; candidate insertion base point
;;  (basically, candidate list is inserted from next virtual line of this point)
(uim-deflocalvar uim-candidate-start 0)
;; distance between the candidate insertion base point and the candidate list
;;  which is used when the preedit string is lapped to next virtual line.
(uim-deflocalvar uim-candidate-vofs 0)
(uim-deflocalvar uim-preedit-overlap 0)
;; original data under candidate
(uim-deflocalvar uim-candidate-original-str nil)
(uim-deflocalvar uim-candidate-original-start nil)
(uim-deflocalvar uim-candidate-original-end nil)
(uim-deflocalvar uim-candidate-end 0)

(uim-deflocalvar uim-candidate-cursor nil)

(uim-deflocalvar uim-candidate-in-echo-region nil)

(uim-deflocalvar uim-show-candidate-upward nil)


(uim-deflocalvar uim-max-candstr 0)
(uim-deflocalvar uim-max-candlabel 0)

(uim-deflocalvar uim-candidate-page-label "") 
(uim-deflocalvar uim-candidate-line-list '())

;; saved undo list and its valid flag
(uim-deflocalvar uim-buffer-undo-list nil)
(uim-deflocalvar uim-buffer-undo-list-saved nil)

(uim-deflocalvar uim-undo-stacking nil)

(uim-deflocalvar uim-preedit-keymap-enabled nil)

(uim-deflocalvar uim-after-change-functions nil)
(uim-deflocalvar uim-fontification-functions nil)
(uim-deflocalvar uim-timer-idle-list nil)

;; save top point of current window to lock scroll
(uim-deflocalvar uim-window-force-scrolled nil)

(uim-deflocalvar uim-window-force-scrolled-original nil)

(uim-deflocalvar uim-buffer-read-only nil)

;; save current menubar temporally for XEmacs
(uim-deflocalvar uim-menubar-temp nil)

;; save real deactivate-mark
(uim-deflocalvar uim-deactivate-mark nil)

;; initialize faces

;; for preedit string

;; plain
(make-face 'uim-preedit-face)

(copy-face 'uim-preedit-face 'uim-preedit-underline-face)
(set-face-underline-p        'uim-preedit-underline-face t)

;; highlight
(make-face 'uim-preedit-highlight-face)
(set-face-foreground 'uim-preedit-highlight-face "White")
(set-face-background 'uim-preedit-highlight-face "Blue3")

(copy-face 'uim-preedit-highlight-face 'uim-preedit-highlight-underline-face)
(set-face-underline-p 'uim-preedit-highlight-underline-face t)

;; separator
(make-face 'uim-separator-face)
(set-face-foreground 'uim-separator-face "LightSkyBlue4")

;; for candidate list

;; odd line
(make-face 'uim-candidate-odd-face)
(set-face-foreground 'uim-candidate-odd-face "blue")
(set-face-background 'uim-candidate-odd-face "snow1")

;; even line
(make-face 'uim-candidate-even-face)
(set-face-foreground 'uim-candidate-even-face "blue")
(set-face-background 'uim-candidate-even-face "snow2")

;; focused line
(make-face 'uim-candidate-selected-face)
(set-face-foreground 'uim-candidate-selected-face "blue")
(set-face-background 'uim-candidate-selected-face "snow3")
;;(set-face-bold-p     'uim-candidate-selected-face t)

;; total number and focused candidate number
(make-face 'uim-candidate-nth-face)
(set-face-foreground 'uim-candidate-nth-face "blue")
(set-face-background 'uim-candidate-nth-face "lavender")


(provide 'uim-var)
