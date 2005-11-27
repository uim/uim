;; 
;;  Copyright (c) 2005, Konosuke Watanabe <nosuke@csc.ne.jp>
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

(defconst uim-el-version "0.0.6-beta2")

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
(setq uim-el-agent \"~/uim-el/uim-el-agent/uim-el-agent\")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Global Variables

;; Type of Emacs
(defconst uim-xemacs (featurep 'xemacs))
(defconst uim-emacs (string-match "^GNU Emacs" (emacs-version)))

;; Supported languages and encodings
;; ("Uim-Language" "Emacs-Language" Emacs-encoding "Uim-Encoding")
(defvar uim-lang-code
  '(("Japanese"              "Japanese"     euc-jp      "EUC-JP")
    ("Korean"                "Korean"       euc-kr      "EUC-KR")
    ("Chinese (Simplified)"  "Chinese-GB"   gb2312      "GB2312")
    ("Chinese (Traditional)" "Chinese-BIG5" big5        "BIG5")
    ("Chinese"               "Chinese-GB"   gb2312      "GB2312")
    ("Vietnamese"            "Vietnamese"   viscii      "VISCII")
    ("Latin"                 "Latin-1"      iso-8859-1  "ISO-8859-1")
    ("Thai"                  "Thai"         thai-tis620 "TIS-620")
    ("Greek"                 "Greek"        iso-8859-7  "ISO-8859-7")
    ("Hebrew"                "Hebrew"       iso-8859-8  "ISO-8859-8")
    ("Laothian"              "Lao"          lao         "MULELAO-1")
    ("Croatian"              "Latin-2"      iso-8859-2  "ISO-8859-2")
    ("Slovenian"             "Latin-2"      iso-8859-2  "ISO-8859-2")
    ("Serbian"               "Latin-2"      iso-8859-2  "ISO-8859-2")
    ("Russian"               "Cyrillic-ISO" iso-8859-5  "ISO-8859-5")
    ("Other"                 "ASCII"        iso-8859-1  "ISO-8859-1")
    ))



;; Alist of supported languages and encodings
(defvar uim-lang-code-alist 
  (mapcar (lambda (x)
	    (list (nth 0 x)
		  (cons 'uim-lang (nth 0 x))
		  (cons 'emacs-lang (nth 1 x))
		  (cons 'emacs-code (nth 2 x))
		  (cons 'uim-code (nth 3 x)))) uim-lang-code))


;; Keymaps for minor-mode
(defvar uim-mode-map nil
  "Keymap used in buffers that have uim-mode turned on.")
(defvar uim-preedit-map nil
  "Keymap used in uim-mode buffers with preedits.")

(defvar uim-escape-map nil
  "Empty keymap for escape key handling.")

(defvar uim-toolbar-map nil
  "Empty keymap for preventing toolbar action on XEmacs.")


;; uim-el-agent related variables
(defvar uim-el-agent-process nil
  "uim-el-agent process.")
(defvar uim-el-agent-buffer nil
  "The buffer for communication with uim-el-agent.")
(defconst uim-el-agent-buffer-name " *uim*"
  "Name of the buffer for communication with uim-el-agent.")


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
(defvar uim-im-list nil)

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

(defvar uim-last-key-vector nil
  "Recent key vector.")

(defvar uim-retry-keys nil)

;; Macro for setting up buffer-local variable
(defmacro uim-deflocalvar (var default &optional documentation)
  `(progn
     (defvar ,var ,default
       (format "%s (local\)" ,documentation))
     (make-variable-buffer-local ',var)))



;;; Buffer Local Variables

(uim-deflocalvar uim-mode-line-string " U"
		 "mode-line string of uim-mode.")

(uim-deflocalvar uim-initialized nil)

;; context ID (0 means "no context")
(uim-deflocalvar uim-context-id 0)

;; IM name which is used in the buffer
(uim-deflocalvar uim-current-im-engine nil) 

;; current property label
(uim-deflocalvar uim-current-prop-label nil)

;; context's encoding of this buffer
(uim-deflocalvar uim-context-encoding 
		 (nth 2 (assoc current-language-environment uim-lang-code)))

;; minor-mode status
(uim-deflocalvar uim-mode nil)

;; code to decode output of uim-el-agent
(uim-deflocalvar uim-decoding-code nil)

;; unprocessed keys
(uim-deflocalvar uim-stacked-key-vector nil)

;; if non-nil, pressed keys are displayed at echo region
(uim-deflocalvar uim-show-keystrokes nil)

(uim-deflocalvar uim-minor-mode-map-alist nil)

(uim-deflocalvar uim-emulation-mode-map-alists nil)

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

(uim-deflocalvar uim-preedit-overlays '())


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
;; original data under candidate
(uim-deflocalvar uim-candidate-original-str nil)
(uim-deflocalvar uim-candidate-original-start nil)
(uim-deflocalvar uim-candidate-original-end nil)
(uim-deflocalvar uim-candidate-end 0)
;; list of overlays of candidate string
(uim-deflocalvar uim-candidate-overlays '())

(uim-deflocalvar uim-show-candidate-upward nil)


(uim-deflocalvar uim-max-candstr 0)
(uim-deflocalvar uim-max-candlabel 0)

(uim-deflocalvar uim-candidate-page-label "") 
(uim-deflocalvar uim-candidate-line-list '())

;; saved undo list and its valid flag
(uim-deflocalvar uim-buffer-undo-list nil)
(uim-deflocalvar uim-buffer-undo-list-saved nil)

(uim-deflocalvar uim-undo-stacking nil)


;; save font-lock mode
(uim-deflocalvar uim-font-lock-mode nil)
(uim-deflocalvar uim-font-lock-verbose nil)

;; save top point of current window to lock scroll
(uim-deflocalvar uim-window-force-scrolled nil)

(uim-deflocalvar uim-buffer-read-only nil)

;; list of font faces for emulating font-lock mode looks
(uim-deflocalvar uim-facelist nil)

;; save current menubar temporally for XEmacs
(uim-deflocalvar uim-menubar-temp nil)

;; save real deactivate-mark
(uim-deflocalvar uim-deactivate-mark nil)

;; initialize faces

;; for preedit string

;; plain
(copy-face 'default 'uim-preedit-face)

(copy-face 'uim-preedit-face 'uim-preedit-underline-face)
(set-face-underline-p        'uim-preedit-underline-face t)

;; highlight
(copy-face 'default  'uim-preedit-highlight-face)
(set-face-foreground 'uim-preedit-highlight-face "White")
(set-face-background 'uim-preedit-highlight-face "Blue3")

(copy-face 'uim-preedit-highlight-face 'uim-preedit-highlight-underline-face)
(set-face-underline-p                  'uim-preedit-highlight-underline-face t)

;; separator
(copy-face 'default 'uim-separator-face)
(set-face-foreground 'uim-separator-face "LightSkyBlue4")

;; for candidate list

;; odd line
(copy-face 'default 'uim-candidate-odd-face)
(set-face-foreground 'uim-candidate-odd-face "blue")
(set-face-background 'uim-candidate-odd-face "snow1")

;; even line
(copy-face 'default 'uim-candidate-even-face)
(set-face-foreground 'uim-candidate-even-face "blue")
(set-face-background 'uim-candidate-even-face "snow2")

;; focused line
(copy-face 'default  'uim-candidate-selected-face)
(set-face-foreground 'uim-candidate-selected-face "blue")
(set-face-background 'uim-candidate-selected-face "snow3")
;;;(set-face-bold-p     'uim-candidate-selected-face t)

;; total number and focused candidate number
(copy-face 'default 'uim-candidate-nth-face)
(set-face-foreground 'uim-candidate-nth-face "blue")
(set-face-background 'uim-candidate-nth-face "lavender")


(provide 'uim-var)
