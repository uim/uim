;;; anthy.scm: Anthy for uim.
;;; charset: EUC-JP
;;;
;;; Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/
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

(require "util.scm")
(require "ustr.scm")
(require "event.scm")
(require "evmap.scm")
(require "event-translator.scm")
(require "legacy-api-bridge.scm")
(require "ng-japanese.scm")
(require-custom "generic-key-custom.scm")
(require-custom "anthy-custom.scm")
(require-custom "anthy-key-custom.scm")


;;; implementations

(define anthy-lib-initialized? #f)

(define anthy-type-hiragana 0)
(define anthy-type-katakana 1)
(define anthy-type-hankana 2)
(define anthy-type-halfwidth 3)
(define anthy-type-fullwidth 4)
(define anthy-type-direct 5)

(define anthy-input-rule-roma 0)
(define anthy-input-rule-kana 1)
(define anthy-input-rule-azik 2)
(define anthy-input-rule-nicola 3)

(define anthy-direct-convert-opposite-kana -1)
(define anthy-direct-convert-hiragana -2)
(define anthy-direct-convert-katakana -3)
(define anthy-direct-convert-hankana -4)
(define anthy-direct-convert-latin -5)
(define anthy-direct-convert-wide-latin -6)

(define anthy-hiragana-mode?
  (lambda (ac)
    (and (anthy-context-on ac)
	 (= (anthy-context-kana-mode ac)
	    anthy-type-hiragana))))

(define anthy-katakana-mode?
  (lambda (ac)
    (and (anthy-context-on ac)
	 (= (anthy-context-kana-mode ac)
	    anthy-type-katakana))))

(define anthy-hankana-mode?
  (lambda (ac)
    (and (anthy-context-on ac)
	 (= (anthy-context-kana-mode ac)
	    anthy-type-hankana))))

(define anthy-direct-mode?
  (lambda (ac)
    (and (not (anthy-context-on ac))
	 (not (anthy-context-wide-latin ac)))))

(define anthy-wide-latin-mode?
  (lambda (ac)
    (and (not (anthy-context-on ac))
	 (anthy-context-wide-latin ac))))

(define anthy-input-state?
  (lambda (ac)
    (and (anthy-context-on ac)
	 (not (anthy-context-converting ac)))))

(define anthy-converting-state?
  (lambda (ac)
    (and (anthy-context-on ac)
	 (anthy-context-converting ac))))

(define anthy-std-indication-handler
  (lambda (label short-desc)
    (lambda (ac)
      (indication-new 'figure_std_action
		      ""
		      label
		      short-desc))))

;; action.scm must be redesigned and simplified to handle non-widget
;; actions flexibly. But now is not the time. So I perform it in a
;; dirty way to defer the redesign. It will be carried out when the
;; ustore framework has been introduced near uim 1.0 as the big API
;; change. -- YamaKen 2005-03-04
(define anthy-valid-actions ())

(define anthy-activate-action!
  (lambda (ac act-id)
    (let* ((act (and (memq act-id anthy-valid-actions)
		     (fetch-action act-id)))
	   (handler (and act
			 (action-handler act))))
      (if handler
	  (handler ac)
	  (context-prop-activate-handler ac (symbol->string act-id))))))

(define anthy-per-state-action-handler
  (lambda (act-id)
    (lambda (ac)
      (cond
       ((anthy-direct-mode? ac)
	(anthy-direct-state-action ac act-id))
       ((anthy-wide-latin-mode? ac)
	(anthy-wide-latin-state-action ac act-id))
       ((anthy-input-state? ac)
	(if (anthy-has-preedit? ac)
	    (anthy-input-state-with-preedit-action ac act-id)
	    (anthy-input-state-no-preedit-action ac act-id)))
       ((anthy-converting-state? ac)
	(anthy-converting-state-action ac act-id))))))

(define anthy-register-action
  (lambda (id indication-handler activity-pred handler)
    (if (not (memq id anthy-valid-actions))
	(set! anthy-valid-actions (cons id anthy-valid-actions)))
    (register-action id indication-handler activity-pred handler)))

(define anthy-register-std-action
  (lambda (id label short-desc handler)
    (anthy-register-action id
			   (anthy-std-indication-handler label short-desc)
			   #f
			   handler)))

(define anthy-register-per-state-action
  (lambda (id label short-desc)
    (anthy-register-std-action id
			       label
			       short-desc
			       (anthy-per-state-action-handler id))))

(anthy-register-per-state-action 'action_anthy_on
				 "On"
				 "On")

(anthy-register-per-state-action 'action_anthy_toggle_kana
				 "Toggle hiragana/katakana mode"
				 "Toggle hiragana/katakana mode")

(anthy-register-per-state-action 'action_anthy_begin_conv
				 "Begin conversion"
				 "Begin conversion")

(anthy-register-per-state-action 'action_anthy_delete
				 "Delete"
				 "Delete")

(anthy-register-per-state-action 'action_anthy_kill
				 "Erase after cursor"
				 "Erase after cursor")

(anthy-register-per-state-action 'action_anthy_kill_backward
				 "Erase before cursor"
				 "Erase before cursor")

(anthy-register-per-state-action 'action_anthy_go_left
				 "Go left"
				 "Go left")

(anthy-register-per-state-action 'action_anthy_go_right
				 "Go right"
				 "Go right")

(anthy-register-per-state-action 'action_anthy_commit_as_opposite_kana
				 "Commit as opposite kana"
				 "Commit as opposite kana")

(anthy-register-per-state-action 'action_anthy_commit_as_hiragana
				 "Commit as hiragana"
				 "Commit as hiragana")

(anthy-register-per-state-action 'action_anthy_commit_as_katakana
				 "Commit as katakana"
				 "Commit as katakana")

(anthy-register-per-state-action 'action_anthy_commit_as_halfkana
				 "Commit as halfwidth kana"
				 "Commit as halfwidth katakana")

(anthy-register-per-state-action 'action_anthy_commit_as_half_alnum
				 "Commit as halfwidth alphanumeric"
				 "Commit as halfwidth alphanumeric")

(anthy-register-per-state-action 'action_anthy_commit_as_full_alnum
				 "Commit as fullwidth alphanumeric"
				 "Commit as fullwidth alphanumeric")

(anthy-register-per-state-action 'action_anthy_prev_page
				 "Previous page"
				 "Previous page of candidate window")

(anthy-register-per-state-action 'action_anthy_next_page
				 "Next page"
				 "Next page of candidate window")

(anthy-register-per-state-action 'action_anthy_commit
				 "Commit"
				 "Commit")

(anthy-register-per-state-action 'action_anthy_extend_segment
				 "Extend segment"
				 "Extend segment")

(anthy-register-per-state-action 'action_anthy_shrink_segment
				 "Shrink segment"
				 "Shrink segment")

(anthy-register-per-state-action 'action_anthy_next_segment
				 "Next segment"
				 "Next segment")

(anthy-register-per-state-action 'action_anthy_prev_segment
				 "Previous segment"
				 "Previous segment")

(anthy-register-per-state-action 'action_anthy_beginning_of_preedit
				 "Beginning of preedit"
				 "Beginning of preedit")

(anthy-register-per-state-action 'action_anthy_end_of_preedit
				 "End of preedit"
				 "End of preedit")

(anthy-register-per-state-action 'action_anthy_backspace
				 "Backspace"
				 "Backspace")

(anthy-register-per-state-action 'action_anthy_next_candidate
				 "Next candidate"
				 "Next candidate")

(anthy-register-per-state-action 'action_anthy_prev_candidate
				 "Previous candidate"
				 "Previous candidate")

(anthy-register-per-state-action 'action_anthy_cancel_conv
				 "Cancel conversion"
				 "Cancel conversion")

;; candidate selections: Don't use lkey_0 because it may reject KP_0
;; (("0") (action_anthy_candidate_0))
(define anthy-candidate-action-map-ruleset ())

(define anthy-register-candidate-actions
  (lambda ()
    (set! anthy-candidate-action-map-ruleset ())
    (for-each (lambda (idx)
		(let* ((idx-str (digit->string idx))
		       (idx-sym (string->symbol idx-str))
		       (act-sym (symbolconc 'action_anthy_candidate_ idx-sym))
		       (label (string-append "Select candidate " idx-str))
		       (ind-handler (anthy-std-indication-handler label label))
		       (act-handler
			(lambda (ac)
			  (and (anthy-converting-state? ac)
			       (anthy-context-candidate-window ac)
			       (anthy-set-relative-candidate idx)))))
		  (anthy-register-action act-sym ind-handler #f act-handler)
		  (set! anthy-candidate-action-map-ruleset
			(cons (list (list idx-str)   ;; event-seq
				    (list act-sym))  ;; action-seq
			      anthy-candidate-action-map-ruleset))))
	      (iota anthy-nr-candidate-max))))

(anthy-register-candidate-actions)

(define anthy-set-mod-state-handler
  (lambda (mod)
    (lambda (ac)
      (let ((mod-state (bitwise-or (anthy-context-mod-state ac)
				   mod)))
	(anthy-context-set-mod-state! ac mod-state)))))

(define anthy-reset-mod-state-handler
  (lambda (mod)
    (lambda (ac)
      (let ((mod-state (bitwise-and (anthy-context-mod-state ac)
				    (bitwise-not mod))))
	(anthy-context-set-mod-state! ac mod-state)))))

(define anthy-register-modifier-action
  (lambda (mod-sym act-sym)
    (let* ((mod-str (symbol->string mod-sym))
	   (mod-var (symbol-value mod-sym))
	   (set-label (string-append "Set " mod-str " state"))
	   (reset-label (string-append "Reset " mod-str " state"))
	   (set-act-sym (symbolconc 'action_set_ act-sym '_state))
	   (reset-act-sym (symbolconc 'action_reset_ act-sym '_state)))
      (anthy-register-std-action set-act-sym
				 set-label
				 set-label
				 (anthy-set-mod-state-handler mod-var))
      (anthy-register-std-action reset-act-sym
				 reset-label
				 reset-label
				 (anthy-reset-mod-state-handler mod-var)))))

(anthy-register-modifier-action 'mod_Shift_L 'shift_l)
(anthy-register-modifier-action 'mod_Shift_R 'shift_r)
(anthy-register-modifier-action 'mod_Control_L 'control_l)
(anthy-register-modifier-action 'mod_Control_R 'control_r)
(anthy-register-modifier-action 'mod_Alt_L 'alt_l)
(anthy-register-modifier-action 'mod_Alt_R 'alt_r)
(anthy-register-modifier-action 'mod_Meta_L 'meta_l)
(anthy-register-modifier-action 'mod_Meta_R 'meta_r)
(anthy-register-modifier-action 'mod_Super_L 'super_l)
(anthy-register-modifier-action 'mod_Super_R 'super_r)
(anthy-register-modifier-action 'mod_Hyper_L 'hyper_l)
(anthy-register-modifier-action 'mod_Hyper_R 'hyper_r)


(define anthy-prepare-activation
  (lambda (ac)
    (anthy-flush ac)
    (anthy-update-preedit ac)))

(anthy-register-action 'action_anthy_hiragana
;;		 (indication-alist-indicator 'action_anthy_hiragana
;;					     anthy-input-mode-indication-alist)
		       (lambda (ac) ;; indication handler
			 '(figure_ja_hiragana
			   "あ"
			   "ひらがな"
			   "ひらがな入力モード"))

		       anthy-hiragana-mode? ;; activity predicate

		       (lambda (ac) ;; action handler
			 (anthy-prepare-activation ac)
			 (anthy-context-set-on! ac #t)
			 (anthy-switch-kana-mode! ac anthy-type-hiragana)))

(anthy-register-action 'action_anthy_katakana
		       (lambda (ac)
			 '(figure_ja_katakana
			   "ア"
			   "カタカナ"
			   "カタカナ入力モード"))
		       anthy-katakana-mode?
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-context-set-on! ac #t)
			 (anthy-switch-kana-mode! ac anthy-type-katakana)))

(anthy-register-action 'action_anthy_hankana
		       (lambda (ac)
			 '(figure_ja_hankana
			   "ｱ"
			   "半角カタカナ"
			   "半角カタカナ入力モード"))
		       anthy-hankana-mode?
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-context-set-on! ac #t)
			 (anthy-switch-kana-mode! ac anthy-type-hankana)))

(anthy-register-action 'action_anthy_direct
		       (lambda (ac)
			 '(figure_ja_direct
			   "a"
			   "直接入力"
			   "直接(無変換)入力モード"))
		       anthy-direct-mode?
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-context-set-on! ac #f)
			 (anthy-context-set-wide-latin! ac #f)
			 (anthy-switch-kana-mode! ac anthy-type-direct)))

(anthy-register-action 'action_anthy_zenkaku
		       (lambda (ac)
			 '(figure_ja_zenkaku
			   "Ａ"
			   "全角英数"
			   "全角英数入力モード"))
		       anthy-wide-latin-mode?
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-context-set-on! ac #f)
			 (anthy-context-set-wide-latin! ac #t)
			 (anthy-switch-kana-mode! ac anthy-type-fullwidth)))

(anthy-register-action 'action_anthy_roma
		       (lambda (ac)
			 '(figure_ja_roma
			   "Ｒ"
			   "ローマ字"
			   "ローマ字入力モード"))
		       (lambda (ac)
			 (= (anthy-context-input-rule ac)
			    anthy-input-rule-roma))
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-switch-ruletree! ac
						 anthy-input-rule-roma
						 (anthy-context-kana-mode ac))))

(anthy-register-action 'action_anthy_kana
		       (lambda (ac)
			 '(figure_ja_kana
			   "か"
			   "かな"
			   "かな入力モード"))
		       (lambda (ac)
			 (= (anthy-context-input-rule ac)
			    anthy-input-rule-kana))
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-switch-ruletree! ac
						 anthy-input-rule-kana
						 (anthy-context-kana-mode ac))))

(anthy-register-action 'action_anthy_azik
		       (lambda (ac)
			 '(figure_ja_azik
			   "Ａ"
			   "AZIK"
			   "AZIK拡張ローマ字入力モード"))
		       (lambda (ac)
			 (= (anthy-context-input-rule ac)
			    anthy-input-rule-azik))
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-switch-ruletree! ac
						 anthy-input-rule-azik
						 (anthy-context-kana-mode ac))))

(anthy-register-action 'action_anthy_nicola
		       (lambda (ac)
			 '(figure_ja_nicola
			   "親"
			   "NICOLA"
			   "NICOLA入力モード"))
		       (lambda (ac)
			 (= (anthy-context-input-rule ac)
			    anthy-input-rule-nicola))
		       (lambda (ac)
			 (anthy-prepare-activation ac)
			 (anthy-switch-ruletree! ac
						 anthy-input-rule-nicola
						 (anthy-context-kana-mode ac))))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define anthy-configure-widgets
  (lambda ()
    (register-widget 'widget_anthy_input_mode
		     (activity-indicator-new anthy-input-mode-actions)
		     (actions-new anthy-input-mode-actions))

    (register-widget 'widget_anthy_kana_input_method
		     (activity-indicator-new anthy-kana-input-method-actions)
		     (actions-new anthy-kana-input-method-actions))
    (context-list-replace-widgets! 'anthy anthy-widgets)))

(define anthy-direct-state-action-map-ruleset
  '((((mod_Control lkey_j))   (action_anthy_on))
    (((mod_Control lkey_J))   (action_anthy_on))
    (((mod_Shift lkey_space)) (action_anthy_on))  ;; generic
    ((lkey_Zenkaku_Hankaku)   (action_anthy_on))  ;; generic
    ))

(define anthy-wide-latin-state-action-map-ruleset
  '((((mod_Control lkey_j))   (action_anthy_on))
    (((mod_Control lkey_J))   (action_anthy_on))
    (((mod_Shift lkey_space)) (action_anthy_on))  ;; generic
    ((lkey_Zenkaku_Hankaku)   (action_anthy_on))  ;; generic
    ))

(define anthy-input-state-no-preedit-action-map-ruleset
  '(
     ;;((lkey_q)                 (action_anthy_toggle_kana))
     ;;((lkey_Q)                 (action_anthy_toggle_kana))
    ;;(()                       (action_anthy_hiragana))
    ;;(()                       (action_anthy_katakana))
    ;;(((mod_Control lkey_q))   (action_anthy_hankana))
    ;;(((mod_Control lkey_Q))   (action_anthy_hankana))
    ;;((lkey_l)                 (action_anthy_direct))
    ;;((lkey_L)                 (action_anthy_direct))
    ;;((mod_Shift lkey_l)       (action_anthy_zenkaku))
    ;;((mod_Shift lkey_L)       (action_anthy_zenkaku))
    ;;(()                       (action_anthy_roma))
    ;;(()                       (action_anthy_kana))
    ;;(()                       (action_anthy_azik))
    ;;(()                       (action_anthy_nicola))
    ))

(define anthy-input-state-with-preedit-action-map-ruleset
  '(
    ;;((lkey_q)                 (action_anthy_toggle_kana))
    ;;((lkey_Q)                 (action_anthy_toggle_kana))
    ((lkey_space)             (action_anthy_begin_conv))  ;; generic
    (((mod_Control lkey_k))   (action_anthy_kill))  ;; generic
    (((mod_Control lkey_K))   (action_anthy_kill))  ;; generic
    (((mod_Control lkey_u))   (action_anthy_kill_backward))  ;; generic
    (((mod_Control lkey_U))   (action_anthy_kill_backward))  ;; generic
     ((lkey_Delete)            (action_anthy_delete))  ;; generic
     (((mod_Control lkey_d))   (action_anthy_delete))  ;; generic
     (((mod_Control lkey_D))   (action_anthy_delete))  ;; generic
     (((mod_Control lkey_h))   (action_anthy_backspace))  ;; generic
     (((mod_Control lkey_H))   (action_anthy_backspace))  ;; generic
     ((lkey_BackSpace)         (action_anthy_backspace))  ;; generic
     ((lkey_Left)              (action_anthy_go_left))  ;; generic
     (((mod_Control lkey_b))   (action_anthy_go_left))  ;; generic
     (((mod_Control lkey_B))   (action_anthy_go_left))  ;; generic
     ((lkey_Right)             (action_anthy_go_right))  ;; generic
     (((mod_Control lkey_f))   (action_anthy_go_right))  ;; generic
     (((mod_Control lkey_F))   (action_anthy_go_right))  ;; generic
     ;;(((mod_Shift lkey_q))     (action_anthy_commit_as_opposite_kana))
     ;;(((mod_Shift lkey_Q))     (action_anthy_commit_as_opposite_kana))
     ((lkey_F6)                (action_anthy_commit_as_hiragana))
     ((lkey_F7)                (action_anthy_commit_as_katakana))
     ((lkey_F8)                (action_anthy_commit_as_halfkana))
     ((lkey_F9)                (action_anthy_commit_as_half_alnum))
     ((lkey_F10)               (action_anthy_commit_as_full_alnum))
     (((mod_Control lkey_j))   (action_anthy_commit))  ;; generic
     (((mod_Control lkey_J))   (action_anthy_commit))  ;; generic
     (((mod_Control lkey_m))   (action_anthy_commit))  ;; generic-return
     (((mod_Control lkey_M))   (action_anthy_commit))  ;; generic-return
     ((lkey_Return)            (action_anthy_commit))  ;; generic-return
     (((mod_Control lkey_a))   (action_anthy_beginning_of_preedit))  ;; generic
     (((mod_Control lkey_A))   (action_anthy_beginning_of_preedit))  ;; generic
     ((lkey_Home)              (action_anthy_beginning_of_preedit))  ;; generic
     (((mod_Control lkey_e))   (action_anthy_end_of_preedit))  ;; generic
     (((mod_Control lkey_E))   (action_anthy_end_of_preedit))  ;; generic
     ((lkey_End)               (action_anthy_end_of_preedit))  ;; generic
     ((lkey_Escape)            (action_anthy_cancel_conv))  ;; generic
     (((mod_Control lkey_g))   (action_anthy_cancel_conv))  ;; generic
     (((mod_Control lkey_G))   (action_anthy_cancel_conv))  ;; generic
    ))

(define anthy-converting-state-action-map-ruleset
  (append
   '(
     ;;((lkey_q)                 (action_anthy_toggle_kana))
     ;;((lkey_Q)                 (action_anthy_toggle_kana))
     ((lkey_Page_Up)           (action_anthy_prev_page))  ;; generic
     ((lkey_Page_Down)         (action_anthy_next_page))  ;; generic
     (((mod_Control lkey_o))   (action_anthy_extend_segment))
     (((mod_Control lkey_O))   (action_anthy_extend_segment))
     ((mod_Shift lkey_Right)   (action_anthy_extend_segment))
     (((mod_Control lkey_i))   (action_anthy_shrink_segment))
     (((mod_Control lkey_I))   (action_anthy_shrink_segment))
     ((mod_Shift lkey_Left)    (action_anthy_shrink_segment))
     (((mod_Control lkey_f))   (action_anthy_next_segment))
     (((mod_Control lkey_F))   (action_anthy_next_segment))
     ((lkey_Right)             (action_anthy_next_segment))
     (((mod_Control lkey_b))   (action_anthy_prev_segment))
     (((mod_Control lkey_B))   (action_anthy_prev_segment))
     ((lkey_Left)              (action_anthy_prev_segment))
     ((lkey_space)             (action_anthy_next_candidate))  ;; generic
     ((lkey_Down)              (action_anthy_next_candidate))  ;; generic
     (((mod_Control lkey_n))   (action_anthy_next_candidate))  ;; generic
     (((mod_Control lkey_N))   (action_anthy_next_candidate))  ;; generic
     ((lkey_Up)                (action_anthy_prev_candidate))  ;; generic
     (((mod_Control lkey_p))   (action_anthy_prev_candidate))  ;; generic
     (((mod_Control lkey_P))   (action_anthy_prev_candidate))  ;; generic

     ;;((lkey_Left)              (action_anthy_go_left))  ;; generic
     ;;(((mod_Control lkey_b))   (action_anthy_go_left))  ;; generic
     ;;(((mod_Control lkey_B))   (action_anthy_go_left))  ;; generic
     ;;((lkey_Right)             (action_anthy_go_right))  ;; generic
     ;;(((mod_Control lkey_f))   (action_anthy_go_right))  ;; generic
     ;;(((mod_Control lkey_F))   (action_anthy_go_right))  ;; generic

     ;;(((mod_Shift lkey_q))     (action_anthy_commit_as_opposite_kana))
     ;;(((mod_Shift lkey_Q))     (action_anthy_commit_as_opposite_kana))
     ((lkey_F6)                (action_anthy_commit_as_hiragana))
     ((lkey_F7)                (action_anthy_commit_as_katakana))
     ((lkey_F8)                (action_anthy_commit_as_halfkana))
     ((lkey_F9)                (action_anthy_commit_as_half_alnum))
     ((lkey_F10)               (action_anthy_commit_as_full_alnum))
     (((mod_Control lkey_j))   (action_anthy_commit))  ;; generic
     (((mod_Control lkey_J))   (action_anthy_commit))  ;; generic
     (((mod_Control lkey_m))   (action_anthy_commit))  ;; generic-return
     (((mod_Control lkey_M))   (action_anthy_commit))  ;; generic-return
     ((lkey_Return)            (action_anthy_commit))  ;; generic-return
     (((mod_Control lkey_a))   (action_anthy_beginning_of_preedit))  ;; generic
     (((mod_Control lkey_A))   (action_anthy_beginning_of_preedit))  ;; generic
     ((lkey_Home)              (action_anthy_beginning_of_preedit))  ;; generic
     (((mod_Control lkey_e))   (action_anthy_end_of_preedit))  ;; generic
     (((mod_Control lkey_E))   (action_anthy_end_of_preedit))  ;; generic
     ((lkey_End)               (action_anthy_end_of_preedit))  ;; generic
     ((lkey_Escape)            (action_anthy_cancel_conv))  ;; generic
     (((mod_Control lkey_g))   (action_anthy_cancel_conv))  ;; generic
     (((mod_Control lkey_G))   (action_anthy_cancel_conv))  ;; generic
     )
   anthy-candidate-action-map-ruleset   
   ))

(define anthy-direct-state-action-map-ruletree
  (evmap-parse-ruleset anthy-direct-state-action-map-ruleset))

(define anthy-wide-latin-state-action-map-ruletree
  (evmap-parse-ruleset anthy-wide-latin-state-action-map-ruleset))

(define anthy-input-state-no-preedit-action-map-ruletree
  (evmap-parse-ruleset anthy-input-state-no-preedit-action-map-ruleset))

(define anthy-input-state-with-preedit-action-map-ruletree
  (evmap-parse-ruleset anthy-input-state-with-preedit-action-map-ruleset))

(define anthy-converting-state-action-map-ruletree
  (evmap-parse-ruleset anthy-converting-state-action-map-ruleset))

(define evmap-context-list-preedit-string
  (lambda (emc-list)
    (apply string-append
	   (apply append
		  (filter-map evmap-context-preedit-string
			      emc-list)))))

(define evmap-ustr-preedit-string
  (lambda (ustr)
    (evmap-context-list-preedit-string (ustr-whole-seq ustr))))

(define evmap-ustr-set-visible-pos!
  (lambda (ustr pos)
    (ustr-cursor-move-beginning! ustr)
    (let self ((rest pos))
      (if (ustr-cursor-at-end? ustr)
	  ustr
	  (let* ((emc (ustr-cursor-frontside ustr))
		 (visible-len (length (evmap-context-preedit-string emc))))
	    (if (and (> visible-len 0)
		     (<= rest 0))
		ustr
		(begin
		  (ustr-cursor-move-forward! ustr)
		  (self (if (> visible-len 0)
			    (- rest visible-len)
			    rest)))))))))

(define evmap-ustr-substr-visible
  (lambda (ustr start len)
    (let ((substr (ustr-dup ustr)))
      (evmap-ustr-set-visible-pos! substr start)
      (ustr-clear-former! substr)
      (evmap-ustr-set-visible-pos! substr len)
      (ustr-clear-latter! substr)
      substr)))

;; returns closer-tree or #f
(define evmap-ustr-input-with-new-emc!
  (lambda (ustr ruletree ev)
    (let* ((emc (evmap-context-new ruletree))
	   (closer-tree (evmap-context-input! emc ev)))
      (and closer-tree
	   (begin
	     (ustr-insert-elem! ustr emc)
	     closer-tree)))))

;; returns closer-tree or #f
(define evmap-ustr-input!
  (lambda (ustr ruletree ev)
    (let* ((last-emc (and (not (ustr-cursor-at-beginning? ustr))
			  (ustr-cursor-backside ustr)))
	   (closer-tree (or (and last-emc
				 (not (evmap-context-complete? last-emc))
				 (evmap-context-input! last-emc ev))
			    (evmap-ustr-input-with-new-emc! ustr ruletree ev))))
      (if (event-loopback ev)
	  (begin
	    (event-set-loopback! ev #f)
	    (evmap-ustr-input! ustr ruletree ev))
	  closer-tree))))

;; returns #t or commit string when consumed
(define evmap-ustr-input-to-immediate-commit!
  (lambda (ustr ev alt-ruletree)
    (let ((last-emc (ustr-end-elem ustr)))
      (and (or (ustr-empty? ustr)
	       (and last-emc
		    (eq? (evmap-context-root last-emc)
			 alt-ruletree)))
	   (evmap-ustr-input! ustr alt-ruletree ev)
	   (let ((last-emc (ustr-end-elem ustr)))
	     (if (evmap-context-complete? last-emc)
		 (let ((commit-str (evmap-ustr-preedit-string ustr)))
		   (ustr-clear! ustr)
		   commit-str)
		 #t))))))

;; TODO: Support following alternative behavior
;; "ちゃ" -> backspace -> "ち"
(define evmap-ustr-backspace!
  (lambda (ustr)
    (let ((last-emc (and (not (ustr-cursor-at-beginning? ustr))
			 (ustr-cursor-backside ustr))))
      (if last-emc
	  (if (evmap-context-complete? last-emc)
	      (ustr-cursor-delete-backside! ustr)
	      (evmap-context-undo! last-emc))))))

(define evmap-ustr-transpose
  (lambda (ustr new-ruletree)
    (fold (lambda (ev new-ustr)
	    (evmap-ustr-input! new-ustr new-ruletree ev)
	    new-ustr)
	  (ustr-new)
	  (append-map-ustr-whole evmap-context-event-seq ustr))))

(define anthy-ruletree
  (let ((alphanumeric-alist
	 (list
	  (cons anthy-type-fullwidth ja-fullwidth-alphanumeric-ruletree)
	  (cons anthy-type-halfwidth ja-halfwidth-alphanumeric-ruletree)
	  (cons anthy-type-direct    ja-direct-ruletree))))
    (lambda (input-rule kana-mode)
      (safe-cdr
       (assoc kana-mode
	      (cond
	       ((= input-rule anthy-input-rule-roma)
		(require "ng-japanese-romaji.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-romaji-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-romaji-katakana-ruletree)
		  (cons anthy-type-hankana   ja-romaji-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-kana)
		(require "ng-japanese-kana.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-kana-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-kana-katakana-ruletree)
		  (cons anthy-type-hankana   ja-kana-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-azik)
		(require "ng-japanese-azik.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-azik-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-azik-katakana-ruletree)
		  (cons anthy-type-hankana   ja-azik-halfkana-ruletree))
		 alphanumeric-alist))
	       ((= input-rule anthy-input-rule-nicola)
		(require "japanese-nicola.scm")
		(append
		 (list
		  (cons anthy-type-hiragana  ja-nicola-hiragana-ruletree)
		  (cons anthy-type-katakana  ja-nicola-katakana-ruletree)
		  (cons anthy-type-hankana   ja-nicola-halfkana-ruletree))
		 alphanumeric-alist))))))))

(define anthy-actmap-ruletree
  (lambda (ac)
    (cond
     ((anthy-direct-mode? ac)
      anthy-direct-state-action-map-ruletree)

     ((anthy-wide-latin-mode? ac)
      anthy-wide-latin-state-action-map-ruletree)

     ((anthy-input-state? ac)
      ;;anthy-input-state-no-preedit-action-map-ruletree
      anthy-input-state-with-preedit-action-map-ruletree)

     ((anthy-converting-state? ac)
      anthy-converting-state-action-map-ruletree))))

(define anthy-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on                 #f)
    (list 'converting         #f)
    (list 'ac-id              #f) ;; anthy-context-id
    (list 'preconv-ustr       #f) ;; preedit strings
    (list 'segments           #f) ;; ustr of candidate indices
    (list 'candidate-window   #f)
    (list 'candidate-op-count 0)
    (list 'wide-latin         #f)
    (list 'kana-mode          anthy-type-hiragana)
    (list 'input-rule         anthy-input-rule-roma)
    (list 'ruletree           #f)    ;; current composition rule
    (list 'keytrans-emc       #f)    ;; evmap-context for key-event translator
    (list 'actmap-emc         #f)    ;; evmap-context for action mapper
    (list 'mod-state          mod_None)    ;; regenerated modifier state
    (list 'mod-lock           mod_None)    ;; modifier lock state
    (list 'mod-stick          mod_None)))) ;; sticky modifier state
(define-record 'anthy-context anthy-context-rec-spec)
(define anthy-context-new-internal anthy-context-new)

(define anthy-context-new
 (lambda (id im)
   (let ((ac (anthy-context-new-internal id im)))
     (if (symbol-bound? 'anthy-lib-init)
	 (set! anthy-lib-initialized? (anthy-lib-init)))
     (if anthy-lib-initialized?
	 (anthy-context-set-ac-id! ac (anthy-lib-alloc-context)))
     (anthy-context-set-widgets! ac anthy-widgets)
     (anthy-context-set-preconv-ustr! ac (ustr-new))
     (anthy-context-set-segments! ac (ustr-new))
     (anthy-context-set-keytrans-emc! ac (key-event-translator-new))

     ;; 2004-08-26 Takuro Ashie <ashie@homa.ne.jp>
     ;;   * I think load-kana-table should be marked as depracated.
     ;;     Because it is a little violent (it overwrites ja-rk-rule table).
     ;;     We should prepare a custom entry like "uim-default-input-rule"
     ;;     instead of using-kana-table.
     (if (and (symbol-bound? 'using-kana-table?)
	      using-kana-table?)
	 (anthy-context-set-input-rule! ac anthy-input-rule-kana))
     (anthy-switch-ruletree! ac
			     (anthy-context-input-rule ac)
			     (anthy-context-kana-mode ac))
     ac)))

(define anthy-switch-ruletree!
  (lambda (ac input-rule kana-mode)
    (let ((ruletree (anthy-ruletree input-rule kana-mode))
	  (actmap-ruletree (anthy-actmap-ruletree ac)))
      (anthy-context-set-input-rule! ac input-rule)
      (anthy-context-set-kana-mode! ac kana-mode)
      (anthy-context-set-ruletree! ac ruletree)
      (anthy-context-set-actmap-emc! ac (evmap-context-new actmap-ruletree)))))

(define anthy-switch-kana-mode!
  (lambda (ac kana-mode)
    (let ((rule (anthy-context-input-rule ac)))
      (anthy-switch-ruletree! ac rule kana-mode))))

(define anthy-toggle-kana-mode!
  (lambda (ac)
    (let* ((kana (anthy-context-kana-mode ac))
	   (opposite-kana (multi-segment-opposite-kana kana)))
      (anthy-switch-kana-mode! ac opposite-kana))))

(define anthy-transpose-preconv!
  (lambda (ac kana-mode)
    (let* ((preconv-ustr (anthy-context-preconv-ustr ac))
	   (rule (anthy-context-input-rule ac))
	   (ruletree (anthy-ruletree rule kana-mode))
	   (transposed (evmap-ustr-transpose preconv-ustr ruletree)))
      (anthy-context-set-preconv-ustr! ac transposed))))

(define anthy-commit-transposed-preconv!
  (lambda (ac kana-mode)
    (anthy-transpose-preconv! ac kana-mode)
    (anthy-commit-preconv! ac)))

(define anthy-commit-preconv!
  (lambda (ac)
    (im-commit ac (evmap-ustr-preedit-string (anthy-context-preconv-ustr ac)))
    (anthy-flush ac)))

(define anthy-input!
  (lambda (ac ev)
    (let ((actmap-emc (if (and (anthy-input-state? ac)
			       (not (anthy-has-preedit? ac)))
			  (evmap-context-new
			   anthy-input-state-no-preedit-action-map-ruletree)
			  (anthy-context-actmap-emc ac))))
      (if (evmap-context-input! actmap-emc ev)
	  (if (evmap-context-complete? actmap-emc)
	      (begin
		(for-each (lambda (act-id)
			    (anthy-activate-action! ac act-id))
			  (evmap-context-action-seq actmap-emc))
		(evmap-context-flush! actmap-emc)
		(anthy-update-preedit ac)))
	  (let* ((rejected-ev-list (evmap-context-event-seq actmap-emc))
		 (consumed-list (map (lambda (rej-ev)
				       (anthy-preedit-input! ac rej-ev))
				     (append rejected-ev-list
					     (list ev)))))
	    (evmap-context-flush! actmap-emc)
	    (if (apply proc-or consumed-list)
		(anthy-update-preedit ac)))))))

;; returns consumed
(define anthy-preedit-input!
  (lambda (ac ev)
    (let* ((preconv-ustr (anthy-context-preconv-ustr ac))
	   (ruletree (anthy-context-ruletree ac))
	   (imm-ruletree ja-immediate-commit-ruletree)
	   (immediate-commit
	    (lambda (alt-ruletree)
	      (let ((consumed (evmap-ustr-input-to-immediate-commit!
			       preconv-ustr
			       ev
			       alt-ruletree)))
		(if (string? consumed)
		    (im-commit ac consumed))
		consumed))))
      (cond
       ((anthy-direct-mode? ac)
	#f)
       ((anthy-wide-latin-mode? ac)
	(immediate-commit ruletree))
       ((anthy-converting-state? ac)
	(if (and (eq? (event-type ev)
		      'key)
		 (char-printable? (key-event-char ev))
;;		 (modifier-match? mod_ignore_Shift
;;				  (key-event-modifier ev))
		 )
	    (begin
	      (anthy-cancel-conv ac)
	      (anthy-input! ac ev))))
       ((anthy-input-state? ac)
	(or (immediate-commit ja-immediate-commit-ruletree)
	    (evmap-ustr-input! preconv-ustr ruletree ev)))))))

(define anthy-init-handler
  (lambda (id im arg)
    (anthy-context-new id im)))

(define anthy-release-handler
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac)))
      (anthy-lib-free-context ac-id))))

(define anthy-flush
  (lambda (ac)
    (ustr-clear! (anthy-context-preconv-ustr ac))
    (ustr-clear! (anthy-context-segments ac))
    (anthy-context-set-converting! ac #f)
    (if (anthy-context-candidate-window ac)
	(im-deactivate-candidate-selector ac))
    (anthy-context-set-candidate-window! ac #f)
    (anthy-context-set-candidate-op-count! ac 0)
    (anthy-context-set-actmap-emc! ac (evmap-context-new
				       (anthy-actmap-ruletree ac)))
    (anthy-update-preedit ac)  ;; TODO: remove this
    ))

(define anthy-begin-input
  (lambda (ac)
    (anthy-context-set-on! ac #t)
    (anthy-context-set-converting! ac #f)))

(define anthy-update-preedit
  (lambda (ac)
    (let ((segments (if (anthy-context-on ac)
			(if (anthy-context-converting ac)
			    (anthy-converting-state-preedit ac)
			    (anthy-input-state-preedit ac))
			())))
      (context-update-preedit ac segments))))

(define anthy-direct-state-action
  (lambda (ac act-id)
    (case act-id
      ((action_anthy_on)
       (anthy-begin-input ac)
       (anthy-switch-kana-mode! ac (anthy-context-kana-mode ac))))))

(define anthy-begin-conv
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (kana (anthy-context-kana-mode ac))
	   (preconv-ustr (anthy-context-preconv-ustr ac))
	   (preconv-str (evmap-ustr-preedit-string preconv-ustr)))
      (if (and (number? (anthy-context-ac-id ac))
	       (> (string-length preconv-str)
		  0))
	  (begin
	    (anthy-lib-set-string ac-id preconv-str)
	    (let ((nr-segments (anthy-lib-get-nr-segments ac-id)))
	      (ustr-set-latter-seq! (anthy-context-segments ac)
				    (make-list nr-segments 0))
	      (anthy-context-set-converting! ac #t)
	      (anthy-context-set-actmap-emc! ac (evmap-context-new
						 (anthy-actmap-ruletree ac)))
	      ))))))

(define anthy-cancel-conv
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
           (preconv-ustr (anthy-context-preconv-ustr ac))
	   (segments (anthy-context-segments ac))
           (cur-seg (ustr-cursor-pos segments))
           (cur-seg-pos (anthy-get-segment-pos ac cur-seg))
           (cur-seg-len (anthy-lib-get-segment-length ac-id cur-seg))
	   (new-pos (+ cur-seg-pos cur-seg-len)))
      (anthy-reset-candidate-window ac)
      (anthy-context-set-converting! ac #f)
      (evmap-ustr-set-visible-pos! preconv-ustr new-pos)
      (ustr-clear! segments)
      (anthy-context-set-actmap-emc! ac (evmap-context-new
					 (anthy-actmap-ruletree ac)))
      (anthy-update-preedit ac)  ;; TODO: remove this
      )))

(define anthy-input-state-no-preedit-action
  (lambda (ac act-id)
    (case act-id
      ((action_anthy_zenkaku)
       (anthy-flush ac)
       (anthy-context-set-on! ac #f)
       (anthy-context-set-wide-latin! ac #t))

      ((action_anthy_direct)
       (anthy-flush ac)
       (anthy-context-set-on! ac #f)
       (anthy-context-set-wide-latin! ac #f))

      ((action_anthy_hankana)
       (anthy-switch-kana-mode! ac anthy-type-hankana))

      ((action_anthy_toggle_kana)
       (anthy-toggle-kana-mode! ac)))))

(define anthy-has-preedit?
  (lambda (ac)
    (not (ustr-empty? (anthy-context-preconv-ustr ac)))))

(define anthy-input-state-with-preedit-action
  (lambda (ac act-id)
    (let ((preconv-ustr (anthy-context-preconv-ustr ac))
	  (kana (anthy-context-kana-mode ac))
	  (transpose (if anthy-commit-transposed-preedit-immediately?
			 anthy-commit-transposed-preconv!
			 anthy-transpose-preconv!))) ;; does not commit
      (case act-id
	((action_anthy_begin_conv)
	 (anthy-begin-conv ac))

	((action_anthy_backspace)
	 (evmap-ustr-backspace! preconv-ustr))

	((action_anthy_delete)
	 (ustr-cursor-delete-frontside! preconv-ustr))

	((action_anthy_kill)
	 (ustr-clear-latter! preconv-ustr))

	((action_anthy_kill_backward)
	 (ustr-clear-former! preconv-ustr))

	((action_anthy_commit_as_opposite_kana)
	 (transpose ac (multi-segment-opposite-kana kana)))

	((action_anthy_commit_as_hiragana)
	 (transpose ac anthy-type-hiragana))

	((action_anthy_commit_as_katakana)
	 (transpose ac anthy-type-katakana))

	((action_anthy_commit_as_halfkana)
	 (transpose ac anthy-type-hankana))

	((action_anthy_commit_as_half_alnum)
	 (transpose ac anthy-type-halfwidth))

	((action_anthy_commit_as_full_alnum)
	 (transpose ac anthy-type-fullwidth))

	;; commit current preedit string, then toggle hiragana/katakana mode.
	((action_anthy_toggle_kana)
	 (anthy-commit-preconv! ac)
	 (anthy-toggle-kana-mode! ac))

	((action_anthy_cancel_conv)
	 (anthy-flush ac))

	((action_anthy_commit)
	 (anthy-commit-preconv! ac))

	((action_anthy_go_left)
	 (ustr-cursor-move-backward! preconv-ustr))

	((action_anthy_go_right)
	 (ustr-cursor-move-forward! preconv-ustr))

	((action_anthy_beginning_of_preedit)
	 (ustr-cursor-move-beginning! preconv-ustr))

	((action_anthy_end_of_preedit)
	 (ustr-cursor-move-end! preconv-ustr))))))

(define anthy-separator
  (lambda (ac)
    (let ((attr (bitwise-or preedit-separator
			    preedit-underline)))
      (and anthy-show-segment-separator?
	   (cons attr anthy-segment-separator)))))

(define anthy-get-segment-pos
  (lambda (ac seg-idx)
    (let ((ac-id (anthy-context-ac-id ac)))
      (apply + (map (lambda (idx)
		      (anthy-lib-get-segment-length ac-id idx))
		    (iota seg-idx))))))

(define anthy-cand-idx->type
  (let ((cand-idx->type-alist
	 (list
	  (cons anthy-direct-convert-hiragana      anthy-type-hiragana)
	  (cons anthy-direct-convert-katakana      anthy-type-katakana) 
	  (cons anthy-direct-convert-hankana       anthy-type-hankana)	 
	  (cons anthy-direct-convert-latin         anthy-type-halfwidth)
	  (cons anthy-direct-convert-wide-latin    anthy-type-fullwidth))))
    (lambda (ac idx)
      (if (= idx anthy-direct-convert-opposite-kana)
	  (multi-segment-opposite-kana (anthy-context-kana-mode ac))
	  (safe-cdr (assv idx cand-idx->type-alist))))))

(define anthy-get-nth-candidate
  (lambda (ac seg-idx cand-idx)
    (let ((ac-id (anthy-context-ac-id ac)))
      (if (>= cand-idx 0)
	  (anthy-lib-get-nth-candidate ac-id seg-idx cand-idx)
	  (let* ((seg-pos (anthy-get-segment-pos ac seg-idx))
		 (seg-len (anthy-lib-get-segment-length ac-id seg-idx))
		 (preconv-ustr (anthy-context-preconv-ustr ac))
		 (seg-ustr (evmap-ustr-substr-visible preconv-ustr
						      seg-pos
						      seg-len))
		 (cand-kana (anthy-cand-idx->type ac cand-idx))
		 (cand-rule (anthy-context-input-rule ac))
		 (cand-ruletree (anthy-ruletree cand-rule cand-kana))
		 (cand-ustr (evmap-ustr-transpose seg-ustr cand-ruletree)))
	    (evmap-ustr-preedit-string cand-ustr))))))

(define anthy-converting-state-preedit
  (lambda (ac)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (separator (anthy-separator ac))
	   (mapped-segs (map (lambda (seg-idx cand-idx)
			       (let* ((attr (if (= seg-idx cur-seg)
						(bitwise-or preedit-reverse
							    preedit-cursor)
						preedit-underline))
				      (cand (anthy-get-nth-candidate
					     ac seg-idx cand-idx)))
				 (cons attr cand)))
			     (iota (ustr-length segments))
			     (ustr-whole-seq segments))))
      (if separator
	  (join separator mapped-segs)
	  mapped-segs))))

(define anthy-input-state-preedit
  (lambda (ac)
    (let* ((preconv-ustr (anthy-context-preconv-ustr ac))
	   (former (ustr-former-seq preconv-ustr))
	   (latter (ustr-latter-seq preconv-ustr)))
      (remove not
	      (list
	       (and (not (ustr-cursor-at-beginning? preconv-ustr))
		    (cons preedit-underline
			  (evmap-context-list-preedit-string former)))
	       (and (anthy-has-preedit? ac)
		    (cons preedit-cursor ""))
	       (and (not (ustr-cursor-at-end? preconv-ustr))
		    (cons preedit-underline
			  (evmap-context-list-preedit-string latter))))))))

(define anthy-get-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (string-append-map (lambda (seg-idx cand-idx)
                           (anthy-get-nth-candidate ac seg-idx cand-idx))
 			 (iota (ustr-length segments))
			 (ustr-whole-seq segments)))))

(define anthy-commit-string
  (lambda (ac)
    (let ((ac-id (anthy-context-ac-id ac))
	  (segments (anthy-context-segments ac)))
      (for-each (lambda (seg-idx cand-idx)
                  (if (>= cand-idx 0)
                      (anthy-lib-commit-segment ac-id seg-idx cand-idx)))
 		(iota (ustr-length segments))
		(ustr-whole-seq segments)))))

(define anthy-commit-converted!
  (lambda (ac)
    (im-commit ac (anthy-get-commit-string ac))
    (anthy-commit-string ac)
    (anthy-reset-candidate-window ac)
    (anthy-flush ac)))

(define anthy-correct-segment-cursor
  (lambda (segments)
    (if (ustr-cursor-at-end? segments)
	(ustr-cursor-move-backward! segments))))

(define anthy-move-segment
  (lambda (ac offset)
    (anthy-reset-candidate-window ac)
    (let ((segments (anthy-context-segments ac)))
      (ustr-cursor-move! segments offset)
      (anthy-correct-segment-cursor segments))))

(define anthy-resize-segment
  (lambda (ac cnt)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments)))
      (anthy-reset-candidate-window ac)
      (anthy-lib-resize-segment ac-id cur-seg cnt)
      (let* ((resized-nseg (anthy-lib-get-nr-segments ac-id))
	     (latter-nseg (- resized-nseg cur-seg)))
	(ustr-set-latter-seq! segments (make-list latter-nseg 0))))))

(define anthy-set-candidate
  (lambda (ac idx)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (compensated-idx (cond
			     ((>= idx max)
			      0)
			     ((< idx anthy-direct-convert-wide-latin)
			      (- max 1))
			     (else
			      idx))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (if (anthy-context-candidate-window ac)
	  ;;(im-select-candidate ac compensated-idx)
	  (begin
	    (im-deactivate-candidate-selector ac)
	    (anthy-update-preedit ac))))))

(define anthy-move-candidate
  (lambda (ac offset)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (n (+ (ustr-cursor-frontside segments)
		 offset))
	   (compensated-n (cond
			   ((>= n max)
			    0)
			   ((< n 0)
			    (- max 1))
			   (else
			    n)))
	   (new-op-count (+ 1 (anthy-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-n)
      (anthy-context-set-candidate-op-count! ac new-op-count)
      (if (and anthy-use-candidate-window?
	       (= (anthy-context-candidate-op-count ac)
		  anthy-candidate-op-count))
	  (begin
	    (anthy-context-set-candidate-window! ac #t)
	    (im-activate-candidate-selector ac max anthy-nr-candidate-max)))
      (if (anthy-context-candidate-window ac)
	  (begin
	    (im-select-candidate ac compensated-n)
	    (anthy-update-preedit ac))))))

(define anthy-move-candidate-in-page
  (lambda (ac numeralc)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (segments (anthy-context-segments ac))
	   (cur-seg (ustr-cursor-pos segments))
	   (max (anthy-lib-get-nr-candidates ac-id cur-seg))
	   (n (ustr-cursor-frontside segments))
	   (cur-page (if (= anthy-nr-candidate-max 0)
	   		 0
			 (quotient n anthy-nr-candidate-max)))
	   (pageidx (- (numeral-char->number numeralc) 1))
	   (compensated-pageidx (cond
				 ((< pageidx 0) ; pressing key_0
				  (+ pageidx 10))
				 (else
				  pageidx)))
	   (idx (+ (* cur-page anthy-nr-candidate-max) compensated-pageidx))
	   (compensated-idx (cond
			     ((>= idx max)
			      (- max 1))
			     (else
			      idx)))
	   (new-op-count (+ 1 (anthy-context-candidate-op-count ac))))
      (ustr-cursor-set-frontside! segments compensated-idx)
      (anthy-context-set-candidate-op-count! ac new-op-count)
      (im-select-candidate ac compensated-idx)
      (anthy-update-preedit ac))))

;; takes index in current page
(define anthy-set-relative-candidate
  (lambda (idx)
    (lambda (ac)
      (and (anthy-converting-state? ac)
	   (anthy-context-candidate-window ac)
	   (anthy-move-candidate-in-page ac ((string->char "0") + idx))))))

(define anthy-reset-candidate-window
  (lambda (ac)
    (if (anthy-context-candidate-window ac)
	(begin
	  (im-deactivate-candidate-selector ac)
	  (anthy-context-set-candidate-window! ac #f)))
    (anthy-context-set-candidate-op-count! ac 0)))

(define anthy-converting-state-action
  (lambda (ac act-id)
    (let ((preconv-ustr (anthy-context-preconv-ustr ac))
	  (segments (anthy-context-segments ac)))
      (case act-id
	;; transpose current segment to opposite kana
	((action_anthy_commit_as_opposite_kana)
	 (anthy-set-candidate ac anthy-direct-convert-opposite-kana))

	;; transpose current segment to hiragana
	((action_anthy_commit_as_hiragana)
	 (anthy-set-candidate ac anthy-direct-convert-hiragana))

	;; transpose current segment to katakana
	((action_anthy_commit_as_katakana)
	 (anthy-set-candidate ac anthy-direct-convert-katakana))

	;; transpose current segment to halfwidth katakana
	((action_anthy_commit_as_halfkana)
	 (anthy-set-candidate ac anthy-direct-convert-hankana))

	;; transpose current segment to halfwidth alphanumeric
	((action_anthy_commit_as_half_alnum)
	 (anthy-set-candidate ac anthy-direct-convert-latin))

	;; transpose current segment to fullwidth alphanumeric
	((action_anthy_commit_as_full_alnum)
	 (anthy-set-candidate ac anthy-direct-convert-wide-latin))

	((action_anthy_prev_page)
	 (if (anthy-context-candidate-window ac)
	     (im-shift-page-candidate ac #f)))

	((action_anthy_next_page)
	 (if (anthy-context-candidate-window ac)
	     (im-shift-page-candidate ac #t)))

	((action_anthy_commit)
	 (anthy-commit-converted! ac))

	((action_anthy_extend_segment)
	 (anthy-resize-segment ac 1))

	((action_anthy_shrink_segment)
	 (anthy-resize-segment ac -1))

	((action_anthy_next_segment)
	 (anthy-move-segment ac 1))

	((action_anthy_prev_segment)
	 (anthy-move-segment ac -1))

	((action_anthy_beginning_of_preedit)
	 (ustr-cursor-move-beginning! segments)
	 (anthy-reset-candidate-window ac))

	((action_anthy_end_of_preedit)
	 (ustr-cursor-move-end! segments)
	 (anthy-correct-segment-cursor segments)
	 (anthy-reset-candidate-window ac))

	((action_anthy_backspace)
	 (anthy-cancel-conv ac)
	 (ustr-cursor-delete-backside! preconv-ustr))

	((action_anthy_next_candidate)
	 (anthy-move-candidate ac 1))

	((action_anthy_prev_candidate)
	 (anthy-move-candidate ac -1))

	((action_anthy_cancel_conv)
	 (anthy-cancel-conv ac))))))

(define anthy-wide-latin-state-action
  (lambda (ac act-id)
    (case act-id
      ((action_anthy_on)
       (anthy-flush ac)
       (anthy-context-set-on! ac #t)
       (anthy-switch-kana-mode! ac (anthy-context-kana-mode ac))))))

(define anthy-key-handler
  (lambda (ac key key-state press?)
    (let* ((ev (legacy-key->key-event key key-state press?))
	   (keytrans-emc (anthy-context-keytrans-emc ac))
	   (act-seq (begin
		      (key-event-print-inspected "key-event:  " ev)
		      (key-event-translator-translate! keytrans-emc ev))))
      (if act-seq
	  (for-each (lambda (act-id)
		      (and (symbol? act-id)
			   (anthy-activate-action! ac act-id)))
		    act-seq))
      (if enable-modifier-translation?
	  (begin
	    (key-event-set-modifier! ev
				     (bitwise-or (key-event-modifier ev)
						 (anthy-context-mod-state ac)
						 (anthy-context-mod-lock ac)
						 (anthy-context-mod-stick ac)))
	    (if (modifier-match? mod_Shift
				 (key-event-modifier ev))
		(key-event-char-upcase! ev))))
      (key-event-print-inspected "translated: " ev)

      (anthy-input! ac ev)
      (if (not (event-consumed ev))
	  (im-commit-raw ac)))))

(define anthy-press-key-handler
  (lambda (ac key key-state)
    (anthy-key-handler ac key key-state #t)))

(define anthy-release-key-handler
  (lambda (ac key key-state)
    (anthy-key-handler ac key key-state #f)))

(define anthy-reset-handler
  (lambda (ac)
    (anthy-flush ac)
    ;; code to commit pending string must not be added to here.
    ;; -- YamaKen 2004-10-21
    ))

(define anthy-get-candidate-handler
  (lambda (ac idx accel-enum-hint)
    (let* ((ac-id (anthy-context-ac-id ac))
	   (cur-seg (ustr-cursor-pos (anthy-context-segments ac)))
	   (cand (anthy-get-nth-candidate ac cur-seg idx)))
      (list cand (digit->string (+ idx 1)) ""))))

(define anthy-set-candidate-index-handler
  (lambda (ac idx)
    (ustr-cursor-set-frontside! (anthy-context-segments ac) idx)
;    (anthy-move-segment ac 1)
    (anthy-update-preedit ac)))

(anthy-configure-widgets)

(register-im
 'anthy
 "ja"
 "EUC-JP"
 anthy-im-name-label
 anthy-im-short-desc
 #f
 anthy-init-handler
 anthy-release-handler
 context-mode-handler
 anthy-press-key-handler
 anthy-release-key-handler
 anthy-reset-handler
 anthy-get-candidate-handler
 anthy-set-candidate-index-handler
 context-prop-activate-handler
)
