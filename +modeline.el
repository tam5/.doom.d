;;; $DOOMDIR/+modeline.el -*- lexical-binding: t; -*-
;;;
;;; /----------------------------------------------------------------------------------
;;; | Modeline
;;; |----------------------------------------------------------------------------------
;;; |
;;; | Most of the mode-line (or modeline, never sure which) is simply taken from
;;; | the doom modeline. However some personal adjustments to make things a
;;; | bit more minimalistic and feel a bit better with all my editor OCD.
;;; /


(after! doom-modeline

  (setq doom-modeline-height 40
        doom-modeline-buffer-encoding nil)

  ;; overrides
  (defun +modeline/update-vcs-icon (&rest _)
    "See `doom-modeline-update-vcs-icon'.
Our override is to adjust the icons and faces."
    (setq doom-modeline--vcs-icon
          (when (and vc-mode buffer-file-name)
            (let* ((backend (vc-backend buffer-file-name))
                   (state   (vc-state buffer-file-name backend)))
              (cond ((memq state '(needs-merge conflict))
                     (doom-modeline-icon 'material "compare_arrows" "" "@" :face 'mode-line :v-adjust -0.2 :height 0.8))
                    ((eq state 'needs-update)
                     (doom-modeline-vcs-icon "arrow-down" "↓" "!" 'mode-line))
                    ((memq state '(removed unregistered))
                     (doom-modeline-vcs-icon "alert" "⚠" "!" 'mode-line))
                    (t
                     (doom-modeline-icon 'octicon "git-branch" "" "@" :face 'mode-line :v-adjust -0.05 :height 0.7)))))))
  (advice-add 'doom-modeline-update-vcs-icon :around #'+modeline/update-vcs-icon)

  (defun +modeline/update-vcs-text (&rest _)
    "See `doom-modeline-update-vcs-text'.
Our override is to adjust the faces."
    (setq doom-modeline--vcs-text
          (when (and vc-mode buffer-file-name)
            (let* ((backend (vc-backend buffer-file-name))
                   (str (if vc-display-status
                            (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                          "")))
              (propertize (if (> (length str) doom-modeline-vcs-max-length)
                              (concat
                               (substring str 0 (- doom-modeline-vcs-max-length 3))
                               "...")
                            str)
                          'mouse-face 'mode-line-highlight
                          'face 'mode-line)))))
  (advice-add 'doom-modeline-update-vcs-text :around #'+modeline/update-vcs-text)

  (defun +modeline/update-buffer-file-state-icon (&rest _)
    "See `doom-modeline-update-buffer-file-state-icon'.
Our override is to adjust the icons and faces."
    (setq doom-modeline--buffer-file-state-icon
          (when doom-modeline-buffer-state-icon
            (ignore-errors
              (concat
               (cond (buffer-read-only
                     (doom-modeline-icon 'faicon "lock" "🔒" "%1*" :face 'mode-line :v-adjust 0 :height 0.8))
                     ((and buffer-file-name (buffer-modified-p)
                           doom-modeline-buffer-modification-icon)
                     (doom-modeline-icon 'faicon "circle" "💾" "%1*" :face 'all-the-icons-lblue :v-adjust 0.20 :height 0.5))
                     ((and buffer-file-name
                           (not (file-exists-p buffer-file-name)))
                      (doom-modeline-buffer-file-state-icon
                       "do_not_disturb_alt" "🚫" "!" 'doom-modeline-urgent))
                     (t (doom-modeline-icon 'faicon "circle" "💾" "%1*" :face 'mode-line :v-adjust 0.20 :height 0.5)))
               (when (or (buffer-narrowed-p)
                         (and (bound-and-true-p fancy-narrow-mode)
                              (fancy-narrow-active-p))
                         (bound-and-true-p dired-narrow-mode))
                 (doom-modeline-buffer-file-state-icon
                  "vertical_align_center" "↕" "><" 'doom-modeline-warning)))))))
  (advice-add 'doom-modeline-update-buffer-file-state-icon :around #'+modeline/update-buffer-file-state-icon)

  (defun +modeline/update-flycheck-icon (_ &optional status)
    "Update flycheck icon via STATUS."
    (setq doom-modeline--flycheck-icon
          (when-let
              ((icon
                (pcase status
                  ('finished  (if flycheck-current-errors
                                  (let-alist (doom-modeline--flycheck-count-errors)
                                    (doom-modeline-checker-icon
                                     (cond ((> .error 0) "error_outline")
                                           ((> .warning 0) "error_outline")
                                           (t "info_outline"))
                                     "🚫" "!"
                                     (cond ((> .error 0) 'doom-modeline-urgent)
                                           ((> .warning 0) 'doom-modeline-warning)
                                           (t 'doom-modeline-info))))
                                (doom-modeline-checker-icon "check" "✓" "-" 'success)))
                  ('running     (doom-modeline-checker-icon "access_time" "⏱" "*" 'doom-modeline-debug))
                  ('no-checker  (doom-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-modeline-debug))
                  ('errored     (doom-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-modeline-urgent))
                  ('interrupted (doom-modeline-checker-icon "pause" "⏸" "=" 'doom-modeline-debug))
                  ('suspicious  (doom-modeline-checker-icon "priority_high" "❗" "!" 'doom-modeline-urgent))
                  (_ nil))))
            (propertize icon
                        'help-echo (concat "Flycheck\n"
                                           (pcase status
                                             ('finished "mouse-1: Display minor mode menu
mouse-2: Show help for minor mode")
                                             ('running "Running...")
                                             ('no-checker "No Checker")
                                             ('errored "Error")
                                             ('interrupted "Interrupted")
                                             ('suspicious "Suspicious")))
                        'mouse-face 'mode-line-highlight
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line down-mouse-1]
                                       flycheck-mode-menu-map)
                                     (define-key map [mode-line mouse-2]
                                       (lambda ()
                                         (interactive)
                                         (describe-function 'flycheck-mode)))
                                     map)))))

  (advice-add 'doom-modeline-update-flycheck-icon :around #'+modeline/update-flycheck-icon)


  ;; building blocks
  (defsubst +modeline/spacer (size)
    (let* ((face (if (doom-modeline--active)
                     'solaire-mode-line-face
                   'mode-line-inactive))
           (fg (face-background face nil t)))
      (propertize (s-repeat size "‡") 'face `(:foreground ,fg))))

  ;; segments
  (doom-modeline-def-segment +modeline/spacer-small
    (+modeline/spacer 2))

  (doom-modeline-def-segment +modeline/spacer
    (+modeline/spacer 8))

  (doom-modeline-def-segment +modeline/vcs
    (let ((meta (doom-modeline-segment--vcs)))
      (concat meta (when meta (doom-modeline-segment--+modeline/spacer)))))

  (doom-modeline-def-segment +modeline/checker
    (let ((meta (doom-modeline-segment--checker)))
      (concat meta (when meta (doom-modeline-segment--+modeline/spacer)))))

  (doom-modeline-def-segment +modeline/matches
    "See segment: `matches'.
Our override is just to remove the buffer size information cuz we don't really need it."
    (concat (doom-modeline--anzu)
            (doom-modeline--phi-search)
            (doom-modeline--evil-substitute)
            (doom-modeline--iedit)
            (doom-modeline--symbol-overlay)
            (doom-modeline--multiple-cursors)))

  ;; the modeline
  (doom-modeline-def-modeline '+modeline/modeline
    '(bar +modeline/spacer-small buffer-info +modeline/spacer buffer-position remote-host selection-info +modeline/matches)
    '(misc-info battery github debug repl +modeline/checker lsp process +modeline/vcs major-mode +modeline/spacer))

  (defun +modeline/set-modeline ()
    (doom-modeline-set-modeline '+modeline/modeline 'default))
  (add-hook 'doom-modeline-mode-hook '+modeline/set-modeline))

;; (setq doom-modeline-buffer-modification-icon t)
