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

  ;; building blocks
  (defsubst +modeline/spacer (size)
    (let* ((face (if (doom-modeline--active)
                     'solaire-mode-line-face
                   'mode-line-inactive))
           (fg (face-background face nil t)))
      (propertize (s-repeat size "‡") 'face `(:foreground ,fg))))

(defun +modeline/update-vcs-icon (&rest _)
    "See `doom-modeline-update-vcs-icon'.
Our override is to adjust the icons and faces."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added needs-merge conflict))
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
                 (state (vc-state buffer-file-name backend))
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

  ;; segments
  (doom-modeline-def-segment +modeline/spacer-small
    (+modeline/spacer 2))

  (doom-modeline-def-segment +modeline/spacer
    (+modeline/spacer 8))

  (doom-modeline-def-segment +modeline/vcs
    (let ((meta (doom-modeline-segment--vcs)))
      (message "my meta %s" meta)
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
  '(misc-info battery github debug repl lsp process checker +modeline/vcs major-mode +modeline/spacer-small))

  (defun +modeline/set-modeline ()
    (doom-modeline-set-modeline '+modeline/modeline 'default))
  (add-hook 'doom-modeline-mode-hook '+modeline/set-modeline))

;; (setq doom-modeline-buffer-modification-icon t)
