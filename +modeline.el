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

;;   (defun +modeline/update-vcs-icon (&rest _)
;;     "See `doom-modeline-update-vcs-icon'.
;; Our override is to adjust the icons and faces."
;;     (setq doom-modeline--vcs-icon
;;           (when (and vc-mode buffer-file-name)
;;             (let* ((backend (vc-backend buffer-file-name))
;;                    (state   (vc-state buffer-file-name backend)))
;;               (cond ((memq state '(edited added))
;;                      (propertize "⇆" 'face 'doom-modeline-info))
;;                     ((eq state 'needs-merge)
;;                      (propertize "⇆" 'face 'doom-modeline-info))
;;                     ((eq state 'needs-update)
;;                      (propertize "↓" 'face 'doom-modeline-info))
;;                     ((memq state '(removed conflict unregistered))
;;                      (propertize "⚠" 'face 'doom-modeline-info))
;;                     (t
;;                    (all-the-icons-insert-octicon "git-branch")))))))
(defun +modeline/update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
                   (doom-modeline-vcs-icon "git-compare" "⇆" "*" 'doom-modeline-info -0.05))
                  ((eq state 'needs-merge)
                   (doom-modeline-vcs-icon "git-merge" "⛙" "?" 'doom-modeline-info))
                  ((eq state 'needs-update)
                   (doom-modeline-vcs-icon "arrow-down" "↓" "!" 'doom-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (doom-modeline-vcs-icon "alert" "⚠" "!" 'doom-modeline-urgent))
                  (t
                   (doom-modeline-icon 'octicon "git-branch" "" "@" :face 'doom-modeline-info :v-adjust -0.05 :height 0.5)))))))
  (advice-add 'doom-modeline-update-vcs-icon :around #'+modeline/update-vcs-icon)

  ;; segments
  (doom-modeline-def-segment +modeline/spacer-small
    (+modeline/spacer 2))

  (doom-modeline-def-segment +modeline/spacer
    (+modeline/spacer 8))

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
  '(bar buffer-info +modeline/spacer buffer-position remote-host selection-info +modeline/matches)
  '(misc-info battery github debug repl lsp process checker vcs major-mode))

  (defun +modeline/set-modeline ()
    (doom-modeline-set-modeline '+modeline/modeline 'default))
  (add-hook 'doom-modeline-mode-hook '+modeline/set-modeline))

;; (setq doom-modeline-buffer-modification-icon t)
