;;; ui/doom-eternal/config.el -*- lexical-binding: t; -*-

(defgroup doom-eternal nil
  "Doom Eternal: When plain old Doom Emacs just isn't enough.

Welcome to the realm of Doom Eternal, where your Emacs experience
ascends to new heights of power, beauty, and crispiness. We take
the already awesome Doom Emacs and crank it up to 11, leaving you
wondering how you ever managed without this sublime extension. With
Doom Eternal, 'Emacs as an editor' is a thing of the past. Prepare
to enter a world where Emacs becomes a way of life. Fasten your seatbelts,
and enjoy the ride!"
  :group 'doom)

(defcustom doom-eternal/frame-parameter-overrides-alist
  '((ns-appearance . 'light)
    (internal-border-width . 0))
  "Alist of params to override when creating new frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-eternal)

(defcustom doom-eternal/command-palette-frame-parameter-overrides-alist
  '((undecorated . nil)
    (undecorated-round . t)
    (left-fringe . 0)
    (right-fringe . 0)
    ;; (header-line-format . '"asdf")
    ;; (mode-line-format . '"asdf")
    (internal-border-width . 0)
    (border-width . 0)
    (ns-appearance . 'light))
  "Alist of params to override when creating new command palette frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-eternal)

(defcustom doom-eternal/command-palette-offset-top-pixels 60
  "The offset in pixels from the top of the frame to place the command palette."
  :type 'integer
  :group 'doom-eternal)

(defface doom-eternal/command-palette-face
  '((t (:inherit t)))
  "Face for the command palette background."
  :group 'doom-eternal)

(add-hook 'window-setup-hook #'+doom-eternal/initial-frame-setup)
(add-hook 'after-make-frame-functions #'+doom-eternal/frame-setup)
(add-hook 'minibuffer-setup-hook #'+doom-eternal/command-palette-setup)

(after! vertico-posframe
  (setq vertico-posframe-border-width 0
        vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset))

;; Done in a hook to ensure loading as late as possible
(add-hook! 'doom-after-modules-config-hook
  (if (fboundp 'fringe-mode) (fringe-mode '(8 . 0)))

  (after! git-gutter-fringe
    (define-fringe-bitmap 'git-gutter-fr:added doom-eternal/bitmap--vertical-bar-left nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified doom-eternal/bitmap--diagonal-lines nil nil '(top repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted doom-eternal/bitmap--triangle-lower-left nil nil 'bottom))

  (after! flycheck
    (define-fringe-bitmap '+doom-eternal/flycheck-fringe-bitmap doom-eternal/bitmap--circle-medium nil nil 'center)
    (flycheck-redefine-standard-error-levels "●" '+doom-eternal/flycheck-fringe-bitmap)

    (setq flycheck-indication-mode 'left-margin)

    (setq-default left-margin-width 1
                  right-margin-width 0)))

;; (defun add-special-character-overlay ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (let ((ov (make-overlay (point-at-bol) (point-at-bol))))
;;         (overlay-put ov 'before-string "➤")
;;         (overlay-put ov 'special-character t))
;;       (forward-line 1))))

;; (defun remove-special-character-overlays ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (dolist (ov (overlays-at (point-at-bol)))
;;         (when (overlay-get ov 'special-character)
;;           (delete-overlay ov)))
;;       (forward-line 1))))

;; (set-face-attribute 'line-number nil :background "red" :box '(:line-width -5 :color "blue") :underline '(:color "green"))

;; (set-face-attribute 'vertico-posframe-border nil :background "red")

;; (defun my-change-minibuffer-prompt (orig-fun &rest args)
;;   (let ((prompt (car args)))
;;     (apply orig-fun
;;            (if (string-prefix-p "Enter" "Enter")
;;                (cons (concat "fookie ") (cdr args))
;;              args))))

;; (advice-add 'read-from-minibuffer :around #'my-change-minibuffer-prompt)
