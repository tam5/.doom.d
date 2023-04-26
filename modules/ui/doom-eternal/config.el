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
    ;; (undecorated-round . t)
    (left-fringe . 20)
    (right-fringe . 50)
    (header-line-format . '"asdf")
    (mode-line-format . '"asdf")
    (internal-border-width . 5)
    (border-width . 10)
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
        vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset)

;; Done in a hook to ensure loading as late as possible
(add-hook! 'doom-after-modules-config-hook
  (after! git-gutter-fringe
    (define-fringe-bitmap 'git-gutter-fr:added doom-eternal/bitmap--vertical-bar-left nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified doom-eternal/bitmap--diagonal-lines nil nil '(top repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted doom-eternal/bitmap--triangle-lower-left nil nil 'bottom)))

; (set-face-attribute 'minibuffer-prompt nil :background "red")

;; (flycheck-define-error-level 'error
;;   :severity 100
;;   :compilation-level 2
;;   :overlay-category 'flycheck-error-overlay
;;   :fringe-bitmap 'my/fringe-bitmap-circle
;;   :fringe-face 'flycheck-fringe-error
;;   :error-list-face 'flycheck-error-list-error)
;; (flycheck-define-error-level 'warning
;;   :severity 10
;;   :compilation-level 1
;;   :overlay-category 'flycheck-warning-overlay
;;   :fringe-bitmap 'my/fringe-bitmap-circle
;;   :fringe-face 'flycheck-fringe-warning
;;   :error-list-face 'flycheck-error-list-warning)
;; (flycheck-define-error-level 'info
;;   :severity -10
;;   :compilation-level 0
;;   :overlay-category 'flycheck-info-overlay
;;   :fringe-bitmap 'my/fringe-bitmap-circle
;;   :fringe-face 'flycheck-fringe-info
;;   :error-list-face 'flycheck-error-list-info)

; (set-face-attribute 'vertico-posframe-border nil :background (face-attribute 'vertical-border :background))

;; (setq flycheck-indication-mode 'left-fringe)

;; (add-to-list 'default-frame-alist '(right-fringe . 20))

;; (set-frame-parameter (selected-frame) 'left-fringe 20)
;; (set-frame-parameter (selected-frame) 'internal-border-width 0)

;; (set-face-attribute 'fringe nil :background nil)

;; (define-fringe-bitmap 'git-gutter-fr:added [224]
;;   nil nil '(center repeated))

        ;; (define-fringe-bitmap 'git-gutter-fr:added [#b1110000000000000]
        ;;   nil 16 '(center repeated))

;; (defun my-change-minibuffer-prompt (orig-fun &rest args)
;;   (let ((prompt (car args)))
;;     (apply orig-fun
;;            (if (string-prefix-p "Enter" "Enter")
;;                (cons (concat "fookie ") (cdr args))
;;              args))))

;; (advice-add 'read-from-minibuffer :around #'my-change-minibuffer-prompt)