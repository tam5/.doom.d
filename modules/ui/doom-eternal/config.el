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

;; Remove arrow icons in the fringe
(setq-default fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(after! vertico-posframe
  (when (modulep! +command-palette)
    (doom-eternal-command-palette-mode +1)))

;; (add-hook 'window-setup-hook #'+doom-eternal/initial-frame-setup)
;; (add-hook 'after-make-frame-functions #'+doom-eternal/frame-setup)

;; Done in a hook to ensure loading as late as possible
(add-hook! 'doom-after-modules-config-hook
  ;; (if (fboundp 'fringe-mode) (fringe-mode '(8 . 0)))

  (after! git-gutter-fringe
    (define-fringe-bitmap 'git-gutter-fr:added doom-eternal/bitmap--vertical-bar-left nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified doom-eternal/bitmap--diagonal-lines nil nil '(top repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted doom-eternal/bitmap--triangle-lower-left nil nil 'bottom))

  (after! flycheck
    (define-fringe-bitmap '+doom-eternal/flycheck-fringe-bitmap doom-eternal/bitmap--circle-medium nil nil 'center)
    (flycheck-redefine-standard-error-levels "⦁" '+doom-eternal/flycheck-fringe-bitmap)
    (setq flycheck-indication-mode 'left-margin)

    (setq-default left-margin-width 0
                  right-margin-width 0)))
