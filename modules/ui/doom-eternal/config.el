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

(defcustom doom-eternal/command-palette-offset-top 60
  "The offset in pixels from the top of the frame to place the command palette."
  :type 'integer
  :group 'doom-eternal)

(defvar doom-eternal/frame-parameter-overrides-alist
  '((ns-appearance . 'light)
    (internal-border-width . 0))
  "Alist of params to override when creating new frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits.")

(defvar doom-eternal/command-palette-frame-parameter-overrides-alist
  '((undecorated . nil)
    (undecorated-round . t)
    (ns-appearance . 'light))
  "Alist of params to override when creating new command palette frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits.")

(defface doom-eternal/command-palette-face
  '((t (:inherit t)))
  ""
  :group 'doom-eternal)

(set-face-attribute 'doom-eternal/command-palette-face nil :background "orange")

(after! vertico-posframe
  (setq
   vertico-posframe-border-width 0
   vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset))

(add-hook 'minibuffer-setup-hook #'+doom-eternal/minibuffer-setup)
(add-hook 'window-setup-hook #'+doom-eternal/initial-frame-setup)
(add-hook 'after-make-frame-functions #'+doom-eternal/frame-setup)
