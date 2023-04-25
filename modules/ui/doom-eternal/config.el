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

(defvar doom-eternal/command-palette-frame-parameter-overrides-alist
  '((undecorated . nil)
    (undecorated-round . t)
    (ns-appearance . 'light)))

;; ;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;; ;; (add-to-list 'default-frame-alist '(internal-border-width . 0))

;; ;; for some reason this has a different affect after the frame is already created, but it makes things more minimal so that's what we want
;; (add-hook 'window-setup-hook #'(lambda () (set-frame-parameter (selected-frame) 'ns-appearance 'light)))

(after! vertico-posframe
  (setq
   vertico-posframe-border-width 0
   vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset))

(add-hook 'minibuffer-setup-hook #'+doom-eternal/setup-minibuffer)
