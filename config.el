;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                                                                            ║
;; ║                                Doom Config                                 ║
;; ║                                                                            ║
;; ╚════════════════════════════════════════════════════════════════════════════╝

(setq user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS")
      lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE"))

(setq confirm-kill-emacs nil)

(load! "+keybinds")
(load! "+ui")
(load! "+theme")

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 0))
