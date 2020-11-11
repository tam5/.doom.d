;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;; /-------------------------------------------------------------------------------------------
;;; | Emacs Config
;;; |-------------------------------------------------------------------------------------------
;;; |
;;; | Welcome. My emacs config is built on top of doom emacs and most features are controlled
;;; | through the 'init.el' file found in this directory. Additional configuration that is
;;; | applied on top of doom is configured via this file. Please keep it neat and tidy!
;;; /

(load! "+utils")
(load! "+ui")
(load! "+behavior")
(load! "+completion")
(load! "+liveintent")
(load! "+keybindings")

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Ari Miller"
      user-mail-address "arimiller92@gmail.com")

(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'vue-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
