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

(setq default-directory "~/")

(load! "core/+utils")
(load! "core/+ui")
(load! "core/+modeline")
(load! "core/+icons")
(load! "core/+behavior")
(load! "core/+completion")
(load! "core/+liveintent")
(load! "core/+keybindings")

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS"))

(add-hook 'web-mode-hook '+js/maybe-enable-prettier)
(add-hook 'typescript-mode-hook '+js/maybe-enable-prettier)
(add-hook 'vue-mode-hook '+js/maybe-enable-prettier)
(add-hook 'js2-mode-hook '+js/maybe-enable-prettier)

(setq lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE"))

(after! lsp-mode
  (setq lsp-ui-doc-border nil))
