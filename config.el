;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                                                                            ║
;; ║                                Doom Config                                 ║
;; ║                                                                            ║
;; ╚════════════════════════════════════════════════════════════════════════════╝

(setq user-full-name (getenv "MY_FULL_NAME")
      user-mail-address (getenv "MY_EMAIL_ADDRESS")
      lsp-intelephense-licence-key (getenv "LICENSE_KEY_INTELEPHENSE"))

(setq confirm-kill-emacs nil
      dired-kill-when-opening-new-dired-buffer t)

(load! "+keybinds")
(load! "+ui")
(load! "+theme")
(load! "+liveintent")

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 0))

(after! (persp-mode recentf)
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (file-truename persp-save-dir)))))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(after! orderless
  (defun +vertico-orderless-dispatch-a  (orig-fun &rest args)
    "Use flex matching by default. There's probably a more straightforward
way you're supposed to do this, but wtvr."
    (or (apply orig-fun args)
        (cons 'orderless-flex (car args))))

  (advice-add '+vertico-orderless-dispatch :around #'+vertico-orderless-dispatch-a))
