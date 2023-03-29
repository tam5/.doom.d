;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                               User Interface                               ║
;; ╚════════════════════════════════════════════════════════════════════════════╝

;; This file controls the ui elements that make up the core look and feel of the
;; editor. If you're suffering from color-theme ADD, it's probably '+theme.el'
;; that you want to be modifying.

;; NOTE To enable transparent-titlebars with the emacs-mac port, you'll need to
;;      run 'defaults write org.gnu.Emacs TransparentTitleBar DARK' from the
;;      command line.

;; NOTE To hide the document icon from the titlebar, you'll need to run
;;      'defaults write org.gnu.Emacs HideDocumentIcon YES' from the
;;      command line.

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq
 ;; The default font to use
  doom-font (font-spec :family "MesloLGL Nerd Font" :size 15 :weight 'normal)
 ; doom-font (font-spec :family "Menlo" :size 14 :weight 'normal)

 ;; The font to use for other things, like the sidebar
 doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

 ;; Remove the extra nosie from the titlebar
 frame-title-format nil)

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                               Indent Guides                                │
;; └────────────────────────────────────────────────────────────────────────────┘

(load! "lisp/bitmaps")

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-auto-top-character-face-perc 20
        highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)

  ;; Disable for performance and cleanliness, togglable via 'SPC-t-i'
  (highlight-indent-guides-mode -1)
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                                  Flycheck                                  │
;; └────────────────────────────────────────────────────────────────────────────┘

(flycheck-define-error-level 'error
  :severity 100
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'my/fringe-bitmap-circle
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)
(flycheck-define-error-level 'warning
  :severity 10
  :compilation-level 1
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'my/fringe-bitmap-circle
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)
(flycheck-define-error-level 'info
  :severity -10
  :compilation-level 0
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'my/fringe-bitmap-circle
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-info)

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                                  Treemacs                                  │
;; └────────────────────────────────────────────────────────────────────────────┘

(after! treemacs
  (setq treemacs-collapse-dirs 0))

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                                Keybind Help                                │
;; └────────────────────────────────────────────────────────────────────────────┘

;; make sure to cache the svgs



;; (defun my-variable-watcher (symbol newval operation where)
;;   (message "Variable %s is now %S (operation: %s, buffer: %s)"
;;            symbol newval operation where))

;; (add-variable-watcher 'svg-lib-style-default #'my-variable-watcher)

(use-package! svg-lib
  :after marginalia
  :init (add-hook 'after-setting-font-hook (lambda () (setq svg-lib-style-default (svg-lib-style-compute-default))))
  :config (load! "lisp/keybinding-icons") ;; when it becomes its own package then we can remove this
  (advice-add 'marginalia-annotate-binding :around #'keybinding-icons-iconify-binding-a)
  (advice-add 'marginalia-annotate-command :override #'keybinding-icons-marginalia-annotate-command-a))

;; (add-hook 'doom-load-theme-hook #'my/refresh-keybinding-icons)

