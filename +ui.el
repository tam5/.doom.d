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


(setq
 ;; The default font to use
  doom-font (font-spec :family "MesloLGL Nerd Font" :size 15 :weight 'normal)

 ;; The font to use for other things, like the sidebar
 doom-variable-pitch-font (font-spec :family "Monaco" :size 13))

;; (set-face-attribute 'default nil :background "red")
;; (set-face-attribute 'solaire-default-face nil :background "green")

(setq frame-title-format nil)

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

;; (add-hook 'doom-load-theme-hook #'my/refresh-keybinding-icons)

