;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                                Some Text                                   ║
;; ╚════════════════════════════════════════════════════════════════════════════╝
;;
;; This file controls the ui elements that make up the core look and feel of the
;; editor. If you're suffering from colortheme ADD, it's probably '+theme.el'
;; that you want to be modifying.

;; NOTE To enable transparent-titlebars with the emacs-mac port, you'll need to
;;      run 'defaults write org.gnu.Emacs TransparentTitleBar DARK' from the
;;      command line.

;; #
;; # defaults write org.gnu.Emacs HideDocumentIcon YES

(load! "lisp/bitmaps")

;; hint: M-x doom/reload-font to refresh font settings
(setq doom-font (font-spec :family "MesloLGL Nerd Font" :size 15 :weight 'normal)
      ; used in the sidebar
      doom-variable-pitch-font (font-spec :family "Monaco" :size 13)


      frame-title-format nil)


;; turned off for performance reasons but can be toggle with XXX
(setq display-line-numbers-type nil)

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-auto-top-character-face-perc 20
        highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)
  ;; disabled for performance and cleanliness, but togglable via SPC-t-i
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))
