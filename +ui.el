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

(load! "lisp/bitmaps")
(load! "lisp/keybinding-icons")

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq
 ;; The default font to use
 doom-font (font-spec :family "MesloLGL Nerd Font" :size 15 :weight 'normal)

 ;; The font to use for other things, like the sidebar
 doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

 ;; Remove the extra nosie from the titlebar
 frame-title-format nil)

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                               Indent Guides                                │
;; └────────────────────────────────────────────────────────────────────────────┘

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

;; (use-package! svg-lib
        ;; :config ())

;;

;; (defcustom my/key-icon-alist
;;   '(("SPC" . "space")
;;     ("s" . "⌘")
;;     ("C" . "control"))
;;   "My list"
;;   :type '(alist :key-type string :value-type string)
;;   :group 'aritest-group)

(defvar my/keybinding-icon-scale-factor 0.7)
(advice-add 'marginalia-annotate-binding :around #'my/iconify-keybinding-hints-a)
(advice-add 'marginalia-annotate-command :override #'my/marginalia-annotate-command)

;; (after! svg-lib
;;   ;; sometimes this is needed, not sure why
;;   (setq svg-lib-style-default (svg-lib-style-compute-default)))


;; (key-description (kbd "SPC x"))  ; "8-x C-f"

;; "\\bC-\\([A-Za-z.,~/\\]+\\)" -> can match the C-

;; (C-\) -> [^ ~]
;; (SPC f h e) [space] [f] [h] [e]
;; (SPC f e) [space] [f] [e]
;; (M-x) [⌥ x]
;; (C-x C-e) [⌥ x] [control e]

;; ⌘ is the Command or Cmd () key
;; ⌃ is the Control or Ctrl key
;; ⌥ is the Option or Alt key
;; ⇧ is the Shift key
;; ⇪ is the Caps Lock key
;; fn is the Function key
;; ⌘ is command (or Cmd. Like the Control key on Windows/PC)
;; ⌥ is option (like Alt on Windows/PC)
;; ⌃ is control (Control-click=Right-click)
;; ⇧ is shift
;; ⇪ is caps lock
;; ← is left arrow
;; → is right arrow
;; ↑ is up arrow
;; ↓ is down arrow
;; ⇥ is tab
;; ⇤ is backtab
;; ↩ is return (or Enter)
;; ⌫ is delete (like Backspace on Windows/PC)
;; ⌦ is forward delete (fn+Delete. Also called Forward Delete)
;; ⇞ is page up (fn+Up Arrow on compact keyboards)
;; ⇟ is page down (fn+Down Arrow on compact keyboards)
;; ↖ is home (fn+Left Arrow on compact keyboards)
;; ↘ is end (fn+Right Arrow on compact keyboards)
;; ⎋ is escape (or esc)
;; ⏏ is eject
