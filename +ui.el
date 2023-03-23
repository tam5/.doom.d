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

(setq
 ;; The default font to use
 doom-font (font-spec :family "MesloLGL Nerd Font" :size 15 :weight 'normal)

 ;; The font to use for other things, like the sidebar
 doom-variable-pitch-font (font-spec :family "Monaco" :size 13)

 ;; Remove the extra nosie from the titlebar
 frame-title-format nil)

(add-to-list 'default-frame-alist '(undecorated-round . t))

;; ┌────────────────────────────────────────────────────────────────────────────┐
;; │                               Indent Guides                                │
;; └────────────────────────────────────────────────────────────────────────────┘

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-auto-top-character-face-perc 20
        highlight-indent-guides-bitmap-function 'my/highlight-indent-guides--bitmap-dots)

  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

;; Disable for performance and cleanliness, togglable via 'SPC-t-i'
(highlight-indent-guides-mode -1)

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

(use-package! svg-lib)

;; (use-package! svg-lib
        ;; :config ())

;;


;; (key-description (kbd "SPC x"))  ; "8-x C-f"

;; "\\bC-\\([A-Za-z.,~/\\]+\\)" -> can match the C-

;; (defun aritest (text)
;;   (insert (propertize text
;;                       'face 'marginalia-key
;;                       'display (svg-lib-tag (key-description text)
;;                                             nil))))

;; (aritest "abc")




;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;;okay so we see that
;; (setq svg-lib-style-default (svg-lib-style-compute-default))
;; seems to fix things, so the question is what's the value of it at start up, and after we require, and so on

;; (defun r-text (text family size)
;;     (propertize text
;;         'face `(:family ,family
;;                 :height ,(* size 10)
;;                 :weight regular
;;                 :foreground "black"
;;                 :box '(:linewidth -1 :color "black" :style nil))))

;; (defun s-text (text family size &optional)
;;   (let* ((font       (font-info (format "%s-%d" family size)))
;;          (font-size  (elt font 2))
;;          (descent    (elt font 9))
;;          (ascent     (elt font 8))
;;          (svg-height (elt font 3))
;;          (char-width (elt font 11))
;;          (svg-width  (* char-width (length text)))
;;          (svg        (svg-create svg-width svg-height)))
;;     (svg-rectangle svg 0 0 svg-width svg-height
;;            :fill "white" :stroke "black")
;;     (svg-text svg text
;;               :fill "black"
;;               :stroke-width 0
;;               :kerning 0
;;               :letter-spacing 0
;;               :font-family family
;;               :font-size font-size
;;               :x 0
;;               :y ascent)
;;     (propertize text 'display (svg-image svg :ascent 'center))))

;; (defun t-text (text)
;;   ;; (let* ((font       (font-info (format "%s-%d" family size)))
;;   ;;        (font-size  (elt font 2))
;;   ;;        (descent    (elt font 9))
;;   ;;        (ascent     (elt font 8))
;;   ;;        (svg-height (elt font 3))
;;   ;;        (char-width (elt font 11))
;;   ;;        (svg-width  (* char-width (length text)))
;;   ;;        (svg        (svg-create svg-width svg-height)))
;;     (propertize text 'display (svg-lib-tag text nil)))

;; can turn the props into a function maybe to allow for theme switching?? or maybe just clear out cached tags?

(defun k-text (text)
  (propertize text 'display (svg-lib-tag text nil
                                         :background (doom-darken (face-attribute 'default :background) 0.4)
                                         :foreground (doom-darken (face-attribute 'default :foreground) 0)
                                         :margin 1
                                         :padding 2
                                         :radius 3
                                         :stroke 0)))

;; (runtest)

(defun runtest-1()
  (insert (k-text "SPC"))
  (insert (k-text "m"))
  (insert (k-text "e"))
  (insert (k-text "r")))

(defun runtest-2()
  (insert (k-text "SPC"))
  (insert (k-text "w"))
  (insert (k-text "C-s")))

(defun runtest ()
  (runtest-1)
  (insert "\n")
  (runtest-2))


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
