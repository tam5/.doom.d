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

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 0))

;; (defun my-advice-key-description (orig-fun &rest args)
;;   "Advice function that modifies the result of `key-description'.
;; Replaces any occurrence of \"SPC\" in the output with \"space\"."
;;   (let ((desc (apply orig-fun args)))
;;     (replace-regexp-in-string "SPC" "space" desc)))

;; (advice-add 'key-description :around #'my-advice-key-description)

;; (key-description (kbd "SPC x"))  ; "8-x C-f"


;; (setq svg-tag-tags
;;       '((":TODO:" . ((svg-tag-make "TODO" :face 'org-tag
;;                                    :radius 0 :inverse t :margin 0)))
;;         (":NOTE:" . ((svg-tag-make "NOTE" :face 'font-lock-comment-face
;;                                    :inverse nil :margin 0 :radius 0)))
;;         ("\([0-9a-zA-Z]\)" . ((lambda (tag)
;;                                 (svg-tag-make tag :beg 1 :end -1 :radius 12))))
;;         ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
;;                                            (svg-tag-make tag :beg 1 :end -1 :radius 8))))
;;         ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
;;                                   (svg-tag-make tag :face 'font-lock-comment-face
;;                                                 :margin 0 :beg 1 :end -1))))))



;; (require 'svg-lib)
;; (require 'svg-tag-mode)

;; (setq svg-tag-tags
;;       '(("\\bC-\\([A-Za-z.,~/\\]+\\)" .
;;          ((lambda (tag) (svg-lib-tag tag nil
;;                                      :radius 4
;;                                      :padding 1
;;                                      :margin 1
;;                                      :stroke 0
;;                                      :background "#673AB7"
;;                                      :foreground "white"
;;                                      :crop-right t))))
;;         (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" .
;;          ((lambda (tag) (svg-tag-make tag :beg 1
;;                                       :end -1
;;                                       :margin 0
;;                                       :crop-left t))))))

;; (defun my-start-svg-thing ()
;;   ""
;;   (svg-tag-mode))

;; (add-hook 'minibuffer-with-setup-hook #'my-start-svg-thing)

;; "\\bC-\\([A-Za-z.,~/\\]+\\)" -> can match the C-

;; (C-\) -> [^ ~]
;; (SPC f h e) [space] [f] [h] [e]
;; (SPC f e) [space] [f] [e]
;; (M-x) [⌥ x]
;; (C-x C-e) [⌥ x] [control e]

;; (set-face-background 'marginalia-key nil)

;; (defun marginalia-annotate-binding (cand)
;;   "Annotate command CAND with keybinding."
;;   (when-let ((sym (intern-soft cand))
;;              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
;;     (format #(" (%s)" 1 5 (face marginalia-key)) (key-description key))))

;; (cookies)

;; (defun cookies ()
;; (insert-image (svg-lib-tag "⌘" nil
;; :radius 4
;; :padding 1
;; :margin 1
;; :stroke 0
;; :background "#673AB7"
;; :foreground "white"
;; ))
;; (insert-image (svg-lib-tag "F" nil
;; :radius 4
;; :padding 1
;; :margin 0
;; :stroke 0
;; :background "#673AB7"
;; :foreground "white"
;; ))
;; )

;; (cookies)
; (defface aritest-face-1
;   '((t (:inherit marginalia-key :background "#cccccc" :foreground "#000000")))
;   "documents"
;   :group 'basic-faces)

; (defface aritest-face-2
;   '((t (:inherit marginalia-key :background "#cccccc" :foreground "#000000")))
;   "documents"
;   :group 'basic-faces)

; (defface aritest-face-3
;   '((t (:inherit marginalia-key :background "#cccccc" :foreground "#000000")))
;   "documents"
;   :group 'basic-faces)

; (defun aritest-insert-bbb ()
;   (interactive)
;   (insert (concat
;            (propertize " " 'face 'aritest-face-3)
;            (propertize "⌘" 'face 'aritest-face-1)
;            (propertize " " 'face 'aritest-face-3)
;            (propertize "F" 'face 'aritest-face-2)
;            (propertize " " 'face 'aritest-face-3)
;            "")))

; ;; (set-face-attribute
; ;;  'aritest-face-1 nil
; ;;  :box '(:line-width (10 . 5) :color "#00ff00" :style 'pressed-button))

; ;; (require 'svg-tag-mode)

; ;; :TODO:

; (dotimes (i 10)
;   (insert-image (svg-lib-tag "TODO" nil :stroke 2 :radius i)))


;; (setq svg-tag-tags
;;       '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO"))))
;;         ("(SPC h F)" . ((lambda (tag) (svg-tag-make "⌘ F"))))
;;         ))



;; (SPC h F)


;; (set-face-attribute 'aritest-face-2 nil :background nil)
;; (set-face-attribute 'aritest-face-3 nil :background nil)

 ;; ⌘ F

;; bbb
 ;; ⌘ F
 ;; ⌘ F

; make own face, add it and test it and compare with svg

;; (key-description (kbd "C-x C-f"))
;; ⌘ F
;; ⌥

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
