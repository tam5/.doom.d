;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-
;;;
;;; /-----------------------------------------------------------------------------------------
;;; | Keybindings
;;; |-----------------------------------------------------------------------------------------
;;; |
;;; | The heart and sole of our editor is, of course, the keybindings. Doom itself provides
;;; | most of the keybindings, but we have some extra to add. Additionally, a few things
;;; | here are just habits that have stuck with me even if they don't make any sense.
;;; /

(defconst +keybindings/prefix ","
  "Define the prefix key. This is the equivalent of the <leader> key in vim.")

;;
;; Prefix based
;;
(map!
 :prefix +keybindings/prefix
 :n "ev" (lambda () (interactive) (find-file (file-truename "~/.doom.d/config.el")))
 :n "ek" (lambda () (interactive) (find-file (file-truename "~/.doom.d/+keybindings.el")))
 :n "es" (lambda () (interactive) (find-file (file-truename "~/.dotfiles/aliases")))
 :n "1" #'+treemacs/toggle
 :n "2" #'+treemacs/find-file
 :n "a" #'lsp-execute-code-action
 :n "j" #'counsel-projectile-switch-project
 :n "f" #'evil-avy-goto-word-0
 :n "v" #'evil-window-vsplit
 :n "h" #'evil-window-split
 :n "p" #'+popup/raise
 :n "d" #'kill-this-buffer
 :n "w" #'save-buffer
 :n "q" #'evil-quit
 :n "s" #'+default/search-project
 :v "s" #'+utils/sort-list
 :nv (concat +keybindings/prefix "d") #'+doom-dashboard/open
 :nv (concat +keybindings/prefix "j") #'json-pretty-print
 :n (concat +keybindings/prefix "b") #'eval-buffer
 :n (concat +keybindings/prefix "s") #'ivy-resume)

;;
;; Non-Prefix based
;;
(map!
 :i "M-`" #'+utils/insert-backtic
 :n "/" #'swiper-isearch
 :n "-" #'dired-jump
 :n "C-p" #'+ivy/projectile-find-file
 :n "gr" #'lsp-find-references
 :ni "C-y" #'emmet-expand-yas
 :nvi "C-a" #'evil-numbers/inc-at-pt
 :nvi "C-s" #'evil-numbers/dec-at-pt
 :nv "M-<down>" #'evil-mc-make-cursor-move-next-line
 :nv "M-<up>" #'evil-mc-make-cursor-move-prev-line
 :nv "s-r" #'imenu
 :nvi "S-s-<return>" #'doom/window-maximize-buffer
 :nv "C-k" #'move-text-up
 :nv "C-j" #'move-text-down)

;;
;; Mode specific
;;

(map! (:when (featurep! :completion company)
       (:after company
        (:map company-active-map
              "TAB"     #'+completion/expand-snippet-or-complete-selection
              [tab]     #'+completion/expand-snippet-or-complete-selection))))

(map! :mode treemacs-mode
      :prefix +keybindings/prefix
      :n "td" #'treemacs-remove-project-from-workspace)

(map! :mode restclient-mode
      :prefix +keybindings/prefix
      :n "r" #'restclient-http-send-current)

(map! :mode php-mode
      :prefix +keybindings/prefix
      :n "tt" #'phpunit-current-test
      :n "tc" #'phpunit-current-class
      :n "ta" #'phpunit-current-project)

(map! :mode evil-markdown-mode
      :nvi "s-b" #'markdown-insert-bold
      :nvi "s-i" #'markdown-insert-italic
      :i "M-`" #'+utils/insert-backtic)
