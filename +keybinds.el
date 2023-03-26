;;; $DOOMDIR/+keybinds.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                                  Keybinds                                  ║
;; ╚════════════════════════════════════════════════════════════════════════════╝
;;
;; Doom has a great keybinding system, but there's certain habits i've picked up
;; over the years that i just can't shake

(defconst my/leader-key ","
  "Define the leader/prefix key, aka <Leader> in vim. This is
separate from the doom-leader-key, so we can get even quicker access
to our most frequently used keybinds.")

;; bread and butter
(map!
 :n "C-p" #'projectile-find-file
 :g "C-s-p" #'execute-extended-command

 :n "-" #'dired-jump

 :prefix my/leader-key
 :n "w" #'basic-save-buffer
 :n "q" #'evil-quit
 :n "d" #'kill-current-buffer

 :n "v" #'evil-window-vsplit
 :n "h" #'evil-window-split

 :n "1" #'+treemacs/toggle

 :n "ev" (lambda () (interactive) (find-file (concat doom-user-dir "config.el")))
 :n "ek" (lambda () (interactive) (find-file (concat doom-user-dir "+keybinds.el")))
 :n "eu" (lambda () (interactive) (find-file (concat doom-user-dir "+ui.el"))))

(map!
 :prefix my/leader-key
 :n "a" #'lsp-execute-code-action)

(map!
 ;; because i use ` as my iterm hotkey
 "M-`" nil
 :i "M-`" (lambda () (interactive) (insert "`")))
