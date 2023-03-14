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

 :n "v" #'evil-window-vsplit
 :n "h" #'evil-window-split)
