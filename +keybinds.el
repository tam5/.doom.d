;;; $DOOMDIR/+keybinds.el -*- lexical-binding: t; -*-

(defconst +keybinds-leader-key ","
  "Define the leader/prefix key, aka <Leader> in vim. This is
separate from the doom-leader-key, so we can get even quicker access
to our most frequently used keybinds.")

;; bread and butter
(map!
 :n "C-p" #'projectile-find-file
 :g "C-s-p" #'execute-extended-command

 :n "-" #'dired-jump

 :prefix +keybinds-leader-key
 :n "w" #'basic-save-buffer
 :n "q" #'evil-quit

 :n "v" #'evil-window-vsplit
 :n "h" #'evil-window-split)
