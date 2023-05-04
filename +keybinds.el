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


;;; expirimental stuff
(defun my/buffer-clear (&optional force)
  "Clear the current buffer. If FORCE is non-nil, clear the buffer"
  (interactive)
  (let ((inhibit-read-only force))
    (erase-buffer)))

(defun my/buffer-clear-force ()
  (interactive)
  (my/buffer-clear t))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))









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
 :n "x" #'aritest

 :n "ev" (lambda () (interactive) (find-file (concat doom-user-dir "config.el")))
 :n "ek" (lambda () (interactive) (find-file (concat doom-user-dir "+keybinds.el")))
 :n "eu" (lambda () (interactive) (find-file (concat doom-user-dir "+ui.el"))))

;; extensions
(map!
 :v "I" #'evil-mc-make-cursor-in-visual-selection-beg
 :v "A" #'evil-mc-make-cursor-in-visual-selection-end

 :prefix my/leader-key
 :n "a" #'lsp-execute-code-action)

;; personal hacks and emacs secific keybinds
(map!
 ;; because i use ` as my iterm hotkey
 "M-`" nil
 :i "M-`" (lambda () (interactive) (insert "`"))

 :prefix my/leader-key :n "m" (lambda () (interactive) (switch-to-buffer-other-window "*Messages*")))

(map!
 :map messages-buffer-mode-map
 "s-k" #'my/buffer-clear-force)

(map!
 :map vterm-mode-map
 "s-k" #'vterm-clear)
