;;; ui/doom-eternal/config.el -*- lexical-binding: t; -*-

(defgroup doom-eternal nil
  "Doom Eternal: When plain old Doom Emacs just isn't enough.

Welcome to the realm of Doom Eternal, where your Emacs experience
ascends to new heights of power, beauty, and crispiness. We take
the already awesome Doom Emacs and crank it up to 11, leaving you
wondering how you ever managed without this sublime extension. With
Doom Eternal, 'Emacs as an editor' is a thing of the past. Prepare
to enter a world where Emacs becomes a way of life. Fasten your seatbelts,
and enjoy the ride!"
  :group 'doom)

(defcustom doom-eternal/frame-parameter-overrides-alist
  '((ns-appearance . 'light)
    (internal-border-width . 0))
  "Alist of params to override when creating new frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-eternal)

(defcustom doom-eternal/command-palette-frame-parameter-overrides-alist
  '((undecorated . nil)
    (undecorated-round . t)
    (left-fringe . 0)
    (right-fringe . 0)
    ;; (header-line-format . '"asdf")
    ;; (mode-line-format . '"asdf")
    (internal-border-width . 0)
    (border-width . 0)
    (ns-appearance . 'light))
  "Alist of params to override when creating new command palette frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-eternal)

;; TODO make it a percent? or a fn? also add one for the width and height
(defcustom doom-eternal/command-palette-offset-top-pixels 60
  "The offset in pixels from the top of the frame to place the command palette."
  :type 'integer
  :group 'doom-eternal)

(defface doom-eternal/command-palette-face
  '((t (:inherit t)))
  "Face for the command palette background."
  :group 'doom-eternal)

(defface doom-eternal/command-palette-input-underline-face
  '((t (:inherit t)))
  "Face for the command palette background."
  :group 'doom-eternal)

;; TODO where does this go?
(set-face-attribute 'doom-eternal/command-palette-input-underline-face nil
                    :underline `(:color ,(doom-blend (face-foreground 'default) (face-background 'default) 0.8)))

;; (set-face-attribute 'doom-eternal/command-palette-input-underline-face nil :background nil)
;; (set-face-attribute 'doom-eternal/command-palette-input-underline-face nil :foreground nil)

;; (set-face-attribute 'doom-eternal/command-palette-input-underline-face nil :height 150)
;; (set-face-attribute 'doom-eternal/command-palette-input-underline-face nil :line-spacing 3)

;; (add-hook 'window-setup-hook #'+doom-eternal/initial-frame-setup)
;; (add-hook 'after-make-frame-functions #'+doom-eternal/frame-setup)
;; (add-hook 'minibuffer-setup-hook #'+doom-eternal/command-palette-setup)

;; TODO extract to command palette package i guess
;; TODO reorganize in general, clean up, comments, etc
(defvar-local doom-eternal/vertico--input-underline-ov nil
  "Overlay showing the candidates.")

(defvar-local doom-eternal/vertico--pseudo-cursor-ov nil
  "Overlay showing the candidates.")

(defun doom-eternal/vertico--display-pseudo-cursor ()
  "Update count overlay `doom-eternal/vertico--pseduo-cursor-ov'.
The vertico-posframe serves as an enhanced visual representation of the
minibuffer without altering its behavior. To emulate focus in this buffer,
a pseudo cursor is necessary."
  (move-overlay doom-eternal/vertico--pseudo-cursor-ov (point-max) (point-max))
  (overlay-put doom-eternal/vertico--pseudo-cursor-ov 'after-string
               (propertize " "
                           'display `(space :width 0.25)
                           'face 'cursor)))

(defun doom-eternal/vertico--display-input-underline ()
  "Update count overlay `doom-eternal/vertico--input-underline-ov'."
  (let* ((inhibit-modification-hooks t)
         (prompt-length (- (point-max) (minibuffer-prompt-end)))
         (remaining-length (- vertico-posframe-width prompt-length))
         (face 'doom-eternal/command-palette-input-underline-face))
  (move-overlay doom-eternal/vertico--input-underline-ov (point-min) (point-max))
    (overlay-put doom-eternal/vertico--input-underline-ov 'after-string
                 (propertize (make-string remaining-length ?\s) 'face face))
    (add-face-text-property (minibuffer-prompt-end) (point-max) 'font-lock-comment-face 'append)

    ))

(defun +doom-eternal/vertico--exhibit ()
  "Exhibit completion UI."
  (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
    (vertico--update 'interruptible)
    (vertico--prompt-selection)
    ;; (vertico--display-count)
    (doom-eternal/vertico--display-pseudo-cursor)
    (doom-eternal/vertico--display-input-underline)
    (vertico--display-candidates (vertico--arrange-candidates))))

;; (setq left-margin-width nil)
;;    (set-window-margins nil left-margin-width right-margin-width)

(defun +doom-eternal/vertico--setup ()
  "Setup completion UI."
  (setq vertico--input t
        doom-eternal/vertico--pseudo-cursor-ov (make-overlay (point-max) (point-max) nil t t)
        doom-eternal/vertico--input-underline-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  ;; Set priority for compatibility with `minibuffer-depth-indicate-mode'
  (overlay-put vertico--count-ov 'priority 1)
  (overlay-put doom-eternal/vertico--input-underline-ov 'priority 2)
  (overlay-put doom-eternal/vertico--pseudo-cursor-ov 'priority 3)
  (setq-local completion-auto-help nil
              completion-show-inline-help nil)
  (use-local-map vertico-map)
  (add-hook 'pre-command-hook #'vertico--prepare nil 'local)
  (add-hook 'post-command-hook #'vertico--exhibit nil 'local))

(defvar doom-eternal/vertico--prompt-remap-alist
  '(("M-x " . "")))

(defun doom-eternal/vertico--remap-prompt (prompt)
  (or (alist-get prompt doom-eternal/vertico--prompt-remap-alist nil nil 'equal)
      prompt))

(defun +doom-eternal/read-from-minibuffer (orig-fun &rest args)
  (let ((prompt (doom-eternal/vertico--remap-prompt (car args))))
  (apply orig-fun (cons prompt (cdr args)))))

(after! vertico-posframe
  (setq vertico-posframe-border-width 0
        vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8)
                                      (border-width . 0)
                                      (left-margin . 20)
                                      (right-margin . 20)
                                      (internal-border-width . 0))
        vertico-posframe-width 100 ;; TODO make more dynamic ish
        vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset)
  (cl-defmethod vertico--setup ()
    (+doom-eternal/vertico--setup)))

;; Done in a hook to ensure loading as late as possible
(add-hook! 'doom-after-modules-config-hook
  ;; (if (fboundp 'fringe-mode) (fringe-mode '(8 . 0)))

  (after! git-gutter-fringe
    (define-fringe-bitmap 'git-gutter-fr:added doom-eternal/bitmap--vertical-bar-left nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified doom-eternal/bitmap--diagonal-lines nil nil '(top repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted doom-eternal/bitmap--triangle-lower-left nil nil 'bottom))

  (after! flycheck
    (define-fringe-bitmap '+doom-eternal/flycheck-fringe-bitmap doom-eternal/bitmap--circle-medium nil nil 'center)
    (flycheck-redefine-standard-error-levels "⦁" '+doom-eternal/flycheck-fringe-bitmap)
    (setq flycheck-indication-mode 'left-margin)

    (setq-default left-margin-width 0
                  right-margin-width 0)))

(advice-add 'read-from-minibuffer :around #'+doom-eternal/read-from-minibuffer)
(advice-add 'vertico--exhibit :override #'+doom-eternal/vertico--exhibit)


;; (require 'whitespace)

(defun set-ui-portion-colors-and-sizes ()
  "Set different colors and sizes for UI portions like margin, fringe, border, internal border, mode-line, and scroll-bar."
  (interactive)
  ;; Margin
  (setq-default left-margin-width 40)
  (setq-default right-margin-width 40)

  ;; Fringe
  ;; (setq-default fringe-mode '(8 . 8)) ; Set fringe width for both left and right sides
  (set-face-background 'fringe "dark green")
  (set-face-background 'solaire-fringe-face "red")

  ;; Border
  (setq border-width 4)
  (add-to-list 'default-frame-alist '(border-width . 4))
  (set-face-background 'border "dark blue")

  ;; Internal Border
  (setq internal-border-width 4)
  (add-to-list 'default-frame-alist '(internal-border-width . 4))
  (set-face-background 'internal-border "dark red")

  ;; Mode Line
  (set-face-background 'mode-line "dark cyan")
  (set-face-foreground 'mode-line "white")

  ;; Scroll Bar
  (set-scroll-bar-mode 'right)
  (set-face-background 'scroll-bar "dark orange")

  ;; Whitespace (for margin background)
  (setq whitespace-style '(face empty))
  (setq whitespace-empty-at-bob-regexp "\\`\\(\\(^[[:space:]]*\\)\\(\n\\|\\'\\)\\)")
  (setq whitespace-empty-at-eob-regexp "\\(\\`\\|\\n\\)\\(\\(^[[:space:]]*\\)\\'\\)")
  (set-face-background 'whitespace-empty "yellow")
  (global-whitespace-mode 1)

  (setq display-line-numbers-type t)
  (global-display-line-numbers-mode t)
  (set-face-foreground 'line-number "dark gray")
  (set-face-foreground 'line-number-current-line "black")

  (setq-default display-fill-column-indicator t)
  (setq-default fill-column 80)
  (set-face-foreground 'fill-column-indicator "dark slate gray")

   (set-face-background 'minibuffer-prompt "dark violet")
  (set-face-foreground 'minibuffer-prompt "white")

  ;; Header Line
  (set-face-background 'header-line "dark turquoise")
  (set-face-foreground 'header-line "black")

  ;; Tab line
  (set-face-background 'tab-line "dark turquoise")
  (set-face-foreground 'tab-line "black")

  (set-face-background 'vertical-border "violet")
  (set-face-foreground 'vertical-border "white")

  (set-face-background 'child-frame-border "pink")
  (set-face-foreground 'child-frame-border "white")

  ;; (set-face-background 'tab-line-tab-inactive "dark violet")
  ;; (set-face-foreground 'tab-line-tab-inactive "white")
  ;; (set-face-background 'tab-line-tab-active "dark violet")
  ;; (set-face-foreground 'tab-line-tab-active "white")
  ;; (set-face-attribute 'tab-line-highlight nil :underline t)
  (setq-default tab-line-show t)

  ;; Cursor
   (set-face-background 'cursor "dark orange"))

;; (set-ui-portion-colors-and-sizes)
