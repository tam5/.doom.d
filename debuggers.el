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
