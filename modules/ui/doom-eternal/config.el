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

(defcustom doom-eternal/command-palette-offset-top-pixels 60
  "The offset in pixels from the top of the frame to place the command palette."
  :type 'integer
  :group 'doom-eternal)

(defface doom-eternal/command-palette-face
  '((t (:inherit t)))
  "Face for the command palette background."
  :group 'doom-eternal)

;; (add-hook 'window-setup-hook #'+doom-eternal/initial-frame-setup)
;; (add-hook 'after-make-frame-functions #'+doom-eternal/frame-setup)
;; (add-hook 'minibuffer-setup-hook #'+doom-eternal/command-palette-setup)

;; (defun my-vertico-posframe-fix-cursor (&rest _args)
;;   (with-current-buffer (posframe-buffer vertico-posframe--frame)
;;     (setq-local cursor-type t)))

;; (advice-add 'vertico-posframe--display :after #'my-vertico-posframe-fix-cursor)

;; (after! vertico-posframe
;;   (setq vertico-posframe-border-width 0
;;         vertico-posframe-poshandler #'doom-eternal/posframe-poshandler-frame-top-center-with-offset))

;; Done in a hook to ensure loading as late as possible
(add-hook! 'doom-after-modules-config-hook
  (if (fboundp 'fringe-mode) (fringe-mode '(8 . 0)))

  (after! git-gutter-fringe
    (define-fringe-bitmap 'git-gutter-fr:added doom-eternal/bitmap--vertical-bar-left nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified doom-eternal/bitmap--diagonal-lines nil nil '(top repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted doom-eternal/bitmap--triangle-lower-left nil nil 'bottom))

  (after! flycheck
    (define-fringe-bitmap '+doom-eternal/flycheck-fringe-bitmap doom-eternal/bitmap--circle-medium nil nil 'center)
    (flycheck-redefine-standard-error-levels "⦁" '+doom-eternal/flycheck-fringe-bitmap)
    (setq flycheck-indication-mode 'left-margin)

    (setq-default left-margin-width 1
                  right-margin-width 0)))

;; (defun add-special-character-overlay ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (let ((ov (make-overlay (point-at-bol) (point-at-bol))))
;;         (overlay-put ov 'before-string "➤")
;;         (overlay-put ov 'special-character t))
;;       (forward-line 1))))

;; (defun remove-special-character-overlays ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (< (point) (point-max))
;;       (dolist (ov (overlays-at (point-at-bol)))
;;         (when (overlay-get ov 'special-character)
;;           (delete-overlay ov)))
;;       (forward-line 1))))

;; (set-face-attribute 'line-number nil :background "red" :box '(:line-width -5 :color "blue") :underline '(:color "green"))

;; (set-face-attribute 'vertico-posframe-border nil :background "red")


(defun my-change-minibuffer-prompt (orig-fun &rest args)
  (let ((prompt (car args)))
    (apply orig-fun
           (if (string-prefix-p "Enter" "Enter")
               (cons (concat (let ((str "Large Text")
                                   (my-face `(:height ,(round (* 1.5 (face-attribute 'default :height))))))
                               (put-text-property 0 (length str) 'face my-face str)
                               str))
                     (cdr args))
             args))))

;; (advice-add 'read-from-minibuffer :around #'my-change-minibuffer-prompt)


;; (add-hook 'minibuffer-setup-hook #'my-draw-hline-in-minibuffer)

(defun my-posframe-show (posframe-buffer &rest args)
  "Modified posframe-show to display cursor in the POSFRAME-BUFFER."
  (let ((posframe (posframe-show posframe-buffer args)))
    (set-buffer posframe-buffer)
    (setq cursor-type t)
    (set-window-buffer (frame-root-window posframe) posframe-buffer)
    posframe))

(setq posframe-mouse-banish nil)

(setq vertico-posframe-show-minibuffer-rules nil)

;; (defun aritest-display-line ()
;;   "Update count overlay `vertico--count-ov'."
;;   (move-overlay vertico--line-ov (point-max) (point-max))
;;   (overlay-put vertico--line-ov 'after-string
;;                "abdefg"))

;; (defun create-vertical-bar-image (width height)
;;   "Create an image of a vertical bar with the specified WIDTH and HEIGHT."
;;   (let ((bar (make-temp-file "vertical-bar" nil ".png")))
;;     (with-temp-buffer
;;       (insert (format "P1\n%d %d\n" width height))
;;       (dotimes (_ height)
;;         (insert (make-string width ?1) "\n"))
;;       (write-region (point-min) (point-max) bar))
;;     (create-image bar 'png)))


(defun vertico--display-line ()
  "Update count overlay `vertico--line-ov'."
  (move-overlay vertico--line-ov (point-max) (point-max))
  (overlay-put vertico--line-ov 'after-string "\n-line-\n"))

;; (defun vertico--pseudo-cursor ()
;;   "Update count overlay `vertico--line-ov'."
;;   (move-overlay vertico--pseudo-cursor-ov (point-max) (point-max))
;;   (overlay-put vertico--pseudo-cursor-ov 'after-string

;; (let ((str " "))
;;   (put-text-property 0 1 'display (create-vertical-bar-image 2 12) str)
;;   str)))

;;                ;; #(" " 0 1 (:display (create-vertical-bar-image 2 12)))))

;;                ;; #(" " 0 1 'display (create-vertical-bar-image 2 12))))

;; (defun create-vertical-bar-string ()
;;   "Create a string with a space character replaced by a vertical bar image."
;;   (propertize " " 'display (create-vertical-bar-image 2 12)))

;; (create-vertical-bar-string)

(defvar-local vertico--line-ov nil
  "Overlay showing the candidates.")

(defvar-local vertico--pseudo-cursor-ov nil
  "Overlay showing the candidates.")

(cl-defgeneric vertico--setup ()
  "Setup completion UI."
  (setq vertico--input t
        vertico--pseudo-cursor-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--line-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  ;; Set priority for compatibility with `minibuffer-depth-indicate-mode'
  (overlay-put vertico--count-ov 'priority 1)
  ;; (overlay-put vertico--candidates-ov 'priority 40)
  (setq-local completion-auto-help nil
              completion-show-inline-help nil)
  (use-local-map vertico-map)
  (add-hook 'pre-command-hook #'vertico--prepare nil 'local)
  (add-hook 'post-command-hook #'vertico--exhibit nil 'local))

(defun debug-overlays ()
  (message "Input: %s" vertico--input)
  (message "vertico--candidates-ov: %s" vertico--candidates-ov)
  (message "vertico--line-ov: %s" vertico--line-ov)
  (message "vertico--count-ov: %s" vertico--candidates-ov))

; (defvar my-posframe-buffer " *my-posframe-buffer*")

; (posframe-delete " *my-posframe-buffer*")

; (when (posframe-workable-p)
;   (posframe-show my-posframe-buffer
;                  :string "This is a test"
;                  :position (point)))

(defun vertico--exhibit ()
  "Exhibit completion UI."
  (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
    (vertico--update 'interruptible)
    (vertico--prompt-selection)
    (vertico--display-count)
    ;; (vertico--display-line)
    (vertico--display-candidates (vertico--arrange-candidates))
    ;; (with-current-buffer (window-buffer (minibuffer-window))
    ;;   (setq-local cursor-type t)
    ;;   (setq-local cursor-in-non-selected-windows 'box))
    ))

;; (setq focus-follows-mouse t)

; (when (posframe-workable-p)
;   (posframe-show " *my-posframe-buffer*"
;                  :string "This is a test"
;                  :position (point)))

;; TODO make a pure posframe example
;;
;; (defun my-full-width-minibuffer-underline ()
;;   (let* ((width (- (window-width) 2)) ; 2 for the prompt's spaces
;;          (separator (make-string width ?━))
;;          (overlay (make-overlay (point-min) (point-max))))
;;     (overlay-put overlay 'before-string
;;                  (propertize (concat separator "\n")
;;                              'face '(:underline t)))))

;; (add-hook 'minibuffer-setup-hook #'my-full-width-minibuffer-underline)


;; (defface my-full-width-minibuffer-underline
;;   '((t :underline t))
;;   "Face for full-width underline in the minibuffer.")

;; (defun my-full-width-minibuffer-underline ()
;;   (let* ((width (- (window-width) 2))
;;          (separator (make-string width ?\s))
;;          (overlay (make-overlay (point-min) (point-max))))
;;     (overlay-put overlay 'after-string
;;                  (propertize (concat separator "\n")
;;                              'face 'my-full-width-minibuffer-underline))))

;; (my-full-width-minibuffer-underline)

;; (add-hook 'minibuffer-setup-hook #'my-full-width-minibuffer-underline)





;; (use-package! vertico-posframe
;;   :hook (vertico-mode . vertico-posframe-mode)
;;   :config
;;   (setq vertico-posframe-parameters '((left-fringe . 8)
;;                                        (right-fringe . 8)
;;                                        (internal-border-width . 2)
;;                                        (minibuffer-0)))
;;   (setq vertico-posframe-show-0 nil))

;; (defun my-draw-hline-in-minibuffer ()
;;   (let ((hline (make-string (window-width) ?━)))
;;     (with-current-buffer (window-buffer (minibuffer-selected-window))
;;       (save-excursion
;;         (goto-char (point-max))
;;         (insert hline)))))

;; (
 ;; add-hook 'minibuffer-setup-hook #'my-draw-hline-in-minibuffer)


;; (defun aritest-debug-me ()
;;   (remove-overlays)

;;   (let ((overlay-2 (make-overlay (point-max) (point-max) nil t t))
;;         (overlay-1 (make-overlay (point-max) (point-max) nil t t))
;;         (overlay-3 (make-overlay (point-min) (point-min) nil t t)))
;;     (overlay-put overlay-2 'before-string "2->") (overlay-put overlay-2 'priority 2)
;;     (overlay-put overlay-3 'before-string "3->") (overlay-put overlay-3 'priority 3)
;;     (overlay-put overlay-1 'before-string "1->") (overlay-put overlay-1 'priority 10)

;;     ))


;; useful stuff here:
(defvar my-orig (symbol-function 'vertico-posframe--show))
(fset 'vertico-posframe--show my-orig)

;;  with after-string, higher priority means earlier in the buffer
;;  with before-string, higher priority means later in the buffer
(defun aritest ()
  (interactive)
  (when (posframe-workable-p)
    ;; (posframe-show (window-buffer (minibuffer-window))
;; (let ((buffer (window-buffer (minibuffer-window))))
(let ((buffer "scratch"))
    (posframe-show buffer
                   :accept-focus t
                   :border-color "#ee7b29"
                   :border-width 2
                   :poshandler 'posframe-poshandler-frame-center
                   :height (round(* (frame-height) 0.90))
                   :width (round(* (frame-width) 0.75))
                   ;; :override-parameters '((cursor-type box))
                   )

    ;; so thisworks we just need to move the point forward
    ;; (with-current-buffer buffer
    ;;   (setq-local cursor-type t)
    ;;   (setq-local cursor-in-non-selected-windows 'box))
    )))

;;; so maybe i need to do more debugging on the whole refocus thing i guess and see what's going on
