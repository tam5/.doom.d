;;; ui/doom-eternal/autoload/+command-palette.el -*- lexical-binding: t; -*-

(require 'vertico-posframe)

(defgroup doom-eternal-command-palette nil
  "Give Emacs a command palette like Sublime Text."
  :group 'doom-eternal)

(defcustom doom-eternal-command-palette/posframe-width 100
  "The width the command palette."
  :type 'integer)

(defcustom doom-eternal-command-palette/posframe-poshandler #'posframe-poshandler-frame-top-center-with-offset
  "The posframe poshandler used by the command palette."
  :type 'function)

(defcustom doom-eternal-command-palette/posframe-parameter-overrides-alist
  '((undecorated . nil)
    (undecorated-round . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (child-frame-border-width . 0)
    (internal-border-width . 0)
    (border-width . 0)
    (ns-appearance . light))
  "Alist of params to override when creating new command palette frames. We don't
just use `default-frame-alist`, as applying some of these parameters
after the frame already exists has some visual benefits."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-eternal-command-palette)

(defface doom-eternal-command-palette/default-face
  '((t (:inherit t)))
  "Face for the command palette background."
  :group 'doom-eternal-command-palette)

(defface doom-eternal-command-palette/input-underline-face
  '((t (:inherit underline)))
  "Face for the command palette input underline."
  :group 'doom-eternal-command-palette)

(defvar-local doom-eternal-command-palette/input-underline-ov nil
  "Overlay showing the input underline.")

(defvar-local doom-eternal-command-palette/pseudo-cursor-ov nil
  "Overlay showing the pseudo cursor.")

(defvar doom-eternal-command-palette/prompt-remap-alist
  '(("M-x " . ""))
  "Alist of prompt remappings.")

(defun doom-eternal-command-palette/remap-prompt (prompt)
  "Apply configured prompt remappings."
  (or (alist-get prompt doom-eternal-command-palette/prompt-remap-alist nil nil 'equal)
      prompt))

(defun +doom-eternal-command-palette/read-from-minibuffer (orig-fun &rest args)
  "Remap the minibuffer prompt before calling `read-from-minibuffer'."
  (let ((prompt (doom-eternal-command-palette/remap-prompt (car args))))
    (apply orig-fun (cons prompt (cdr args)))))

(defun doom-eternal-command-palette/display-pseudo-cursor ()
  "Update the overlay `doom-eternal-command-palette/pseduo-cursor-ov'.
The vertico-posframe serves as an enhanced visual representation of the
minibuffer without altering its behavior. To emulate focus in this buffer,
a pseudo cursor is necessary."
  (move-overlay doom-eternal-command-palette/pseudo-cursor-ov (point) (point))
  (overlay-put doom-eternal-command-palette/pseudo-cursor-ov 'after-string
               (propertize " " 'display `(space :width 0.25) 'face 'cursor)))

(defun doom-eternal-command-palette/display-input-underline ()
  "Update the overlay `doom-eternal-command-palette/input-underline-ov'.
This overlay enhances the UI by adding a horizontal line under the input,
making it look more like a traditional input field."
  (let* ((width vertico-posframe-width)
         (face 'doom-eternal-command-palette/input-underline-face))
    (move-overlay doom-eternal-command-palette/input-underline-ov (point-min) (point-max))
    (overlay-put doom-eternal-command-palette/input-underline-ov 'after-string
                 (concat "" (propertize (make-string width ?\s) 'face face)))
    (add-face-text-property (minibuffer-prompt-end) (point-max) face 'append)))

(defun +doom-eternal-command-palette/vertico--setup ()
  "Setup completion UI."
  (setq vertico--input t
        doom-eternal-command-palette/pseudo-cursor-ov (make-overlay (point-max) (point-max) nil t t)
        doom-eternal-command-palette/input-underline-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  ;; Set priority for compatibility with `minibuffer-depth-indicate-mode'
  (overlay-put vertico--count-ov 'priority 1)
  (overlay-put doom-eternal-command-palette/input-underline-ov 'priority 2)
  (overlay-put doom-eternal-command-palette/pseudo-cursor-ov 'priority 3)
  (setq-local completion-auto-help nil
              completion-show-inline-help nil)
  (use-local-map vertico-map)
  (add-hook 'pre-command-hook #'vertico--prepare nil 'local)
  (add-hook 'post-command-hook #'vertico--exhibit nil 'local))

(defun +doom-eternal-command-palette/vertico--exhibit ()
  "Exhibit completion UI."
  (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
    (vertico--update 'interruptible)
    (vertico--prompt-selection)
    ;; (vertico--display-count)
    (doom-eternal-command-palette/display-pseudo-cursor)
    ;; (doom-eternal-command-palette/display-input-underline)
    (vertico--display-candidates (vertico--arrange-candidates))))

;;;###autoload
(defun +doom-eternal-command-palette/frame-setup ()
  "Perform setup for the command palette's frame."
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (when solaire-mode
        (with-current-buffer (window-buffer (frame-selected-window frame))
          (dolist (attribute '(:background))
            (set-face-attribute 'solaire-default-face frame attribute (face-attribute 'doom-eternal-command-palette/default-face attribute)))))
      (dolist (pair doom-eternal-command-palette/posframe-parameter-overrides-alist)
        (set-frame-parameter frame (car pair) (cdr pair))))))

;;;###autoload
(defun posframe-poshandler-frame-top-center-with-offset (info)
  "Modified version of `posframe-poshandler-frame-top-center', allowing
customizable offset at the top."
  (let ((offset 120))
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          offset)))

;;;###autoload
(define-minor-mode doom-eternal-command-palette-mode
  "Minor mode for the command palette."
  :lighter "" ; should be obvious enough
  :init-value nil
  :global t
  (if doom-eternal-command-palette-mode
      (progn
        (setq
         vertico-posframe-width doom-eternal-command-palette/posframe-width
         vertico-posframe-poshandler #'posframe-poshandler-frame-top-center-with-offset)
        (cl-defmethod vertico--setup ()
          (+doom-eternal-command-palette/vertico--setup))
        (add-hook 'minibuffer-setup-hook #'+doom-eternal-command-palette/frame-setup)
        (advice-add 'vertico--exhibit :override #'+doom-eternal-command-palette/vertico--exhibit)
        (advice-add 'read-from-minibuffer :around #'+doom-eternal-command-palette/read-from-minibuffer))
    (progn
        (remove-hook 'minibuffer-setup-hook #'+doom-eternal-command-palette/frame-setup)
        (advice-remove 'vertico--exhibit #'+doom-eternal-command-palette/vertico--exhibit)
        (advice-remove 'read-from-minibuffer #'+doom-eternal-command-palette/read-from-minibuffer))))