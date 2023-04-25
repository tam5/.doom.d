;;; ui/doom-eternal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +doom-eternal/minibuffer-setup ()
  "Perform extra setup for the minibuffer."
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (when solaire-mode
        (with-current-buffer (window-buffer (frame-selected-window frame))
          (dolist (attribute '(:background))
            (set-face-attribute 'solaire-default-face frame attribute (face-attribute 'doom-eternal/command-palette-face attribute)))))
      (dolist (pair doom-eternal/command-palette-frame-parameter-overrides-alist)
        (set-frame-parameter frame (car pair) (cdr pair))))))

;;;###autoload
(defun +doom-eternal/frame-setup (frame)
  "Perform extra setup for a frame."
  (dolist (pair doom-eternal/frame-parameter-overrides-alist)
    (set-frame-parameter frame (car pair) (cdr pair))))

;;;###autoload
(defun +doom-eternal/initial-frame-setup ()
  "Perform extra setup for the initial frame."
  (+doom-eternal/frame-setup (selected-frame)))

;;;###autoload
(defun doom-eternal/posframe-poshandler-frame-top-center-with-offset (info)
  "Modified version of `posframe-poshandler-frame-top-center', allowing
customizable offset at the top."
  (let ((offset doom-eternal/command-palette-offset-top))
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          offset)))
