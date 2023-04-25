;;; ui/doom-eternal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-eternal/posframe-poshandler-frame-top-center-with-offset (info)
  "Modified version of `posframe-poshandler-frame-top-center', allowing
customizable offset at the top."
  (let ((offset doom-eternal/command-palette-offset-top))
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          offset)))

;;;###autoload
(defun +doom-eternal/minibuffer-setup ()
  ""
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (dolist (pair doom-eternal/command-palette-frame-parameter-overrides-alist)
        (set-frame-parameter frame (car pair) (cdr pair))))))

;;;###autoload
(defun +doom-eternal/frame-setup (frame)
  ""
  (dolist (pair doom-eternal/frame-parameter-overrides-alist)
    (set-frame-parameter frame (car pair) (cdr pair))))

;;;###autoload
(defun +doom-eternal/initial-frame-setup ()
  ""
  (+doom-eternal/frame-setup (selected-frame)))
