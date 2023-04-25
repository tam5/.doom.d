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
(defun +doom-eternal/setup-minibuffer ()
  ""
  ;; (turn-off-solaire-mode) ;; todo this is not the best, rly we just want to exclude some buffers
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (dolist (pair doom-eternal/command-palette-frame-parameter-overrides-alist)
        (set-frame-parameter frame (car pair) (cdr pair))))))

      ;; ;; explain? or functionify
      ;; (set-frame-parameter frame 'undecorated nil)
      ;; (set-frame-parameter frame 'undecorated-round t)
      ;; (set-frame-parameter frame 'ns-appearance 'light))))

;;;###autoload
(defun +doom-eternal/setup-window ()
  ""
  (set-frame-parameter (selected-frame) 'ns-appearance 'light))
