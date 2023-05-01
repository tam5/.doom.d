my t
my other text
and more text

(progn
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
      (make-line-smaller (line-beginning-position 1) (line-end-position 1))))

(defun make-line-smaller (start end)
  (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'before-string
                     (propertize
                      " \n"
                      'face `(:background "orange" :family "Tiny" :height 0.01)
                      'line-height 0.5
                      'line-spacing -30
                      ))))

      ;; (overlay-put overlay 'after-string (apply #'propertize "    E" 0 4 props-list))

(defun create-line-overlay (start end above-px below-px)
  "Create an underline overlay for the region between START and END.
PADDING-ABOVE specifies the number of pixels above the horizontal line.
PADDING-BELOW specifies the number of pixels below the horizontal line."
  (let* ((font-height-px (frame-char-height))
         (adjusted-line-height (+ above-px below-px))
         (raise-factor (/ adjusted-line-height (float font-height-px))))
    (setq-local underline-minimum-offset above-px)
    (let ((overlay (make-overlay start end))
          (props-list `(face (:underline (:color "red" :position ,below-px))
                        display (raise ,raise-factor))))
      (overlay-put overlay 'after-string (apply #'propertize "    E" 0 4 props-list))
      (overlay-put overlay 'display `(raise ,raise-factor))
      (overlay-put overlay 'face `(:underline (:color "red" :position ,below-px))))))
