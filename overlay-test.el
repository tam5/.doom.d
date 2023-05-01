my t
my other text
and more text

(put-text-property 0 3 'face '(:background "red"))

(progn
  (remove-overlays)
  (create-line-overlay 1 5 4 4))

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
