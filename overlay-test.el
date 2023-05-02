my test thing
my other text
and more text

(progn
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))
    (put-text-property (line-beginning-position 1) (line-end-position 1) 'face '(:box (:color "black" :line-width (0 . 20))))
      (create-line-overlay (line-beginning-position 1) (line-end-position 1) 8 4)
      ))

(defun create-multicolor-box (text top-color right-color bottom-color left-color)
  "Create a multicolor box around the text using display properties."
  (concat
   (propertize " " 'display `((space :align-to 0 :height (1 . ,top-color))))
   (propertize " " 'display `((space :width 0 :background ,left-color)))
   text
   (propertize " " 'display `((space :width 0 :background ,right-color)))
   (propertize "\n")
   (propertize " " 'display `((space :align-to 0 :height (1 . ,bottom-color))))))

(let ((text "Hello, world!"))
  (insert (create-multicolor-box text "red" "green" "blue" "yellow")))

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
