;;; ui/doom-eternal/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst doom-eternal/bitmap--diagonal-lines
  [#b10010000
   #b00110000
   #b01100000
   #b11000000]
  "Bitmap for diagonal lines.")

;;;###autoload
(defconst doom-eternal/bitmap--vertical-bar-left
  [#b11110000]
  "Bitmap for 4px wide vertical bar on the left half.")

;;;###autoload
(defconst doom-eternal/bitmap--triangle-lower-left
  [#b10000000
   #b11000000
   #b11100000
   #b11110000]
  "Bitmap for a triangle on the lower left half.")

;;;###autoload
(defconst doom-eternal/bitmap--circle-medium
  [#b00000000
   #b00111000
   #b01111100
   #b01111100
   #b01111100
   #b00111000
   #b00000000]
  "Bitmap for a medium sized circle.")

;;;###autoload
(defun +doom-eternal/frame-setup (frame)
  "Perform extra setup for a frame."
  (dolist (pair doom-eternal/frame-parameter-overrides-alist)
    (set-frame-parameter frame (car pair) (cdr pair))))

;;;###autoload
(defun +doom-eternal/initial-frame-setup ()
  "Perform extra setup for the initial frame."
  (+doom-eternal/frame-setup (selected-frame)))
