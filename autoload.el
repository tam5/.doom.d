;;; $DOOMDIR/autoload.el -*- lexical-binding: t; -*-

;; ╔════════════════════════════════════════════════════════════════════════════╗
;; ║                                                                            ║
;; ║                                 Autoloads                                  ║
;; ║                                                                            ║
;; ╚════════════════════════════════════════════════════════════════════════════╝

;;;###autoload
(defun my/center-header-text (start end)
  "Center the text inside of a fancy header. For example,
calling it on a region with ' |  text        |' will move
text to the center of the spacing."
  (interactive "r")
    (when (use-region-p)
        (let* ((current-line (buffer-substring start end))
               (parts (split-string current-line "\\s-\\{2,\\}"))
               (center-length (- (length current-line) (length (car parts)) (length (nth 2 parts))))
               (center-text-length (length (nth 1 parts)))
               (left-padding (/ (- center-length center-text-length) 2))
               (right-padding (- center-length center-text-length left-padding))
               (new-line (concat
                          (nth 0 parts)
                          (make-string left-padding ?\s)
                          (nth 1 parts)
                          (make-string right-padding ?\s)
                          (nth 2 parts))))
          (delete-region start end)
          (save-excursion
            (goto-char (region-beginning))
            (insert new-line)))))
