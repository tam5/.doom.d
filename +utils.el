;;; $DOOMDIR/+utils.el -*- lexical-binding: t; -*-
;;;
;;; /---------------------------------------------------------------------------------------
;;; | Configuration Utilities
;;; |---------------------------------------------------------------------------------------
;;; |
;;; | This file contains additional utilities that can be used by the rest of the config.
;;; | The file should only contain definitions, but shouldn't actually impact anything
;;; | unless these functions are called or invoked somewhere else in the directory.
;;; /

(defun +utils/set-dashboard-font ()
  "Set the font for the doom dashboard."
  (setq buffer-face-mode-face '+ui/dashboard-face)
  (buffer-face-mode))

(defun +utils/insert-backtic ()
  "Insert a backtick character. Since I have backtick mapped as
a hotkey for iTerm, we sometimes need to programmitcally insert one."
  (interactive)
  (insert "`"))

(defun +utils/sort-list ()
  "Sort the list in the current region alphabetically. When a
a single line is selected, the words of the line will be sorted. When
multiple lines are selected, the lines will sort themselves instead."
  (interactive)
  (if (< 1 (count-lines (region-beginning) (region-end)))
      (call-interactively 'sort-lines)
  (sort-regexp-fields nil "[a-z]+" "\\&" (region-beginning) (region-end))))

(defun +utils/sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

(defun +utils/what-face (pos)
  "Get the faces of the current position. Used for debugging."
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun +utils/multiline-args (start end)
  "Convert single line args into multi line args by splitting on commas.
To use it properly, select everything within the delimiters."
  (interactive "r\n")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s" x))
          (split-string (buffer-substring start end)) "\n")))
    (delete-region start end)
    (insert insertion))
  (save-excursion
    (goto-char (region-beginning))
    (insert "\n"))
  (save-excursion
    (goto-char (region-end))
    (message "Start: %s, End %s" (region-beginning) (region-end))
    (insert "\n"))
  (evil-indent (region-beginning) (region-end)))

(defun +utils/toggle-frame-decoration ()
  "Toggle the frame as undecorated."
  (interactive)
  (let* ((frame (selected-frame))
         (current-decoration (frame-parameter frame 'undecorated)))
    (set-frame-parameter frame 'undecorated (not current-decoration))))

(defun +utils/project-file-exists-p (filename)
  "Check if a file exists within the current project"
  (projectile-file-exists-p (concat (projectile-project-root) filename)))

(defun +utils/project-file-contains (file str)
  "Check if a string is present in the projects package.json file. Usefule
for checking if a dependency is meant to be present."
  (when (projectile-project-p)
      (let ((config (concat (projectile-project-root) file)))
        (when (file-exists-p config)
          (with-temp-buffer
            (insert-file-contents config)
            (goto-char 1)
            (search-forward str nil t))))))

(defun +utils/maybe-enable-prettier ()
  "Enable prettier-js only if the project seems to use it."
  (when (or (+utils/project-file-exists-p ".prettierrc")
            (+utils/project-file-contains "package.json" "prettier"))
    (prettier-js-mode 1)))
