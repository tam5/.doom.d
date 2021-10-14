;;; core/utils/+project.el -*- lexical-binding: t; -*-

(defun +project/file-name (filename)
  "Get the filename relative to the proiject root."
  (concat (projectile-project-root) filename))

(defun +project/file-exists-p (filename)
  "Check if a file exists within the current project"
  (projectile-file-exists-p (+project/file-name filename)))

(defun +project/file-contains (file str)
  "Check if a string is present in the projects package.json file. Usefule
for checking if a dependency is meant to be present."
  (when (projectile-project-p)
      (let ((config (+project/file-name file)))
        (when (file-exists-p config)
          (with-temp-buffer
            (insert-file-contents config)
            (goto-char 1)
            (search-forward str nil t))))))
