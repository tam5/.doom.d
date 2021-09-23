;;; core/utils/+php.el -*- lexical-binding: t; -*-

(load! "+json")
(load! "+project")

(defun +php/read-project-composer-file ()
  "Read a project's composer file."
  (when (+project/file-exists-p "composer.json")
    (+json/read-file (+project/file-name "composer.json"))))

(defun +php/do-psr4-replacement (str)
  "Read a project's composer file and string-replace psr-4 mappings."
  (when (+project/file-exists-p "composer.json")
    (let* ((composer (+php/read-project-composer-file))
           (autoload (gethash "autoload" composer))
           (psr4 (gethash "psr-4" autoload)))
    (maphash (lambda (to from)
               (setq str (string-replace from to str)))
             psr4)
    str)))

(defun +php/guess-namespace ()
  "Guess the namespace of the current file."
  (when (+project/file-exists-p "composer.json")
    (let* ((filename (file-relative-name buffer-file-name (projectile-project-root)))
           (dirname (file-name-directory filename))
           (candidate (file-name-sans-extension dirname))
           (replaced (+php/do-psr4-replacement candidate))
           (flipped (string-replace "/" "\\" replaced)))
      (string-trim flipped "\\\\" "\\\\"))))

(defun +php/guess-classname ()
  "Guess the classname of the current file."
  (file-name-base buffer-file-name))

(defun +php/insert-namespace ()
  (interactive)
  (insert (concat "namespace " (+php/guess-namespace) ";")))

;; php lint on save
;; sort imports ( sort lines by length )
