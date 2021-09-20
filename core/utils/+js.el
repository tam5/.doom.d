;;; core/utils/+js.el -*- lexical-binding: t; -*-

(load! "+project")

(defun +js/maybe-enable-prettier ()
  "Enable prettier-js only if the project seems to use it."
  (when (or (+project/file-exists-p ".prettierrc")
            (+project/file-contains "package.json" "prettier"))
    (prettier-js-mode 1)))
