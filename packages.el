;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! svg-lib)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
