;;; lisp/keybinding-icons.el -*- lexical-binding: t; -*-

(use-package! svg-lib)

;; (defcustom my/key-icon-alist
;;   '(("SPC" . "space")
;;     ("s" . "⌘")
;;     ("C" . "control"))
;;   "My list"
;;   :type '(alist :key-type string :value-type string)
;;   :group 'aritest-group)

;; (defadvice! my/iconify-keybinding-hints-a (cand &rest _)
;;   "todo"
;;   :around #'marginalia-annotate-binding
;;   (when-let ((sym (intern-soft cand))
;;              (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
;;     (format #(" (%s)" 1 5 (face marginalia-key))
;;             (propertize (key-description key)
;;                         ;; 'face 'marginalia-key
;;                         'display (svg-lib-tag (key-description key) nil
;;                                               :background (doom-darken (face-attribute 'default :background) 0.4)
;;                                               :foreground (doom-darken (face-attribute 'default :foreground) 0)
;;                                               :margin 1
;;                                               :padding 2
;;                                               :radius 3
;;                                               :stroke 0)))))

;;
(defun my/advice-key-description (orig-fun &rest args)
  "Advice function that modifies the result of `key-description'.
Replaces any occurrence of \"SPC\" in the output with \"space\"."
  (let ((desc (apply orig-fun args)))
    (replace-regexp-in-string "SPC" "space" desc)))

;;;;;;;;;

(defun my/make-keybinding-icon (key)
  ""
  (svg-lib-tag key nil
   :background (doom-darken (face-attribute 'default :background) 0.4)
   :foreground (doom-darken (face-attribute 'default :foreground) 0)
   :margin 1 :padding 2 :radius 3 :stroke 0))

(defun my/map-to-keybinding-icon-str (key)
  "todo"
  (mapconcat (lambda (part) (propertize part 'display (my/make-keybinding-icon part)))
             (split-string (key-description key) " ")))

(defun my/iconify-keybinding-hints-a (_ &rest args)
  "Annotate command CAND with keybinding."
  (when-let ((cand (car args))
             (sym (intern-soft cand))
             (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (propertize (my/map-to-keybinding-icon-str key) 'face 'marginalia-key)))

;; sometimes this is needed, not sure why
;; (setq svg-lib-style-default (svg-lib-style-compute-default))

(advice-add 'marginalia-annotate-binding :around #'my/iconify-keybinding-hints-a)
