;;; lisp/keybinding-icons.el -*- lexical-binding: t; -*-

(use-package! svg-lib)

;; (defun my/advice-key-description (orig-fun &rest args)
;;   "Advice function that modifies the result of `key-description'.
;; Replaces any occurrence of \"SPC\" in the output with \"space\"."
;;   (let ((desc (apply orig-fun args)))
;;     (replace-regexp-in-string "SPC" "space" desc)))

(defvar my/keybinding-icon-str-max-icons 8)

(defun my/make-keybinding-icon (key)
  ""
  (svg-lib-tag key nil
   :background (doom-lighten (face-attribute 'default :background) 0.1)
   :foreground (doom-lighten (face-attribute 'default :background) 0.6)
   ;; :height my/keybinding-icon-scale-factor
   ;; :font-size (round (* (face-attribute 'default :height)
   ;;                      (* my/keybinding-icon-scale-factor .1)))
   :font-size 13
   :font-weight 'bold
   :margin .5 :padding .5 :radius 3 :stroke 0))

(defun my/map-to-keybinding-icon-str (keybinding)
  ""
  (let* ((description (key-description keybinding))
         (binding-parts (delq "" (split-string description " ")))
         (filler-width (- 5 (length binding-parts))))
    (concat (mapconcat (lambda (part)
                           (propertize part 'display (my/make-keybinding-icon part)))
                       binding-parts)
            (propertize " " 'display `(space :width ,filler-width)))))

(defun my/iconify-keybinding-hints-a (_ &rest args)
  ""
  (let* ((cand (car args))
             (sym (intern-soft cand))
             (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (propertize (my/map-to-keybinding-icon-str key) 'face 'marginalia-key)))

;; (defun my/marginalia-annotate-command (cand)
;;   "Annotate command CAND with its documentation string.
;; Similar to `marginalia-annotate-symbol', but does not show symbol class."
;;   (when-let (sym (intern-soft cand))
;;     (let* ((binding-str (marginalia-annotate-binding cand))
;;            (padding-str ""))
;;            ;; (padding-str (make-string (- 16 (length binding-str)) ?\s)))
;;            (marginalia--fields
;;             ((concat binding-str padding-str (marginalia--documentation (marginalia--function-doc sym))))))))

(defun my/marginalia-annotate-command (cand)
  ""
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia-annotate-binding cand) :truncate 0.165)
     ((marginalia--function-doc sym) :truncate 1.0 :face 'marginalia-documentation))))

;; (key-description (kbd "SPC x"))  ; "8-x C-f"
