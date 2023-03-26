;;; lisp/keybinding-icons.el -*- lexical-binding: t; -*-

(use-package! svg-lib)

;; (defun my/advice-key-description (orig-fun &rest args)
;;   "Advice function that modifies the result of `key-description'.
;; Replaces any occurrence of \"SPC\" in the output with \"space\"."
;;   (let ((desc (apply orig-fun args)))
;;     (replace-regexp-in-string "SPC" "space" desc)))

(defun my/make-keybinding-icon (key)
  (svg-lib-tag key nil
   :background (doom-lighten (face-attribute 'default :background) 0.1)
   :foreground (doom-lighten (face-attribute 'default :background) 0.6)
   ;; :height my/keybinding-icon-scale-factor
   ;; :font-size (round (* (face-attribute 'default :height)
   ;;                      (* my/keybinding-icon-scale-factor .1)))
   :font-size 13
   :font-weight 'bold
   :margin .5 :padding .5 :radius 3 :stroke 0))

(defun my/make-filler-icon ()
  (svg-lib-tag "x" nil
   :background (face-attribute 'solaire-default-face :background)
   :foreground (face-attribute 'solaire-default-face :background)
   ;; :foreground (doom-lighten (face-attribute 'default :background) 0.0)
   ;; :height my/keybinding-icon-scale-factor
   ;; :font-size (round (* (face-attribute 'default :height)
   ;;                      (* my/keybinding-icon-scale-factor .1)))
   :font-size 13
   :font-weight 'bold
   :margin .5 :padding .5 :radius 3 :stroke 0))

(face-attribute 'minibuffer-prompt :background)

(defvar my/keybinding-icon-str-max-icons 8)

(defun my/map-to-keybinding-icon-str (keybinding)
  (let* ((description (key-description keybinding))
         (binding-parts (split-string description " ")))
    (nconc binding-parts (make-list (- my/keybinding-icon-str-max-icons (length binding-parts)) ""))
    (concat (mapconcat (lambda (part)
                         (if (string-empty-p part) (propertize " " 'display (my/make-filler-icon))
                           (propertize part 'display (my/make-keybinding-icon part))))
                       binding-parts))))

(defun my/iconify-keybinding-hints-a (_ &rest args)
  "Annotate command CAND with keybinding."
  (let* ((cand (car args))
             (sym (intern-soft cand))
             (key (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (propertize (my/map-to-keybinding-icon-str key) 'face 'marginalia-key)))

;; next, adjust for non binded
(defun my/marginalia-annotate-command (cand)
  "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
  (when-let (sym (intern-soft cand))
    (let* ((binding-str (marginalia-annotate-binding cand))
           (padding-str (make-string (- 16 (length binding-str)) ?\s)))
           (marginalia--fields
            ((concat binding-str padding-str (marginalia--documentation (marginalia--function-doc sym))))))))

;; (key-description (kbd "SPC x"))  ; "8-x C-f"

;; (my/iconify-keybinding-hints-a nil (kbd "SPC x"))
;; (length (propertize (my/map-to-keybinding-icon-str (kbd "SPCx v e")) 'face 'marginalia-key))
;; (length (key-description (kbd "SPCx v e")))

;; (length (propertize (my/map-to-keybinding-icon-str (kbd "SPC x v")) 'face 'marginalia-key))
