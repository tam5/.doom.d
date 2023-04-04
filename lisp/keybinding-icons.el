;;; lisp/keybinding-icons.el -*- lexical-binding: t; -*-

;; TODO describe binding?
;;
;; NOTE
;; replace-regepx-in-string will run the replacement function for each match
;; \\1 \\2 \\3 etc. are the matched groups in each match
;;
;; ex: C-g C-J
;; will run 3 times
;; run 1 - matches first C, \\1 is C, \\2 is empty, \\3 is empty
;; run 2 - matches second C, \\1 is C, \\2 is empty, \\3 is empty
;; run 3 - matches J, \\1 is J, \\2 is empty, \\3 is empty
;; case-fold-search has to be nil to match with case sensitivity
;; (let ((case-fold-search nil))
;;   (insert (replace-regexp-in-string "\\([[:upper:]]\\)" "1:\\1 2:\\2 3:\\3 4:\\4 5:\\5 6:\\6 7:\\7" "C-g C-J" t)))

;; (insert (replace-regexp-in-string "\\(\\(\\w+\\) ?\\(\\w+\\)? ?\\(\\w+\\)?\\)" "1-\\1 2-\\2 3-\\3 4-\\4 5-\\5 6-\\6" "abc def ghi" ))
;; 1-abc def ghi 2-abc 3-def 4-ghi 5- 6-

(defface keybinding-icons-keybinding-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for keybinding icons.")

(defvar keybinding-icons-prettify nil
  "If non-nil, prettify keybindings.")

(defvar keybinding-icons-prettifier-alist
  '(("\\<SPC\\>" . "space")
    ("\\<TAB\\>" . "⇥")
    ("\\<RET\\>" . "↩")
    ("\\<DEL\\>" . "⌫")
    ("\\<ESC\\>" . "⎋")
    ("\\bC-\\([^ ]+\\)" . "⌃-\\1")
    ("\\bs-\\([^ ]+\\)" . "⌘-\\1")
    ("\\bM-\\([^ ]+\\)" . "⌥-\\1")
    ("<\\<delete\\>>" . "⌫")
    ("<\\<backspace\\>>" . "⌫")
    ("<\\<up\\>>" . "↑")
    ("<\\<down\\>>" . "↓")
    ("<\\<left\\>>" . "←")
    ("<\\<right\\>>" . "→")
    ("<\\<prior\\>>" . "⇟")
    ("<\\<next\\>>" . "⇞")
    ("<\\<escape\\>>" . "⎋")
    ("<\\<\\([^>]+\\)\\>>" . "\\1")
    ("\\([[:upper:]]\\)" . (lambda (match) (concat "⇧-" (downcase match)))))
  "Alist of regexps to replace in keybindings. the order matters")

(defvar keybinding-icons--assumed-max-chords 5)

(defun keybinding-icons--prettify-keybinding (keybinding)
  "Map parts of KEYBINDING to their corresponding strings
using `keybinding-icons-prettifier-alist`."
  (let ((result keybinding)
        (case-fold-search nil))
    (dolist (mapping keybinding-icons-prettifier-alist)
      (let* ((regex (car mapping))
             (replacement (if (functionp (cdr mapping)) (cdr mapping)
                            (lambda (&rest _) (cdr mapping))))
             (start-index 0))
        (save-match-data
          (while (string-match regex result start-index)
            (save-match-data
              (setq result
                    (replace-regexp-in-string
                     regex
                     (funcall replacement (match-string 1 result))
                     result
                     t))
              (setq start-index (min (match-end 0)(length result))))
            ))))
    result))

;; font-size .085
;; margin .4
;; padding .6

;; so the font size + the margin + the padding
;; has to equal a whole number which is the size of the chars basically, unless we hack
;; the lib-tag function

;;  :font-size (round (* (face-attribute 'default :height)
;; -   ;;                      (* my/keybinding-icon-scale-factor .1)))
;; the height needs to be a bit smaller than 1 so it will fit inside, to
;; make even padding on top and bottom, use this in combo with padding,
;; so if padding is .6 then height should be .6?, except then the height gets screwed
;;
;; svg-height = window-font-height * :height
;; svg-width = ((length label) + padding) * window-font-width) + (margin *aref font-info 11)
;; 190 * .1 = 19
(defun keybinding-icons--get-or-create-svg (keybinding)
  "Get or create an svg icon for KEY."
  (svg-lib-tag (replace-regexp-in-string "-" " " keybinding) nil
               :background (doom-lighten (face-attribute 'default :background) 0.01)
               :foreground (face-attribute 'keybinding-icons-keybinding-face :foreground)
               ;; :height 1
               ;; :height 0.84
               ;; :font-size 15
               ;; font size gets rounded anyway
               ;; ascent is used from the font info (svg-ascent is center)
               ;; height i guess should be something that gives a whole number to the font size?

               ;; :font-weight 'bold
               ;; :font-family "Arial"

               ;; if these don't add up to 1 then alignment issues happen
               :margin .4 :padding .6
               :radius 3 :stroke 0))

(defun keybinding-icons--save-svg-file (keybinding filename)
  "Save the SVG icon for KEYBINDING as a file with name FILENAME.
Meant for debugging only."
  (let* ((image (keybinding-icons--get-or-create-svg keybinding))
         (data (plist-get (cdr image) :data)))
    (with-temp-buffer
      (insert data)
      (xml-mode)
      (format-all-buffer)
      (let ((formatted (buffer-string)))
        (write-region formatted nil filename)))))


;; NOTES
;; so it seems ascent + descent should equal svg-height which also should equal
;; window-font-height and txt-char-height.
;;
;;ascent and descent should be left alone, they are handled by the font itself.
;;
;;in emacs the ascent is usually bigger than the descent, and usually there
;;is more space at the top of the letters than below. this looks normal
;;on a line with capital and lowercase, but when we make the svg, we want the
;;letters to be centered, but that's not so simple since capital letters are
;;differnet sizes than lowercase letters.
;;one option could be to force the shift modifier and keep everything lowercase?
;;
;;so what is left then is to add the lowercaser and the shift,
;;make the font size a drop saller
;;make even padding all around
;;ensure margin
;;align everything once again
;;
;; SVG-HEIGHT = txt-char-height * height
;; SVG-HEIGHT = window-font-height * height

(defun keybinding-icons--debug-svgs (keybinding)
  "Save SVG icons for KEYBINDINGS to a file in the current directory."
  (write-region
   (with-output-to-string
     (dotimes (i 20)
       (unless (< i 4)
         (let* ((font-name "MesloLGL Nerd Font")
                (font-size i)
                (font-name-full (concat font-name "-" (number-to-string font-size)))
                (font-info (font-info font-name-full)))
           (setq doom-font (font-spec :family font-name :size font-size :weight 'semi-light))
           (doom/reload-font)
           (princ (concat
                   (format "Font: %s\n" font-name-full)
                   (format "Open name: %s\n" (aref font-info 0))
                   (format "window-font-height: %s\n" (window-font-height))
                   (format "-----------\n")
                   (format "Full name: %s\n" (aref font-info 1))
                   (format "Size: %s\n" (aref font-info 2))
                   (format "Height: %s\n" (aref font-info 3))
                   (format "Baseline offset: %s\n" (aref font-info 4))
                   (format "Relative compose: %s\n" (aref font-info 5))
                   (format "Default ascent: %s\n" (aref font-info 6))
                   (format "Max width: %s\n" (aref font-info 7))
                   (format "Ascent: %s\n" (aref font-info 8))
                   (format "Descent: %s\n" (aref font-info 9))
                   (format "Space width: %s\n" (aref font-info 10))
                   (format "Average width: %s\n" (aref font-info 11))
                   (format "Filename: %s\n" (aref font-info 12))
                   (format "Capability: %s\n" (aref font-info 13))
                   (format "-----------\n")
                   ))
           (svg-lib-tag-debug (replace-regexp-in-string "-" " " keybinding) nil
                              :background (doom-lighten (face-attribute 'default :background) 0.01)
                              :foreground (face-attribute 'keybinding-icons-keybinding-face :foreground)
                              :height 1
                              ;; :height 0.84
                              :font-size i
                              ;; font size gets rounded anyway
                              ;; ascent is used from the font info (svg-ascent is center)
                              ;; height i guess should be something that gives a whole number to the font size?

                              :font-weight 'bold
                              :font-family font-name ;; also try with size inside it?

                              ;; if these don't add up to 1 then alignment issues happen
                              :margin 0 :padding 0
                              :radius 3 :stroke 0)
           (princ "-------------------------------------------\n\n")
           (keybinding-icons--save-svg-file keybinding (format "~/Desktop/keybinding-icons-tests/%s_%d.svg" keybinding font-size))
           (setq doom-font (font-spec :family font-name :size 14))
           (doom/reload-font)
           ))))
   nil (format "~/Desktop/keybinding-icons-tests/%s.log" keybinding)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun keybinding-icons-toggle-prettify ()
  "Toggle `keybinding-icons-prettify'."
  (interactive)
  (setq keybinding-icons-prettify (not keybinding-icons-prettify)))

(defun keybinding-icons--make-keybinding-icon-string (char-array)
  ""
  (let* ((orig-key-description (key-description char-array))
         (desired-description (if keybinding-icons-prettify
                                   (keybinding-icons--prettify-keybinding orig-key-description)
                                 orig-key-description))
         (binding-parts (delq "" (split-string desired-description " ")))
         (filler-width (- keybinding-icons--assumed-max-chords (length binding-parts))))
    ;; (message "orig-key-description: %s" orig-key-description)
    ;; (message "desired-description: %s" desired-description)
    ;; (message "binding-parts: %s" binding-parts)
    (concat (mapconcat (lambda (part)
                         (propertize part 'display (keybinding-icons--get-or-create-svg part)))
                       binding-parts)
            (propertize " " 'display `(space :width ,filler-width)))))

(defun keybinding-icons-iconify-binding-a (_ &rest args)
  ""
  (let* ((cand (car args))
         (sym (intern-soft cand))
         (char-array (and (commandp sym) (where-is-internal sym nil 'first-only))))
    (keybinding-icons--make-keybinding-icon-string char-array)))

(defun keybinding-icons-marginalia-annotate-command-a (cand)
  ""
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia-annotate-binding cand) :truncate 0.165 :face 'keybinding-icons-keybinding-face)
     ;; ((format "\n"))
     ((marginalia--function-doc sym) :truncate 1.0 :face 'marginalia-documentation))))

;; left TODO
;; - make sizes and stuff dynamic
;; - make colors and stuff better and more dynamic
;; - make a cache for the svgs
