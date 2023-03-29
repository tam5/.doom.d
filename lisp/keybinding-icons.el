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

(require 'cl-lib)

(defface keybinding-icons-keybinding-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for keybinding icons.")

(defvar keybinding-icons-prettify nil
  "If non-nil, prettify keybindings.")

;; (defun keybinding-icons--shift-and-lowercase-replacement (match-data)
;;   (let ((matched (match-string 0 match-data)))
;;     (concat "shift-" (downcase matched))))

(defun keybinding-icons--shift-and-lowercase-replacement (match)
  (message "match %s" match)
  (format "test %s-" match))

;; (dotimes (i (length keybinding-icons-prettifier-alist))
;;   (let ((key (car (nth i keybinding-icons-prettifier-alist)))
;;         (replacement (cdr (nth i keybinding-icons-prettifier-alist))))
;;     (message "key: %s, replacement: %s, type: %s" key replacement (type-of replacement))))


;; (functionp (cdr (nth 18 keybinding-icons-prettifier-alist)))

(defvar keybinding-icons-prettifier-alist
  '(("\\<SPC\\>" . "space")
    ("\\bC-\\([^ ]+\\)" . "⌃-\\1")
    ("\\bs-\\([^ ]+\\)" . "⌘-\\1")
    ("\\bM-\\([^ ]+\\)" . "⌥-\\1")
    ("\\<TAB\\>" . "⇥")
    ("\\<RET\\>" . "↩")
    ("\\<DEL\\>" . "⌫")
    ("\\<ESC\\>" . "⎋")
    ("<\\<delete\\>>" . "⌫")
    ("<\\<backspace\\>>" . "⌫")
    ("<\\<up\\>>" . "↑")
    ("<\\<down\\>>" . "↓")
    ("<\\<left\\>>" . "←")
    ("<\\<right\\>>" . "→")
    ("<\\<prior\\>>" . "⇟")
    ("<\\<next\\>>" . "⇞")
    ("<\\<escape\\>>" . "⎋")

    ("\\([[:upper:]]\\)" . (lambda (match) (message "match %s, type %s" match (type-of match)) (concat "shift")))

    ("<\\<\\([^>]+\\)\\>>" . "\\1"))
  "Alist of regexps to replace in keybindings.")

;; (defvar my/keybinding-icon-svg-cache (make-hash-table :test 'equal))
;;
(defvar keybinding-icons--assumed-max-chords 5)

(defun keybinding-icons--prettify-keybinding (keybinding)
  "Map parts of KEYBINDING to their corresponding strings
using `keybinding-icons-prettifier-alist`."
  (let ((result keybinding)
        (case-fold-search nil))
    (dolist (mapping keybinding-icons-prettifier-alist)
      (let* ((regex (car mapping))
            (replacement (if (functionp (cdr mapping))
                             (cdr mapping)
                           (lambda (&rest _) (cdr mapping)))))
        (setq result
              (replace-regexp-in-string
               regex
               (funcall replacement
                        (save-match-data
                          (when (and (string-match regex result) (match-string 1 result))
                            (message "-----------------------------")
                            (message "1 binding %s matched %s" result (match-string 1 result))
                            (message "2 binding %s matched %s" result (match-string 1 result))
                            (match-string 1 result))
                          ))
               result
               t))))
    result))

(let ((case-fold-search nil)
      (str "C-x Y D"))
  (when (string-match "\\([[:upper:]]\\)" str)
    (message "Matched: %s" (match-string 1 str))))


;; (my/string-match-all "\\([[:upper:]]\\)" "C-x Y")

(defun my/string-match-all (regexp string)
  (let ((start 0)
        matches)
    (save-match-data
      (while (string-match regexp string start)
        (let ((match (match-string 0 string)))
          (push match matches))
        (setq start (match-end 0))))
    (nreverse matches)))


;; (if (functionp replacement)
;;     (funcall replacement (match-string 1))
;;     ;; (lambda (_)
;;     ;;   (let ((all-matched-groups (cl-loop for i from 0 to (1- (/ (length (match-data)) 2))
;;     ;;                                   collect (match-string i result))))
;;     ;;     (funcall replacement matches result)))
;; replacement)
;; result t t)))))
;; result))

;; (key-description (kbd "C-c C-S-o"))


;; (match-data)

;; (string-match-p "\\b\\([[:upper:]]\\)" "C-x C-G")
;; ;; (match-data)

;; (replace-regexp-in-string "")

;; (s-match-strings-all "\\b\\([[:upper:]]\\)" "C-x C-G")

(defun example-function (str)
  (save-match-data
    (when (string-match "\\([[:upper:]]\\)" str)
      (message "Matched data: %s" (match-data))
      (message "Matched 0: %s" (match-string 0 str))
      (message "Matched 1: %s" (match-string 1 str))
      (message "Matched 2: %s" (match-string 2 str))
      (message "Matched 3: %s" (match-string 3 str))
      (message "Matched 4: %s" (match-string 4 str))
      (message "Matched 5: %s" (match-string 5 str))
      )))

;; (example-function "C-x J-G")

;; (defun keybinding-icons--prettify-keybinding (keybinding)
;;   "Map parts of KEYBINDING to their corresponding strings
;; using `keybinding-icons-prettifier-alist`."
;;   (let ((result keybinding)
;;         (case-fold-search nil))
;;     (dolist (mapping keybinding-icons-prettifier-alist)
;;       (let ((regexp (car mapping))
;;             (replacement (cdr mapping)))
;;         (setq result (replace-regexp-in-string
;;                       regexp
;;                       (if (stringp replacement)
;;                           replacement
;;                         (lambda (_)
;;                           (let ((all-matched-groups (loop for i from 0 to (1- (/ (length (match-data)) 2))
;;                                                           collect (match-string i result))))
;;                             (funcall replacement all-matched-groups))))
;;                       result t t))))
;;     result)

;;   (when (string-match-p regexp keybinding)
;;     (setq result (replace-regexp-in-string regexp replacement) result t t)))
;; result))))

;; (keybinding-icons--prettify-keybinding "C-x")

;; font-size .085
;; margin .4
;; padding .6

;;;;;;;;;;;;;;;
;; ;; (replace-regexp-in-string REGEXP REP STRING) ;; FIXEDCASE LITERAL SUBEXP START)
;; (replace-regexp-in-string "\\b[A-Z]\\b" (funcall (lambda (matches) (message "matches: %s" matches) (format "jh"))
;;                                                  (message "match data %s" (s-match-strings-all))

;;                                                  ) "G")

;; (let ((case-fold-search nil))
;;   (s-match "\\bs-\\([^ ]+\\)" "s-j K"))

;;;;;;;;;;;;;;;

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
               :height 1
               ;; :height 0.84
               :font-size 15
               ;; font size gets rounded anyway
               ;; ascent is used from the font info (svg-ascent is center)
               ;; height i guess should be something that gives a whole number to the font size?

               :font-weight 'bold
               :font-family "MesloLGL Nerd Font"

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

;; (keybinding-icons--debug-svgs "C-c C-c")

;; Open name: -*-MesloLGL Nerd Font-regular-normal-normal-*-14-*-*-*-p-0-iso10646-1
;; Full name: MesloLGL Nerd Font:pixelsize=14:weight=regular:slant=normal:width=normal:spacing=0:scalable=true
;; Size: 14
;; Height: 22 ---- the text char height is 16 pixels
;; Baseline offset: 0
;; Relative compose: 0
;; Default ascent: 0
;; Max width: 8
;; Ascent: 16
;; Descent: 6
;; Space width: 8
;; Average width: 8
;; Filename: nil
;; Capability: nil

;; Open name: -*-MesloLGL Nerd Font-regular-normal-normal-*-19-*-*-*-p-0-iso10646-1
;; Full name: MesloLGL Nerd Font:pixelsize=19:weight=regular:slant=normal:width=normal:spacing=0:scalable=true
;; Size: 19
;; Height: 29 ---- the text char height is 22 pixels
;; Baseline offset: 0
;; Relative compose: 0
;; Default ascent: 0
;; Max width: 11
;; Ascent: 21
;; Descent: 8
;; Space width: 11
;; Average width: 11
;; Filename: nil
;; Capability: nil


;; The returned value is a vector of 14 elements:
;;   [ OPENED-NAME FULL-NAME SIZE HEIGHT BASELINE-OFFSET RELATIVE-COMPOSE
;;     DEFAULT-ASCENT MAX-WIDTH ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
;;     FILENAME CAPABILITY ]

;; foreground: unspecified
;; background: #2a2e36
;; crop-left: nil
;; crop-right: nil
;; alignment: 0.5
;; stroke: 0
;; height: 1
;; radius: 3
;; margin: 0
;; padding: 0
;; font-size-o: 19.0
;; font-family: MesloLGL Nerd Font
;; font-weight: 700
;; txt-char-width: 11
;; txt-char-height: 22
;; font-info: [-*-MesloLGL Nerd Font-regular-normal-normal-*-19-*-*-*-p-0-iso10646-1 MesloLGL Nerd Font:pixelsize=19:weight=regular:slant=normal:width=normal:spacing=0:scalable=true 19 29 0 0 0 11 21 8 11 11 nil nil]
;; font-size: 19
;; ascent: 21
;; tag-char-width: 11
;; tag-width: 33
;; tag-height: 22
;; svg-width: 33
;; svg-height: 22
;; svg-ascent: center
;; tag-x: 0.0
;; text-x: 0.0
;; text-y: 21
;; tag-x: 0.0
;; tag-width: 33
;; text-x: 0.0
;; tag-width: 33
;; text-x: 0.0



;; 14.2 * 2x = 17
;; 14.2 * x = 17
;; 17/14.2 = 1.2 or 0.6
;; 17 * .94 = 16
;; 15.96x = 15
;; 15/15.96 = 0.94
;; .94 * 18.48 = 17.3
;; foreground: unspecified
;; background: #2a2e36
;; crop-left: nil
;; crop-right: nil
;; alignment: 0.5
;; stroke: 0
;; height: 0.84
;; radius: 3
;; margin: 0.4
;; padding: 0.6
;; font-size-o: 15.96
;; font-size: 15
;; font-family: MesloLGL Nerd Font
;; font-weight: 700
;; txt-char-width: 11
;; txt-char-height: 22 pixels
;; font-info: [-*-MesloLGL Nerd Font-regular-normal-normal-*-15-*-*-*-p-0-iso10646-1 MesloLGL Nerd Font:pixelsize=15:weight=regular:slant=normal:width=normal:spacing=0:scalable=true 15 23 0 0 0 9 17 6 9 9 nil nil]
;; font-size: 15
;; ascent: 17
;; tag-char-width: 9
;; tag-width: 17.6
;; tag-height: 18.48

;; svg-width: 22.0
;; svg-height: 18.48 pixels
;;
;; svg-ascent: center
;; tag-x: 2.1999999999999993
;; text-x: 6.5
;; text-y: 17
;; tag-x: 2.1999999999999993
;; tag-width: 17.6
;; text-x: 6.5
;; tag-width: 17.6
;; text-x: 6.5



(defun keybinding-icons-toggle-prettify ()
  "Toggle `keybinding-icons-prettify'."
  (interactive)
  (setq keybinding-icons-prettify (not keybinding-icons-prettify)))

(defun keybinding-icons--make-keybinding-icon-string (char-array)
  ""
  (let* ((orig-key-description (key-description char-array))
         (descired-description (if keybinding-icons-prettify
                                   (keybinding-icons--prettify-keybinding orig-key-description)
                                 orig-key-description))
         (binding-parts (delq "" (split-string descired-description " ")))
         (filler-width (- keybinding-icons--assumed-max-chords (length binding-parts))))
    (concat (mapconcat (lambda (part)
                         (propertize part 'display (keybinding-icons--get-or-create-svg part)))
                       binding-parts)
            (propertize " " 'display `(space :width ,filler-width)))))

(defun keybinding-icons-iconify-binding-a (_ &rest args)
  ""
  (let* ((cand (car args))
         (sym (intern-soft cand))
         (char-array (and (commandp sym) (where-is-internal sym nil 'first-only))))
    ;; (message "candidate: %s" cand)
    ;; (message "sym: %s" sym)
    ;; (message "thing: %s" (where-is-internal sym nil 'first-only))
    ;; (message "keybinding: %s" char-array)
    (keybinding-icons--make-keybinding-icon-string char-array)))

;; (char-to-string 32)
;; (char-to-string 9)
;; (char-to-string 82)

;; (key-binding (vector 32 9 82))



;; (map! "C-S-y" #'aritest) ;; 33554457
;; (map! "C-Y" #'aritest)

;; (defun aritest-3 () (interactive))

;; (map! "C-x Z Y" #'aritest-3)

;; (vector "C-S-y")
;; (where-is-internal 'aritest)

;; (kbd "C-S-y") ;; 33554457
;; (key-description (vector 33554457)) ;; C-S-y

;; (defun key-description-to-vector (key-description)
;;   (let* ((case-fold-search nil)
;;          (key-description (replace-regexp-in-string
;;                            "\\bC-\\([[:upper:]]\\)"
;;                            (lambda (_)
;;                              (format "C-%c" (+ 96 (string-to-char (match-string 1)))))
;;                            key-description)))
;;     (kbd key-description)))


;; (kbd "C-Y") ;; 
;; (kbd "C-x Y") ;; Y
;; (key-description "") ;; C-y
;; (key-description-to-vector "C-x Y") ;; 24 67109019

;; (key-description (vector  24 67109019)) ;; C-x C-


;; (kbd "SPC g G") ;; gG
;; (key-description (kbd "SPC g G")) ;; SPC g G

;; (where-is-internal 'magit-status-here) ;; 32 103 71
;; (key-description (vector 32 103 71)) ;; SPC g G
;; (kbd "SPC g G") ;; gG
;; (key-description (kbd "SPC g G")) ;; SPC g G

(defun keybinding-icons-marginalia-annotate-command-a (cand)
  ""
  (when-let (sym (intern-soft cand))
    (marginalia--fields
     ((marginalia-annotate-binding cand) :truncate 0.165 :face 'keybinding-icons-keybinding-face)
     ((marginalia--function-doc sym) :truncate 1.0 :face 'marginalia-documentation))))

;; left TODO
;; - make sizes and stuff dynamic
;; - make colors and stuff better and more dynamic
;; - make a cache for the svgs
