;;; themes/doom-github-dark-dimmed-theme.el -*- lexical-binding: t; -*-
;; Added:
;; Author:
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-github-dark-dimmed-theme nil
  "Options for the `doom-github-dark-dimmed' theme."
  :group 'doom-themes)

(defcustom doom-github-dark-dimmed-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-github-dark-dimmed-theme
  :type 'boolean)

(defcustom doom-github-dark-dimmed-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-github-dark-dimmed-theme
  :type 'boolean)

(defcustom doom-github-dark-dimmed-brighter-text nil
  "If non-nil, default text will be brighter."
  :group 'doom-github-dark-dimmed-theme
  :type 'boolean)

(defcustom doom-github-dark-dimmed-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-github-dark-dimmed-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-github-dark-dimmed
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  ((bg         '("#24292e" "#002b36" "brightwhite" ))
   (fg         (if doom-github-dark-dimmed-brighter-text
                   '("#BBBBBB" "#BBBBBB" "brightwhite")
                 '("#e1e4e8" "#839496" "brightwhite")))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1f2428" "#00212B" "white"       ))
   (fg-alt     '("#657b83" "#657b83" "white"       ))

   ;; dark bg: #1f2428
   ;; editor bg: #24292e
   ;; editor hl line: #2B3036
   ;; indent guide? #2E363D
   ;;
   ;; (doom-lighten )
   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#073642" "#073642" "black"       ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#405A61" "#405A61" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue) ;; ?? todo
   (vertical-bar   "#1B1F23")
   (selection      dark-blue) ;; ?? nav-flash?
   (builtin        "#f97583")
   ;; (comments       (if doom-github-dark-dimmed-brighter-comments blue base5))
   (comments       "#6a737d")
   (doc-comments   "#6a737d")
   (constants      "#79b8ff")
   (functions      "#b392f0")
   (keywords       "#f97583")
   (methods        "#b392f0") ;; ????
   (operators      "#e1e4e8") ;; ???? like a ' in elisp
   (type           "#79b8ff")
   (strings        "#9ecbff")
   (variables      "#e1e4e8")
   (numbers        "#79b8ff")
   (region         "#2A4668")
   ;; (region-inactive         base0) ;;; TOOD
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    "#2471CB")
   (vc-added       "#28A745")
   (vc-deleted     "#EA4A5A")

   )
   ;; custom categories
  ;;  (-modeline-bright doom-github-dark-dimmed-brighter-modeline)
  ;;  (-modeline-pad
  ;;   (when doom-github-dark-dimmed-padded-modeline
  ;;     (if (integerp doom-github-dark-dimmed-padded-modeline) doom-github-dark-dimmed-padded-modeline 4)))

  ;;  (modeline-fg     nil)
  ;;  (modeline-fg-alt base5)

  ;;  (modeline-bg
  ;;   (if -modeline-bright
  ;;       base3
  ;;     `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
  ;;  (modeline-bg-alt
  ;;   (if -modeline-bright
  ;;       base3
  ;;     `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
  ;;  (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
  ;;  (modeline-bg-inactive-alt (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  ;; (((font-lock-comment-face &override)
  ;;   :background (if doom-github-dark-dimmed-brighter-comments (doom-lighten bg 0.05)))
  ;;  ((font-lock-keyword-face &override) :weight 'bold)
  ;;  ((font-lock-constant-face &override) :weight 'bold)
  ;;  ((line-number &override) :foreground base4)
  ;;  ((line-number-current-line &override) :foreground fg)
  ;;  (mode-line
  ;;   :background modeline-bg :foreground modeline-fg
  ;;   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
  ;;  (mode-line-inactive
  ;;   :background modeline-bg-inactive :foreground modeline-fg-alt
  ;;   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
  ;;  (mode-line-emphasis :foreground (if -modeline-bright base8 highlight)))

  ;;  ;;;; centaur-tabs
  ;;  (centaur-tabs-active-bar-face :background blue)
  ;;  (centaur-tabs-modified-marker-selected
  ;;   :inherit 'centaur-tabs-selected :foreground blue)
  ;;  (centaur-tabs-modified-marker-unselected
  ;;   :inherit 'centaur-tabs-unselected :foreground blue)
  ;;  ;;;; company
  ;;  (company-tooltip-selection     :background dark-cyan)
  ;;  ;;;; css-mode <built-in> / scss-mode
  ;;  (css-proprietary-property :foreground orange)
  ;;  (css-property             :foreground green)
  ;;  (css-selector             :foreground blue)
  ;;  ;;;; doom-modeline
  ;;  (doom-modeline-bar :background blue)
  ;;  (doom-modeline-evil-emacs-state  :foreground magenta)
  ;;  (doom-modeline-evil-insert-state :foreground blue)
  ;;  ;;;; elscreen
  ;;  (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
  ;;  ;;;; helm
  ;;  (helm-selection :inherit 'bold
  ;;                  :background selection
  ;;                  :distant-foreground bg
  ;;                  :extend t)
  ;;  ;;;; markdown-mode
  ;;  (markdown-markup-face :foreground base5)
  ;;  (markdown-header-face :inherit 'bold :foreground red)
  ;;  (markdown-url-face    :foreground teal :weight 'normal)
  ;;  (markdown-reference-face :foreground base6)
  ;;  ((markdown-bold-face &override)   :foreground fg)
  ;;  ((markdown-italic-face &override) :foreground fg-alt)
  ;;  ;;;; outline <built-in>
  ;;  ((outline-1 &override) :foreground blue)
  ;;  ((outline-2 &override) :foreground green)
  ;;  ((outline-3 &override) :foreground teal)
  ;;  ((outline-4 &override) :foreground (doom-darken blue 0.2))
  ;;  ((outline-5 &override) :foreground (doom-darken green 0.2))
  ;;  ((outline-6 &override) :foreground (doom-darken teal 0.2))
  ;;  ((outline-7 &override) :foreground (doom-darken blue 0.4))
  ;;  ((outline-8 &override) :foreground (doom-darken green 0.4))
  ;;  ;;;; org <built-in>
  ;;  ((org-block &override) :background base0)
  ;;  ((org-block-begin-line &override) :foreground comments :background base0)
  ;;  ;;;; solaire-mode
  ;;  (solaire-mode-line-face
  ;;   :inherit 'mode-line
  ;;   :background modeline-bg-alt
  ;;   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
  ;;  (solaire-mode-line-inactive-face
  ;;   :inherit 'mode-line-inactive
  ;;   :background modeline-bg-inactive-alt
  ;;   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme face overrides-
  ((bold        :weight 'bold :foreground (if bold 'unspecified base8))
    (bold-italic :inherit '(bold italic))
    (italic      :slant  'italic)
    (escape-glyph :foreground cyan)
    (default :background bg :foreground fg)
    (fringe  :inherit 'default :foreground base4)
    (region               :background region :distant-foreground (doom-darken fg 0.2) :extend t)
    (highlight            :background highlight :foreground base0 :distant-foreground base8)
    (cursor               :background highlight)
    (shadow               :foreground base5)
    (minibuffer-prompt    :foreground highlight)
    (tooltip              :background bg-alt :foreground fg)
    (secondary-selection  :background grey :extend t)
    (lazy-highlight
     (&dark  :background (doom-darken highlight 0.3)   :foreground base8 :distant-foreground base0 :weight 'bold)
     (&light :background (doom-blend bg highlight 0.7) :foreground base0 :distant-foreground base8))
    (match                :foreground green      :background base0 :weight 'bold)
    (trailing-whitespace  :background red)
    (nobreak-space        :inherit 'escape-glyph :underline t)
    (vertical-border      :background vertical-bar :foreground vertical-bar)
    (link                 :foreground highlight :underline t :weight 'bold)
    (error   :foreground error)
    (warning :foreground warning)
    (success :foreground success)

    ;; "#1f2428"

    ;;;; font-lock-* faces
    (font-lock-builtin-face              :foreground builtin)
    (font-lock-comment-face              :foreground comments)
    (font-lock-comment-delimiter-face    :inherit 'font-lock-comment-face)
    (font-lock-doc-face                  :inherit 'font-lock-comment-face :foreground doc-comments)
    (font-lock-constant-face             :foreground constants)
    (font-lock-function-name-face        :foreground functions)
    (font-lock-keyword-face              :foreground keywords)
    (font-lock-string-face               :foreground strings)
    (font-lock-type-face                 :foreground type)
    (font-lock-variable-name-face        :foreground variables)
    (font-lock-warning-face              :inherit 'warning)
    (font-lock-negation-char-face        :inherit 'bold :foreground operators)
    (font-lock-preprocessor-face         :inherit 'bold :foreground operators)
    (font-lock-preprocessor-char-face    :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)
    ;;;; mode-line / header-line
    (mode-line           :background bg     :foreground fg     :distant-foreground bg)
    (mode-line-active    :inherit 'mode-line)
    (mode-line-inactive  :background bg-alt :foreground fg-alt :distant-foreground bg-alt)
    (mode-line-emphasis  :foreground highlight :distant-foreground bg)
    (mode-line-highlight :inherit 'highlight :distant-foreground bg)
    (mode-line-buffer-id :weight 'bold)
    (header-line :inherit 'mode-line)
    (header-line-highlight :inherit 'mode-line-highlight)
    ;;;; tab-line/tab-bar (Emacs 27+)
    (tab-line :background bg-alt :foreground bg-alt)
    (tab-line-tab :background bg :foreground fg)
    (tab-line-tab-inactive :inherit 'tab-line-tab :background bg-alt :foreground fg-alt)
    (tab-line-tab-inactive-alternate :inherit 'tab-line-tab-inactive)
    (tab-line-tab-current :background bg :foreground fg)
    ;; (tab-line-special )
    (tab-line-highlight :inherit 'tab-line-tab)
    (tab-line-close-highlight :foreground highlight)
    ((tab-bar &inherit tab-line))
    ((tab-bar-tab &inherit tab-line-tab))
    ((tab-bar-tab-inactive &inherit tab-line-tab-inactive))
    ;;;; Line numbers
    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground "#444D56" :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)
    (line-number-current-line
     :inherit '(hl-line default)
     :foreground fg :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)

    ;;;; --- Package faces ----------------------
    ;; What follows are faces for all the packages doom-themes explicitly
    ;; supports. Headings are formatted as such:
    ;;
    ;;   PACKAGE [<built-in>] [<modes:some-mode[, ...]>]
    ;;
    ;; The purpose of this is to make it easy to jump to via `imenu', or search
    ;; for with isearch, swiper, etc.
    ;;;; agda-mode <modes:agda2-mode>
    (agda2-highlight-keyword-face                 :inherit 'font-lock-keyword-face)
    (agda2-highlight-string-face                  :inherit 'font-lock-string-face)
    (agda2-highlight-number-face                  :inherit 'font-lock-string-face)
    (agda2-highlight-symbol-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-primitive-type-face          :inherit 'font-lock-type-face)
    (agda2-highlight-bound-variable-face          :inherit 'font-lock-variable-name-face)
    (agda2-highlight-inductive-constructor-face   :inherit 'font-lock-type-face)
    (agda2-highlight-coinductive-constructor-face :inherit 'font-lock-type-face)
    (agda2-highlight-datatype-face                :inherit 'font-lock-type-face)
    (agda2-highlight-field-face                   :inherit 'font-lock-type-face)
    (agda2-highlight-function-face                :inherit 'font-lock-function-name-face)
    (agda2-highlight-module-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-postulate-face               :inherit 'font-lock-type-face)
    (agda2-highlight-primitive-face               :inherit 'font-lock-type-face)
    (agda2-highlight-macro-face                   :inherit 'font-lock-function-name-face)
    (agda2-highlight-record-face                  :inherit 'font-lock-type-face)
    (agda2-highlight-error-face                   :inherit 'font-lock-warning-face)
    (agda2-highlight-dotted-face                  :inherit 'font-lock-variable-name-face)
    (agda2-highlight-unsolved-meta-face           :inherit 'font-lock-warning-face)
    (agda2-highlight-unsolved-constraint-face     :inherit 'font-lock-warning-face)
    (agda2-highlight-termination-problem-face     :inherit 'font-lock-warning-face)
    (agda2-highlight-positivity-problem-face      :inherit 'font-lock-warning-face)
    (agda2-highlight-incomplete-pattern-face      :inherit 'font-lock-warning-face)
    (agda2-highlight-typechecks-face              :inherit 'font-lock-warning-face)
    ;;;; auctex <modes:latex-mode>
    (font-latex-bold-face         :inherit 'bold)
    (font-latex-italic-face       :inherit 'italic)
    (font-latex-math-face         :foreground blue)
    (font-latex-sectioning-0-face :foreground blue    :weight 'ultra-bold)
    (font-latex-sectioning-1-face :foreground magenta :weight 'semi-bold)
    (font-latex-sectioning-2-face :foreground violet  :weight 'semi-bold)
    (font-latex-sectioning-3-face :foreground (doom-lighten blue 0.3)    :weight 'semi-bold)
    (font-latex-sectioning-4-face :foreground (doom-lighten magenta 0.3) :weight 'semi-bold)
    (font-latex-sectioning-5-face :foreground (doom-lighten violet 0.3)  :weight 'semi-bold)
    (font-latex-script-char-face  :foreground dark-blue)
    (font-latex-string-face       :inherit 'font-lock-string-face)
    (font-latex-warning-face      :inherit 'font-lock-warning-face)
    (font-latex-verbatim-face     :inherit 'fixed-pitch :foreground violet :slant 'italic)
    (TeX-error-description-error    :inherit 'error   :weight 'bold)
    (TeX-error-description-warning  :inherit 'warning :weight 'bold)
    (TeX-error-description-tex-said :inherit 'success :weight 'bold)
    ;;;; alert
    (alert-high-face         :inherit bold :foreground warning)
    (alert-low-face          :foreground grey)
    (alert-moderate-face     :inherit bold :foreground fg-alt)
    (alert-trivial-face      :foreground doc-comments)
    (alert-urgent-face       :inherit bold :foreground error)
    ;;;; all-the-icons
    (all-the-icons-blue       :foreground blue)
    (all-the-icons-blue-alt   :foreground teal)
    (all-the-icons-cyan       :foreground cyan)
    (all-the-icons-cyan-alt   :foreground cyan)
    (all-the-icons-dblue      :foreground dark-blue)
    (all-the-icons-dcyan      :foreground dark-cyan)
    (all-the-icons-dgreen     :foreground (doom-darken green 0.3))
    (all-the-icons-dmaroon    :foreground (doom-darken magenta 0.3))
    (all-the-icons-dorange    :foreground (doom-darken orange 0.3))
    (all-the-icons-dpink      :foreground (doom-lighten red 0.15))
    (all-the-icons-dpurple    :foreground (doom-darken violet 0.3))
    (all-the-icons-dred       :foreground (doom-darken red 0.3))
    (all-the-icons-dsilver    :foreground (doom-lighten grey 0.1))
    (all-the-icons-dyellow    :foreground (doom-darken yellow 0.3))
    (all-the-icons-green      :foreground green)
    (all-the-icons-lblue      :foreground (doom-lighten blue 0.3))
    (all-the-icons-lcyan      :foreground (doom-lighten cyan 0.3))
    (all-the-icons-lgreen     :foreground (doom-lighten green 0.3))
    (all-the-icons-lmaroon    :foreground (doom-lighten magenta 0.3))
    (all-the-icons-lorange    :foreground (doom-lighten orange 0.3))
    (all-the-icons-lpink      :foreground (doom-lighten red 0.55))
    (all-the-icons-lpurple    :foreground (doom-lighten violet 0.3))
    (all-the-icons-lred       :foreground (doom-lighten red 0.3))
    (all-the-icons-lsilver    :foreground (doom-lighten grey 0.7))
    (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
    (all-the-icons-maroon     :foreground magenta)
    (all-the-icons-orange     :foreground orange)
    (all-the-icons-pink       :foreground (doom-lighten red 0.35))
    (all-the-icons-purple     :foreground violet)
    (all-the-icons-purple-alt :foreground (doom-blend violet grey 0.15))
    (all-the-icons-red        :foreground red)
    (all-the-icons-red-alt    :foreground (doom-blend red grey 0.15))
    (all-the-icons-silver     :foreground (doom-lighten grey 0.45))
    (all-the-icons-yellow     :foreground yellow)
    ;;;; all-the-icons-dired
    (all-the-icons-dired-dir-face    :foreground doc-comments)
    ;;;; annotate
    (annotate-annotation           :background (doom-blend highlight bg 0.1) :foreground doc-comments)
    (annotate-annotation-secondary :background (doom-blend green bg 0.1)     :foreground doc-comments)
    (annotate-highlight            :background (doom-blend highlight bg 0.1) :underline highlight)
    (annotate-highlight-secondary  :background (doom-blend green bg 0.1)     :underline green)
    ;;;; ansi-color <built-in>
    (ansi-color-black          :foreground bg      :background bg)
    (ansi-color-red            :foreground red     :background red)
    (ansi-color-green          :foreground green   :background green)
    (ansi-color-yellow         :foreground yellow  :background yellow)
    (ansi-color-blue           :foreground blue    :background blue)
    (ansi-color-magenta        :foreground magenta :background magenta)
    (ansi-color-cyan           :foreground cyan    :background cyan)
    (ansi-color-white          :foreground fg      :background fg)
    (ansi-color-bright-black   :foreground base0   :background base2)
    (ansi-color-bright-red     :foreground (doom-lighten red 0.15)     :background (doom-lighten red 0.15))
    (ansi-color-bright-green   :foreground (doom-lighten green 0.15)   :background (doom-lighten green 0.15))
    (ansi-color-bright-yellow  :foreground (doom-lighten yellow 0.15)  :background (doom-lighten yellow 0.15))
    (ansi-color-bright-blue    :foreground (doom-lighten blue 0.15)    :background (doom-lighten blue 0.15))
    (ansi-color-bright-magenta :foreground (doom-lighten magenta 0.15) :background (doom-lighten magenta 0.15))
    (ansi-color-bright-cyan    :foreground (doom-lighten cyan 0.15)    :background (doom-lighten cyan 0.15))
    (ansi-color-bright-white   :foreground base8   :background base8)
    ;;;; anzu
    (anzu-replace-highlight :background base0 :foreground red   :weight 'bold :strike-through t)
    (anzu-replace-to        :background base0 :foreground green :weight 'bold)
    ;;;; avy
    (avy-background-face :foreground comments)
    (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (avy-lead-face-0
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.3))
     (&light :background (doom-darken highlight 0.3)))
    (avy-lead-face-1
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.6))
     (&light :background (doom-darken highlight 0.6)))
    (avy-lead-face-2
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.9))
     (&light :background (doom-darken highlight 0.9)))
    ;;;; bookmark
    (bookmark-face :background (doom-blend highlight bg 0.1) :extend t)
    ;;;; bookmark+
    (bmkp-*-mark :foreground bg :background yellow)
    (bmkp->-mark :foreground yellow)
    (bmkp-D-mark :foreground bg :background red)
    (bmkp-X-mark :foreground red)
    (bmkp-a-mark :background red)
    (bmkp-bad-bookmark :foreground bg :background yellow)
    (bmkp-bookmark-file :foreground violet :background bg-alt)
    (bmkp-bookmark-list :background bg-alt)
    (bmkp-buffer :foreground blue)
    (bmkp-desktop :foreground bg :background violet)
    (bmkp-file-handler :background red)
    (bmkp-function :foreground green)
    (bmkp-gnus :foreground orange)
    (bmkp-heading :foreground yellow)
    (bmkp-info :foreground cyan)
    (bmkp-light-autonamed :foreground bg-alt :background cyan)
    (bmkp-light-autonamed-region :foreground bg-alt :background red)
    (bmkp-light-fringe-autonamed :foreground bg-alt :background violet)
    (bmkp-light-fringe-non-autonamed :foreground bg-alt :background green)
    (bmkp-light-mark :foreground bg :background cyan)
    (bmkp-light-non-autonamed :foreground bg :background violet)
    (bmkp-light-non-autonamed-region :foreground bg :background red)
    (bmkp-local-directory :foreground bg :background violet)
    (bmkp-local-file-with-region :foreground yellow)
    (bmkp-local-file-without-region :foreground comments)
    (bmkp-man :foreground violet)
    (bmkp-no-jump :foreground comments)
    (bmkp-no-local :foreground yellow)
    (bmkp-non-file :foreground green)
    (bmkp-remote-file :foreground orange)
    (bmkp-sequence :foreground blue)
    (bmkp-su-or-sudo :foreground red)
    (bmkp-t-mark :foreground violet)
    (bmkp-url :foreground blue :underline t)
    (bmkp-variable-list :foreground green)
    ;;;; calfw
    (cfw:face-title              :foreground blue                     :weight 'bold :height 2.0 :inherit 'variable-pitch)
    (cfw:face-header             :foreground (doom-blend blue bg 0.8) :weight 'bold)
    (cfw:face-sunday             :foreground (doom-blend red bg 0.8)  :weight 'bold)
    (cfw:face-saturday           :foreground (doom-blend red bg 0.8)  :weight 'bold)
    (cfw:face-holiday            :background bg-alt :weight 'bold)
    (cfw:face-grid               :foreground vertical-bar)
    (cfw:face-periods            :foreground yellow)
    (cfw:face-toolbar)
    (cfw:face-toolbar-button-off :foreground base6                    :weight 'bold             :inherit 'variable-pitch)
    (cfw:face-toolbar-button-on  :foreground blue                     :weight 'bold             :inherit 'variable-pitch)
    (cfw:face-default-content    :foreground fg)
    (cfw:face-day-title          :foreground fg                       :weight 'bold)
    (cfw:face-today-title        :foreground bg  :background blue     :weight 'bold)
    (cfw:face-default-day                                             :weight 'bold)
    (cfw:face-today              :weight 'bold)
    (cfw:face-annotation         :foreground violet)
    (cfw:face-disable            :foreground grey)
    (cfw:face-select             :background region)
    ;;;; centaur-tabs
    ((centaur-tabs-default &inherit tab-bar) :box nil)
    ((centaur-tabs-selected &inherit tab-bar-tab) :box nil)
    ((centaur-tabs-unselected &inherit tab-bar-tab-inactive) :box nil)
    (centaur-tabs-selected-modified   :background bg :foreground teal)
    (centaur-tabs-unselected-modified :background bg-alt :foreground teal)
    (centaur-tabs-active-bar-face :background (if (bound-and-true-p -modeline-bright) modeline-bg highlight))
    (centaur-tabs-modified-marker-selected
     :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight)
     :inherit 'centaur-tabs-selected)
    (centaur-tabs-modified-marker-unselected
     :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight)
     :inherit 'centaur-tabs-unselected)
    ;;;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight :distant-foreground base0 :weight 'bold)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (company-tooltip-search-selection :background (doom-darken selection 0.25))
    (company-tooltip-selection  :background selection :weight 'bold)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground violet :distant-foreground bg)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                              :foreground comments)
    (company-preview-common     :background base3 :foreground highlight)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)
    ;;;; company-box
    (company-box-candidate :foreground fg)
    ;;;; corfu
    (corfu-default :inherit 'tooltip)
    (corfu-current :background bg :foreground fg)
    ;;;; circe
    (circe-fool :foreground doc-comments)
    (circe-highlight-nick-face :weight 'bold :foreground constants)
    (circe-prompt-face :weight 'bold :foreground highlight)
    (circe-server-face :foreground comments)
    (circe-my-message-face :weight 'bold)
    ;;;; cperl <built-in>
    (cperl-array-face          :weight 'bold :inherit 'font-lock-variable-name-face)
    (cperl-hash-face           :weight 'bold :slant 'italic :inherit 'font-lock-variable-name-face)
    (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)
    ;;;; compilation <built-in>
    (compilation-column-number  :inherit 'font-lock-comment-face)
    (compilation-line-number    :foreground highlight)
    (compilation-error   :inherit 'error   :weight 'bold)
    (compilation-warning :inherit 'warning :slant 'italic)
    (compilation-info    :inherit 'success)
    (compilation-mode-line-exit :inherit 'compilation-info)
    (compilation-mode-line-fail :inherit 'compilation-error)
    ;;;; custom <built-in>
    (custom-button                  :foreground blue   :background bg     :box '(:line-width 1 :style none))
    (custom-button-unraised         :foreground violet :background bg     :box '(:line-width 1 :style none))
    (custom-button-pressed-unraised :foreground bg     :background violet :box '(:line-width 1 :style none))
    (custom-button-pressed          :foreground bg     :background blue   :box '(:line-width 1 :style none))
    (custom-button-mouse            :foreground bg     :background blue   :box '(:line-width 1 :style none))
    (custom-variable-button         :foreground green  :underline t)
    (custom-saved                   :foreground green  :background (doom-blend green bg 0.2) :bold bold)
    (custom-comment                 :foreground fg     :background region)
    (custom-comment-tag             :foreground grey)
    (custom-modified                :foreground blue   :background (doom-blend blue bg 0.2))
    (custom-variable-tag            :foreground magenta)
    (custom-visibility              :foreground blue   :underline 'unspecified)
    (custom-group-subtitle          :foreground red)
    (custom-group-tag               :foreground violet)
    (custom-group-tag-1             :foreground blue)
    (custom-set                     :foreground yellow :background bg)
    (custom-themed                  :foreground yellow :background bg)
    (custom-invalid                 :foreground red    :background (doom-blend red bg 0.2))
    (custom-variable-obsolete       :foreground grey   :background bg)
    (custom-state                   :foreground green  :background (doom-blend green bg 0.2))
    (custom-changed                 :foreground blue   :background bg)
    ;;;; cider
    ;; (cider-stacktrace-error-class-face :inherit 'font-lock-warning-face)
    ;; (cider-stacktrace-error-message-face :inherit 'font-lock-doc-face)
    ;; (cider-stacktrace-filter-active-face :inherit 'button :underline t :weight 'normal)
    ;; (cider-stacktrace-filter-inactive-face :inherit 'cider-stacktrace-filter-active-face :underline nil)
    ;; (cider-stacktrace-face :inherit 'default)
    ;; (cider-stacktrace-ns-face :inherit 'font-lock-comment-face)
    ;; (cider-stacktrace-fn-face :inherit 'default :weight 'bold)
    ;; (cider-docview-emphasis-face :inherit 'default :underline t)
    ;; (cider-docview-strong-face :inherit 'default :underline t :weight 'bold)
    ;; (cider-docview-literal-face :inherit 'font-lock-string-face)
    ;; (cider-docview-table-border-face :inherit 'shadow)
    (cider-debug-code-overlay-face :background base3)
    ;; (cider-debug-prompt-face :inherit font-lock-builtin-face :underline t)
    (cider-enlightened-face
     :inherit 'cider-result-overlay-face :box `(:color ,orange :line-width -1))
    (cider-enlightened-local-face :foreground orange :weight 'bold)
    ;; (cider-repl-prompt-face :inherit 'font-lock-keyword-face)
    ;; (cider-repl-stdout-face :inherit 'font-lock-string-face)
    ;; (cider-repl-stderr-face :inherit 'font-lock-warning-face)
    ;; (cider-repl-input-face :weight 'bold)
    ;; (cider-repl-result-face )
    (cider-result-overlay-face :background base3 :box `(:line-width -1 :color base5))
    (cider-fringe-good-face    :foreground green)
    (cider-deprecated-face     :background (doom-blend bg yellow 0.8))
    (cider-instrumented-face   :background (doom-blend bg red 0.8))
    (cider-traced-face         :background (doom-blend bg cyan 0.8))
    ;; (cider-reader-conditional-face :inherit 'font-lock-comment-face)
    (cider-error-highlight-face
     `((((supports :underline (:style wave)))
        (:inherit unspecified :underline (:style wave :color ,(car error))))
       (t (:inherit font-lock-warning-face :underline t))))
    (cider-warning-highlight-face
     `((((supports :underline (:style wave)))
        (:underline (:style wave :color ,(car warning)) :inherit unspecified))
       (t (:inherit font-lock-warning-face :underline (:color ,(car warning))))))
    (cider-test-failure-face :background (doom-blend bg error 0.7))
    (cider-test-error-face   :background orange)
    (cider-test-success-face
     (&light :foreground base0 :background green)
     (&dark  :foreground green :background base0))
    ;;;; diff-hl
    (diff-hl-change :foreground vc-modified :background vc-modified)
    (diff-hl-delete :foreground vc-deleted :background vc-deleted)
    (diff-hl-insert :foreground vc-added :background vc-added)
    ;;;; diff-mode <built-in>
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-context
     (&dark  :foreground (doom-darken fg 0.12))
     (&light :foreground (doom-lighten fg 0.12)))
    (diff-removed :foreground red :background base3)
    (diff-header  :foreground cyan)
    (diff-file-header :foreground blue)
    (diff-hunk-header :foreground violet)
    (diff-refine-added   :inherit 'diff-added :inverse-video t)
    (diff-refine-changed :inherit 'diff-changed :inverse-video t)
    (diff-refine-removed :inherit 'diff-removed :inverse-video t)
    ;;;; dired <built-in>
    (dired-directory  :foreground builtin)
    (dired-ignored    :foreground comments)
    (dired-flagged    :foreground red)
    (dired-header     :foreground blue :weight 'bold)
    (dired-mark       :foreground orange :weight 'bold)
    (dired-marked     :foreground magenta :weight 'bold :inverse-video t)
    (dired-perm-write :foreground fg :underline t)
    (dired-symlink    :foreground cyan :weight 'bold)
    (dired-warning    :foreground warning)
    ;;;; dired+
    (diredp-file-name              :foreground base8)
    (diredp-dir-name               :foreground base8 :weight 'bold)
    (diredp-ignored-file-name      :foreground base5)
    (diredp-compressed-file-suffix :foreground base5)
    (diredp-symlink                :foreground violet)
    (diredp-dir-heading            :foreground blue  :weight 'bold)
    (diredp-file-suffix            :foreground violet)
    (diredp-read-priv              :foreground magenta)
    (diredp-write-priv             :foreground green)
    (diredp-exec-priv              :foreground yellow)
    (diredp-rare-priv              :foreground red   :weight 'bold)
    (diredp-dir-priv               :foreground blue  :weight 'bold)
    (diredp-no-priv                :foreground base5)
    (diredp-number                 :foreground magenta)
    (diredp-date-time              :foreground blue)
    ;;;; dired-k
    (dired-k-modified :foreground vc-modified :weight 'bold)
    (dired-k-commited :foreground green :weight 'bold)
    (dired-k-added :foreground vc-added :weight 'bold)
    (dired-k-untracked :foreground teal :weight 'bold)
    (dired-k-ignored :foreground base5 :weight 'bold)
    (dired-k-directory :foreground blue :weight 'bold)
    ;;;; dired-subtree
    (dired-subtree-depth-1-face :background (doom-darken bg-alt 0.02))
    (dired-subtree-depth-2-face :background (doom-darken bg-alt 0.04))
    (dired-subtree-depth-3-face :background (doom-darken bg-alt 0.06))
    (dired-subtree-depth-4-face :background (doom-darken bg-alt 0.08))
    (dired-subtree-depth-5-face :background (doom-darken bg-alt 0.10))
    (dired-subtree-depth-6-face :background (doom-darken bg-alt 0.12))
    ;;;; diredfl
    (diredfl-autofile-name          :foreground base4)
    (diredfl-compressed-file-name   :foreground yellow)
    (diredfl-compressed-file-suffix :foreground (doom-blend orange bg 0.6))
    (diredfl-date-time              :foreground cyan :weight 'light)
    (diredfl-deletion               :foreground red :background (doom-blend red bg 0.2) :weight 'bold)
    (diredfl-deletion-file-name     :foreground red :background (doom-blend red bg 0.2))
    (diredfl-dir-heading            :foreground blue :weight 'bold)
    (diredfl-dir-name               :foreground blue)
    (diredfl-dir-priv               :foreground blue)
    (diredfl-exec-priv              :foreground green)
    (diredfl-executable-tag         :foreground green)
    (diredfl-file-name              :foreground fg)
    (diredfl-file-suffix            :foreground (doom-blend fg bg 0.6))
    (diredfl-flag-mark              :foreground yellow :background (doom-blend yellow bg 0.2) :weight 'bold)
    (diredfl-flag-mark-line         :background (doom-blend yellow bg 0.1))
    (diredfl-ignored-file-name      :foreground comments)
    (diredfl-link-priv              :foreground violet)
    (diredfl-no-priv                :inherit 'shadow)
    (diredfl-number                 :foreground orange)
    (diredfl-other-priv             :foreground magenta)
    (diredfl-rare-priv              :foreground fg)
    (diredfl-read-priv              :foreground yellow)
    (diredfl-symlink                :foreground violet)
    (diredfl-tagged-autofile-name   :foreground base5)
    (diredfl-write-priv             :foreground red)
    ;;;; disk-usage
    (disk-usage-children            :foreground yellow)
    (disk-usage-percent             :foreground violet)
    (disk-usage-size                :foreground blue)
    (disk-usage-symlink             :foreground cyan :weight 'bold)
    ;;;; doom-modeline
    (doom-modeline-eldoc-bar :background green)
    (doom-modeline-bar-inactive) ; transparent
    ;;;; doom-themes
    (doom-themes-visual-bell :background error)
    ;;;; ediff <built-in>
    (ediff-fine-diff-A    :background (doom-blend selection bg 0.7) :weight 'bold :extend t)
    (ediff-fine-diff-B    :inherit 'ediff-fine-diff-A)
    (ediff-fine-diff-C    :inherit 'ediff-fine-diff-A)
    (ediff-current-diff-A :background (doom-blend selection bg 0.3) :extend t)
    (ediff-current-diff-B :inherit 'ediff-current-diff-A)
    (ediff-current-diff-C :inherit 'ediff-current-diff-A)
    (ediff-even-diff-A    :inherit 'hl-line)
    (ediff-even-diff-B    :inherit 'ediff-even-diff-A)
    (ediff-even-diff-C    :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-A     :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-B     :inherit 'ediff-odd-diff-A)
    (ediff-odd-diff-C     :inherit 'ediff-odd-diff-A)
    ;;;; elfeed
    (elfeed-log-debug-level-face :foreground comments)
    (elfeed-log-error-level-face :inherit 'error)
    (elfeed-log-info-level-face  :inherit 'success)
    (elfeed-log-warn-level-face  :inherit 'warning)
    (elfeed-search-date-face     :foreground violet)
    (elfeed-search-feed-face     :foreground blue)
    (elfeed-search-tag-face      :foreground comments)
    (elfeed-search-title-face    :foreground comments)
    (elfeed-search-filter-face   :foreground violet)
    (elfeed-search-unread-count-face :foreground yellow)
    (elfeed-search-unread-title-face :foreground fg :weight 'bold)
    ;;;; elixir-mode <modes:elixir-mode>
    (elixir-atom-face (&light :foreground dark-blue)
                      (&dark  :foreground cyan))
    (elixir-attribute-face :foreground violet)
    ;;;; elscreen
    (elscreen-tab-background-face     :background bg)
    (elscreen-tab-control-face        :background bg     :foreground bg)
    (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
    (elscreen-tab-other-screen-face   :background bg     :foreground fg-alt)
    ;;;; enh-ruby-mode <modes:enh-ruby-mode>
    (enh-ruby-heredoc-delimiter-face :inherit 'font-lock-string-face)
    (enh-ruby-op-face                :foreground operators)
    (enh-ruby-regexp-delimiter-face  :inherit 'enh-ruby-regexp-face)
    (enh-ruby-regexp-face            :foreground constants)
    (enh-ruby-string-delimiter-face  :inherit 'font-lock-string-face)
    (erm-syn-errline                 :underline `(:style wave :color ,error))
    (erm-syn-warnline                :underline `(:style wave :color ,warning))
    ;;;; erc <built-in>
    (erc-button :weight 'bold :underline t)
    (erc-default-face :inherit 'default)
    (erc-action-face  :weight 'bold)
    (erc-command-indicator-face :weight 'bold)
    (erc-direct-msg-face :foreground magenta)
    (erc-error-face :inherit 'error)
    (erc-header-line :background (doom-darken bg-alt 0.15) :foreground highlight)
    (erc-input-face :foreground green)
    (erc-current-nick-face :foreground green :weight 'bold)
    (erc-timestamp-face :foreground blue :weight 'bold)
    (erc-nick-default-face :weight 'bold)
    (erc-nick-msg-face :foreground magenta)
    (erc-nick-prefix-face :inherit 'erc-nick-default-face)
    (erc-my-nick-face :foreground green :weight 'bold)
    (erc-my-nick-prefix-face :inherit 'erc-my-nick-face)
    (erc-notice-face :foreground comments)
    (erc-prompt-face :foreground highlight :weight 'bold)
    ;;;; eshell <built-in>
    (eshell-prompt        :foreground highlight :weight 'bold)
    (eshell-ls-archive    :foreground magenta)
    (eshell-ls-backup     :foreground yellow)
    (eshell-ls-clutter    :foreground red)
    (eshell-ls-directory  :foreground blue)
    (eshell-ls-executable :foreground green)
    (eshell-ls-missing    :foreground red)
    (eshell-ls-product    :foreground orange)
    (eshell-ls-readonly   :foreground orange)
    (eshell-ls-special    :foreground violet)
    (eshell-ls-symlink    :foreground cyan)
    (eshell-ls-unreadable :foreground base5)
    ;;;; evil
    (evil-ex-info                   :foreground error :slant 'italic)
    (evil-ex-search                 :background highlight :foreground base0 :weight 'bold)
    (evil-ex-substitute-matches     :background base0     :foreground red   :weight 'bold :strike-through t)
    (evil-ex-substitute-replacement :background base0     :foreground green :weight 'bold)
    (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)
    ;;;; evil-mc
    (evil-mc-cursor-default-face :background magenta :foreground base0)
    (evil-mc-region-face         :inherit 'region)
    (evil-mc-cursor-bar-face     :height 1 :background magenta :foreground base0)
    (evil-mc-cursor-hbar-face    :underline `(:color ,highlight))
    ;;;; evil-snipe
    (evil-snipe-first-match-face :foreground highlight :background dark-blue :weight 'bold)
    (evil-snipe-matches-face     :foreground highlight :underline t :weight 'bold)
    ;;;; evil-googles
    (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))
    ;;;; eww
    (eww-form-checkbox :inherit 'eww-form-file)
    (eww-form-file   :inherit 'eww-form-submit :background bg-alt)
    (eww-form-select :inherit 'eww-form-submit :background bg-alt)
    (eww-form-submit :inherit 'eww-form-text :box `(:line-width 2 :style released-button) :background base2)
    (eww-form-text :box `(:line-width 1 :color ,base3) :background bg :foreground fg :distant-foreground bg)
    (eww-form-textarea :inherit 'eww-form-text)
    (eww-invalid-certificate :foreground error)
    (eww-valid-certificate :foreground highlight)
    ;;;; eyebrowse
    (eyebrowse-mode-line-active :weight 'bold :foreground highlight)
    ;;;; flycheck
    (flycheck-error          :underline `(:style wave :color ,red))
    (flycheck-warning        :underline `(:style wave :color ,yellow))
    (flycheck-info           :underline `(:style wave :color ,green))
    (flycheck-fringe-error   :inherit 'fringe :foreground error)
    (flycheck-fringe-warning :inherit 'fringe :foreground warning)
    (flycheck-fringe-info    :inherit 'fringe :foreground success)
    ;;;; flycheck-posframe
    (flycheck-posframe-face            :inherit 'default)
    (flycheck-posframe-background-face :background bg-alt)
    (flycheck-posframe-error-face      :inherit 'flycheck-posframe-face :foreground error)
    (flycheck-posframe-info-face       :inherit 'flycheck-posframe-face :foreground fg)
    (flycheck-posframe-warning-face    :inherit 'flycheck-posframe-face :foreground warning)
    ;;;; flymake
    (flymake-error   :underline `(:style wave :color ,red))
    (flymake-note    :underline `(:style wave :color ,green))
    (flymake-warning :underline `(:style wave :color ,orange))
    ;;;; flyspell <built-in>
    (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)
    (flyspell-duplicate :underline `(:style wave :color ,warning) :inherit 'unspecified)
    ;;;; flx-ido
    (flx-highlight-face :weight 'bold :foreground yellow :underline nil)
    ;;;; git-commit
    (git-commit-summary               :foreground strings)
    (git-commit-overlong-summary      :inherit 'error          :background base0 :slant 'italic :weight 'bold)
    (git-commit-nonempty-second-line  :inherit 'git-commit-overlong-summary)
    (git-commit-keyword               :foreground cyan         :slant 'italic)
    (git-commit-pseudo-header         :foreground doc-comments :slant 'italic)
    (git-commit-known-pseudo-header   :foreground doc-comments :weight 'bold     :slant 'italic)
    (git-commit-comment-branch-local  :foreground magenta)
    (git-commit-comment-branch-remote :foreground green)
    (git-commit-comment-detached      :foreground orange)
    (git-commit-comment-heading       :foreground keywords)
    (git-commit-comment-file          :foreground violet)
    (git-commit-comment-action)
    ;;;; git-gutter
    (git-gutter:modified :inherit 'fringe :foreground vc-modified)
    (git-gutter:added    :inherit 'fringe :foreground vc-added)
    (git-gutter:deleted  :inherit 'fringe :foreground vc-deleted)
    ;;;; git-gutter+
    (git-gutter+-modified :inherit 'fringe :foreground vc-modified :background 'unspecified)
    (git-gutter+-added    :inherit 'fringe :foreground vc-added    :background 'unspecified)
    (git-gutter+-deleted  :inherit 'fringe :foreground vc-deleted  :background 'unspecified)
    ;;;; git-gutter-fringe
    ((git-gutter-fr:modified &inherit git-gutter:modified))
    ((git-gutter-fr:added    &inherit git-gutter:added))
    ((git-gutter-fr:deleted  &inherit git-gutter:deleted))
    ;;;; gnus (built-in)
    (gnus-group-mail-1           :weight 'bold :foreground fg)
    (gnus-group-mail-2           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-3           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-1-empty     :foreground base5)
    (gnus-group-mail-2-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-mail-3-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-1           :inherit 'gnus-group-mail-1)
    (gnus-group-news-2           :inherit 'gnus-group-news-1)
    (gnus-group-news-3           :inherit 'gnus-group-news-1)
    (gnus-group-news-4           :inherit 'gnus-group-news-1)
    (gnus-group-news-5           :inherit 'gnus-group-news-1)
    (gnus-group-news-6           :inherit 'gnus-group-news-1)
    (gnus-group-news-1-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-2-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-3-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-4-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-5-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-news-6-empty     :inherit 'gnus-group-news-1-empty)
    (gnus-group-mail-low         :inherit 'gnus-group-mail-1 :weight 'normal)
    (gnus-group-mail-low-empty   :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-low         :inherit 'gnus-group-mail-1 :foreground base5)
    (gnus-group-news-low-empty   :inherit 'gnus-group-news-low :weight 'normal)
    (gnus-header-content         :inherit 'message-header-other)
    (gnus-header-from            :inherit 'message-header-other)
    (gnus-header-name            :inherit 'message-header-name)
    (gnus-header-newsgroups      :inherit 'message-header-other)
    (gnus-header-subject         :inherit 'message-header-subject)
    (gnus-summary-cancelled      :foreground red :strike-through t)
    (gnus-summary-high-ancient   :foreground (doom-lighten base5 0.2) :inherit 'italic)
    (gnus-summary-high-read      :foreground (doom-lighten fg 0.2))
    (gnus-summary-high-ticked    :foreground (doom-lighten magenta 0.2))
    (gnus-summary-high-unread    :foreground (doom-lighten green 0.2))
    (gnus-summary-low-ancient    :foreground (doom-darken base5 0.2) :inherit 'italic)
    (gnus-summary-low-read       :foreground (doom-darken fg 0.2))
    (gnus-summary-low-ticked     :foreground (doom-darken magenta 0.2))
    (gnus-summary-low-unread     :foreground (doom-darken green 0.2))
    (gnus-summary-normal-ancient :foreground base5 :inherit 'italic)
    (gnus-summary-normal-read    :foreground fg)
    (gnus-summary-normal-ticked  :foreground magenta)
    (gnus-summary-normal-unread  :foreground green :inherit 'bold)
    (gnus-summary-selected       :foreground blue :weight 'bold)
    (gnus-cite-1                 :foreground violet)
    (gnus-cite-2                 :foreground yellow)
    (gnus-cite-3                 :foreground magenta)
    (gnus-cite-4                 :foreground green)
    (gnus-cite-5                 :foreground green)
    (gnus-cite-6                 :foreground green)
    (gnus-cite-7                 :foreground magenta)
    (gnus-cite-8                 :foreground magenta)
    (gnus-cite-9                 :foreground magenta)
    (gnus-cite-10                :foreground yellow)
    (gnus-cite-11                :foreground yellow)
    (gnus-signature              :foreground yellow)
    (gnus-x-face                 :background base5 :foreground fg)
    ;;;; goggles
    (goggles-changed :inherit 'region)
    (goggles-removed :background (doom-blend red bg-alt 0.25) :extend t)
    (goggles-added   :background (doom-blend green bg-alt 0.25))
    ;;;; helm
    (helm-selection
     (&all :inherit 'bold :background selection :extend t)
     (&dark  :distant-foreground highlight)
     (&light :distant-foreground base0))
    (helm-match :inherit 'bold :foreground highlight :distant-foreground base8)
    (helm-source-header          :background base2 :foreground keywords :weight 'bold)
    (helm-visible-mark           :inherit '(bold highlight))
    (helm-moccur-buffer          :inherit 'link)
    (helm-ff-file                :foreground fg)
    (helm-ff-prefix              :foreground keywords)
    (helm-ff-dotted-directory    :foreground grey)
    (helm-ff-directory           :foreground variables)
    (helm-ff-executable          :foreground base8 :inherit 'italic)
    (helm-grep-match             :foreground highlight :distant-foreground red)
    (helm-grep-file              :foreground methods)
    (helm-grep-lineno            :foreground base5)
    (helm-grep-finish            :foreground green)
    ;;;; helm-swoop
    (helm-swoop-target-line-face       :foreground highlight :inverse-video t)
    (helm-swoop-target-line-block-face :foreground yellow)
    (helm-swoop-target-word-face       :foreground green :inherit 'bold)
    (helm-swoop-target-number-face     :foreground base5)
    ;;;; helm-rg
    (helm-rg-active-arg-face        :foreground green  :weight 'normal)
    (helm-rg-base-rg-cmd-face       :foreground grey   :weight 'normal)
    (helm-rg-colon-separator-ripgrep-output-face :foreground base8)
    (helm-rg-directory-cmd-face     :foreground (doom-darken yellow 0.25) :background base0 :weight 'normal)
    (helm-rg-directory-header-face  :foreground base8 :background base0 :weight 'bold)
    (helm-rg-error-message          :foreground red)
    (helm-rg-extra-arg-face         :foreground yellow :weight 'normal)
    (helm-rg-file-match-face        :foreground cyan :underline t)
    (helm-rg-inactive-arg-face      :foreground grey   :weight 'normal)
    (helm-rg-line-number-match-face :foreground orange :underline t)
    (helm-rg-match-text-face        :foreground base8 :background violet)
    (helm-rg-title-face             :foreground violet :background base0 :weight 'bold)
    ;;;; helpful
    (helpful-heading :weight 'bold :height 1.2)
    ;;;; hi-lock <built-in>
    (hi-yellow   :background yellow)
    (hi-pink     :background magenta)
    (hi-red-b    :foreground red :weight 'bold)
    (hi-green    :background green)
    (hi-green-b  :foreground green :weight 'bold)
    (hi-blue     :background blue)
    (hi-blue-b   :foreground blue :weight 'bold)
    ;; (hi-black-b  :weight 'bold)
    ;; (hi-black-hb :inherit 'variable-pitch :weight 'bold :height 1.67)
    ;;;; hideshow <built-in>
    (+fold-hideshow-folded-face :inherit 'font-lock-comment-face
                                :weight 'light
                                :background (doom-darken bg 0.125))
    ;;;; highlight-numbers-mode
    (highlight-numbers-number :inherit 'bold :foreground numbers)
    ;;;; highlight-indentation-mode
    (highlight-indentation-face                :inherit 'hl-line)
    (highlight-indentation-current-column-face :background base1)
    (highlight-indentation-guides-odd-face     :inherit 'highlight-indentation-face)
    (highlight-indentation-guides-even-face    :inherit 'highlight-indentation-face)
    ;;;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground type)
    (highlight-quoted-quote  :foreground operators)
    ;;;; highlight-symbol
    (highlight-symbol-face
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1) :distant-foreground fg-alt))
    ;;;; highlight-thing
    (highlight-thing
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1) :distant-foreground fg-alt))
    ;;;; hl-fill-column-face
    (hl-fill-column-face :inherit '(hl-line shadow))
    ;;;; hl-line (built-in)
    (hl-line :background "#2B3036" :extend t)
    ;;;; hl-todo
    (hl-todo :foreground red :weight 'bold)
    ;;;; hlinum
    (linum-highlight-face :foreground fg :weight 'normal)
    ;;;; hydra
    (hydra-face-red      :foreground red     :weight 'bold)
    (hydra-face-blue     :foreground blue    :weight 'bold)
    (hydra-face-amaranth :foreground magenta :weight 'bold)
    (hydra-face-pink     :foreground violet  :weight 'bold)
    (hydra-face-teal     :foreground teal    :weight 'bold)
    ;;;; iedit
    (iedit-occurrence :foreground magenta :weight 'bold :inverse-video t)
    (iedit-read-only-occurrence :inherit 'region)
    ;;;; ido <built-in>
    (ido-first-match :foreground orange)
    (ido-indicator   :foreground red :background bg)
    (ido-only-match  :foreground green)
    (ido-subdir      :foreground violet)
    (ido-virtual     :foreground comments)
    ;;;; imenu-list
    ;; (imenu-list-entry-face)
    (imenu-list-entry-face-0 :foreground highlight)
    (imenu-list-entry-subalist-face-0 :inherit 'imenu-list-entry-face-0 :weight 'bold)
    (imenu-list-entry-face-1 :foreground green)
    (imenu-list-entry-subalist-face-1 :inherit 'imenu-list-entry-face-1 :weight 'bold)
    (imenu-list-entry-face-2 :foreground yellow)
    (imenu-list-entry-subalist-face-2 :inherit 'imenu-list-entry-face-2 :weight 'bold)
    ;;;; indent-guide
    ((indent-guide-face &inherit highlight-indentation-face))
    ;;;; isearch <built-in>
    (isearch :inherit 'lazy-highlight :weight 'bold)
    (isearch-fail :background error :foreground base0 :weight 'bold)
    ;;;; ivy
    (ivy-current-match :background region :extend t)
    (ivy-minibuffer-match-face-1
     :foreground (doom-lighten grey 0.14)
     :weight 'light)
    (ivy-minibuffer-match-face-2
     :inherit 'ivy-minibuffer-match-face-1
     :foreground magenta :background base1 :weight 'semi-bold)
    (ivy-minibuffer-match-face-3
     :inherit 'ivy-minibuffer-match-face-2
     :foreground green :weight 'semi-bold)
    (ivy-minibuffer-match-face-4
     :inherit 'ivy-minibuffer-match-face-2
     :foreground yellow :weight 'semi-bold)
    (ivy-minibuffer-match-highlight :foreground violet)
    (ivy-highlight-face :foreground violet)
    (ivy-confirm-face :foreground success)
    (ivy-match-required-face :foreground error)
    (ivy-virtual :inherit 'italic :foreground doc-comments)
    (ivy-modified-buffer :inherit 'bold :foreground warning)
    ;;;; ivy-posframe
    (ivy-posframe :background (doom-darken bg-alt 0.2))
    (ivy-posframe-border :inherit 'internal-border)
    ;;;; selectrum
    (selectrum-current-candidate :background region :extend t)
    ;;;; vertico
    (vertico-current :background region :extend t)
    ;;;; vertico-posframe
    ;;(vertico-posframe :inherit 'default)
    (vertico-posframe-border :background grey)
    (vertico-posframe-border-2 :background red)
    (vertico-posframe-border-3 :background green)
    (vertico-posframe-border-4 :background blue)
    (vertico-posframe-border-fallback :background yellow)
    ;;;; jabber
    (jabber-activity-face          :foreground red   :weight 'bold)
    (jabber-activity-personal-face :foreground blue  :weight 'bold)
    (jabber-chat-error             :foreground red   :weight 'bold)
    (jabber-chat-prompt-foreign    :foreground red   :weight 'bold)
    (jabber-chat-prompt-local      :foreground blue  :weight 'bold)
    (jabber-chat-prompt-system     :foreground green :weight 'bold)
    (jabber-chat-text-foreign      :foreground fg)
    (jabber-chat-text-local        :foreground fg)
    (jabber-rare-time-face         :foreground green)
    (jabber-roster-user-away       :foreground yellow)
    (jabber-roster-user-chatty     :foreground green :weight 'bold)
    (jabber-roster-user-dnd        :foreground red)
    (jabber-roster-user-error      :foreground red)
    (jabber-roster-user-offline    :foreground fg)
    (jabber-roster-user-online     :foreground green :weight 'bold)
    (jabber-roster-user-xa         :foreground cyan)
    ;;;; jdee <modes:jdee-mode>
    (jdee-font-lock-number-face      :foreground numbers)
    (jdee-font-lock-operator-face    :foreground operators)
    (jdee-font-lock-constant-face    :inherit 'font-lock-constant-face)
    (jdee-font-lock-constructor-face :foreground methods)
    (jdee-font-lock-public-face      :inherit 'font-lock-keyword-face)
    (jdee-font-lock-protected-face   :inherit 'font-lock-keyword-face)
    (jdee-font-lock-private-face     :inherit 'font-lock-keyword-face)
    (jdee-font-lock-modifier-face    :inherit 'font-lock-type-face)
    (jdee-font-lock-doc-tag-face     :foreground violet)
    (jdee-font-lock-italic-face      :inherit 'italic)
    (jdee-font-lock-bold-face        :inherit 'bold)
    (jdee-font-lock-link-face        :foreground blue :italic nil :underline t)
    ;;;; js2-mode <modes:js2-mode,js2-jsx-mode>
    (js2-function-param    :foreground variables)
    (js2-function-call     :foreground functions)
    (js2-object-property   :foreground violet)
    (js2-jsdoc-tag         :foreground doc-comments)
    (js2-external-variable :foreground operators)
    ;;;; keycast
    (keycast-command :inherit 'mode-line-emphasis)
    (keycast-key     :inherit '(bold mode-line-highlight))
    ;;;; ledger-mode <modes:ledger-mode>
    (ledger-font-posting-date-face    :foreground blue)
    (ledger-font-posting-amount-face  :foreground yellow)
    (ledger-font-posting-account-face :foreground base8)
    (ledger-font-payee-cleared-face   :foreground violet :weight 'bold)
    (ledger-font-payee-uncleared-face :foreground base5  :weight 'bold)
    (ledger-font-xact-highlight-face  :background base0)
    ;;;; linum <built-in>
    ((linum &inherit line-number))
    ;;;; linum-relative
    ((linum-relative-current-face &inherit line-number-current-line))
    ;;;; lui
    (lui-time-stamp-face :foreground violet)
    (lui-highlight-face :foreground highlight)
    (lui-button-face :foreground builtin :underline t)
    ;;;; magit
    (magit-bisect-bad        :foreground red)
    (magit-bisect-good       :foreground green)
    (magit-bisect-skip       :foreground orange)
    (magit-blame-hash        :foreground cyan)
    (magit-blame-date        :foreground red)
    (magit-blame-heading     :foreground orange :background base3 :extend t)
    (magit-branch-current    :foreground blue)
    (magit-branch-local      :foreground cyan)
    (magit-branch-remote     :foreground green)
    (magit-cherry-equivalent :foreground violet)
    (magit-cherry-unmatched  :foreground cyan)
    (magit-diff-added             :foreground (doom-darken vc-added 0.2)  :background (doom-blend vc-added bg 0.1) :extend t)
    (magit-diff-added-highlight   :foreground vc-added                    :background (doom-blend vc-added bg 0.2) :weight 'bold :extend t)
    (magit-diff-base              :foreground (doom-darken orange 0.2) :background (doom-blend orange bg 0.1) :extend t)
    (magit-diff-base-highlight    :foreground orange                   :background (doom-blend orange bg 0.2) :weight 'bold :extend t)
    (magit-diff-context           :foreground (doom-darken fg 0.4) :background bg :extend t)
    (magit-diff-context-highlight :foreground fg                   :background bg-alt :extend t)
    (magit-diff-file-heading           :foreground fg :weight 'bold :extend t)
    (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :weight 'bold :extend t)
    (magit-diff-hunk-heading           :foreground bg                    :background (doom-blend violet bg 0.3) :extend t)
    (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :weight 'bold :extend t)
    (magit-diff-lines-heading          :foreground yellow :background red :extend t :extend t)
    (magit-diff-removed                :foreground (doom-darken vc-deleted 0.2) :background (doom-blend vc-deleted base3 0.1) :extend t)
    (magit-diff-removed-highlight      :foreground vc-deleted                   :background (doom-blend vc-deleted base3 0.2) :weight 'bold :extend t)
    (magit-diffstat-added              :foreground vc-added)
    (magit-diffstat-removed            :foreground vc-deleted)
    (magit-dimmed :foreground comments)
    (magit-hash :foreground comments)
    (magit-header-line :background dark-blue :foreground base8 :weight 'bold
                       :box `(:line-width 3 :color ,dark-blue))
    (magit-filename :foreground violet)
    (magit-log-author :foreground orange)
    (magit-log-date :foreground blue)
    (magit-log-graph :foreground comments)
    (magit-process-ng :inherit 'error)
    (magit-process-ok :inherit 'success)
    (magit-reflog-amend :foreground magenta)
    (magit-reflog-checkout :foreground blue)
    (magit-reflog-cherry-pick :foreground green)
    (magit-reflog-commit :foreground green)
    (magit-reflog-merge :foreground green)
    (magit-reflog-other :foreground cyan)
    (magit-reflog-rebase :foreground magenta)
    (magit-reflog-remote :foreground cyan)
    (magit-reflog-reset :inherit 'error)
    (magit-refname :foreground comments)
    (magit-section-heading :foreground blue :weight 'bold :extend t)
    (magit-section-heading-selection :foreground orange :weight 'bold :extend t)
    (magit-section-highlight :inherit 'hl-line)
    (magit-section-secondary-heading :foreground violet :weight 'bold :extend t)
    (magit-sequence-drop :foreground red)
    (magit-sequence-head :foreground blue)
    (magit-sequence-part :foreground orange)
    (magit-sequence-stop :foreground green)
    (magit-signature-bad :inherit 'error)
    (magit-signature-error :inherit 'error)
    (magit-signature-expired :foreground orange)
    (magit-signature-good :inherit 'success)
    (magit-signature-revoked :foreground magenta)
    (magit-signature-untrusted :foreground yellow)
    (magit-tag :foreground yellow)
    ;;;; make-mode <built-in> <modes:makefile-mode,makefile-automake-mode,makefile-makepp-mode,makefile-gmake-mode,makefile-imake-mode,makefile-bsdmake-mode>
    (makefile-targets :foreground blue)
    ;;;; man <built-in> <mode:Man-mode>
    (Man-overstrike :inherit 'bold :foreground operators)
    (Man-underline :inherit 'underline :foreground keywords)
    ;;;; markdown-mode <modes:markdown-mode,gfm-mode>
    (markdown-header-face           :inherit 'bold :foreground highlight)
    (markdown-header-delimiter-face :inherit 'markdown-header-face)
    (markdown-metadata-key-face     :foreground red)
    (markdown-list-face             :foreground red)
    (markdown-link-face             :foreground highlight)
    (markdown-url-face              :foreground magenta :weight 'normal)
    (markdown-italic-face           :inherit 'italic :foreground violet)
    (markdown-bold-face             :inherit 'bold   :foreground orange)
    (markdown-markup-face           :foreground operators)
    (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
    (markdown-pre-face              :foreground strings)
    (markdown-code-face             :background base3 :extend t)
    (markdown-reference-face        :foreground doc-comments)
    (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face) :extend nil)
    (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
    (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
    (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
    (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
    (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
    ;;;; marginalia
    (marginalia-documentation   :inherit 'font-lock-doc-face)
    (marginalia-file-priv-dir   :foreground blue)
    (marginalia-file-priv-exec  :foreground green)
    (marginalia-file-priv-link  :foreground violet)
    (marginalia-file-priv-other :foreground magenta)
    (marginalia-file-priv-rare  :foreground fg)
    (marginalia-file-priv-read  :foreground yellow)
    (marginalia-file-priv-write :foreground red)
    (marginalia-number          :foreground numbers)
    (marginalia-size            :foreground violet)
    (marginalia-lighter         :foreground violet)
    ;;;; message <built-in>
    (message-header-name       :foreground green)
    (message-header-subject    :foreground highlight :weight 'bold)
    (message-header-to         :foreground highlight :weight 'bold)
    (message-header-cc         :inherit 'message-header-to :foreground (doom-darken highlight 0.15))
    (message-header-other      :foreground violet)
    (message-header-newsgroups :foreground yellow)
    (message-header-xheader    :foreground doc-comments)
    (message-separator         :foreground comments)
    (message-mml               :foreground comments :slant 'italic)
    ((message-cited-text   &inherit gnus-cite-1))
    ((message-cited-text-1 &inherit gnus-cite-2))
    ((message-cited-text-2 &inherit gnus-cite-3))
    ((message-cited-text-3 &inherit gnus-cite-4))
    ((message-cited-text-4 &inherit gnus-cite-5))
    ;;;; mic-paren
    (paren-face-match    :foreground red   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)
    ;;;; minimap
    (minimap-current-line-face :background selection)
    (minimap-active-region-background :background vertical-bar)
    ;; mm
    (mm-uu-extract :background (doom-blend highlight base2 0.07) :foreground (doom-blend highlight fg 0.15))
    ;;;; mmm-mode
    (mmm-init-submode-face :background (doom-blend red bg 0.1))
    (mmm-cleanup-submode-face :background (doom-blend yellow bg 0.1))
    (mmm-declaration-submode-face :background (doom-blend cyan bg 0.1))
    (mmm-comment-submode-face :background (doom-blend blue bg 0.1))
    (mmm-output-submode-face :background (doom-blend violet bg 0.1))
    (mmm-special-submode-face :background (doom-blend green bg 0.1))
    (mmm-code-submode-face :background bg-alt)
    (mmm-default-submode-face) ; make transparent
    ;;;; multiple cursors
    (mc/cursor-face :inherit 'cursor)
    ;;;; nav-flash
    (nav-flash-face :background selection :foreground base8 :weight 'bold)
    ;;;; neotree
    (neo-root-dir-face   :foreground strings :background bg :box `(:line-width 4 :color ,bg))
    (neo-file-link-face  :foreground fg)
    (neo-dir-link-face   :foreground highlight)
    (neo-expand-btn-face :foreground highlight)
    (neo-vc-edited-face  :foreground yellow)
    (neo-vc-added-face   :foreground green)
    (neo-vc-removed-face :foreground red :strike-through t)
    (neo-vc-conflict-face :foreground magenta :weight 'bold)
    (neo-vc-ignored-face  :foreground comments)
    (doom-neotree-dir-face :foreground highlight)
    (doom-neotree-file-face :foreground base8)
    (doom-neotree-hidden-file-face :foreground comments)
    (doom-neotree-text-file-face :foreground fg)
    (doom-neotree-data-file-face :foreground violet)
    (doom-neotree-media-file-face :inherit 'doom-neotree-hidden-file-face)
    ;;;; nlinum
    ((nlinum-current-line &inherit line-number-current-line))
    ;;;; nlinum-hl
    ((nlinum-hl-face &inherit line-number-current-line))
    ;;;; nlinum-relative
    ((nlinum-relative-current-face &inherit line-number-current-line))
    ;;;; notmuch
    ;; (notmuch-crypto-decryption               :foreground blue-l)
    ;; (notmuch-crypto-part-header              :foreground yellow-l)
    ;; (notmuch-crypto-signature-bad            :foreground red-l)
    ;; (notmuch-crypto-signature-good           :foreground base1)
    ;; (notmuch-crypto-signature-good-key       :foreground aqua-l)
    ;; (notmuch-crypto-signature-unknown        :foreground yellow)
    ;; (notmuch-hello-logo-background           :foreground fg)
    (notmuch-message-summary-face            :foreground grey)
    (notmuch-search-count                    :foreground comments)
    (notmuch-search-date                     :foreground numbers)
    (notmuch-search-flagged-face             :foreground (doom-blend red base4 0.5))
    (notmuch-search-matching-authors         :foreground blue)
    (notmuch-search-non-matching-authors     :foreground fg)
    (notmuch-search-subject                  :foreground fg)
    (notmuch-search-unread-face              :weight 'bold)
    (notmuch-tag-added                       :foreground green :weight 'normal)
    (notmuch-tag-deleted                     :foreground red :weight 'normal)
    (notmuch-tag-face                        :foreground yellow :weight 'normal)
    (notmuch-tag-flagged                     :foreground yellow :weight 'normal)
    (notmuch-tag-unread                      :foreground yellow :weight 'normal)
    (notmuch-tree-match-author-face          :foreground blue :weight 'bold)
    (notmuch-tree-match-date-face            :foreground numbers :weight 'bold)
    (notmuch-tree-match-face                 :foreground fg)
    (notmuch-tree-match-subject-face         :foreground fg)
    (notmuch-tree-match-tag-face             :foreground yellow)
    (notmuch-tree-match-tree-face            :foreground comments)
    (notmuch-tree-no-match-author-face       :foreground blue)
    (notmuch-tree-no-match-date-face         :foreground numbers)
    (notmuch-tree-no-match-face              :foreground base5)
    (notmuch-tree-no-match-subject-face      :foreground base5)
    (notmuch-tree-no-match-tag-face          :foreground yellow)
    (notmuch-tree-no-match-tree-face         :foreground yellow)
    (notmuch-wash-cited-text                 :foreground base4)
    (notmuch-wash-toggle-button :foreground fg)
    ;;;; lsp-mode
    (lsp-face-highlight-textual
     (&all   :weight 'bold)
     (&light :background base3 :foreground base0 :distant-foreground base8)
     (&dark  :background (doom-blend highlight bg 0.3) :foreground base8 :distant-foreground base0))
    (lsp-face-highlight-read    :inherit 'lsp-face-highlight-textual)
    (lsp-face-highlight-write   :inherit 'lsp-face-highlight-textual)
    (lsp-headerline-breadcrumb-separator-face :inherit 'shadow)
    ;;;; lsp-ui
    (lsp-ui-doc-background :inherit 'tooltip)
    (lsp-ui-peek-filename :inherit 'mode-line-buffer-id)
    (lsp-ui-peek-header :foreground fg :background (doom-lighten bg 0.1) :bold bold)
    (lsp-ui-peek-selection :foreground bg :background blue :bold bold)
    (lsp-ui-peek-list :background (doom-darken bg 0.1))
    (lsp-ui-peek-peek :background (doom-darken bg 0.1))
    (lsp-ui-peek-highlight :inherit 'lsp-ui-peek-header :background region :foreground bg :box t)
    (lsp-ui-peek-line-number :foreground success)
    (lsp-ui-sideline-code-action :foreground (doom-blend highlight bg 0.85))
    (lsp-ui-sideline-current-symbol :inherit 'highlight)
    (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
                                 :background bg-alt :extend t)
    ;;;; objed
    (objed-mode-line :inherit 'warning :weight 'bold)
    (objed-hl        :inherit 'region :background (doom-blend region bg 0.5))
    ;;;; orderless
    (orderless-match-face-0 :weight 'bold :foreground (doom-blend blue    fg 0.6) :background (doom-blend blue    bg 0.1))
    (orderless-match-face-1 :weight 'bold :foreground (doom-blend magenta fg 0.6) :background (doom-blend magenta bg 0.1))
    (orderless-match-face-2 :weight 'bold :foreground (doom-blend green   fg 0.6) :background (doom-blend green   bg 0.1))
    (orderless-match-face-3 :weight 'bold :foreground (doom-blend yellow  fg 0.6) :background (doom-blend yellow  bg 0.1))
    ;;;; org <built-in> <modes:org-mode>
    (org-archived                 :foreground doc-comments)
    (org-block                    :background base3    :extend t)
    (org-block-background         :background base3    :extend t)
    (org-block-begin-line         :inherit 'org-block  :foreground comments)
    (org-block-end-line           :inherit 'org-block-begin-line)
    (org-checkbox                 :inherit 'org-todo)
    (org-checkbox-statistics-done :inherit 'org-done)
    (org-checkbox-statistics-todo :inherit 'org-todo)
    (org-cite                     :foreground (doom-blend teal fg 0.9))
    (org-cite-key                 :foreground (doom-blend teal fg 0.6) :underline t)
    (org-code                     :inherit 'org-block :foreground orange)
    (org-date                     :foreground yellow)
    (org-default                  :inherit 'variable-pitch)
    (org-document-info            :foreground builtin)
    (org-document-title           :foreground builtin         :weight 'bold)
    (org-done                     :inherit 'org-headline-done :strike-through nil :weight 'bold)
    (org-drawer                   :foreground comments)
    (org-ellipsis                 :foreground comments :underline nil)
    (org-footnote                 :foreground orange)
    (org-formula                  :foreground cyan)
    (org-headline-done            :foreground base5)
    (org-hide                     :foreground bg)
    (org-latex-and-related        :foreground base8           :weight 'bold)
    (org-link                     :inherit 'link              :foreground highlight)
    (org-list-dt                  :foreground highlight)
    (org-meta-line                :foreground doc-comments)
    (org-priority                 :foreground red)
    (org-property-value           :foreground doc-comments)
    (org-quote                    :inherit 'org-block :slant 'italic)
    (org-special-keyword          :foreground doc-comments)
    (org-table                    :foreground violet)
    (org-tag                      :foreground doc-comments :weight 'normal)
    (org-todo                     :foreground green :bold 'inherit)
    (org-verbatim                 :foreground green)
    (org-warning                  :foreground warning)
    ;; Omitted because we rely on style they inherit from the outline-N faces
    ;;(org-level-1)
    ;;(org-level-2)
    ;;(org-level-3)
    ;;(org-level-4)
    ;;(org-level-5)
    ;;(org-level-6)
    ;;(org-level-7)
    ;;(org-level-8)
    ;;;; org-agenda <built-in>
    (org-agenda-done :inherit 'org-done)
    (org-agenda-dimmed-todo-face :foreground comments)
    (org-agenda-date          :foreground violet :weight 'ultra-bold)
    (org-agenda-date-today    :foreground (doom-lighten violet 0.4)   :weight 'ultra-bold)
    (org-agenda-date-weekend  :foreground (doom-darken violet 0.4)  :weight 'ultra-bold)
    (org-agenda-structure     :foreground fg :weight 'ultra-bold)
    (org-agenda-clocking      :background (doom-blend blue bg 0.2))
    (org-upcoming-deadline         :foreground (doom-blend fg bg 0.8))
    (org-upcoming-distant-deadline :foreground (doom-blend fg bg 0.5))
    (org-scheduled            :foreground fg)
    (org-scheduled-today      :foreground base7)
    (org-scheduled-previously :foreground base8)
    (org-time-grid            :foreground comments)
    (org-sexp-date            :foreground fg)
    ;;;; org-habit
    (org-habit-clear-face          :weight 'bold :background base4)
    (org-habit-clear-future-face   :weight 'bold :background base3)
    (org-habit-ready-face          :weight 'bold :background (doom-blend blue bg-alt 0.5))
    (org-habit-ready-future-face   :weight 'bold :background (doom-blend blue bg-alt 0.3))
    (org-habit-alert-face          :weight 'bold :background (doom-blend yellow bg-alt 0.5))
    (org-habit-alert-future-face   :weight 'bold :background (doom-blend yellow bg-alt 0.3))
    (org-habit-overdue-face        :weight 'bold :background (doom-blend red bg-alt 0.5))
    (org-habit-overdue-future-face :weight 'bold :background (doom-blend red bg-alt 0.3))
    ;;;; org-journal <modes:org-journal-mode>
    (org-journal-highlight :foreground highlight)
    (org-journal-calendar-entry-face :foreground magenta :slant 'italic)
    (org-journal-calendar-scheduled-face :foreground red :slant 'italic)
    ;;;; org-pomodoro
    (org-pomodoro-mode-line :foreground red)
    (org-pomodoro-mode-line-overtime :foreground warning :weight 'bold)
    ;;;; org-ref
    (org-ref-acronym-face    :foreground violet)
    (org-ref-cite-face       :foreground yellow :weight 'light :underline t)
    (org-ref-glossary-face   :foreground magenta)
    (org-ref-label-face      :foreground blue)
    (org-ref-ref-face        :inherit 'link :foreground teal)
    ;;;; outline <built-in>
    ;; NOTE org-mode's org-level-N faces inherit these outline-N faces.
    (outline-1 :foreground blue                        :weight 'bold :extend t)
    (outline-2 :foreground magenta                     :weight 'bold :extend t)
    (outline-3 :foreground violet                      :weight 'bold :extend t)
    (outline-4 :foreground (doom-lighten blue 0.25)    :weight 'bold :extend t)
    (outline-5 :foreground (doom-lighten magenta 0.25) :weight 'bold :extend t)
    (outline-6 :foreground (doom-lighten blue 0.5)     :weight 'bold :extend t)
    (outline-7 :foreground (doom-lighten magenta 0.5)  :weight 'bold :extend t)
    (outline-8 :foreground (doom-lighten blue 0.8)     :weight 'bold :extend t)
    ;;;; parenface
    (paren-face :foreground comments)
    ;;;; parinfer
    (parinfer-pretty-parens:dim-paren-face :foreground base5)
    (parinfer-smart-tab:indicator-face :foreground base5)
    ;;;; perspective
    (persp-selected-face :foreground blue :weight 'bold)
    ;;;; persp-mode
    (persp-face-lighter-default :foreground highlight :weight 'bold)
    (persp-face-lighter-buffer-not-in-persp :foreground doc-comments)
    (persp-face-lighter-nil-persp :foreground comments)
    ;;;; pkgbuild-mode <modes:pkgbuild-mode>
    (pkgbuild-error-face :underline `(:style wave :color ,red))
    ;;;; popup
    (popup-face           :inherit 'tooltip)
    (popup-tip-face       :inherit 'popup-face :foreground violet :background base0)
    (popup-selection-face :background selection)
    ;;;; power
    (powerline-active0   :inherit 'mode-line :background bg)
    (powerline-active1   :inherit 'mode-line :background (doom-lighten 'bg 0.025))
    (powerline-active2   :inherit 'mode-line :foreground base8 :background (doom-lighten 'bg 0.08))
    (powerline-inactive0 :inherit 'mode-line-inactive :background base2)
    (powerline-inactive1 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.02))
    (powerline-inactive2 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.04))
    ;;;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground "#9ecbff")
    (rainbow-delimiters-depth-2-face :foreground "#ffab70")
    (rainbow-delimiters-depth-3-face :foreground "#b392f0")
    (rainbow-delimiters-depth-4-face :foreground "#9ecbff")
    (rainbow-delimiters-depth-5-face :foreground "#ffab70")
    (rainbow-delimiters-depth-6-face :foreground "#b392f0")
    (rainbow-delimiters-depth-7-face :foreground "#9ecbff")
    (rainbow-delimiters-depth-8-face :foreground "#ffab70")
    (rainbow-delimiters-depth-9-face :foreground "#b392f0")
    (rainbow-delimiters-base-error-face :inherit 'rainbow-delimiters-base-face :foreground error)
    (rainbow-delimiters-base-face :inherit 'default)
    (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
    ;;;; re-builder <built-in>
    (reb-match-0 :foreground orange  :inverse-video t)
    (reb-match-1 :foreground magenta :inverse-video t)
    (reb-match-2 :foreground green   :inverse-video t)
    (reb-match-3 :foreground yellow  :inverse-video t)
    ;;;; rjsx-mode <modes:rjsx-mode>
    (rjsx-tag :foreground type)
    (rjsx-attr :foreground functions)
    ;;;; rpm-spec-mode <modes:rpm-spec-mode>
    (rpm-spec-macro-face        :foreground yellow)
    (rpm-spec-var-face          :foreground violet)
    (rpm-spec-tag-face          :foreground blue)
    (rpm-spec-obsolete-tag-face :foreground red)
    (rpm-spec-package-face      :foreground orange)
    (rpm-spec-dir-face          :foreground green)
    (rpm-spec-doc-face          :foreground orange)
    (rpm-spec-ghost-face        :foreground comments)
    (rpm-spec-section-face      :foreground magenta)
    ;;;; rst <built-in> <modes:rst-mode>
    (rst-block :inherit 'font-lock-constant-face)
    (rst-level-1 :inherit 'rst-adornment :weight 'bold)
    (rst-level-2 :inherit 'rst-adornment :weight 'bold)
    (rst-level-3 :inherit 'rst-adornment :weight 'bold)
    (rst-level-4 :inherit 'rst-adornment :weight 'bold)
    (rst-level-5 :inherit 'rst-adornment :weight 'bold)
    (rst-level-6 :inherit 'rst-adornment :weight 'bold)
    ;;;; sh-script <built-in> <modes:sh-mode,shell-script-mode>
    (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
    (sh-quoted-exec :inherit 'font-lock-preprocessor-face)
    ;;;; show-paren <built-in>
    ((show-paren-match &inherit paren-face-match))
    ((show-paren-mismatch &inherit paren-face-mismatch))
    ;;;; smart-mode-line
    (sml/charging          :foreground green)
    (sml/discharging       :foreground yellow :weight 'bold)
    (sml/filename          :foreground violet :weight 'bold)
    (sml/git               :foreground blue)
    (sml/modified          :foreground cyan)
    (sml/outside-modified  :foreground cyan)
    (sml/process           :weight 'bold)
    (sml/read-only         :foreground cyan)
    (sml/sudo              :foreground orange :weight 'bold)
    (sml/vc-edited         :foreground green)
    ;;;; smartparens
    (sp-pair-overlay-face :background region)
    ((sp-show-pair-match-face    &inherit show-paren-match))
    ((sp-show-pair-mismatch-face &inherit show-paren-mismatch))
    ;;;; smerge-tool
    (smerge-lower :background (doom-blend green bg 0.2))
    (smerge-upper :background (doom-blend red base3 0.2))
    (smerge-base  :background (doom-blend blue bg 0.2))
    (smerge-markers :background comments :foreground bg :distant-foreground fg :weight 'bold)
    (smerge-refined-added   :inherit 'diff-added :inverse-video t)
    (smerge-refined-removed :inherit 'diff-removed :inverse-video t)
    ;; Emacs <25 compatibility
    ((smerge-mine  &inherit smerge-upper))
    ((smerge-other &inherit smerge-lower))
    ;;;; solaire-mode
    (solaire-default-face  :inherit 'default :background bg-alt)
    (solaire-hl-line-face  :inherit 'hl-line :background bg :extend t)
    (solaire-org-hide      :inherit 'org-hide :foreground bg-alt)
    ;;;; spaceline
    (spaceline-highlight-face   :background highlight)
    (spaceline-modified         :background vc-modified)
    (spaceline-unmodified       :background constants)
    (spaceline-python-venv      :foreground magenta :distant-foreground violet)
    (spaceline-flycheck-error   :inherit 'error     :distant-background base0)
    (spaceline-flycheck-warning :inherit 'warning   :distant-background base0)
    (spaceline-flycheck-info    :inherit 'success   :distant-background base0)
    (spaceline-evil-normal      :background blue)
    (spaceline-evil-insert      :background green)
    (spaceline-evil-emacs       :background cyan)
    (spaceline-evil-replace     :background orange)
    (spaceline-evil-visual      :background grey)
    (spaceline-evil-motion      :background magenta)
    ;;;; spell-fu
    (spell-fu-incorrect-face
     `((((supports :underline (:style wave)))
        (:underline (:style wave :color ,(car error))))
       (t (:inherit error :underline t))))
    ;;;; stripe-buffer
    (stripe-highlight
     (&light :background base5)
     (&dark  :background base3))
    ;;;; symbol-overlay
    (symbol-overlay-default-face
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1)  :distant-foreground fg-alt))
    (symbol-overlay-face-1 :background (doom-blend blue bg 0.4)    :distant-foreground fg-alt)
    (symbol-overlay-face-2 :background (doom-blend violet bg 0.4)  :distant-foreground fg-alt)
    (symbol-overlay-face-3 :background (doom-blend yellow bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-4 :background (doom-blend orange bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-5 :background (doom-blend red bg 0.3)     :distant-foreground fg-alt)
    (symbol-overlay-face-6 :background (doom-blend magenta bg 0.3) :distant-foreground fg-alt)
    (symbol-overlay-face-7 :background (doom-blend green bg 0.4)   :distant-foreground fg-alt)
    (symbol-overlay-face-8 :background (doom-blend cyan bg 0.2)    :distant-foreground fg-alt)
    ;;;; swiper
    (swiper-line-face    :background blue    :foreground base0)
    (swiper-match-face-1 :inherit 'unspecified :background base0   :foreground base5)
    (swiper-match-face-2 :inherit 'unspecified :background orange  :foreground base0 :weight 'bold)
    (swiper-match-face-3 :inherit 'unspecified :background magenta :foreground base0 :weight 'bold)
    (swiper-match-face-4 :inherit 'unspecified :background green   :foreground base0 :weight 'bold)
    ;;;; tabbar
    (tabbar-default             :foreground bg :background bg :height 1.0)
    (tabbar-highlight           :foreground fg :background selection :distant-foreground bg)
    (tabbar-button              :foreground fg :background bg)
    (tabbar-button-highlight    :inherit 'tabbar-button :inverse-video t)
    (tabbar-modified            :inherit 'tabbar-default :foreground red :weight 'bold)
    (tabbar-unselected          :inherit 'tabbar-default :foreground base5)
    (tabbar-unselected-modified :inherit 'tabbar-modified)
    (tabbar-selected
     :inherit 'tabbar-default :weight 'bold
     :foreground fg :background bg-alt)
    (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)
    ;;;; telephone-line
    (telephone-line-accent-active :foreground fg :background base4)
    (telephone-line-accent-inactive :foreground fg :background base2)
    (telephone-line-projectile :foreground green)
    (telephone-line-evil :foreground fg :weight 'bold)
    (telephone-line-evil-insert :background (doom-blend green bg 0.5) :weight 'bold)
    (telephone-line-evil-normal :background (doom-blend red bg 0.5) :weight 'bold)
    (telephone-line-evil-visual :background (doom-blend orange bg 0.5) :weight 'bold)
    (telephone-line-evil-replace :background (doom-color bg-alt) :weight 'bold)
    (telephone-line-evil-motion :background (doom-blend blue bg 0.5) :weight 'bold)
    (telephone-line-evil-operator :background (doom-blend violet bg 0.5) :weight 'bold)
    (telephone-line-evil-emacs :background (doom-blend magenta bg 0.5) :weight 'bold)
    ;;;; term <built-in>
    (term               :foreground fg)
    (term-bold          :weight 'bold)
    (term-color-black   :background base0   :foreground base0)
    (term-color-red     :background red     :foreground red)
    (term-color-green   :background green   :foreground green)
    (term-color-yellow  :background yellow  :foreground yellow)
    (term-color-blue    :background blue    :foreground blue)
    (term-color-magenta :background magenta :foreground magenta)
    (term-color-cyan    :background cyan    :foreground cyan)
    (term-color-white   :background base8   :foreground base8)
    ;;;; terraform-mode
    (terraform--resource-type-face :foreground type)
    (terraform--resource-name-face :foreground strings)
    ;;;; tldr
    (tldr-command-itself   :foreground bg :background green :weight 'semi-bold)
    (tldr-title            :foreground yellow :bold t :height 1.4)
    (tldr-description      :foreground fg :weight 'semi-bold)
    (tldr-introduction     :foreground (doom-blend blue bg 0.8) :weight 'semi-bold)
    (tldr-code-block       :foreground green :background region :weight 'semi-bold)
    (tldr-command-argument :foreground fg :background region )
    ;;;; typescript-mode <modes:typescript-mode,typescript-tsx-mode>
    (typescript-jsdoc-tag :foreground doc-comments)
    (typescript-jsdoc-type :foreground (doom-darken doc-comments 0.15))
    (typescript-jsdoc-value :foreground (doom-lighten doc-comments 0.15))
    ;;;; treemacs
    (treemacs-directory-face        :foreground fg)
    (treemacs-file-face             :foreground fg)
    (treemacs-fringe-indicator-face :foreground highlight)
    (treemacs-git-added-face        :foreground vc-added)
    (treemacs-git-conflict-face     :foreground red)
    (treemacs-git-modified-face     :foreground violet)
    (treemacs-git-untracked-face    :inherit 'font-lock-doc-face)
    (treemacs-on-failure-pulse-face :foreground base0 :background error   :extend t)
    (treemacs-on-success-pulse-face :foreground base0 :background success :extend t)
    (treemacs-root-face             :inherit 'font-lock-string-face :weight 'bold       :height 1.2)
    (treemacs-tags-face             :foreground highlight)
    ;;;; twittering-mode
    (twitter-divider  ; custom face in Doom Emacs
     (&light :underline `(:color ,(doom-lighten vertical-bar 0.2)))
     (&dark  :underline `(:color ,(doom-darken vertical-bar 0.2))))
    ;;;; undo-tree
    (undo-tree-visualizer-default-face       :foreground base5)
    (undo-tree-visualizer-current-face       :foreground green :weight 'bold)
    (undo-tree-visualizer-unmodified-face    :foreground base5)
    (undo-tree-visualizer-active-branch-face :foreground blue)
    (undo-tree-visualizer-register-face      :foreground yellow)
    ;;;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0 :weight 'light)
    (vimish-fold-fringe  :foreground magenta)
    ;;;; visual-regexp
    (vr/group-0 :background blue    :foreground bg)
    (vr/group-1 :background magenta :foreground bg)
    (vr/group-2 :background green   :foreground bg)
    (vr/match-0 :background (doom-blend green bg 0.2) :foreground fg)
    (vr/match-1 :background (doom-blend green bg 0.4) :foreground fg)
    (vr/match-separator-face :inherit 'bold :foreground red)
    ;;;; volatile-highlights
    (vhl/default-face :background grey)
    ;;;; vterm
    (vterm-color-black   :background (doom-lighten base0 0.25)   :foreground base0)
    (vterm-color-red     :background (doom-lighten red 0.25)     :foreground red)
    (vterm-color-green   :background (doom-lighten green 0.25)   :foreground green)
    (vterm-color-yellow  :background (doom-lighten yellow 0.25)  :foreground yellow)
    (vterm-color-blue    :background (doom-lighten blue 0.25)    :foreground blue)
    (vterm-color-magenta :background (doom-lighten magenta 0.25) :foreground magenta)
    (vterm-color-cyan    :background (doom-lighten cyan 0.25)    :foreground cyan)
    (vterm-color-white   :background (doom-lighten base8 0.25)   :foreground base8)
    ;;;; web-mode <modes:web-mode>
    (web-mode-block-control-face     :foreground builtin)
    (web-mode-block-delimiter-face   :foreground builtin)
    (web-mode-css-property-name-face :foreground type)
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground methods)
    (web-mode-html-tag-bracket-face  :foreground methods)
    (web-mode-html-attr-name-face    :foreground type)
    (web-mode-html-attr-value-face   :foreground strings)
    (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
    (web-mode-block-control-face     :foreground orange)
    (web-mode-html-tag-bracket-face  :foreground operators)
    (web-mode-json-key-face          :foreground strings)
    (web-mode-json-context-face      :foreground strings)
    (web-mode-keyword-face           :foreground keywords)
    (web-mode-string-face            :foreground strings)
    (web-mode-type-face              :foreground type)
    ;;;; wgrep <built-in>
    (wgrep-face :weight 'bold :foreground green :background base5)
    (wgrep-delete-face :foreground base3 :background red)
    (wgrep-done-face   :foreground blue)
    (wgrep-file-face   :foreground comments)
    (wgrep-reject-face :foreground red :weight 'bold)
    ;;;; which-func
    (which-func :foreground blue)
    ;;;; which-key
    (which-key-key-face                   :foreground green)
    (which-key-group-description-face     :foreground violet)
    (which-key-command-description-face   :foreground blue)
    (which-key-local-map-description-face :foreground magenta)
    ;;;; whitespace <built-in>
    (whitespace-empty    :background base3)
    (whitespace-space    :foreground base4)
    (whitespace-newline  :foreground base4)
    (whitespace-tab
     :foreground base4
     :background (if (default-value 'indent-tabs-mode) 'unspecified base3))
    (whitespace-indentation
     :foreground base4
     :background (if (default-value 'indent-tabs-mode) base3 'unspecified))
    (whitespace-trailing :inherit 'trailing-whitespace)
    (whitespace-line     :background base0 :foreground red :weight 'bold)
    ;;;; widget
    (widget-button-pressed :foreground red)
    (widget-documentation  :foreground green)
    (widget-single-line-field :background base3 :distant-foreground bg)
    (widget-field :background base3 :distant-foreground bg
                  :box `(:line-width -1 :color ,grey) :extend t)
    ;;;; window-divider
    (window-divider :inherit 'vertical-border)
    (window-divider-first-pixel :inherit 'window-divider)
    (window-divider-last-pixel  :inherit 'window-divider)
    ;;;; winum
    (winum-face :inherit 'bold :foreground highlight)
    ;;;; woman <built-in>
    (woman-bold :inherit 'Man-overstrike)
    (woman-italic :inherit 'Man-underline)
    ;;;; xah-elisp-mode
    (xah-elisp-at-symbol     :inherit 'font-lock-warning-face)
    (xah-elisp-cap-variable  :inherit 'font-lock-preprocessor-face)
    (xah-elisp-command-face  :inherit 'font-lock-type-face)
    (xah-elisp-dollar-symbol :inherit 'font-lock-variable-name-face)
    ;;;; workgroups2
    (wg-current-workgroup-face :foreground base0 :background highlight)
    (wg-other-workgroup-face   :foreground base5)
    (wg-divider-face           :foreground grey)
    (wg-brace-face             :foreground highlight)
    ;;;; yasnippet
    (yas-field-highlight-face :inherit 'match)
    ;;;; xref <built-in>
    ((xref-file-header &inherit compilation-info))
    ((xref-line-number &inherit compilation-line-number))
    ((xref-match &inherit match))
    ;;;; --- END Package faces ------------------
    )
  "TODO")

;;; doom-github-dark-dimmed-theme.el ends here
