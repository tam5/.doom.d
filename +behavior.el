;;; $DOOMDIR/+behavior.el -*- lexical-binding: t; -*-
;;;
;;; /-----------------------------------------------------------------------------------------
;;; | Behavior
;;; |-----------------------------------------------------------------------------------------
;;; |
;;; | This file is used to tweak some of the behavior of the features we have enabled. I'm
;;; | not really sure what else to say about this file but maybe when we put more stuff
;;; | here I'll think of something more interesting to put here. Who knows..........
;;; /

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; make company show up faster
(setq company-idle-delay 0
      company-minimum-prefix-length 0
      company-lsp-cache-candidates t)

(setq lsp-keep-workspace-alive t
      lsp-file-watch-threshold nil
      lsp-eldoc-render-all nil
      lsp-ui-doc-enable t
      lsp-ui-doc-max-height 20
      lsp-ui-doc-delay .7
      lsp-ui-doc-max-width 150)
