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

(after! company-mode
  ;; make company show up faster
  (setq company-idle-delay 0
        company-minimum-prefix-length 0
        company-lsp-cache-candidates t))
