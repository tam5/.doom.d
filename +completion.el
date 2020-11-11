;;; $DOOMDIR/+completion.el -*- lexical-binding: t; -*-
;;;
;;; /-----------------------------------------------------------------------------------------
;;; | Completion
;;; |-----------------------------------------------------------------------------------------
;;; |
;;; |
;;; |
;;; |
;;; /

(setq +file-templates-dir (concat (dir!) "/templates/"))

;; override this function
(defun +file-templates-check-h ()
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`+file-templates-alist' that applies to it."
  (and buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (not (member (substring (buffer-name) 0 1) '("*" " ")))
       ;; (not (file-exists-p buffer-file-name)) ; remove this check
       (not (buffer-modified-p))
       (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
         (apply #'+file-templates--expand rule))))

;; make company show up faster
(setq company-idle-delay 0
      company-minimum-prefix-length 0
      company-lsp-cache-candidates t)

(defun +completion/expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
      (null (yas/expand))
      (company-abort))
      (company-complete-selection)))
