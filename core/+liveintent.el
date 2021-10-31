;;; $DOOMDIR/liveintent.el -*- lexical-binding: t; -*-
;;;
;;; /-------------------------------------------------------------------------------------------
;;; | LiveIntent config
;;; |-------------------------------------------------------------------------------------------
;;; /

(defconst li-username "***REMOVED***"
  "My LiveIntent username.")

(defconst li-password "***REMOVED***"
  "My LiveIntent password.")

(defvar heimdall-token nil
  "The current heimdall token.")

(defvar heimdall-refresh-token nil
  "The current heimdall refresh token.")

(defvar heimdall-host nil
  "The current heimdall host.")

(defvar merlin-token nil
  "The current merlin token.")

(defvar merlin-host nil
  "The current merlin host.")

(defun +liveintent/restclient-hook ()
  "Update token from a request."
  (save-excursion
    (save-match-data
      ;; update regexp to extract required data
      (when (re-search-forward "\"access_token\":\"\\(.*?\\)\"" nil t)
        (setq heimdall-token (match-string 1)))
      (when (re-search-forward "\"token\":\"\\(.*?\\)\"" nil t)
        (setq merlin-token (match-string 1)))
      (when (re-search-forward "\"refresh_token\":\"\\(.*?\\)\"" nil t)
        (setq heimdall-refresh-token (match-string 1))))))

(add-hook 'restclient-response-received-hook #'+liveintent/restclient-hook)
