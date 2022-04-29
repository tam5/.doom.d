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

(defconst li-client-id "***REMOVED***"
  "Local LiveIntent client id.")

(defconst li-client-secret "***REMOVED***"
  "Local LiveIntent client secret.")

(defvar api-token nil
  "The current api token.")

(defvar li-token nil
  "The current api token.")

(defvar refresh-token nil
  "The current heimdall refresh token.")

(defun +liveintent/restclient-hook ()
  "Update token from a request."
  (save-excursion
    (save-match-data
      ;; update regexp to extract required data
      (when (re-search-forward "\"access_token\":\"\\(.*?\\)\"" nil t)
        (setq api-token (match-string 1)))
      (when (re-search-forward "\"li_token\":\"\\(.*?\\)\"" nil t)
        (setq li-token (match-string 1)))
      (when (re-search-forward "\"refresh_token\":\"\\(.*?\\)\"" nil t)
        (setq refresh-token (match-string 1))))))

(add-hook 'restclient-response-received-hook #'+liveintent/restclient-hook)
