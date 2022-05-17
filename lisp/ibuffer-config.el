;; -*- coding: utf-8; lexical-binding: t; -*-
;; ibuffer-config.el --- Customizations for `ibuffer'

;;; Commentary:

;;; Code:
(defun key/d ()
  (interactive)
  (when (string-equal major-mode "ibuffer-mode")
    (call-interactively 'ibuffer-mark-for-delete))
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'dired-flag-file-deletion))
  (xah-delete-backward-char-or-bracket-text))

(defun key/x ()
  (interactive)
  (when (string-equal major-mode "ibuffer-mode")
    (call-interactively 'ibuffer-do-kill-on-deletion-marks))
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'dired-do-flagged-delete))
  (xah-cut-line-or-region))

(define-key xah-fly-command-map (kbd "d") 'key/d)
(define-key xah-fly-command-map (kbd "x") 'key/x)

(provide 'ibuffer-config)
;;; ibuffer-config.el ends here
