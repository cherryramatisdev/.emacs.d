;; -*- coding: utf-8; lexical-binding: t; -*-
;; ibuffer-config.el --- Customizations for `ibuffer'

;;; Commentary:

;;; Code:
(defun key/d ()
  (interactive)
  (if (string-equal major-mode "ibuffer-mode")
      (call-interactively 'ibuffer-mark-for-delete)
    (xah-delete-backward-char-or-bracket-text)))

(defun key/x ()
  (interactive)
  (if (string-equal major-mode "ibuffer-mode")
      (call-interactively 'ibuffer-do-kill-on-deletion-marks)
    (xah-cut-line-or-region)))

(define-key xah-fly-command-map (kbd "d") 'key/d)
(define-key xah-fly-command-map (kbd "x") 'key/x)

(provide 'ibuffer-config)
;;; ibuffer-config.el ends here
