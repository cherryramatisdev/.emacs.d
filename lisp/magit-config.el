;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:
(add-hook 'magit-status-hook 'xah-fly-command-mode-activate)

(define-key my-leader (kbd "g") 'magit-status)

(defun key/s ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-stage)
    (open-line 1)))

(defun key/q ()
  (interactive)
  (when (string-equal major-mode "magit-status-mode")
    (magit-mode-bury-buffer t))
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'quit-window))
  (when (string-equal major-mode "ibuffer-mode")
    (call-interactively 'quit-window))
  (xah-reformat-lines))

(defun key/c ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-commit)
    (xah-copy-line-or-region)))

(defun key/b ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-branch nil)
    (xah-toggle-letter-case)))

(define-key xah-fly-command-map (kbd "s") 'key/s)
(define-key xah-fly-command-map (kbd "q") 'key/q)
(define-key xah-fly-command-map (kbd "c") 'key/c)
(define-key xah-fly-command-map (kbd "b") 'key/b)

(provide 'magit-config)
;;; magit-config.el ends here
