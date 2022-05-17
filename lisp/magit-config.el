;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:
(define-key my-leader (kbd "g") 'magit-status)

(defun key/s ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-stage)
    (open-line 1)))

(defun key/q ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-mode-bury-buffer t)
    (xah-reformat-lines)))

(defun key/c ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-commit)
    (xah-copy-line-or-region)))

(defun key/p ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-push)
    (xah-insert-space-before)))

(define-key xah-fly-command-map (kbd "s") 'key/s)
(define-key xah-fly-command-map (kbd "q") 'key/q)
(define-key xah-fly-command-map (kbd "c") 'key/c)
(define-key xah-fly-command-map (kbd "p") 'key/p)

(provide 'magit-config)
;;; magit-config.el ends here
