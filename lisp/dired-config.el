;; -*- coding: utf-8; lexical-binding: t; -*-
;; dired-config.el --- Configuration for dired

;;; Commentary:

;;; Code:
(define-key dired-mode-map (kbd "C-b") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-f") 'dired-find-file)

(defun my/rename-file ()
  (interactive)
  (rename-file (thing-at-point 'word 'no-properties) (read-string "Insert new name: "))
  (call-interactively 'revert-buffer))

(define-key dired-mode-map (kbd "R") 'my/rename-file)

(provide 'dired-config)
;;; dired-config.el ends here
