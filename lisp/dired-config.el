;; -*- coding: utf-8; lexical-binding: t; -*-
;; dired-config.el --- Configuration for dired

;;; Commentary:

;;; Code:
(define-key dired-mode-map (kbd "C-b") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-f") 'dired-find-file)
(define-key dired-mode-map (kbd "C-w") (lambda () (interactive) (progn (kill-buffer) (delete-window))))

(provide 'dired-config)
;;; dired-config.el ends here
