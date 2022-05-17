;; -*- coding: utf-8; lexical-binding: t; -*-
;; ibuffer-config.el --- Ibuffer configuration

;;; Commentary:

;;; Code:
(require 'ibuffer)

(define-key ibuffer-mode-map (kbd "C-w") (lambda () (interactive) (progn (kill-buffer) (delete-window))))

(provide 'ibuffer-config)
;;; ibuffer-config.el ends here
