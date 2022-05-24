;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'magit))
    (error "Please install magit"))

(require 'magit)

(add-hook 'magit-status-hook 'xah-fly-command-mode-activate)

(define-key my-leader (kbd "g") 'magit-status)

(define-key magit-status-mode-map (kbd "C-w") 'my/kill-window)

(provide 'magit-config)
;;; magit-config.el ends here
