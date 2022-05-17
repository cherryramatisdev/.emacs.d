;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:
(require 'magit)

(add-hook 'magit-status-hook 'xah-fly-command-mode-activate)

(define-key my-leader (kbd "g") 'magit-status)
(define-key magit-status-mode-map (kbd "C-w") 'magit-mode-bury-buffer)

(provide 'magit-config)
;;; magit-config.el ends here
