;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'magit))
    (error "Please install magit"))

(require 'magit)

(add-hook 'magit-status-hook 'xah-fly-command-mode-activate)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'magit-config)
;;; magit-config.el ends here
