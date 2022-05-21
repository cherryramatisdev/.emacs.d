;; -*- coding: utf-8; lexical-binding: t; -*-
;; perspectives.el --- Configuring workspaces on emacs

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'perspective))
    (error "Please install perspective"))

(require 'perspective)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-x x"))

(persp-mode 1)

(provide 'perspectives)
;;; perspectives.el ends here
