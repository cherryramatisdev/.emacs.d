;; -*- coding: utf-8; lexical-binding: t; -*-
;; perspectives.el --- Configuring workspaces on emacs

;;; Commentary:

;;; Code:

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(provide 'perspectives)
;;; perspectives.el ends here
