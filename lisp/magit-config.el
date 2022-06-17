;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status)
	 :map my-leader
	 ("g" . magit-status)))

(provide 'magit-config)
;;; magit-config.el ends here
