;; -*- coding: utf-8; lexical-binding: t; -*-
;; dired-config.el --- Configuration for dired

;;; Commentary:

;;; Code:
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	 ("TAB" . dired-find-file)
	 ("<backtab>" . dired-up-directory)))

(provide 'dired-config)
;;; dired-config.el ends here
