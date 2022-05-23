;; -*- coding: utf-8; lexical-binding: t; -*-
;; company-config.el --- Completion at point configuration

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'company))
    (error "Please install company"))

(if (not (package-installed-p 'company-tabnine))
    (error "Please install company tabnine"))

(require 'company)
(require 'company-tabnine)

(add-to-list 'company-backends #'company-tabnine)

(setq company-idle-delay 0)
(setq company-show-numbers t)

(global-company-mode)

(provide 'company-config)
;;; company-config.el ends here
