;; -*- coding: utf-8; lexical-binding: t; -*-
;; corfu-config.el --- Completion at point configuration

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'corfu))
    (error "Please install corfu"))

(require 'corfu)

(setq corfu-cycle t)
(setq corfu-separator ?\s)
(setq corfu-quit-at-boundary nil)
(setq corfu-quit-no-match t)
(setq corfu-preview-current t)
(setq corfu-preselect-first nil)
(setq corfu-echo-documentation t)

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t

(global-set-key (kbd "C-n") 'corfu-complete)
(define-key corfu-map (kbd "C-n") 'corfu-next)
(define-key corfu-map (kbd "C-p") 'corfu-previous)

(global-corfu-mode)

(provide 'corfu-config)
;;; corfu-config.el ends here
