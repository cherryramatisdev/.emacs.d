;; -*- coding: utf-8; lexical-binding: t; -*-
;; completion-config.el --- Completion at point configuration

;;; Commentary:

;;; Code:
(if (not (package-installed-p 'corfu))
    (error "Please install corfu"))

(if (not (package-installed-p 'cape))
    (error "Please install cape"))

(require 'corfu)

(setq-default corfu-cycle t)
(setq-default corfu-auto t)
(setq-default corfu-auto-prefix 2)
(setq-default corfu-auto-delay 0.0)
(setq-default corfu-quit-at-boundary t)
(setq-default corfu-echo-documentation 0.25)
(setq-default corfu-preview-current 'insert)
(setq-default corfu-preselect-first nil)

(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
;; (define-key corfu-map (kbd [tab]) 'corfu-next)
;; (define-key corfu-map (kbd [backtab]) 'corfu-previous)

(global-corfu-mode)

(require 'cape)

;; Add `completion-at-point-functions', used by `completion-at-point'
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence then pcomplete capf, no errors or messages
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                                 corfu-quit-no-match t
                                 corfu-auto nil)
            (corfu-mode)))

(provide 'completion-config)
;;; completion-config.el ends here
