;; -*- coding: utf-8; lexical-binding: t; -*-
;; keybindings.el --- General keybindings that work on all modes

;;; Commentary:

;;; Code:
(define-prefix-command 'my-leader)

(define-key my-leader (kbd "c") 'compile)
(define-key my-leader (kbd "r") 'recompile)

(define-key my-leader (kbd "p") nil)
(define-key my-leader (kbd "f") 'project-find-file)
(define-key my-leader (kbd "pp") 'project-switch-project)
(define-key my-leader (kbd "pc") 'project-compile)
(define-key my-leader (kbd "pe") 'project-eshell)

(define-key xah-fly-command-map (kbd "<backspace>") my-leader)

(provide 'keybindings)
;;; keybindings.el ends here
