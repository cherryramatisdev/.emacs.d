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
 
(define-key my-leader (kbd "s") 'persp-frame-switch)
(define-key my-leader (kbd "TAB") 'persp-prev)

(define-key xah-fly-command-map (kbd "<backspace>") my-leader)

(global-set-key (kbd "C-k") 'forward-paragraph)

;; This is needed so I can bound C-i without changing TAB
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'backward-paragraph)

(provide 'keybindings)
;;; keybindings.el ends here
