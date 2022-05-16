;; -*- coding: utf-8; lexical-binding: t; -*-
;; keybindings.el --- General keybindings that work on all modes

;;; Commentary:

;;; Code:
;; TODO: discover what leader to set this
(define-prefix-command 'my-leader)

(define-key my-leader (kbd "c") 'compile)
(define-key my-leader (kbd "r") 'recompile)

(provide 'keybindings)
;;; keybindings.el ends here
