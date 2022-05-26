;; -*- coding: utf-8; lexical-binding: t; -*-
;; keybindings.el --- General keybindings that work on all modes

;;; Commentary:

;;; Code:
(defun key/d ()
  (interactive)
  (when (string-equal major-mode "ibuffer-mode")
    (call-interactively 'ibuffer-mark-for-delete))
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'dired-flag-file-deletion))
  (xah-delete-backward-char-or-bracket-text))

(defun key/g ()
  (interactive)
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'revert-buffer))
  (xah-delete-current-text-block))

(defun key/x ()
  (interactive)
  (when (string-equal major-mode "ibuffer-mode")
    (call-interactively 'ibuffer-do-kill-on-deletion-marks))
  (when (string-equal major-mode "dired-mode")
    (call-interactively 'dired-do-flagged-delete))
  (when (string-equal major-mode "magit-status-mode")
    (call-interactively 'magit-discard))
  (xah-cut-line-or-region))

(defun key/s ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-stage)
    (open-line 1)))

(defun key/c ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-commit)
    (xah-copy-line-or-region)))

(defun key/b ()
  (interactive)
  (if (string-equal major-mode "magit-status-mode")
      (magit-branch nil)
    (xah-toggle-letter-case)))

(define-key xah-fly-command-map (kbd "d") 'key/d)
(define-key xah-fly-command-map (kbd "x") 'key/x)
(define-key xah-fly-command-map (kbd "s") 'key/s)
(define-key xah-fly-command-map (kbd "c") 'key/c)
(define-key xah-fly-command-map (kbd "b") 'key/b)
(define-key xah-fly-command-map (kbd "g") 'key/g)

(defun my/find-file ()
  (interactive)
  (cond
   ((string-equal major-mode "dired-mode") (find-file (read-string "Insert file name: ")))
   (t (find-file (read-file-name "Select file: ")))))

(define-key xah-fly-command-map (kbd "SPC i e") 'my/find-file)

;; ssss-----------------------------------------
;; Local Leader
(define-prefix-command 'my-leader)

(define-key my-leader (kbd "c") 'compile)
(define-key my-leader (kbd "r") 'recompile)

(define-key my-leader (kbd "p") nil)
(define-key my-leader (kbd "f") 'project-find-file)
(define-key my-leader (kbd "pc") 'project-compile)
(define-key my-leader (kbd "pe") 'project-eshell)

(define-key my-leader (kbd "s") 'persp-switch)
(define-key my-leader (kbd "TAB") 'persp-next)
(define-key my-leader (kbd "<backspace>") 'persp-kill)
 
(define-key xah-fly-command-map (kbd "<backspace>") my-leader)

(global-set-key (kbd "C-k") 'forward-paragraph)

;; This is needed so I can bound C-i without changing TAB
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'backward-paragraph)

(defun my/kill-window ()
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (quit-window t)))

(global-set-key (kbd "C-w") 'my/kill-window)

(provide 'keybindings)
;;; keybindings.el ends here
