;; -*- coding: utf-8; lexical-binding: t; -*-
;; keybindings.el --- General keybindings that work on all modes

;;; Commentary:

;;; Code:
(defun key/d ()
  (interactive)
  (cond
   ((string-equal major-mode "ibuffer-mode") (call-interactively 'ibuffer-mark-for-delete))
   ((string-equal major-mode "dired-mode") (call-interactively 'dired-flag-file-deletion))
   (t (xah-delete-backward-char-or-bracket-text))))

(defun key/g ()
  (interactive)
  (cond
   ((string-equal major-mode "dired-mode") (call-interactively 'revert-buffer))
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-refresh))
   (t (xah-delete-current-text-block))))

(defun key/x ()
  (interactive)
  (cond
   ((string-equal major-mode "ibuffer-mode")(call-interactively 'ibuffer-do-kill-on-deletion-marks))
   ((string-equal major-mode "dired-mode") (call-interactively 'dired-do-flagged-delete))
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-discard))
   (t (xah-cut-line-or-region))))

(defun key/s ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-stage))
   (t (call-interactively 'open-line))))

(defun key/c ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-commit))
   (t (xah-copy-line-or-region))))

(defun key/b ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-branch)
    (t (xah-toggle-letter-case)))))

(defun key/q ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (magit-mode-bury-buffer t))
   (t (xah-reformat-lines))))

(define-key xah-fly-command-map (kbd "d") 'key/d)
(define-key xah-fly-command-map (kbd "x") 'key/x)
(define-key xah-fly-command-map (kbd "s") 'key/s)
(define-key xah-fly-command-map (kbd "c") 'key/c)
(define-key xah-fly-command-map (kbd "b") 'key/b)
(define-key xah-fly-command-map (kbd "g") 'key/g)
(define-key xah-fly-command-map (kbd "q") 'key/q)

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
 
(define-key xah-fly-command-map (kbd "<backspace>") 'my-leader)

(defun cancel ()
  (interactive)
  (keyboard-quit)
  (xah-fly-command-mode-activate))

(xah-fly--define-keys
 xah-fly-shared-map
 '(("<home>" . cancel))
 :direct)

(defun my/kill-window ()
  (interactive)
  (if (one-window-p)
      (kill-buffer)
    (quit-window t)))

(global-set-key (kbd "C-x k") 'my/kill-window)

(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)

(provide 'keybindings)
;;; keybindings.el ends here
