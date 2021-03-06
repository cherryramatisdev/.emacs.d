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

(defun key/r ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-rebase))
   (t (call-interactively 'kill-word))))

(defun key/c ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-commit))
   (t (xah-copy-line-or-region))))

(defun key/b ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-branch))
   (t (xah-toggle-letter-case))))

(defun key/v ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-gh))
   (t (xah-paste-or-paste-previous))))

(defun key/q ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (magit-mode-bury-buffer t))
   (t (xah-reformat-lines))))

(defun key/dash ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-section-toggle))
   ((string-equal major-mode "dired-mode") (call-interactively 'dired-up-directory))
   (t (xah-backward-punct))))

(defun key/equal ()
  (interactive)
  (cond
   ((string-equal major-mode "magit-status-mode") (call-interactively 'magit-section-toggle))
   (t (xah-forward-punct))))

(define-key xah-fly-command-map (kbd "d") 'key/d)
(define-key xah-fly-command-map (kbd "x") 'key/x)
(define-key xah-fly-command-map (kbd "s") 'key/s)
(define-key xah-fly-command-map (kbd "c") 'key/c)
(define-key xah-fly-command-map (kbd "b") 'key/b)
(define-key xah-fly-command-map (kbd "g") 'key/g)
(define-key xah-fly-command-map (kbd "q") 'key/q)
(define-key xah-fly-command-map (kbd "r") 'key/r)
(define-key xah-fly-command-map (kbd "v") 'key/v)
(define-key xah-fly-command-map (kbd "-") 'key/dash)
(define-key xah-fly-command-map (kbd "=") 'key/equal)

;; ssss-----------------------------------------
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

;; ssss-----------------------------------------
(defun confirm-stuff ()
  "This function will serve as C-c C-c for various buffer modes, like on magit commit that you do C-c C-c to make a commit effectively, we'll now just use TAB RET for example"
  (interactive)
  (cond
   ((string-equal major-mode "text-mode") (call-interactively 'with-editor-finish))
   (t (message ";)"))))

(defun cancel-stuff ()
  "This function will serve as C-c C-k for various buffer modes, like on magit commit that you do C-c C-k to make a commit effectively, we'll now just use TAB DEL for example"
  (interactive)
  (cond
   ((string-equal major-mode "text-mode") (call-interactively 'with-editor-cancel))
   (t (message ";)"))))

(define-key xah-fly-command-map (kbd "TAB") 'my-c-c-leader)
(define-key my-c-c-leader (kbd "RET") 'confirm-stuff)
(define-key my-c-c-leader (kbd "DEL") 'cancel-stuff)

(defun cancel ()
  (interactive)
  (keyboard-quit)
  (xah-fly-command-mode-activate))

(define-key xah-fly-command-map (kbd "<home>") 'cancel)

(xah-fly--define-keys
 xah-fly-shared-map
 '(("<home>" . cancel))
 :direct)

(provide 'keybindings)
;;; keybindings.el ends here
