(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

(ensure-package-installed 
 'async
 'web-mode 
 'typescript-mode
 'magit
 'perspective
 'ag
 'corfu
 'cape
 'eglot
 'ace-jump-mode
 'expand-region
 'multiple-cursors
 )

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq inhibit-startup-screen t)

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

(setq visible-bell 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 5)
(setq global-mark-ring-max 5)

(setq display-line-numbers 'relative)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

(delete-selection-mode 1)

(setq shift-select-mode nil)

(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(setq search-whitespace-regexp "[-_ \t\n]+")

;; (setq x-selection-timeout 300)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard-manager nil)

(electric-indent-mode 0)
(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 4)

(setq sentence-end-double-space nil )

(defalias 'yes-or-no-p 'y-or-n-p)

(setq auto-insert-directory "~/.emacs.d/templates/")
(add-hook 'find-file-hook 'auto-insert)
(define-auto-insert "\.el" "emacs-lisp-lib.el")

(load-theme 'modus-vivendi)

(set-face-attribute 'default nil :height 140)

(progn
  ;; minibuffer setup
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1)
  ;; big minibuffer height, for ido to show choices vertically
  (setq max-mini-window-height 0.5)
  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))

(progn
  ;; minibuffer enhanced completion
  (require 'icomplete)
  (icomplete-mode 1)
  ;; show choices vertically
  (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (define-key icomplete-minibuffer-map (kbd "C-f") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-b") 'icomplete-backward-completions))

(fido-vertical-mode 1)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(require 'keybindings)

(require 'dired)

(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top))

(setq eshell-destroy-buffer-when-process-dies t)

(require 'org)

(defun insert-code-block ()
  (interactive)
  (insert "#+BEGIN_SRC emacs-lisp")
  (newline)
  (insert "#+END_SRC")
  (previous-line)
  (end-of-line)
  (newline))

(define-key org-mode-map (kbd "C-c C-b") 'insert-code-block)

(progn
  ;; org-mode
  ;; make “org-mode” syntax color code sections
  (setq org-src-fontify-natively t)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil))

(require 'ts-js-config)
(require 'magit-config)
(require 'perspectives)
(require 'dired-config)
(require 'isearch-config)
(require 'ibuffer-config)
(require 'completion-config)
(setq projects (delete ".." (delete "." (directory-files "D:\\git"))))
(setq projectPrefix "D:\\git")
(require 'switch-project)
(require 'eglot-config)

(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'iy-go-to-char)
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

(require 'async)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)
(setq-default async-bytecomp-allowed-packages '(all))
