;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'load-path "~/.emacs.d/lisp")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)

(fido-vertical-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package naysayer-theme
  :init (load-theme 'naysayer t))

(use-package s)

(require 'xah)
(require 'dired-config)
(require 'isearch-config)
(require 'keybindings)
(require 'magit-config)

(require 'perspectives)
(require 'switch-project)
(setq projects (delete ".." (delete "." (directory-files "d:\\git"))))
(setq projectPrefix "d:\\git")
(require 'ts-js-config)

(use-package cider)

(use-package tempel
  :after (xah-fly-keys)
  :bind (("M-[" . tempel-previous)
	 ("M-]" . tempel-next)
	 ("M-=" . tempel-expand))
  :config
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))


(if (not (daemonp))
    (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cider naysayer-theme use-package yaml-mode xah-fly-keys web-mode typescript-mode tempel sly powershell perspective persp-mode multiple-cursors magit expand-region evil eglot crux corfu company-tabnine cape async ag ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
