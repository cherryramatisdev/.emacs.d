;; -*- coding: utf-8; lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
(electric-pair-mode 1)

(fido-vertical-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package s)

(use-package naysayer-theme
  :init (load-theme 'naysayer t))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
	 ("TAB" . dired-find-file)
	 ("<backtab>" . dired-up-directory)))

(define-key isearch-mode-map (kbd "TAB") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<backtab>") 'isearch-repeat-backward)

(use-package xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  :init
  (xah-fly-keys))

(use-package magit
  :bind (("C-x g" . magit-status)
	 :map my-leader
	 ("g" . magit-status))
  :config
  (defun open-current-pr-external ()
    "Open current PR on the external browser."
    (interactive)
    (magit-git-command-topdir "gh pr view -w"))

  (defun open-current-pr-locally ()
    "Open current PR on a local buffer.

TODO: fix this function because it's not working right now."
    (interactive)
    (magit-git-command-topdir "gh pr view"))

  (transient-define-prefix magit-gh ()
    "Run some `gh' commands."
    ["Actions"
     ("e" "Open on external browser" open-current-pr-external)
     ("l" "Open on local buffer (Not working yet)" open-current-pr-locally)]))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.tsx\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight nil))

(use-package typescript-mode
  :hook ((subword-mode . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(defun prettier-format ()
    (interactive)
    (call-process "yarn" nil "*prettier-output*" nil "prettier" "--write" (buffer-file-name))
    (revert-buffer nil 'no-confirm t))

(defun run-prettier-on-save ()
    "Format current file if it's running web mode"
    (interactive)
    (when (string-equal major-mode "web-mode")
      (prettier-format))
    (when (string-equal major-mode "typescript-mode")
      (prettier-format)))

(add-hook 'after-save-hook 'run-prettier-on-save)

(use-package cider)

(use-package tempel
  :after (xah-fly-keys)
  :bind (("M-[" . tempel-previous)
	 ("M-]" . tempel-next)
	 ("M-=" . tempel-expand)))

(use-package elixir-mode
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(require 'keybindings)
(require 'switch-project)
(setq projects (delete ".." (delete "." (directory-files "d:\\git"))))
(setq projectPrefix "d:\\git")

(if (not (daemonp))
    (server-start))
