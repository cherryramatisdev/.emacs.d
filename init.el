;; -*- coding: utf-8; lexical-binding: t; -*-
;; Emacs settings

;; ssss-----------------------------------------
;; Configure emacs access to MELPA

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
 'xah-fly-keys 
 'web-mode 
 'typescript-mode
 'magit
 'perspective
 'ag
 'company
 'company-tabnine
 'eglot
 )

;; ssss-----------------------------------------
;; Add libraries to emacs world
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ssss-----------------------------------------
;; Load xah fly keys
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(xah-fly-keys 1)

;; ssss-----------------------------------------
;; Define general keybindings
(require 'keybindings)

;; ssss-----------------------------------------
;; initial window and default window
(setq inhibit-startup-screen t)

;; ssss-----------------------------------------
;; This is important for emacs use LF line endings instead of CRLF:
;; https://www.aleksandrhovhannisyan.com/blog/crlf-vs-lf-normalizing-line-endings-in-git/
;; https://stackoverflow.com/questions/1674481/how-to-configure-gnu-emacs-to-write-unix-or-dos-formatted-files-by-default
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

;; ssss-----------------------------------------
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; ssss-----------------------------------------
;; user interface

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

(load-theme 'modus-vivendi)

;; Disable bells on windows, so annoying
(setq visible-bell 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; ssss-----------------------------------------
(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top))

;; ssss-----------------------------------------

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 5)
(setq global-mark-ring-max 5)

;; ssss-----------------------------------------
;; Emacs: Font Setup http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html

;; set default font
(set-frame-font
 (cond
  ((string-equal system-type "windows-nt")
   (if (member "Consolas" (font-family-list)) "Consolas" nil))
  ((string-equal system-type "darwin")
   (if (member "Menlo" (font-family-list)) "Menlo-16" nil))
  ((string-equal system-type "gnu/linux")
   (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
  (t nil))
 t t)

;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")))

;; set font for chinese characters
(set-fontset-font
 t
 '(#x4e00 . #x9fff)
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
    ((member "SimHei" (font-family-list)) "SimHei")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Hei" (font-family-list)) "Hei")
    ((member "Heiti SC" (font-family-list)) "Heiti SC")
    ((member "Heiti TC" (font-family-list)) "Heiti TC")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))

(set-face-attribute 'default nil :height 140)

;; ssss-----------------------------------------

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


;; make buffer switch command do suggestions, also for find-file command
(fido-vertical-mode 1)

;; ssss-----------------------------------------
;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; ssss-----------------------------------------
;; editing related

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

(setq shift-select-mode nil)

(electric-pair-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

;; bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://xahlee.info/emacs/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard-manager nil)

;; ssss---------------------------------------------------
;; indentation, end of line

(electric-indent-mode 0)

(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 4)

(setq sentence-end-double-space nil )

;; ssss---------------------------------------------------

(progn
 ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://xahlee.info/emacs/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))

;; ssss---------------------------------------------------
;; edit related

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        ))

;; ssss---------------------------------------------------

;; convenient
(defalias 'yes-or-no-p 'y-or-n-p)

;; ssss---------------------------------------------------
(progn
  ;; org-mode
  ;; make “org-mode” syntax color code sections
  (setq org-src-fontify-natively t)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil))

;; ssss---------------------------------------------------

(when (fboundp 'eww)
  (defun xah-rename-eww-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer (concat "eww " $title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'xah-rename-eww-buffer))

;; ssss---------------------------------------------------
;; Load libraries and configure packages
(require 'ts-js-config)
(require 'magit-config)
(require 'perspectives)
(require 'dired-config)
(require 'isearch-config)
(require 'ibuffer-config)
(require 'company-config)
;; (require 'eglot-config)

;; ssss---------------------------------------------------
;; configure file templates for newly created files
(setq auto-insert-directory "~/.emacs.d/templates/")
(add-hook 'find-file-hook 'auto-insert)
(define-auto-insert "\.el" "emacs-lisp-lib.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahk-syntax-directory "c:/Users/cherr/Desktop/scripts/user.ahk")
 '(package-selected-packages
   '(company-tabnine company eglot corfu ag magit typescript-mode web-mode xah-fly-keys)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
