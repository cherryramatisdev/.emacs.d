;; -*- coding: utf-8; lexical-binding: t; -*-
;; ts-js-config.el --- Configuration for ts/js files

;;; Commentary:
;; This configuration is prepared to work with react.js files/projects

;;; Code:
(if (not (package-installed-p 'web-mode))
    (error "Please install web-mode"))

(if (not (package-installed-p 'typescript-mode))
    (error "Please install typescript-mode"))

(require 'web-mode)
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
        web-mode-enable-current-element-highlight nil)

(require 'typescript-mode)
(setq typescript-indent-level 2)
(add-hook 'typescript-mode #'subword-mode)

(defun prettier-format ()
  (interactive)
  (call-process "yarn" nil "*prettier-output*" nil "prettier" "--write" (buffer-file-name))
  (revert-buffer nil 'no-confirm t))

(defun run-prettier-on-save ()
  "Format current file if it's running web mode"
  (interactive)
  (when (string-equal major-mode "web-mode")
    (prettier-format)))

(add-hook 'after-save-hook 'run-prettier-on-save)

(provide 'ts-js-config)
;;; ts-js-config.el ends here
