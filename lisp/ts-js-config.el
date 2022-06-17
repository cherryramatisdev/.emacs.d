;; -*- coding: utf-8; lexical-binding: t; -*-
;; ts-js-config.el --- Configuration for ts/js files

;;; Commentary:
;; This configuration is prepared to work with react.js files/projects

;;; Code:
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

(provide 'ts-js-config)
;;; ts-js-config.el ends here
