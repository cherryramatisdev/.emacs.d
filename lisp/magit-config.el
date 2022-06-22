;; -*- coding: utf-8; lexical-binding: t; -*-
;; magit-config.el --- Basic config for magit

;;; Commentary:

;;; Code:

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

(provide 'magit-config)
;;; magit-config.el ends here
