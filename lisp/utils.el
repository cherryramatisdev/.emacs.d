;; -*- coding: utf-8; lexical-binding: t; -*-
;; utils.el --- Util functions to not lose work.

;;; Commentary:

;;; Code:
(require 's)

(defun generate-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://xahlee.info/emacs/emacs/elisp_generate_uuid.html'
Version 2020-06-04"
  (cond
   ((string-equal system-type "windows-nt")(s-trim (shell-command-to-string "pwsh.exe -Command [guid]::NewGuid().toString()")))
   ((string-equal system-type "darwin") (s-trim (shell-command-to-string "uuidgen")))
   ((string-equal system-type "gnu/linux") (s-trim (shell-command-to-string "uuidgen")))))

(provide 'utils)
;;; utils.el ends here
