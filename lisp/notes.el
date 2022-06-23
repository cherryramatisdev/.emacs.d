;; -*- coding: utf-8; lexical-binding: t; -*-
;; notes.el --- Function to switch between projects.

;;; Commentary:

;;; Code:
(require 's)

(setq notes-dir "d:\\notes")

(defun ask-title-when-not-defined (&optional title)
  (if title
      (s-replace " " "-" title)
    (s-replace " " "-" (read-string "Note title: "))))

(defun notes/create-new (&optional note-title)
  "`notes/create-new' is responsible for creating a new annotation on the `notes-dir' directory."
  (interactive)
  (let ((buffer-name (format "%s__%s" (format-time-string "%Y%m%d%H%M%S") (ask-title-when-not-defined note-title))))
    (find-file (expand-file-name (format "%s/%s" notes-dir buffer-name)))))

(provide 'notes)
;;; notes.el ends here
