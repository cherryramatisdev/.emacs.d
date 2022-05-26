;; -*- coding: utf-8; lexical-binding: t; -*-
;; switch-project.el --- Function to switch between projects.

;;; Commentary:

;;; Code:
(defun magit-status-fullscreen ()
  (interactive)
  (magit-status)
  (delete-other-windows))

(defun my/switch-project ()
  (interactive)
  (let ((selectedProject (completing-read "Select project: " projects)))
    (progn
      (persp-switch selectedProject)
      (cd (expand-file-name (format "%s/%s" projectPrefix selectedProject)))
      (magit-status-fullscreen)
      )))

(define-key my-leader (kbd "pp") 'my/switch-project)

(provide 'switch-project)
;;; switch-project.el ends here
