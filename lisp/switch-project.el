;; -*- coding: utf-8; lexical-binding: t; -*-
;; switch-project.el --- Function to switch between projects.

;;; Commentary:

;;; Code:
(setq projects (delete ".." (delete "." (directory-files "~/git"))))

(defun my/switch-project ()
  (interactive)
  (let ((selectedProject (completing-read "Select project: " projects)))
    (progn
      (persp-switch selectedProject)
      (project-switch-project (expand-file-name (format "~/git/%s" selectedProject)))
      )))

(define-key my-leader (kbd "pp") 'my/switch-project)

(provide 'switch-project)
;;; switch-project.el ends here
