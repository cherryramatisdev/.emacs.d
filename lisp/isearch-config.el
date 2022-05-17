;; -*- coding: utf-8; lexical-binding: t; -*-
;; isearch-config.el --- $description

;;; Commentary:

;;; Code:

(define-key isearch-mode-map (kbd "TAB") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<backtab>") 'isearch-repeat-backward)

(provide 'isearch-config)
;;; isearch-config.el ends here
