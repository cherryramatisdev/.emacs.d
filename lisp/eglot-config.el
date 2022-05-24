;; -*- coding: utf-8; lexical-binding: t; -*-
;; eglot-config.el --- Configure LSP client

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'eglot))
    (error "Install `eglot' package"))

(require 'eglot)

(add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))

(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

(provide 'eglot-config)
;;; eglot-config.el ends here
