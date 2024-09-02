;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-go.el --- Go
;;; Commentary:

;;; Code:

(xsc-install-packages '(go-mode
			go-dlv))

(add-hook 'go-mode-hook
	  (lambda () (setq tab-width 4)))

(add-hook 'go-ts-mode-hook
	  (lambda () (setq tab-width 4)))

(setq go-ts-mode-indent-offset 4)

;; En windows "dlv debug" desde emacs da un error de que stdin is not a terminal
(when (eq system-type 'windows-nt)
  (defadvice dlv (before dlv-set-noninteractive-mswindows activate compile)
    "Incluir por defecto el flag --allow-non-terminal-interactive=true"
    (interactive
     (list (read-from-minibuffer "Run dlv: " "dlv debug --allow-non-terminal-interactive=true")))))

(provide 'xsc-go)
;;; xsc-go.el ends here
