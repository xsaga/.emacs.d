;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-racket.el --- Racket lang
;;; Commentary:
;;; Code:

(xsc-install-packages '(racket-mode))

(add-hook 'racket-mode-hook 'enable-paredit-mode)

(when (eq system-type 'windows-nt)
  (let ((racket-path "C:/Program Files/Racket"))
    (if (file-readable-p (concat (file-name-as-directory racket-path) "Racket.exe"))
	(add-to-list 'exec-path racket-path)
      (display-warning 'xsc "Racket.exe not found" :warning))))

(provide 'xsc-racket)
;;; xsc-racket.el ends here
