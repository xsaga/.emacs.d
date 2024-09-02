;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-racket.el --- Racket lang
;;; Commentary:
;;; Code:

(xsc-install-packages '(racket-mode))

(add-hook 'racket-mode-hook 'enable-paredit-mode)

(provide 'xsc-racket)
;;; xsc-racket.el ends here
