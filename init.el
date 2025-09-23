;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; init.el --- Emacs configuration file
;;; Commentary:
;; Requires min Emacs 28.1
;; uses the following functions
;; 'length>', 'with-environment-variables', 'modus' themes.
;;; Code:

(add-to-list 'load-path (locate-user-emacs-file "xsc-lisp"))

;; mover lisp autogenerado por M-x customize a otro archivo
;; (describe-variable 'custom-file)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(require 'xsc-packages)
(require 'xsc-completion)
(require 'xsc-magit)

;; (require 'xsc-highlight-thing)

;; (require 'xsc-go)
;; (require 'xsc-latex)
;; (require 'xsc-python)
;; (require 'xsc-racket)

(require 'xsc-ui)
(require 'xsc-utils)
(require 'xsc-gallery)
(require 'meneame)

(when (eq system-type 'windows-nt)
  (require 'xsc-mswindows))

(when (and (eq system-type 'gnu/linux)
           (file-directory-p "/mnt/c/Windows"))
  (require 'xsc-wsl))

(require 'xsc-keybindings)

;; Spelling
(setq ispell-dictionary "american")
(add-hook 'text-mode-hook 'flyspell-mode)

;; enable abbrev mode
(setq-default abbrev-mode t)

;;; init.el ends here
