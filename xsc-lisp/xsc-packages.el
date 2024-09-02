;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-packages.el --- Packages
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 27)
  (package-initialize))

(defvar xsc-basic-packages
  '(
    highlight-thing ;; https://github.com/fgeller/highlight-thing.el
    magit
    paredit ;; pseudo-structurally editing Lisp code
    rainbow-delimiters ;; highlight parenthesis
    rainbow-mode ;; colorize color names in buffers
    swiper ;; https://github.com/abo-abo/swiper
    unfill ;; Do the opposite of fill-paragraph or fill-region
    which-key ;; display the key bindings following a command
    )
  "Lista de paquetes basicos que hay que mantener instalados.")

(defun xsc-install-package (pkg)
  "Install PKG if not already installed."
  (unless (package-installed-p pkg)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install pkg)))

(defun xsc-install-packages (pkg-list)
  "Install all packages from the PKG-LIST list."
  (dolist (pkg pkg-list)
    (if (xsc-install-package pkg)
	(message "<xsc> Installed %s" pkg)
      (message "<xsc> %s already installed" pkg))))

;; instalar los paquetes que no esten instalados
(xsc-install-packages xsc-basic-packages)

(setq which-key-idle-delay 2.0
      which-key-max-description-length 35
      which-key-show-docstrings nil)

(which-key-mode)

(add-hook 'prog-mode-hook #'rainbow-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)


(require 'highlight-thing)
(setq highlight-thing-what-thing 'symbol
      highlight-thing-prefer-active-region t
      highlight-thing-limit-to-region-in-large-buffers-p t
      highlight-thing-narrow-region-lines 20
      highlight-thing-large-buffer-limit 5000
      highlight-thing-delay-seconds 0.2
      highlight-thing-all-visible-buffers-p nil)

(global-highlight-thing-mode)

(provide 'xsc-packages)
;;; xsc-packages.el ends here
