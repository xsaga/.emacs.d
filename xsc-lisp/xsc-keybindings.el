;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-keybindings.el --- Keybindings
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-<tab>") 'hippie-expand)


(global-set-key (kbd "C-c s e") 'xsc-everything-search)
(global-set-key (kbd "C-c o t") 'xsc-open-windows-terminal)


;; usando emacs desde la terminal 'emacs -nw'
;; no reconoce Ctr-Shift-*, lo interpreta como C-*.
;; https://emacs.stackexchange.com/questions/32294/how-to-make-emacs-recognise-c-shift-combinations-in-terminal-mode
(if (display-graphic-p)
    (global-set-key (kbd "C-S-s") 'swiper)
  (global-set-key (kbd "C-s") 'swiper))

(global-set-key (kbd "C-x g") 'magit-status)


;; by default 'C-h a' calls command-apropos
;; which only searches interactive function
(global-set-key (kbd "C-h a") 'apropos)


(global-set-key (kbd "C-c d") 'xsc-diff-buffer-with-saved-file)
(global-set-key (kbd "C-c o f") 'xsc-infer-file-open-in-external-app)
(global-set-key (kbd "C-c f f") 'xsc-infer-file-find-file)
(global-set-key (kbd "C-c o d") 'xsc-infer-directory-open-in-external-app)
(global-set-key (kbd "C-c f d") 'xsc-infer-directory-dired)


(global-set-key (kbd "C-c k a") 'xsc-kill-around-balanced-delimiters)
(global-set-key (kbd "C-c k w") 'xsc-kill-around-word)

(provide 'xsc-keybindings)
;;; xsc-keybindings.el ends here
