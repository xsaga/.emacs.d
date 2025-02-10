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
(global-set-key (kbd "M-u") 'xsc-toggle-capitalize-upcase-or-downcase-previous-word)

;; https://karthinks.com/software/emacs-window-management-almanac/
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(windmove-default-keybindings) ;; Shift + flecha (right, up, left, down)
(windmove-swap-states-default-keybindings '(control shift)) ;; Control + Shift + flecha (right, up, left, down)
(setq windmove-create-window t)

(provide 'xsc-keybindings)
;;; xsc-keybindings.el ends here
