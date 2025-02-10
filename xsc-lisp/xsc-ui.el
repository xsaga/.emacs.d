;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-ui.el --- UI
;;; Commentary:
;;; Code:

;; (info "(Emacs) Frames")
;; F10 to access menu in text terminals.
;; TODO disable menu in text terminals.
(menu-bar-mode 1)
(tool-bar-mode -1)
(tab-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; if display-time-format is nil, the defaults
;; depend on display-time-24hr-format, display-time-day-and-date
;; C-h f format-time-string
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-default-load-average 0)
(setq display-time-format "%A %H:%M")

(setq inhibit-splash-screen t)

(display-time-mode 1)

(global-hl-line-mode)

;; esto funciona?
(require 'battery)
(unless (null battery-status-function)
  (unless (string= "N/A" (cdadr (funcall battery-status-function)))
    (display-battery-mode 1)))

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode -1))

(cond
 ((member "Inconsolata" (font-family-list)) (set-frame-font "Inconsolata 12" nil t) (message "<xsc> Font set to Inconsolata"))
 ((member "Cascadia Code" (font-family-list)) (set-frame-font "Cascadia Code 12" nil t) (message "<xsc> Font set to Cascadia Code"))
 ((member "Consolas" (font-family-list)) (set-frame-font "Consolas 12" nil t) (message "<xsc> Font set to Consolas")))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Position-Parameters.html
(add-to-list 'default-frame-alist '(left . (+ 0)))
(add-to-list 'default-frame-alist '(top . (+ 0)))
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Size-Parameters.html
(add-to-list 'default-frame-alist '(width  . 0.5))
(add-to-list 'default-frame-alist '(height . 1.0))

;; --------
;; http://xahlee.info/emacs/emacs/emacs_encoding_decoding_faq.html

;; (info "(Emacs) Language Environments")
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Language-Environments.html
;; M-x describe-language-environment
(set-language-environment "Spanish")
;; (info "(Emacs) Coding Systems")
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Coding-Systems.html
;; M-x describe-coding-system; M-x list-coding-system
;; (info "(Emacs) Recognizing Coding")
;; The priority list of coding systems (M-x list-coding-system)
;; depends on the selected language environment. For "Spanish"
;; the first one is iso-latin-1. Modify the priority list to
;; prefer utf-8 with unix line endings.
(prefer-coding-system 'utf-8-unix)
;; --------

;; scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 3)

;; whitespace
;; setq-default is for all buffers
(setq-default show-trailing-whitespace t)
;; disable for specific modes
(add-hook 'calendar-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'diff-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(setq-default indicate-buffer-boundaries t)

;; bell
;; (info "(elisp) Beeping")
(setq ring-bell-function 'ignore)

;; Calendar
;; https://www.emacswiki.org/emacs/CalendarLocalization
;; (info "(Emacs) Calendar Customizing")
(setq calendar-week-start-day 1
      calendar-day-name-array ["domingo" "lunes" "martes" "miércoles"
                               "jueves" "viernes" "sábado"]
      calendar-day-abbrev-array ["dom" "lun" "mar" "mié" "jue" "vie" "sáb"]
      calendar-day-header-array [" D" " L" " M" " X" " J" " V" " S"]
      calendar-month-name-array ["enero" "febrero" "marzo" "abril"
				 "mayo" "junio" "julio" "agosto"
				 "septiembre" "octubre" "noviembre"
				 "diciembre"])

;; all holidays known to emacs are in 'calendar-holidays' variable
;; The general holidays 'holiday-general-holidays' are, by default, holidays common throughout the United States.
;; ;; (require 'holidays)
(setq holiday-general-holidays nil
      calendar-christian-all-holidays-flag nil
      holiday-local-holidays '((holiday-fixed 1 1 "Año Nuevo")
			       (holiday-fixed 1 6 "Día de Reyes (Epifanía del Señor)")
			       (holiday-fixed 3 19 "Día del Padre (San José)")
			       (holiday-fixed 5 1 "Día del trabajador")
			       (holiday-float 5 0 1 "Día de la Madre")
			       (holiday-fixed 6 24 "San Juan")
			       (holiday-fixed 7 25 "Día de Santiago Apóstol")
			       (holiday-fixed 8 15 "Día de la Asunción de María")
			       (holiday-fixed 9 6 "Día de Elcano")
			       (holiday-fixed 10 12 "Fiesta Nacional o Día de la Hispanidad")
			       (holiday-fixed 11 1 "Día de Todos los Santos")
			       (holiday-fixed 12 6 "Día de la Constitución")
			       (holiday-fixed 12 8 "Día de la Inmaculada Concepción")
			       (holiday-fixed 12 25 "Día de Navidad")
			       (holiday-easter-etc -2 "Viernes Santo")
			       (holiday-easter-etc -46 "Miércoles de Ceniza (Carnaval)"))
      holiday-other-holidays nil)

;; ==================================================================
;; (load-theme 'solarized-light t)
;; (load-theme 'birds-of-paradise-plus t)
;; ver creamsody-theme
;; (load-theme 'modus-operandi t)
(load-theme 'leuven t)

;; (set-face-attribute 'font-lock-comment-face nil :foreground "DarkGoldenrod4" :weight 'heavy)
;; (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "DarkGoldenrod4")

;; (set-background-color "linen")

(show-paren-mode 1)
(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t)

(provide 'xsc-ui)
;;; xsc-ui.el ends here
