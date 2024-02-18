;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-completion.el --- Completion and selection
;;; Commentary:
;;; Code:

(xsc-install-packages '(vertico orderless corfu))

;;                      '(basic substring partial-completion orderless flex) ;; original
(setq completion-styles '(orderless basic)
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq orderless-matching-styles '(orderless-literal orderless-regexp))

(vertico-mode)
(vertico-mouse-mode)
(vertico-grid-mode)
(vertico-multiform-mode)
(setq vertico-count 5
      vertico-cycle t
      vertico-grid-min-columns 1
      vertico-grid-max-columns 16)

;; (setq vertico-multiform-commands '((xsc-everything-search buffer)))
;; (setq vertico-multiform-commands '((xsc-everything-search (vertico-count . 20))))
;; (setq vertico-multiform-commands '((xsc-everything-search (vertico-count . 20)
;; 							  (vertico-grid-max-columns . 1))))
(setq vertico-multiform-commands '((xsc-everything-search (vertico-grid-max-columns . 1)
							  (vertico-count . 25))))

;; Configure vertico with Ido-like directory navigation commands
(define-key vertico-map "\r" #'vertico-directory-enter)
(define-key vertico-map "\d" #'vertico-directory-delete-char)
(define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; Using vertico for the completion "breaks" the 'rgrep' command
;; because when it asks you for a file pattern, after pressing enter,
;; vertico returns the closest match of the pattern from the candidate
;; list instead of returning the pattern itself. This is the default
;; behavior of the 'vertico-exit' function. However, if you call
;; 'vertico-exit' with a prefix arg (or call the 'vertico-exit-input'
;; function) it returns the input itself, which is the behavior I want
;; when using 'rgrep'. Use M-RET instead of RET. TODO, automate.
;; setting '((rgrep ....)) in vertico multiform does not work... why?


(setq corfu-auto t
      corfu-cycle t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(global-corfu-mode)

(provide 'xsc-completion)
;;; xsc-completion.el ends here
