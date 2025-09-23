;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-completion.el --- Completion and selection
;;; Commentary:
;;; Code:

(xsc-install-packages '(orderless))

(require 'orderless)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles partial-completion)))
      completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completions-max-height 20
      completions-sort (if (version< "30.1" emacs-version) 'historical 'alphabetical)
      completion-show-help nil
      completion-auto-select 'second-tab
      completion-auto-help 'visible
      completions-format 'horizontal)

(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(when (version< "30.1" emacs-version)
  (global-completion-preview-mode)
  (define-key completion-preview-active-mode-map (kbd "C-n") 'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "C-p") 'completion-preview-prev-candidate))

(provide 'xsc-completion)
;;; xsc-completion.el ends here
