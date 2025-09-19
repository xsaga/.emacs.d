;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-highlight-thing.el --- highlight-thing mode configuration https://github.com/fgeller/highlight-thing.el
;;; Comments:
;;; Code:

(xsc-install-packages '(highlight-thing))

(require 'highlight-thing)

(setq highlight-thing-what-thing 'symbol
      highlight-thing-prefer-active-region t
      highlight-thing-limit-to-region-in-large-buffers-p t
      highlight-thing-narrow-region-lines 20
      highlight-thing-large-buffer-limit 5000
      highlight-thing-delay-seconds 0.2
      highlight-thing-all-visible-buffers-p nil)

(global-highlight-thing-mode)

(provide 'xsc-highlight-thing)
;;; xsc-highlight-thing.el ends here
