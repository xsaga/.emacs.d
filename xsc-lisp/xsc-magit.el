;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-magit.el --- Git interface
;;; Comments:
;;; Code:

(xsc-install-packages '(magit))

(setq magit-diff-refine-hunk t)
(setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))

(provide 'xsc-magit)
;;; xsc-magit.el ends here
