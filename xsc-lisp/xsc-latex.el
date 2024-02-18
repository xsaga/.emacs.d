;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-latex.el --- LaTeX
;;; Commentary:
;;; Code:

(setq lua-filter-grammar-path (expand-file-name "~/text4grammar.lua"))

(xsc-install-packages '(auctex))


;; https://tex.stackexchange.com/questions/295304/how-to-cite-using-emacsauctexreftex
;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; https://ondahostil.wordpress.com/2019/02/07/lo-que-he-aprendido-auctex/
(setq TeX-parse-self t
      TeX-auto-save t)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)

(defun xsc-latex-region-to-plain-text (start end)
  "Convert latex in region to a plain text version with minimal markup to use for spell checking in other programs (Grammarly etc.). Calls Pandoc.
https://github.com/xsaga/pandoc-filter-text4grammar
First 10-02-2023
Updated 15-02-2023"
  (interactive "r")
  (let ((results-buffer (generate-new-buffer "LaTeX to plain")))
    (call-process-region (when (use-region-p) start) ;; if START in call-process-region is nil, use whole buffer
			 end
			 "pandoc" nil results-buffer t "--from=latex" "--to=plain" "--wrap=none" "--lua-filter" lua-filter-grammar-path)

    (with-current-buffer results-buffer
      (goto-char (point-min))
      (while (re-search-forward "\\\\[a-zA-Z]+{\\([^}]+\\)}" nil t) (replace-match "\\1"))

      (goto-char (point-min))
      (while (re-search-forward "[_^]?{\\([^}]+\\)}" nil t) (replace-match "\\1"))

      (goto-char (point-min))
      (while (search-forward "\\prime" nil t) (replace-match "'" nil t))

      (goto-char (point-min))
      (while (search-forward "\\times" nil t) (replace-match "x" nil t))

      (goto-char (point-min))
      (while (search-forward "\\" nil t) (replace-match "" nil t))

      (goto-char (point-min))
      (while (search-forward " " nil t) (replace-match " " nil t))

      (goto-char (point-min)))

    (display-buffer results-buffer)))

(defun xsc-convert-cite-to-citeauthor ()
  (interactive)
  (let* ((citekey (thing-at-point 'word 'no-properties))
	 (result (format "\\citeauthor*{%s}" citekey)))
    (kill-new result)
    (message "Copied: `%s`" (current-kill 0 t))
    ))

(defun xsc-bib-copy-entry-doi ()
  (interactive)
  (let ((entry (buffer-substring-no-properties
		(save-excursion (search-backward "{") (1+ (point)))
		(save-excursion (search-forward "}") (1- (point))))))
    (kill-new (concat "doi.org/" entry))
    (message "Copied: `%s`" (current-kill 0 t))
    ))

;; (global-set-key (kbd "C-c a") 'xsc-convert-cite-to-citeauthor)
;; (global-set-key (kbd "C-c c") 'xsc-bib-copy-entry-doi)

(provide 'xsc-latex)
;;; xsc-latex.el ends here
