;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-gallery --- Simple image gallery
;;; Comments:
;; Create a temporary HTML file containing links to various image
;; files and PDFs within a directory. Renders the file using eww.
;;; Code:

(defun xsc-directory-image-gallery (basedir)
  "Visualize all images in BASEDIR.
Make recursive?
Version: 15-10-2022."
  (interactive "D")
  (let ((file-regexp "\\.\\(svg\\|pdf\\|png\\|jpg\\|jpeg\\|gif\\|bmp\\)$")
	(viz-contents "<!DOCTYPE html>
<html>
  <head>
    <!-- <meta http-equiv=\"refresh\" content=\"1\" /> -->
  </head>
<body>\n")
	(html-end "</body>
</html>\n"))
    (dolist (file-path (directory-files basedir t file-regexp))
      (let ((image-tag (if (string-match "\\.pdf$" file-path)
			   (concat "<embed src=\"" "file:///" file-path "\" type=\"application/pdf\" frameBorder=\"0\"" "></embed>\n")
			 (concat "<img src=\"" "file:///" file-path "\"></img>\n")))
	    (file-attr (file-attributes file-path)))
	(setq viz-contents (concat viz-contents "<h1>" file-path "</h1>\n"))
	(setq viz-contents (concat viz-contents "<p>Modification time: " (format-time-string "%F %T %z" (time-convert (file-attribute-modification-time file-attr) 'integer)) "</p>\n"))
	(setq viz-contents (concat viz-contents "<p>Filesize (bytes): " (int-to-string (file-attribute-size file-attr)) "</p>\n"))
	(setq viz-contents (concat viz-contents image-tag "\n"))))
    (setq viz-contents (concat viz-contents html-end))
    (eww-open-file (make-temp-file "xsc-directory-image-gallery" nil ".html" viz-contents))
    (message "You can use 'eww-browse-with-external-browser' function to show the page in an external browser.")))

(provide 'xsc-gallery)
;;; xsc-gallery.el ends here
