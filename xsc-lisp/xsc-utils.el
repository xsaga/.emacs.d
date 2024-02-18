;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-utils.el --- Utils
;;; Commentary:
;;; Code:

(defun xsc-diff-buffer-with-saved-file ()
  "Same as 'diff-buffer-with-file', but automatically choosing the current buffer."
  (interactive)
  (diff-buffer-with-file (current-buffer))
  (other-window 1))

(defun xsc-timestamp-dmyz ()
  "Date timestamp."
  (interactive)
       (insert (format-time-string "%d-%m-%Y %Z")))

(defun xsc-timestamp-hms ()
  "Hour timestamp."
  (interactive)
       (insert (format-time-string "%H:%M:%S")))

(defun xsc-timestamp-ymd ()
  "Date timestamp ISO 8601"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun xsc-unix-timestamp-region-to-date (start end)
  "Select a region containing a unix timestamp and convert it to a human readable date."
  (interactive "r")
  (if (use-region-p)
      (message (format-time-string "%F %T %Z"
				   (seconds-to-time (string-to-number (buffer-substring start end)))
				   ))))

(defun xsc-sort-comma-separated-words (start end)
  "sort comma separated words. For python imports for example."
  (interactive "r")
  (if (use-region-p)
      (let ( (contents (buffer-substring start end)) )
	(progn
	  (kill-region (region-beginning) (region-end))
	  (insert (string-join (sort (mapcar 'string-trim (split-string contents ",")) 'string-lessp) ", "))
	  ))
    ))

(defun xsc-delete-blank-lines-in-region (start end)
  "Delete empty lines in region.
https://www.masteringemacs.org/article/removing-blank-lines-buffer
13-01-2023"
  (interactive "r")
  (flush-lines "^[[:blank:]]*$" start end))

(defun xsc-get-directory-desperately ()
  "If thing at point is a file, get the associated directory. If the first element of kill-ring is a directory, return it. If it is a file, return the parent directory. If kill-ring does not have a file or directory, get the directory associated with the current buffer. If current buffer has no associated file but it is in dired mode, return directory name. If the current buffer has no associated file, ask the user for a directory starting from home directory.
Version: 22-12-2022"
  (let ((clipboard (if (length> kill-ring 0)
		       (substring-no-properties (current-kill 0 t))
		     nil)))
    (cond
     ;; thing at point
     ((ffap-guess-file-name-at-point)
      (message "Directory inferred from: filename at point")
      (file-name-directory (expand-file-name (ffap-guess-file-name-at-point))))
     ;; 'ffap-guess-file-name-at-point' works OK when point is on strings representing
     ;; paths with forwardshlashes and single backslashes, but it returns nil when point
     ;; is on a string with double backslashes (escaping the backslash in a string).
     ;; Is this a bug? or I am using it wrong? But it works if you extract the string and
     ;; check if it is a file: (emacs considers empty string as home, avoid that)
     ((and (not (string= "" (ffap-string-at-point)))
	   (file-exists-p (expand-file-name (ffap-string-at-point))))
      (message "Directory inferred from: string at point")
      (file-name-directory (expand-file-name (ffap-string-at-point))))
     ;; first element of kill ring is a directory or file
     ((and clipboard (file-exists-p clipboard))
      (message "Directory inferred from: kill ring")
      (file-name-directory clipboard))
     ;; the buffer has an associated file
     ((buffer-file-name (current-buffer))
      (message "Directory inferred from: buffer file name")
      (file-name-directory (buffer-file-name (current-buffer))))
     ;; in dired
     ((string= major-mode "dired-mode")
      (message "Directory inferred from: dired")
      (expand-file-name (if (listp dired-directory)
			    (car dired-directory)
			  dired-directory)))
     ;; default
     (t
      (message "Directory inferred from: ask user")
      (expand-file-name
       (read-directory-name "Select directory: " (getenv "HOME")))
      ))
    ))

(defun xsc-get-filename-desperately ()
  "If thing at point is a file, return it. If the first element of kill-ring is a file, return it. If it is a directory, ask the user starting from that directory. If kill-ring does not have a file or directory, get the file associated with the current buffer. If the current buffer has no associated file but it is in dired mode, ask the user starting from that directory.  If the current buffer has no associated file, ask the user for a file starting from home directory.
Version: 23-12-2022"
  (let ((clipboard (if (length> kill-ring 0)
		       (substring-no-properties (current-kill 0 t))
		     nil)))
    (cond
     ;; thing at point
     ((ffap-guess-file-name-at-point)
      (message "File inferred from: filename at point")
      (expand-file-name (ffap-guess-file-name-at-point)))
     ;; 'ffap-guess-file-name-at-point' works OK when point is on strings representing
     ;; paths with forwardshlashes and single backslashes, but it returns nil when point
     ;; is on a string with double backslashes (escaping the backslash in a string).
     ;; Is this a bug? or I am using it wrong? But it works if you extract the string and
     ;; check if it is a file: (emacs considers empty string as home, avoid that)
     ((and (not (string= "" (ffap-string-at-point)))
	   (file-exists-p (expand-file-name (ffap-string-at-point))))
      (message "File inferred from: string at point")
      (expand-file-name (ffap-string-at-point)))
     ;; first element of kill ring is a directory
     ((and clipboard (file-directory-p clipboard))
      (message "File inferred from: kill ring directory, ask user")
      (expand-file-name
       (read-file-name "Select file: " clipboard)))
     ;; first element of kill ring is a file
     ((and clipboard (file-exists-p clipboard))
      (message "File inferred from: kill ring")
      clipboard)
     ;; the buffer has an associated file
     ((buffer-file-name (current-buffer))
      (message "File inferred from: buffer file name")
      (buffer-file-name (current-buffer)))
     ;; in dired
     ((string= major-mode "dired-mode")
      (message "File inferred from: dired, ask user")
      (expand-file-name (read-file-name "Select file: " (if (listp dired-directory)
							    (car dired-directory)
							  dired-directory))))
     ;; default
     (t
      (message "File inferred from: ask user")
      (expand-file-name
       (read-file-name "Select file: " (getenv "HOME")))
      ))
    ))

(defun xsc-infer-file-open-in-external-app ()
  "Infer filename and open in external app.
Perform the default action of the OS on a filename. The filename is inferred using 'xsc-get-filename-desperately'.
Based on Xah Lee's xah-open-in-external-app http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html
Version 23-12-2022"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (let ((command-string (format "powershell -Command \"Invoke-Item -LiteralPath\" '%s'" (shell-quote-argument (xsc-get-filename-desperately)))))
      (message "Calling file open in external: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))

(defun xsc-infer-file-find-file ()
  "Infer filename and call find-file."
  (interactive)
  (find-file (xsc-get-filename-desperately)))

(defun xsc-infer-directory-open-in-external-app ()
  "Infer directory and open in external app.
Perform the default action of the OS on a directory. The directory is inferred using 'xsc-get-directory-desperately'.
Based on Xah Lee's xah-open-in-external-app http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html
Version 23-12-2022"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (let ((command-string (format "powershell -Command \"Invoke-Item -LiteralPath\" '%s'" (shell-quote-argument (xsc-get-directory-desperately)))))
      (message "Calling directory open in external: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))

(defun xsc-infer-directory-dired ()
  "Infer directory and call dired."
  (interactive)
  (dired (xsc-get-directory-desperately)))

(defun xsc-infer-file-open-in-firefox ()
  "Infer filename and open in Firefox.
Version 24-12-2022
Updated 06-01-2023"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    ;; previous version: (command-string (format "powershell -Command \"Start-Process firefox '-private-window file://%s'\"" (xsc-get-filename-desperately)))
    ;; Bug when filename contains space characters. Firefox will only consider the string up to the first space.
    (let ((command-string (format "powershell -Command \"Start-Process firefox '-private-window %s'\"" (concat "file://" (url-encode-url (xsc-get-filename-desperately))))))
      (message "Open file in Firefox: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))

(defun xsc-infer-directory-open-in-firefox ()
  "Infer directory and open in Firefox.
Version 24-12-2022
Updated 06-01-2023"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    ;; previous version: (command-string (format "powershell -Command \"Start-Process firefox '-private-window file://%s'\"" (xsc-get-directory-desperately)))
    ;; Bug when filename contains space characters. Firefox will only consider the string up to the first space.
    (let ((command-string (format "powershell -Command \"Start-Process firefox '-private-window %s'\"" (concat "file://" (url-encode-url (xsc-get-directory-desperately))))))
      (message "Open directory in Firefox: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))

(defun xsc-infer-file-open-in-edge ()
  "Infer filename and open in Microsoft Edge.
For flags, see: https://textslashplain.com/2022/01/05/edge-command-line-arguments/
Check flags in edge with edge://version
Version 24-12-2022
Updated 06-01-2023"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    ;; previous version: (command-string (format "powershell -Command \"Start-Process msedge '--inprivate file://%s'\"" (xsc-get-filename-desperately)))
    ;; Bug when filename contains space characters. Edge splits the url by space and opens a tab for each substring.
    ;; If original url is file://c:/Users/someone/Desktop/docu - google.es/
    ;; first tab: file://c:/Users/someone/Desktop/docu
    ;; second tab: -
    ;; third tab will open google page. If the string were SOMETHING.OTHER.TLD/dir/subdir/file, it would contact that page...
    (let ((command-string (format "powershell -Command \"Start-Process msedge '--inprivate %s'\"" (concat "file://" (url-encode-url (xsc-get-filename-desperately))))))
      (message "Open file in Microsoft Edge: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))

(defun xsc-infer-directory-open-in-edge ()
  "Infer directory and open in Microsoft Edge.
For flags, see: https://textslashplain.com/2022/01/05/edge-command-line-arguments/
Check flags in edge with edge://version
Version 24-12-2022
Updated 06-01-2023"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    ;; previous version: (command-string (format "powershell -Command \"Start-Process msedge '--inprivate file://%s'\"" (xsc-get-directory-desperately)))
    ;; Bug when filename contains space characters. Edge splits the url by space and opens a tab for each substring.
    ;; If original url is file://c:/Users/someone/Desktop/docu - google.es/
    ;; first tab: file://c:/Users/someone/Desktop/docu
    ;; second tab: -
    ;; third tab will open google page. If the string were SOMETHING.OTHER.TLD/dir/subdir/file, it would contact that page...
    (let ((command-string (format "powershell -Command \"Start-Process msedge '--inprivate %s'\"" (concat "file://" (url-encode-url (xsc-get-directory-desperately))))))
      (message "Open directory in Microsoft Edge: %s" command-string)
      (shell-command command-string)))
   (t
    (message "%s %s" system-type "Not supported"))))


(provide 'xsc-utils)
;;; xsc-utils.el ends here
