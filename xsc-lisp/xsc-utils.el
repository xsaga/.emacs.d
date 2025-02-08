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


(defun xsc-kill-around-balanced-delimiters (n)
  "Kill text around balanced delimiters.
By default, delimiters are not included.
If called with universal argument, include delimiters.
Version 18-08-2024"
  (interactive "p")
  (let ((offset (if (= n 1)
		    1
		  0)))
    (kill-region (save-excursion
		   (backward-up-list 1 t t)
		   (+ (point) offset))
		 (save-excursion
		   (backward-up-list -1 t t)
		   (- (point) offset)))))

(defun xsc-kill-around-word ()
  "Kill any contiguous sequence of word and symbol characters.
Version 18-08-2024
Updated 25-08-2024"
  (interactive)
  (kill-region (save-excursion
		 ;;(skip-syntax-backward "^ ")
		 (skip-syntax-backward "w_")
		 (point))
	       (save-excursion
		 ;;(skip-syntax-forward "^ ")
		 (skip-syntax-forward "w_")
		 (point))))

;; diff de dos directorios
;; Idea inicial 2024-08-23
;; ver tambien: paquete emacs "ztree", programa de gnome "meld"

;; Para hashear un buffer emacs tiene dos funciones:
;; buffer-hash y secure-hash (ver secure-hash-algorithms)
;; buffer-hash es mas rapido pero no adecuado para criptografia
;; y su valor puede cambiar en distintas versiones o instancias de emacs
;; pero para esta funcion supongo que no importa.

;; diferencia en el tiempo de ejecucion:
;; (let ((starttime (current-time)))
;;   (dotimes (x 1000)
;;     (with-temp-buffer
;;       (set-buffer-multibyte nil)
;;       (insert-file-contents-literally "c:/Users/xabie/Desktop/borrar/emacs/Makefile.in")
;;       (secure-hash 'md5 (current-buffer))))
;;   (message "took: %f seconds" (float-time (time-subtract (current-time) starttime))))  ; approx 0.98s

;; (let ((starttime (current-time)))
;;   (dotimes (x 1000)
;;     (with-temp-buffer
;;       (set-buffer-multibyte nil)
;;       (insert-file-contents-literally "c:/Users/xabie/Desktop/borrar/emacs/Makefile.in")
;;       (buffer-hash)))
;;   (message "took: %f seconds" (float-time (time-subtract (current-time) starttime))))  ; approx 0.18s

;; usar find-file-literally es mucho mas lento que insert-file-contents-literally
;; approx x10 veces mas lento.
(require 'diff)
(require 'cl-lib)

(defun xsc-diff-directories--get-file-content-checksum (filename)
  (with-temp-buffer
    ;; (find-file-literally filename) ;; mucho mas lento
    (set-buffer-multibyte nil) ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-09/msg01037.html
    (insert-file-contents-literally filename)
    ;;(secure-hash 'md5 (current-buffer))
    (buffer-hash)))

(defun xsc-diff-directories--get-diff-string (fileA fileB)
  (with-temp-buffer
    (diff-no-select fileA
		    fileB
		    nil
		    t
		    (current-buffer))
    (buffer-substring (point-min) (point-max))))

(if (eq system-type 'windows-nt)
    (defun xsc-diff-directories--files-equal-p (file1 file2)
      "Check if FILE1 and FILE2 files are equal by computing a checksum."
      (let ((sum1 (xsc-diff-directories--get-file-content-checksum file1))
	    (sum2 (xsc-diff-directories--get-file-content-checksum file2)))
	(string= sum1 sum2)))
  (defun xsc-diff-directories--files-equal-p (file1 file2)
    "Check if FILE1 and FILE2 files are equal using diff-no-select."
    ;; No puedo usar simplemente call-process:
    ;; (= 0 (call-process diff-command nil nil nil "-q" file1 file2))
    ;; porque no es compatible con tramp.
    ;; return nil, si hay diferencias entre file1 y file2.
    (with-temp-buffer
      (diff-no-select file1 file2 "-q" t (current-buffer))
      (goto-char (point-min))
      (search-forward "Diff finished (no differences)." nil t))))

(defun xsc-diff-directories--directory-files-recursively (dir)
  (directory-files-recursively dir ".*" nil
			       (lambda (subdir) ;; ignorar ciertos subdirs
				 (and (file-readable-p subdir)
				      (not (string= ".git" (file-name-base subdir)))
				      (not (string= ".ccls-cache" (file-name-base subdir)))))))


(defun xsc-diff-directories (dirA dirB)
  "Diff two directories DIRA (old) and DIRB (new).
Version: 23-08-2024
Updated: 08-02-2025"
  (interactive
   ;; en un let, para que el segundo directorio lo lea del padre del primero.
   (let ((dir1 (expand-file-name (read-directory-name "DirA (old): "))))
     (list dir1
	   (expand-file-name (read-directory-name "DirB (new): " (file-name-parent-directory dir1))))))
  (message "scanning directories...")
  (let* ((start-time (current-time))
	 (results-buffer (generate-new-buffer "*DiffDir*"))
	 (filesA (mapcar
		  (lambda (f) (file-relative-name f dirA))
		  (xsc-diff-directories--directory-files-recursively dirA)))
	 (filesB (mapcar
		  (lambda (f) (file-relative-name f dirB))
		  (xsc-diff-directories--directory-files-recursively dirB)))
	 ;; por defecto la comparacion usa eql, no funciona con strings
	 (diff-filesA-filesB (cl-set-difference filesA filesB :test #'equal))
	 (diff-filesB-filesA (cl-set-difference filesB filesA :test #'equal))
	 (intersection-files (cl-intersection filesA filesB :test #'equal))
	 (num-changes 0))

    (message (format "%d files in DirA, %d files in DirB. Detecting differences..."
		     (length filesA)
		     (length filesB)))

    (display-buffer results-buffer)
    (princ (format "DirA: %s\n      (dired \"%s\")\n" dirA dirA) results-buffer)
    (princ (format "DirB: %s\n      (dired \"%s\")\n\n" dirB dirB) results-buffer)
    (redisplay)

    (when diff-filesA-filesB
      (setq num-changes (+ num-changes (length diff-filesA-filesB)))
      (princ "There are files in DirA not shown in DirB:\n" results-buffer)
      (princ (mapconcat (lambda (f) (concat "--- " f))
			diff-filesA-filesB "\n")
	     results-buffer)
      (princ "\n\n" results-buffer)
      (redisplay))

    (when diff-filesB-filesA
      (setq num-changes (+ num-changes (length diff-filesB-filesA)))
      (princ "There are files in DirB not shown in DirA:\n" results-buffer)
      (princ (mapconcat (lambda (f) (concat "+++ " f))
			diff-filesB-filesA "\n")
	     results-buffer)
      (princ "\n\n" results-buffer)
      (redisplay))

    (when intersection-files
      (dolist (f intersection-files)
	(let ((fileA (expand-file-name f dirA))
	      (fileB (expand-file-name f dirB)))
	  (if (not (xsc-diff-directories--files-equal-p fileA fileB))
	      (let* ((fileAattrs (file-attributes fileA))
		     (fileBattrs (file-attributes fileB))
		     (fileAmodtime (file-attribute-modification-time fileAattrs))
		     (fileBmodtime (file-attribute-modification-time fileBattrs)))
		(setq num-changes (1+ num-changes))
		(princ (format "=/= %s differ:\n" f) results-buffer)
		(princ (format "(diff \"%s\"    ;; %s (%s)\n      \"%s\")    ;; %s (%s)\n\n"
				fileA (format-time-string "%F %T" fileAmodtime) (if (time-less-p fileAmodtime fileBmodtime) "older" "newer")
				fileB (format-time-string "%F %T" fileBmodtime) (if (time-less-p fileBmodtime fileAmodtime) "older" "newer"))
		       results-buffer)
		(redisplay))))))

    (princ (format "Took: %f seconds.\n" (float-time (time-subtract (current-time) start-time))) results-buffer)

    (if (= num-changes 0)
	(message "No differences.")
      (message "There are %d differences." num-changes))))

;; usar overlays para mostrar y ocultar los diffs ?
;; overlays pruebas
;; (with-current-buffer results-buffer
;;   (let ((start (point))
;; 	    ov)
;; 	(insert "Esto es un overlay\n")
;; 	(setq ov (make-overlay start (point)))
;; 	(overlay-put ov 'invisible t)
;; 	))
;; (defun xsc-toggle-overlay ()
;;   (interactive)
;;   (let* ((ov-start (next-overlay-change (point)))
;; 	 (ov (car (overlays-at ov-start))))
;;     (message "overlay starts: %d" ov-start)
;;     (overlay-put ov 'invisible (not (overlay-get ov 'invisible)))))

(provide 'xsc-utils)
;;; xsc-utils.el ends here
