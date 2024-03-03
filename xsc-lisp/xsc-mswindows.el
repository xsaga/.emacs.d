;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-mswindows.el --- Windows
;;; Commentary:
;;; Code:

(require 'grep)

;; default-directory
;; Use 'expand-file-name' to canonicalize the home directory. Without 'expand-file-name' you
;; get "C:\Users\someone/", when using ido-mode or vertico with ido-like navigation enabled,
;; pressing backspacke won't work properly (will not delete the whole directory name) for
;; directories with \ as a separator. After expanding the file name, you get "c:/Users/someone/"
;; and ido-like navigation works properly.
(when (getenv "HOME") (setq default-directory (expand-file-name (concat (getenv "HOME") "/"))))

;; emoji
(let ((emoji-font-list '("Segoe UI Historic"
			 "Segoe UI Symbol"
			 "Segoe UI Emoji")))
  (dolist (font-name emoji-font-list)
    (when (member font-name (font-family-list))
      (message "<xsc> setting font %s" font-name)
      (set-fontset-font t 'unicode (font-spec :family font-name) nil 'prepend)
      )))

;; Git for Windows en PATH
(setq git-usr-bin-path "C:/Program Files/Git/usr/bin")

(unless (file-accessible-directory-p git-usr-bin-path)
  (display-warning 'xsc "Git not found. Consider installing Git." :warning))

(when (and (file-accessible-directory-p git-usr-bin-path)
	   (not (string-match-p (regexp-quote git-usr-bin-path) (getenv "PATH"))))
  ;; set exec-path and PATH env variable:
  (push git-usr-bin-path exec-path) ;; para ediff (y otros)
  (setenv "PATH" (concat git-usr-bin-path path-separator (getenv "PATH"))))


;; This grep is from Git Bash, but the null device is NUL from Windows. Do not use it.
(grep-apply-setting 'grep-use-null-device nil)

;; OLD
;;; Borrar NUL de find | grep
;;; default "find <D> <X> -type f <F> -exec grep <C> -n <R> \"{}\" NUL \";\""
;;; (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH <R> \"{}\" \";\"")


;; everything (voidtools.com)
;; TODO use 'mlocate' or 'plocate' in linux to implement the same functionality.
(setq everything-path "C:/Program Files/Everything"
      everything-cli "es.exe")

(unless (file-readable-p (concat (file-name-as-directory everything-path) "Everything.exe"))
  (display-warning 'xsc "'Everything' search program not found. Consider installing Everything (https://www.voidtools.com)" :warning))
(unless (file-readable-p (concat (file-name-as-directory everything-path) everything-cli))
  (display-warning 'xsc (format "'Everything' Command Line Interface not found. Consider installing it inside %s" everything-path) :warning))

(add-to-list 'exec-path everything-path)

;; TODO. Proporcionar otra opcion/funcion para que los resultados de
;; everything se muestren en un buffer de dired en vez de completing-read.
;; Dired tiene una funcionalidad para crear un buffer de dired a partir de
;; una lista de paths arbitrarios. (ver manual dired)
;; (dired '("dired buffer name" ("/dir1/path1"
;;                               "/dir1/path2"
;;                               "/dir2/path1"
;;                               "/dirx/pathy"))
(defun xsc-everything-search (use-regexp match-case query)
  "Query Everything and copy the selection to the kill ring."
  (interactive (list
		(y-or-n-p "Search using regular expressions? ")
		(y-or-n-p "Match case? ")
		(read-string "Query: ")))
  (with-temp-buffer
    (let ((arg-list (split-string query " ")))
      (when use-regexp (add-to-list 'arg-list "-r"))
      (when match-case (add-to-list 'arg-list "-i"))
      (message "Searching for '%s'. Arguments: %s ..." query arg-list)
      ;; if the search query is empty (""), everything takes a lot of time to generate all results
      ;; better if we export the database as a txt and read the file directly
      (if (string= "" query)
	  ;; export database and read the file
	  (progn
	    (let ((db-tmp-file (make-temp-file "emacs-everything-db" nil ".txt")))
	      (call-process everything-cli nil nil nil "-export-txt" db-tmp-file "!C:\\Windows")
	      ;; (call-process everything-cli nil nil nil "-export-txt" db-tmp-file) ;; no filters
	      (insert-file-contents db-tmp-file)
	      (delete-file db-tmp-file))) ;; insert-file-contents-literally ?
	;; query everything
	(apply #'call-process everything-cli nil t nil arg-list)))
    (let ((items (split-string
		  (string-trim (buffer-string))
		  "\n")))
      (if (string= "" (buffer-string))
	  (progn
	    (message "No matches found.")
	    nil)
	;; expand-file-name to canonicalize the selected file (also convert windows backslash to forwardshlash)
	(kill-new (expand-file-name (completing-read (format "Found %s matches: " (length items)) items)))
	(message "Copied to the kill ring: %s" (current-kill 0 t))))))

(defun xsc-open-windows-terminal (working-dir)
  "Open Windows Terminal (wt.exe) with PATH as the current working directory.
First implementation 22-12-2022"
  (interactive (list (xsc-get-directory-desperately)))
  ;; wt.exe no tiene un flag para listar los profiles!?
  (let* ((parsed-profile-list
	  (let ((wt-settings-file (concat (file-name-as-directory (car (directory-files (format "C:/Users/%s/AppData/Local/Packages/" user-login-name)
											t "Microsoft.WindowsTerminal" 1)))
					  "LocalState/settings.json"))
		(parsed-profiles nil))
	    (when (file-exists-p wt-settings-file)
	      ;; parse json file
	      (with-temp-buffer
		(insert-file-contents-literally wt-settings-file)
		(setq parsed-profiles
		      (gethash "list"
			       (gethash "profiles"
					(json-parse-buffer))))))
	    (mapcar (lambda (x) "Get profile names" (gethash "name" x))
		    parsed-profiles)
	    ))
	 (profile (if parsed-profile-list
		      (completing-read "Profile: " parsed-profile-list)
		    nil)))
    (let ((terminal-command-args (concat "wt.exe" " --tabColor \"#c64e3b\""
					 (if profile (format " -p \"%s\"" profile) "")
					 (if working-dir (format " -d \"%s\"" working-dir) ""))))
      (message "Calling Windows Terminal: %s" terminal-command-args)

      ;; Cuando emacs crea un subproceso, el subproceso hereda las variables de entorno que aparecen
      ;; en la variable 'process-environment'. Una de esas variables es TERM=dumb (por lo menos en
      ;; Windows). Cuando ejecuto windows terminal por defecto, git diff, ipython, etc. no funcionan
      ;; correctamente porque la terminal es dumb. (bash: echo $TERM, powershell $env:TERM). Al
      ;; ejecutar windows terminal a mano fuera de emacs, windows no define la variable TERM.
      (with-environment-variables (("TERM" (if (string= "dumb" (getenv "TERM"))
					       nil
					     (getenv "TERM"))))
	(shell-command terminal-command-args))
      )))

;; Spelling.
;; Por alguna razon, aspell no funciona adecuadamente para castellano.
;; Las palabras con tilde (o con ñ) las rompe en dos por la letra con tilde
;; y entonces las sugerencias no funcionan correctamente. Si una de las sugerencias
;; tiene tilde, la pone bien pero luego lo vuelve a marcar como incorrecta porque la
;; divide en dos. Esto pasa con el wrapper de aspell usando wsl y tambien instalando
;; aspell usando msys2. Usando hunspell instalado con msys2 funciona bien, pero los
;; resultados no parecen tan buenos como aspell (cuando funciona bien).

;; Por ahora mejor uso hunspell con msys2, si existe. Si no, aspell en wsl.
;; En msys2 solo tiene el diccionario ingles:
;; pacman -Ss hunspell
;; pacman -S mingw-w64-ucrt-x86_64-hunspell mingw-w64-ucrt-x86_64-hunspell-en
;; Para el castellano hay que instalarlo a mano.
;; De libreoffice bajar el diccionario (extension .oxt, que es realmente un .zip)
;; https://extensions.libreoffice.org/en/extensions/show/spanish-dictionaries
;; Y mover los .aff y .dic a la carpeta de diccionarios de hunspell.
;; verificar con: hunspell -D
;; Ver ejemplo: https://www.emacswiki.org/emacs/Swiss_German_Hunspell_on_Windows_using_MSYS2

(if (file-exists-p "c:/msys64/msys2_shell.cmd")
    (progn
      (message "<xsc> msys2 found.")
      (if (file-exists-p "c:/msys64/ucrt64/bin/hunspell.exe")
	  (progn
	    (message "<xsc> hunspell.exe found under msys2")
	    (setq ispell-program-name "c:/msys64/ucrt64/bin/hunspell.exe")
	    ;; para que funcione hunspell, parece que hace falta la variable LANG
	    (setenv "LANG" "en_US"))
	(message "<xsc> hunspell.exe not installed under msys2")))
  (progn
    (message "<xsc> msys2 not found.")
    ;; fallback a aspell usando wsl
    (with-temp-buffer
      (if (= 0 (call-process "wsl.exe" nil t nil "aspell" "--version"))
	  (progn
	    ;; create the WSL aspell wrapper for windows
	    (with-temp-file (expand-file-name "~/aspell_autogenerated.cmd")
	      (insert "@echo off\n")
	      (insert "wsl aspell %*\n"))
	    (setq ispell-program-name (expand-file-name "~/aspell_autogenerated.cmd"))
	    (message "<xsc> Aspell found using WSL: %s" (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
	(message "<xsc> Failed calling aspell using WSL: %s" (string-trim (buffer-substring-no-properties (point-min) (point-max))))))))

;; Racket
(setq racket-path "C:/Program Files/Racket")
(unless (file-readable-p (concat (file-name-as-directory racket-path) "Racket.exe"))
  (display-warning 'xsc "Racket.exe not found" :warning))

(add-to-list 'exec-path racket-path)

(provide 'xsc-mswindows)
;;; xsc-mswindows.el ends here
