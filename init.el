(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 27)
  (package-initialize))

(require 'cl-lib) ;; cl-remove-if, ...
(require 'rx) ;; cargar esto antes de instalar company-anaconda

;; ========== windows ==========

(when (eq system-type 'windows-nt)
  ;; default-directory
  (setq inhibit-splash-screen t)
  (when (getenv "HOME") (setq default-directory (concat (getenv "HOME") "/")))
  ;; Git for Windows en PATH
  (if (and (file-accessible-directory-p "C:\\Program Files\\Git\\usr\\bin") (not (string-match-p (regexp-quote "C:\\Program Files\\Git\\usr\\bin") (getenv "PATH"))))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin;" (getenv "PATH")))
    (message "No Git for Windows directory found or already in PATH"))
  ;; Borrar NUL de find | grep
  ;; default "find <D> <X> -type f <F> -exec grep <C> -n <R> \"{}\" NUL \";\""
  (require 'grep)
  (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH <R> \"{}\" \";\""))


;; (server-start)

;; ========== instalar paquetes ==========

;; *** leeme ***
;; Si añado un paquete en la variable my-packages y reinicio Emacs, el paquete
;; se instala pero en M-x list-packages aparece como dependencia y no como instalado.
;; Por esto, si se hace M-x package-autoremove intentara borrar los nuevos paquetes.
;; Esto pasa porque la variable package-selected-packages no se ha actualizado para
;; incluir el nuevo paquete.
;; En cambio, si en vez de reiniciar re-evaluo (eval-defun) la variable my-packages
;; y vuelvo a ejecutar (C-x C-e) el codigo para instalar los paquetes, la variable
;; package-selected-packages si que se actualiza para incluir los nuevos paquetes.
;; Tambien puedo borrar el contenido de custom.el y reiniciar emacs para
;; actualizar todo.

;; lista de paquetes:
;; para evaluarlo de nuevo despues de añadir paquetes
;; no usar C-x C-e (eval-lastsexp), hay que usar
;; C-M x (eval-defun)

;; antes de instalar autocompletion para python:
;; anaconda-mode, company-anaconda, flycheck
;; hay que instalar en el sistema:
;; python3-setuptools, python3-ipython, python3-jedi, python3-flake8
;; python3-mypy (y mypy en ubuntu)

(defvar my-packages '(magit ;; interface to git
		      which-key ;; display the key bindings following a command
		      solarized-theme ;; theme
		      smex ;; M-x enhancement for Emacs
		      swiper ;; alternative to isearch
		      rainbow-mode ;; colorize color names / hex colors
		      rainbow-delimiters ;; highlight parenthesis
		      birds-of-paradise-plus-theme ;; brown/orange color theme
		      company ;; auto completion
		      anaconda-mode ;; code completion, navigation, doc... python
		      company-anaconda ;; anaconda backend for company
		      flycheck ;; syntax checker
		      flycheck-mypy ;; python type annotations checker
		      git-gutter ;; diff with repo
		      avy
		      yaml-mode
		      telephone-line
		      beacon
		      )
  "Lista de paquetes que hay que mantener instalados.")

;; info sobre python
;; http://nasseralkmim.github.io/notes/2017/03/11/minimalist-python-developement-environment-in-emacs/

;; instalar los paquetes que no esten instalados
(let ((not-installed-packages (cl-remove-if #'package-installed-p my-packages)))
  (if not-installed-packages
      (progn
	(message "%s" "Hay paquetes sin actualizar, actualizando...")
	(package-refresh-contents)
	(dolist (pkg not-installed-packages)
	  (package-install pkg))
	(message "%s" "Hecho."))
    (message "%s" "Todo actualizado")))

;; ========== customization file ==========
;; mover lisp autogenerado por M-x customize a otro archivo
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; ========== modificar UI ==========
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(if (member "Inconsolata" (font-family-list))
    (set-frame-font "Inconsolata 14" nil t))

;; scrolling
(setq scroll-conservatively 10)
(setq scroll-margin 3)

;; whitespace
(setq-default show-trailing-whitespace t)

;; ========== mis-funciones ==========
(defun xsc-diff-buffer-with-saved-file ()
  "Same as 'diff-buffer-with-file', but automatically choosing the current buffer."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key (kbd "C-c d") 'xsc-diff-buffer-with-saved-file)

(defun xsc-timestamp-dmyz ()
  "Date timestamp."
  (interactive)
       (insert (format-time-string "%d-%m-%Y %Z")))

(defun xsc-timestamp-hms ()
  "Hour timestamp."
  (interactive)
       (insert (format-time-string "%H:%M:%S")))

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

;; (load-theme 'solarized-light t)
;; (load-theme 'birds-of-paradise-plus t)
;; ver creamsody-theme

;; ========== magit ==========
;; https://magit.vc/
(global-set-key (kbd "C-x g") 'magit-status)

;; ========== which-key ==========
;; https://github.com/justbur/emacs-which-key
(which-key-mode)

;; ========== ido ==========
;; built-in
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(setq ido-use-filename-at-point nil)
(ido-mode 1)

;; ========== smex ==========
;; https://github.com/nonsequitur/smex/blob/master/README.markdown
;; sustituir 'execute-extended-command por 'smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ========== swiper ==========
;; https://github.com/abo-abo/swiper
;; usando emacs desde la terminal 'emacs -nw'
;; no reconoce Ctr-Shift-*, lo interpreta como C-*.
;; https://emacs.stackexchange.com/questions/32294/how-to-make-emacs-recognise-c-shift-combinations-in-terminal-mode
(if (display-graphic-p)
    (global-set-key (kbd "C-S-s") 'swiper) ;; en emacs GUI
  (global-set-key (kbd "C-s") 'swiper))

;; ========== rainbow-mode  ==========
(add-hook 'prog-mode-hook #'rainbow-mode)

;; ========== rainbow-delimiters  ==========
(show-paren-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ========== python  ==========
(setq python-shell-interpreter "python3")

;; ========== company  ==========
(add-hook 'after-init-hook #'global-company-mode)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0)
;; por defecto las sugerencias de company
;; se mueven con M-n y M-p. Cambiar a usar C-n y C-p
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; ========== anaconda-mode  ==========
;; https://github.com/proofit404/anaconda-mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; ========== company-anaconda  ==========
;; https://github.com/proofit404/company-anaconda
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; ========== flycheck  ==========
(global-flycheck-mode)
;; poner antes python-shell-interpreter a python3
(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
(flycheck-add-next-checker 'python-flake8 '(warning . python-mypy) t)

;; ========== git-gutter  ==========
(global-git-gutter-mode +1)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; ========== avy ==========
(global-set-key (kbd "C-.") 'avy-goto-char)
(global-set-key (kbd "C-,") 'avy-goto-line)
(define-key isearch-mode-map (kbd "C-.") 'avy-isearch)

;; ========== yaml ==========
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; ========= telephone-line ==========
(require 'telephone-line)
(telephone-line-mode 0)

;; ========= beacon ==========
(beacon-mode 1)
