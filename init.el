(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib) ;; cl-remove-if, ...

;; ========== instalar paquetes ==========
;; lista de paquetes:
;; para evaluarlo de nuevo despues de añadir paquetes
;; no usar C-x C-e (eval-lastsexp), hay que usar
;; C-M x (eval-defun)
(defvar my-packages '(magit
		      which-key
		      solarized-theme)
  "Lista de paquetes que hay que mantener instalados.")

;; instalar los paquetes que no esten instalados
(let ((status-my-packages (mapcar 'package-installed-p my-packages)))
  (if (member nil status-my-packages)
      (progn
	(message "%s" "Hay paquetes sin actualizar, actualizando...")
	(package-refresh-contents)
	(dolist (pkg (cl-remove-if 'package-installed-p my-packages))
	  (package-install pkg))
	(message "%s" "Hecho."))
    (message "%s" "Todo actualizado")))

;; mover lisp autogenerado por M-x customize a otro archivo
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ========== modificar UI ==========
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(load-theme 'solarized-light t)

;; ========== modificar paquetes ==========
(which-key-mode)
