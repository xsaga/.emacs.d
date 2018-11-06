
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib) ;; remove-if, ...

;; ========== instalar paquetes ==========

;; lista de paquetes
(defvar my-packages '(magit)
  "Lista de paquetes que hay que mantener instalados.")

;; instalar los paquetes que no esten instalados
(let ((status-my-packages (mapcar 'package-installed-p my-packages)))
  (if (member nil status-my-packages)
      (progn
	(message "%s" "Hay paquetes sin actualizar, actualizando...")
	(package-refresh-contents)
	(dolist (pkg (remove-if 'package-installed-p my-packages))
	  (package-install pkg))
	(message "%s" "Hecho."))
    (message "%s" "Todo actualizado")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

