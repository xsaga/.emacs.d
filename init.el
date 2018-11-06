
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib) ;; remove-if, ...

(defvar my-packages '(magit)
  "Lista de paquetes que hay que mantener instalados.")

(let ((status-my-packages (mapcar 'package-installed-p my-packages)))
  (if (member nil status-my-packages)
      (progn
	(message "%s" "Hay paquetes sin actualizar, actualizando...")
	(package-refresh-contents)
	(dolist (pkg (remove-if 'package-installed-p my-packages))
	  (package-install pkg))
	(message "%s" "Hecho."))
    (message "%s" "Todo actualizado")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
