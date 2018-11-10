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
(defvar my-packages '(magit ;; interface to git
		      which-key ;; display the key bindings following a command
		      solarized-theme ;; theme
		      smex ;; M-x enhancement for Emacs
		      swiper ;; alternative to isearch
		      )
  "Lista de paquetes que hay que mantener instalados.")

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
(load custom-file)

;; ========== modificar UI ==========
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(load-theme 'solarized-light t)
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
(setq ido-use-filename-at-point t)
(ido-mode 1)

;; ========== smex ==========
;; https://github.com/nonsequitur/smex/blob/master/README.markdown
;; sustituir 'execute-extended-command por 'smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ========== swiper ==========
;; https://github.com/abo-abo/swiper
(global-set-key (kbd "C-S-s") 'swiper)
