;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-python.el --- Python
;;; Commentary:

;; --- En Windows ---

;; Instalar python desde python.org, creo que es mas completo que lo
;; que incluye python desde el Microsoft store. En el instalador
;; seleccionar añadir python al path.

;; Crear un venv:
;; python -m venv venv
;; .\venv\Scripts\Activate.ps1
;; Si da un error relacionado con execution policies, ejecuta primero
;; Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
;; Y despues activa el venv. (https://docs.python.org/dev/library/venv.html)

;; En Emacs, activar el virtual environment modificando la siguiente
;; variable, tal y como explica la documentación de python.el:
;; (setq python-shell-virtualenv-root "c:/Users/someone/somewhere/venv/")
;; Con anaconda-mode, se puede usar la funcion pythonic-activate, que
;; pregunta por un directorio y establece el valor de esa variable al
;; directorio.

;; Por defecto en python-mode para windows no hay autocompletion
;; porque el python para Windows no trae readline, y es necesario para
;; que funcione el autocompletado. El pyreadline original para Windows
;; esta deprecated https://ipython.org/pyreadline. Como alternativa,
;; existe pyreadline3
;; https://github.com/pyreadline3/pyreadline3. Mejor instalarlo dentro
;; de cada venv con pip install pyreadline3.
;; https://bugs.python.org/issue45870

;;; Code:

(xsc-install-packages '(anaconda-mode))

(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'xsc-python)
;;; xsc-python.el ends here
