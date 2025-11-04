;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; xsc-gptel.el --- LLMs, Generative AI, etc.
;;; Comments:
;; Para usar Gemini:
;; crear una API key en https://aistudio.google.com/
;; e incluir la siguiente linea en .authinfo.gpg:
;; machine generativelanguage.googleapis.com login apikey password <API_KEY>
;;; Code:

(xsc-install-packages '(gptel))

(setq
 gptel-model 'gemini-2.5-pro-exp-03-25
 gptel-backend (gptel-make-gemini "Gemini"
		 :key #'gptel-api-key  ;; poner api key en .authinfo.gpg
                 :stream t))

(setq gptel-default-mode 'org-mode)

(provide 'xsc-gptel)
;;; xsc-gptel.el ends here
