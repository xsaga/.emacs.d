;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; meneame.el --- Lector de meneame.net sin distracciones
;;; Commentary:
;; Visita meneame.net usando eww y parsea las noticias de la portada
;; de las N páginas más recientes. Puntua cada noticia en función de
;; la URL y palabras clave encontradas en el titular y la entradilla.
;; Las noticias con una puntuación mayor o igual a cero, se presentan
;; en un buffer distinto a las noticias con una puntuación negativa.
;; Define un Major mode para navegar entre las distintas noticias.
;; Incluye función para clasificar noticias (clickbait,basura,normal)
;; y guardar el resultado en un dataset (json).

;;; TODO:
;; Integrar con AIP de ChatGPT para puntuar el nivel de clickbait de
;; las noticias. Da resultados muy buenos con los siguientes prompts:

;; A continuación te voy a dar unos titulares en español, uno por cada
;; linea, y tu me vas a decir si esos titulares son clickbait o
;; no. Quiero que puntues el titular de 0 a 100, donde 0 es un título
;; sin clickbait y 100 es un título claramete clickbait. No des
;; ninguna explicación sobre por qué es o no clickbait, solo muestra
;; la puntuación usando el siguiente formato "Score: NUMERO", donde
;; NUMERO es la puntuación. De acuerdo?

;; A continuación te voy a dar unas noticias en español, uno por cada
;; linea, y tu me vas a decir si esas noticias son clickbait o
;; no. Cada noticia está representado en formato JSON y contiene los
;; siguientes campos: titular, entradilla, url. Quiero que puntues la
;; noticia de 0 a 100 teniendo en cuenta todos los campos, donde 0 es
;; una noticia sin clickbait y 100 es una noticia claramete
;; clickbait. No des ninguna explicación sobre por qué es o no
;; clickbait, solo muestra la puntuación usando el siguiente formato
;; "Score: NUMERO", donde NUMERO es la puntuación. De acuerdo?

;; A continuación te voy a dar unas noticias en español, uno por cada
;; linea, y tu me vas a decir si esas noticias son clickbait o
;; no. Cada noticia está representado en formato JSON y contiene los
;; siguientes campos: titular, entradilla, url. Quiero que puntues la
;; noticia de 0 a 100 teniendo en cuenta todos los campos, donde 0 es
;; una noticia sin clickbait y 100 es una noticia claramete
;; clickbait. Muestra la puntuación usando el siguiente formato
;; "Score: NUMERO", donde NUMERO es la puntuación, y después genera
;; una explicación de por qué has puntuado así la noticia. De acuerdo?

;;; Code:

(setq meneame-address "https://old.meneame.net"
      meneame-buffer-name "*Meneame*"
      meneame-ignore-buffer-name "*Meneame-ignore*"
      meneame-dataset-path (expand-file-name "~/Documents/meneame-dataset.json")
      hostname-score-assoc '(
			     ;; hostname . score
			     ("twitter.com" . -2)
			     ("x.com" . -2)
			     ("mobile.twitter.com" . -2)
			     ("mobile.x.com" . -2)
			     ("threadreaderapp.com" . -1)
			     ("www.elmundotoday.com" . -1)
			     )
      hostname-filename-score-assoc '(
				      ;; hostname+filename . score
				      ("/virales/" . -5) ;; huffingtonpost.es/virales/...
				      )
      keywords-score-assoc '(
			     ;; keyword-regexp . (score-en-titular . score-en-entradilla)
			     ("eran? de esperar" . (-0.5 . 0))
			     ("[^s]?cr[iy]pto" . (-1 . -1)) ;; "cr[iy]pto" hace tambien match con suscriptor...
			     ("bitcoin" . (-1 . -1))
			     ("cr[iy]ptogra[fp]" . (+5 . +5))
			     ("pantomi[mn]a full" . (-1 . 0))
			     ))

(defun meneo-score (url-hostname-articulo url-hostname-filename-articulo titular entradilla)
  "Puntua un meneo en funcion del URL-HOSTNAME-ARTICULO, URL-HOSTNAME-FILENAME-ARTICULO y las palabras del TITULAR y la ENTRADILLA.
Creado 12-11-2022"
  (let ((score 0)
	(case-fold-search t))
    ;; url hostname
    (when (assoc url-hostname-articulo hostname-score-assoc)
      (setq score (+ score (cdr (assoc url-hostname-articulo hostname-score-assoc)))))
    ;; url hostname + filename
    (dolist (hostfile-map hostname-filename-score-assoc)
      (let ((patron (car hostfile-map))
	    (puntos (cdr hostfile-map)))
	(when (string-match-p patron url-hostname-filename-articulo)
	  (setq score (+ score puntos)))))
    ;; keywords en TITULAR o ENTRADILLA
    (dolist (keyword-map keywords-score-assoc)
      (let ((kw (car keyword-map))
	    (score-titular (cadr keyword-map))
	    (score-entradilla (cddr keyword-map)))
	;; titular
	(when (string-match-p kw titular)
	  (setq score (+ score score-titular)))
	;; entradilla
	(when (string-match-p kw entradilla)
	  (setq score (+ score score-entradilla)))
	))
    score))

(defun meneame-parse (output-buffer output-ignore-buffer number-of-parsed-news)
  "Parsea las noticias de meneame.net. Recursivo.
Los resultados se guardan en el buffer OUTPUT-BUFFER.
NUMBER-OF-PARSED-NEWS se incrementa en cada iteración.
Devuelve el número de noticias parseadas.
Creado 31-10-2022
Actualizado 12-11-2022 15-02-2023"
  (condition-case exception
      (progn
        (re-search-forward "^[0-9]+? meneos")
        (re-search-forward "^[0-9]+? clics")
	(let ((case-fold-search nil))
          (re-search-forward "^por .* publicado")) ;; "^por .* a .* publicado" esto elimina las noticias tipo articulo porque no hay 'a'
        (beginning-of-line)
        (forward-line -3)
        (let* ((titulo (string-replace "\n" " " (string-trim (buffer-substring (save-excursion (backward-paragraph) (point)) (save-excursion (forward-paragraph) (point))))))
               (descripcion (progn
                              (forward-line 4)
                              (string-trim (buffer-substring (point) (save-excursion (re-search-forward "^[0-9]+ comentarios") (beginning-of-line) (forward-line -1) (point))))
                              ))
               (seccion (progn
                          (re-search-forward "^[0-9]+ comentarios")
                          (forward-line -1)
                          (end-of-line)
                          (string-trim (buffer-substring (point) (save-excursion (backward-word) (point))))
                          ))
	       (url-titular (url-generic-parse-url (get-text-property 0 'shr-url titulo)))
	       (url-host-titular (url-host url-titular))
	       (puntuacion (meneo-score url-host-titular (concat url-host-titular (url-filename url-titular)) (substring-no-properties titulo) (substring-no-properties descripcion))))
          (with-current-buffer (if (>= puntuacion 0) output-buffer output-ignore-buffer)
            (insert (format "* (%s) %s [%s] <%s>\n%s\n\n"
			    puntuacion
			    titulo
			    url-host-titular
			    (substring-no-properties seccion)
			    descripcion))))

        ;; recursivamente
        (meneame-parse output-buffer output-ignore-buffer (1+ number-of-parsed-news))
        )
    ('search-failed
     (progn
       (message "%s Noticias encontradas!" number-of-parsed-news)
       number-of-parsed-news
       ))
    ))

(defun mnm (page-numbers)
  "Obtener las noticias de meneame.net.
Creado 31-10-2022
Actualizado 12-11-2022"
  (interactive (list
		(read-number "Número de páginas? " 1)))
  (let ((this-buffer (current-buffer))
        (eww-buffer-name "*eww-old.meneame.net*")
        ;; why eww-mnm-buffer is a #<killed buffer> ???
        (results-buffer (get-buffer-create meneame-buffer-name))
	(ignored-results-buffer (get-buffer-create meneame-ignore-buffer-name))
	(total-news 0))
    (dotimes (page page-numbers)
      (let* ((meneame-url (if (= page 0) meneame-address (concat meneame-address (format "/?page=%s" (1+ page)))))
	     (url-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:129.0) Gecko/20100101 Firefox/129.0") ;; No usar el user agent de Emacs.
	     (eww-mnm-buffer (eww-browse-url meneame-url t)))
	(switch-to-buffer this-buffer)
	(message "Cargando %s ..." meneame-url)
	(sit-for 2)
	(with-current-buffer eww-buffer-name
	  (goto-char (point-min))
	  (setq total-news (+ total-news
			      (meneame-parse results-buffer ignored-results-buffer 0)
			      )))
	(kill-buffer eww-buffer-name)))
    (message "%s Noticias parseadas en total" total-news)
    (switch-to-buffer results-buffer)
    (goto-char (point-min))
    (meneame-mode)
    ))

(defun meneame-mode-siguiente-noticia ()
  ""
  (interactive)
  (when (looking-at-p "*")
    (forward-char))
  (re-search-forward "^* ")
  (beginning-of-line))

(defun meneame-mode-anterior-noticia ()
  ""
  (interactive)
  (re-search-backward "^* ")
  (beginning-of-line))

(defun meneame-mode-toggle-ignore-normal ()
  ""
  (interactive)
  (cond
   ((string= meneame-buffer-name (buffer-name (current-buffer)))
    ;; en principal, moverse a ignore si está disponible
    (if (get-buffer meneame-ignore-buffer-name)
	(progn
	  (switch-to-buffer meneame-ignore-buffer-name)
	  (meneame-mode)
	  (when (eobp) (goto-char (point-min))))
      (message "%s no disponible" meneame-ignore-buffer-name))
    )
   ((string= meneame-ignore-buffer-name (buffer-name (current-buffer)))
    ;; en ignore, moverse a principal si está disponible
    (if (get-buffer meneame-buffer-name)
	(progn
	  (switch-to-buffer meneame-buffer-name)
	  (meneame-mode)
	  )
      (message "%s no disponible" meneame-buffer-name))
    )))

(defun meneame-mode-quit ()
  ""
  (interactive)
  ;; set buffer to not modified to prevent kill-buffer from prompting
  (set-buffer-modified-p nil)
  (kill-buffer))

(defun meneame-mode-get-url ()
  ""
  (interactive)
  (let ((url-prop (save-excursion
		    (prop-match-value (text-property-search-forward 'shr-url)))))
    (kill-new url-prop)
    (message "Copied to the kill ring: %s" url-prop)))

(defun meneame-mode-clasificar-noticia ()
  "Version 18-02-2023"
  (interactive)
  (when (looking-at-p "*") (forward-char))
  (re-search-backward "^*")
  (let* ((url-prop (save-excursion
		     (prop-match-value (text-property-search-forward 'shr-url))))
	 (titular (save-excursion
		    (re-search-forward " (-?[0-9]+) \\(.*\\) \\[.*\\] <.*>$")
		    (substring-no-properties (match-string 1))))
	 (hostname (save-excursion
		     (re-search-forward " (-?[0-9]+) .* \\[\\(.*\\)\\] <.*>$")
		     (match-string 1)))
	 (categoria (save-excursion
		      (re-search-forward " (-?[0-9]+) .* \\[.*\\] <\\(.*\\)>$")
		      (match-string 1)))
	 (entradilla (save-excursion
		       (let (p1)
			 (forward-line)
			 (setq p1 (point))
			 (if (re-search-forward "^*" nil t) ;; falla si es la ultima noticia del buffer
			   (forward-line -1)
			   (goto-char (point-max)))
			 (string-replace "\n" " " (string-trim (buffer-substring-no-properties p1 (point)))))
		       ))
	 (timestamp (time-convert nil 'integer))
	 (identifier (md5 url-prop))
	 (label (completing-read (concat "Clasificando '" titular "': ")
				 '("clickbait"
				   "basura"
				   "normal")))
	 (news-object `((id . ,identifier)
			(label . ,label)
			(titular . ,titular)
			(entradilla . ,entradilla)
			(categoria . ,categoria)
			(url . ,url-prop)
			(hostname . ,hostname)
			(timestamp . ,timestamp))))
    ;; check si la noticia ya esta incluida
    (if (and (file-exists-p meneame-dataset-path)
	     (with-temp-buffer
	       (insert-file-contents-literally meneame-dataset-path)
	       (goto-char (point-min))
	       (search-forward identifier nil t)))
	(message "La noticia '%s' ya está clasificada." titular)
      ;; guardar clasificacion
      (with-temp-buffer
	(insert (json-encode news-object))
	(insert "\n")
	(append-to-file (point-min) (point-max) meneame-dataset-path))))
  (meneame-mode-siguiente-noticia))

(defvar meneame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'meneame-mode-siguiente-noticia)
    (define-key map "p" 'meneame-mode-anterior-noticia)
    (define-key map "i" 'meneame-mode-toggle-ignore-normal)
    (define-key map "l" 'meneame-mode-get-url)
    (define-key map "k" 'meneame-mode-clasificar-noticia)
    (define-key map "q" 'meneame-mode-quit)
    map)
  "docstring for meneame-mode map")

;; los links pierden el color y la fuente cambia a la default...
;; (defface meneame-mode-bullet
;;   '((t :foreground "orange"
;;        :weight bold))
;;   "doc"
;;   :group 'meneame-mode)

;; (defvar meneame-mode-highlights '(("*" . 'meneame-mode-bullet))
;;   "")

(define-derived-mode meneame-mode fundamental-mode "🐘"
  "Meneame mode"
  ;; (setq font-lock-defaults '(meneame-mode-highlights))
  )

(provide 'meneame)
;;; meneame.el ends here
