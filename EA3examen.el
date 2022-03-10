;; Idiomas nuevos pueden añadirse aquí con sus correspondientes prefijos,
;; añadiendo en su caso un define-key para el submenú

(setq es '("ES- " "Español"))
(setq eu '("EU- " "Euskera"))
(setq en '("EN- " "Inglés"))
(setq fr '("FR- " "Francés"))
(setq languages '(es eu en))

(setq buffer-file-coding-system 'latin-1)

;; (let (value)
;; (setq prefijos (dolist (element (reverse languages) value)
;;               (setq value (cons (car (eval element)) value))))
;; )

(setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))

(defun categoriza ()
  "Inserta una categoría en el lugar del cursor"
  (interactive)
  (search-backward "begin{question}")
  (backward-char 2)
  (newline)
  (insert (concat "%%%cat: " cat))
  (insert " %%%")
  )

(defun babeliza (prefijos)
  "Babeliza línea o región.
Toma una línea o región y añade a cada una de sus líneas un prefijo
por cada idioma en la lista 'prefijos'"
  (interactive "*p")
  (setq new-beg (make-marker))
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
		      (buffer-substring (region-beginning)
					(- (region-end) 1) )
		    (progn
		      (setq resultado "")
		      (while prefijos
			(setq resultado
			      (concat (concat (car prefijos)
					      (thing-at-point 'line) )
				      resultado))
			(setq prefijos (cdr prefijos)))
		      (if (< 0 (forward-line 1))
			  (newline))
		      resultado
		      )
		    )
		  )
	    )
	(if use-region
	    (progn
	      (string-insert-rectangle (region-beginning)
				       (- (region-end) 1 )
				       (car prefijos))
	      (setq prefijos (cdr prefijos))
	      (while prefijos
		(setq new-beg (point))
		(insert text)
		(string-insert-rectangle new-beg
					 (region-end)
					 (car prefijos))
		(setq prefijos (cdr prefijos))
		(newline)
		)
	      )
	  )
	(if use-region nil
	  (insert text))
	)
      )
    (if use-region nil
      (kill-whole-line))
    )
  )

(defun babeliza-proxy()
  (interactive)
  (babeliza prefijos)
  )

(coding-system-list)

(defun Rnw2TeX ()
  "Procesa .Rnw para convertir a .tex"
  (interactive)
  (shell-command
   (format "R CMD Sweave %s "
	   (shell-quote-argument (buffer-file-name))))
  (setq newbuf (replace-regexp-in-string ".Rnw" ".tex" (buffer-file-name)))
  (find-file newbuf)
  (set-buffer newbuf)
  )

(defun TeX2Moodle ()
  "Procesa .tex para convertir a .xml"
  (interactive)
  (shell-command
   (format "moodleiza %s "
	   (shell-quote-argument (buffer-file-name))))
  (setq newbuf (replace-regexp-in-string ".tex" "-clean.xml" (buffer-file-name)))
  (find-file newbuf)
  (set-buffer newbuf)
  )

(defun desglose-preguntas ()
  "Examina la fuente .Rnw de un examen y proporciona desglose temático"
  (interactive)
  (shell-command
   (format "desglosa %s "
	   (shell-quote-argument (buffer-file-name))))
  (setq newbuf (replace-regexp-in-string ".Rnw" "-desglose.txt" (buffer-file-name)))
  (find-file newbuf)
  (set-buffer newbuf)
  )


(defun crear-versiones (prefijos)
  "Procesa .tex para crear distintas versiones"
  (interactive "*p")
  (let (
	(ver    prefijos)
	(oldbuf (current-buffer))
	(start  (point-min))
	(end    (point-max))
	(newbuf (replace-regexp-in-string ".Rnw" ".tex" (buffer-name)))
	)
    (while ver
      (setq preservar (car ver))
      (setq borrar (remq preservar prefijos))
      (setq version (concat (substring preservar 0 2)
			    (concat "." newbuf)))
      (with-current-buffer (get-buffer-create version)
	(save-excursion
	  (delete-region (point-min) (point-max))
	  (insert-buffer-substring oldbuf start end)
	  (perform-replace (concat "^" (substring preservar 0 3))
			   "" nil t nil nil nil (point-min) (point-max))
	  (setq pautas (concat "^\\("
			       (mapconcat (function (lambda (x) (substring x 0 3)))
					  borrar "\\|") "\\)"))  ;;
	  (flush-lines pautas (point-min) (point-max) nil) ;;
	  (let ((coding-system-for-write 'latin-1)
		(coding-system-for-read  'latin-1))
	    (write-region (point-min) (point-max) version nil t nil nil)
	    (run-hooks 'LaTeX-mode-hook)
	    (latex-mode)
	    (find-file version))
	  (save-buffer)
	  ))
      (setq ver (cdr ver))
      )
    )
  )

(defun crear-versiones-proxy()
  (interactive)
  (crear-versiones prefijos)
  )

(defun conmuta-idioma-es ()
  (interactive)
  (setq languages (if (memq 'es languages)
		      (delq 'es languages)
		    (setq languages (cons 'es languages))
		    )
	)
  (setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))
  )

(defun conmuta-idioma-eu ()
  (interactive)
  (setq languages (if (memq 'eu languages)
		      (delq 'eu languages)
		    (setq languages (cons 'eu languages))
		    )
	)
  (setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))
  )

(defun conmuta-idioma-en ()
  (interactive)
  (setq languages (if (memq 'en languages)
		      (delq 'en languages)
		    (setq languages (cons 'en languages))
		    )
	)
  (setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))
  )

(defun conmuta-idioma-fr ()
  (interactive)
  (setq languages (if (memq 'fr languages)
		      (delq 'fr languages)
		    (setq languages (cons 'fr languages))
		    )
	)
  (setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))
  )

(global-set-key [f4] 'nueva-pregunta)
(global-set-key [f7] 'babeliza-proxy)
(global-set-key [f8] 'crear-versiones-proxy)

(defvar menu-x (make-sparse-keymap))

(define-key menu-x [o] '(menu-item "--"))
(defvar submenua (make-sparse-keymap))
(define-key menu-x [n] (cons "Idiomas" submenua))


(define-key submenua [r]
  `(menu-item "Francés" conmuta-idioma-fr
	      :button (:toggle . (memq 'fr languages))
	      :help "Añade/elimina idioma francés")
  )

(define-key submenua [q]
  `(menu-item "Inglés" conmuta-idioma-en
	      :button (:toggle . (memq 'en languages))
	      :help "Añade/elimina idioma inglés")
  )

(define-key submenua [p]
  `(menu-item "Euskera" conmuta-idioma-eu
	      :button (:toggle . (memq 'eu languages))
	      :help "Añade/elimina idioma euskera")
  )

(define-key submenua [m]
  `(menu-item "Español" conmuta-idioma-es
	      :button (:toggle . (memq 'es languages))
	      :help "Añade/elimina idioma español")
  )


(define-key menu-x [f8]
  `(menu-item "Crear versiones" crear-versiones-proxy
	      :help "Crea versiones .tex de todos los idomas selecccionados")
  )

(define-key menu-x [l]
  `(menu-item "Desglose por temas ( .Rnw -> .txt)" desglosa-preguntas
	      :help "Proporciona el desglose por temas de las preguntas de un examen")
  )

(define-key menu-x [k] '(menu-item "--"))

(define-key menu-x [j]
  `(menu-item "Moodleiza ( .tex -> .xml)" TeX2Moodle
	      :help "Procesa .tex para generar .xml")
  )

(define-key menu-x [hg]
  `(menu-item ".Rnw -> .tex" Rnw2TeX
	      :help "Procesa .Rnw para generar .tex")
  )

(define-key menu-x [f7]
  `(menu-item "Babeliza" babeliza-proxy
	      :help "Replica región o línea en todos los idiomas")
  )

(define-key menu-x [f6]
  `(menu-item "Elimina línea" kill-current-line
	      :help "Elimina línea")
  )

(define-key menu-x [f5]
  `(menu-item "Duplica línea/región"  duplicate-line-or-region
	      :help "Duplica región o línea sin babelizar")
  )

(defvar submenuz (make-sparse-keymap))
(define-key menu-x [e] (cons "Añadir categoría" submenuz))

(define-key submenuz [y01]
  `(menu-item "R"  (lambda () (interactive)
			    (setq cat "R")
			    (categoriza)
			    )
	      :help "Añade categoría 'R'") )

(define-key submenuz [y1]
  `(menu-item "Muestreo"  (lambda () (interactive)
			    (setq cat "Muestreo")
			    (categoriza)
			    )
	      :help "Añade categoría 'Muestreo'") )

(define-key submenuz [y2]
  `(menu-item "Contraste hipótesis"  (lambda () (interactive)
				       (setq cat "Contraste hipótesis")
				       (categoriza)
				       )
	      :help "Añade categoría 'Contrate hipótesis'") )

(define-key submenuz [y3]
  `(menu-item "Estimación: método momentos"  (lambda () (interactive)
					       (setq cat "Método momentos")
					       (categoriza)
					       )
	      :help "Añade categoría 'Método momentos'") )
(define-key submenuz [y4]
  `(menu-item "Estimación: máxima  verosimilitud"  (lambda () (interactive)
						     (setq cat "Máxima verosimilitud")
						     (categoriza)
						     )
	      :help "Añade categoría 'Máxima verosimilitud'") )
(define-key submenuz [y5]
  `(menu-item "Estimación: propiedades estimadores"  (lambda () (interactive)
						       (setq cat "Propiedades estimadores")
						       (categoriza)
						       )
	      :help "Añade categoría 'Propiedades estimadores'") )
(define-key submenuz [y6]
  `(menu-item "Distribuciones: (normal, chi, t, F,...)"  (lambda () (interactive)
							   (setq cat "Distribuciones normal, chi...")
							   (categoriza)
							   )
	      :help "Añade categoría 'Distribuciones normal, chi...'") )

(define-key submenuz [y61]
  `(menu-item "Función característica"  (lambda () (interactive)
							   (setq cat "Función característica")
							   (categoriza)
							   )
	      :help "Añade categoría 'Función característica'") )

(define-key submenuz [y62]
  `(menu-item "Convergencias"  (lambda () (interactive)
							   (setq cat "Convergencias")
							   (categoriza)
							   )
	      :help "Añade categoría 'Convergencias'") )


(define-key submenuz [y7]
  `(menu-item "Distribuciones: Poisson"  (lambda () (interactive)
					   (setq cat "Distribuciones: Poisson")
					   (categoriza)
					   )
	      :help "Añade categoría 'Distribuciones: Poisson'") )
(define-key submenuz [y8]
  `(menu-item "Distribuciones: binomial"  (lambda () (interactive)
					    (setq cat "Distribuciones: binomial")
					    (categoriza)
					    )
	      :help "Añade categoría 'Distribuciones: binomial'") )

(define-key menu-x [g] '(menu-item "--"))
(defvar submenu (make-sparse-keymap))
(define-key menu-x [d] (cons "Crear..." submenu))

(define-key submenu [f4]
  `(menu-item "Cuestión" nueva-pregunta
	      :help "Crea una nueva pregunta vacía.")
  )

(define-key submenu [gg]
  `(menu-item "Bloque"
	      (lambda ()
		(interactive)
		(nuevo-bloque)
		(nueva-pregunta))
	      :help "Crea un nuevo bloque con una pregunta vacía.")
  )

(define-key submenu [a]
  `(menu-item "Examen"
	      (lambda (fichero)
		(interactive "sIntroducir nombre fichero: ")
		(message "Fichero: %s" fichero)
		(setq newbuf fichero)
		(find-file newbuf)
		(set-buffer newbuf)
		(highlight-lines-matching-regexp "^EU-" 'hi-green-b)
		(highlight-lines-matching-regexp "^ES-" 'hi-black-b)
		(highlight-lines-matching-regexp "^EN-" 'hi-blue-b)
		(highlight-lines-matching-regexp "^FR-" 'hi-yellow-b)
		(let ((coding-system-for-read 'latin-1))
		  (insert-file-contents "/usr/local/share/emacs/site-lisp/EA3plantilla.Rnw"))
		(save-buffer)
		)
	      :help "Crea nueva plantilla de examen. Usar sólo en buffer limpio")
  )

					;Define el botón "EA3" como contenedor de los demás submenús
(define-key-after
  (lookup-key global-map [menu-bar])    ;keymap is "key" menu-bar
  [ea]                                  ;key name
  (cons "EA3" menu-x)                 ;label. "key" is menu-x
  'buffer                               ;After buffer button
  )


;;  (defun op (path)                        ;cd to path
;;     (cd path)
;;     (call-interactively 'find-file)       ;open find-file window
;;  )

;; (defun opf (file)                       ;find named file
;;   (find-file file)
;;   )

(define-skeleton nueva-pregunta
  "Inserta una nueva pregunta en el buffer de trabajo."
  ""
  "\\begin{question}\n\n"
  "\\choice[!]{\n "
  -
  "\n}\n"
  "\\choice{  \n\n}\n"
  "\\choice{  \n\n}\n"
  "\\choice{  \n\n}\n"
  "\\choice{  \n\n}\n"
  "\\end{question}\n\n"
  )

(define-skeleton nuevo-bloque
  "Inserta un nuevo bloque en el buffer de trabajo."
  ""
  "\\begin{block}\n"
  "\\comienzobloque\n\n"
  -
  "\n\n\\finalbloque\n"
  "\\end{block}\n"
  "\\bigskip\n"
  )

(add-hook 'Rnw-mode-hook
	  (lambda ()
	    (highlight-lines-matching-regexp "^EU-" 'hi-green-b)
	    (highlight-lines-matching-regexp "^ES-" 'hi-black-b)
	    (highlight-lines-matching-regexp "^EN-" 'hi-blue-b)
	    (highlight-lines-matching-regexp "^FR-" 'hi-yellow-b)
	    (add-to-list 'TeX-command-list
			 '("Sweave" "R CMD Sweave %s.Rnw"
			   TeX-run-compile nil (latex-mode) :help "Run Sweave") t)
	    (add-to-list 'TeX-command-list
			 '("Documento EN" "versiones.sh %s.tex ; pdflatex EN.%s.tex ; xpdf EN.%s.pdf"
			   TeX-run-compile nil (latex-mode) :help "Documento EN") t)
	    (add-to-list 'TeX-command-list
			 '("Documento ES" "versiones.sh %s.tex ; pdflatex ES.%s.tex ; xpdf ES.%s.pdf"
			   TeX-run-compile nil (latex-mode) :help "Documento ES") t)
	    (add-to-list 'TeX-command-list
			 '("Documento EU" "versiones.sh %s.tex ; pdflatex EU.%s.tex ; xpdf EU.%s.pdf"
			   TeX-run-command nil (latex-mode) :help "Documento EU") t)
	    )
	  )
