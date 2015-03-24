;; Idiomas nuevos pueden añadirse aquí con sus correspondientes prefijos,
;; añadiendo en su caso un define-key para el submenú

(setq es '("ES- " "Español"))
(setq eu '("EU- " "Euskera"))
(setq en '("EN- " "Inglés"))
(setq fr '("FR- " "Francés"))
(setq languages '(es eu en fr))

;; (let (value)
;; (setq prefijos (dolist (element (reverse languages) value)
;;               (setq value (cons (car (eval element)) value))))
;; )

(setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))

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
	  (flush-lines pautas (point-min) (point-max) nil)               ;;
	  (write-region (point-min) (point-max) version nil t nil nil)
	  (run-hooks 'LaTeX-mode-hook)
	  (latex-mode)
	  (find-file version)
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
  `(menu-item "Crear versiones"
	       (crear-versiones-proxy)
	      )
  )

(define-key menu-x [j] '(menu-item "--"))
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
		(nuevo-examen)
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

(define-skeleton nuevo-examen
      "Inserta una plantilla de examen en el buffer de trabajo.
    A utilizar sólo con un buffer vacío."
      ""
"\\documentclass[10pt,a4paper,twocolumn]{examdesign}\n"
"\\usepackage[T1]{fontenc}\n"
"\\usepackage[utf8]{inputenc}  %  latin1, utf8 o lo que se desee.\n"
"EN- \\usepackage[english]{babel}\n"
"ES- \\usepackage[english,spanish]{babel}\n"
"EU- \\usepackage[english,basque]{babel}\n"
"                               %  Hay que incluir 'english' en todo caso,\n"
"                               %  y el lenguaje que se desee activar (spanish,\n"
"                               %  basque) en último lugar.\n"
"EN- \\selectlanguage{english}\n"
"ES- \\selectlanguage{spanish}\n"
"EU- \\selectlanguage{basque}    %  Parece haber un 'bug' que hace que\n"
"                                %  esto no funcione: el lenguage activo\n"
"                                %  es en todo caso el señalado en\n"
"                                %  último lugar en el \\usepackage[...]{babel}\n"
"\n"
"\\usepackage{EA3examen}         %  Modificaciones e internacionalización del\n"
"                                %  formato examdesign.\n"
"\\usepackage{amsmath}\n"
"\\usepackage{eurosym}\n"
"\\usepackage{booktabs}          %  Para componer tablas decentes.\n"
"% \\usepackage{dcolumn}         %  Encolumnado de tablas alineando el\n"
"                                %  punto decimal.\n"
"% \\usepackage{times}\n\n"
"%%%  Parámetros que no necesitan ser cambiados (pero pueden serlo)  %%%\n\n"
"\\setrandomseed{121464}                     % para mismos resultados\n"
"                                            % en ejecuciones sucesivas.\n"
"%%%  Parámetros que DEBEN ser adecuados por el usuario  %%%%%%%%%%%%%%%\n"
"\\asignatura{\n"
-
"EN- Statistics Applied\\\\[2mm] to Economics}\n"
"ES- Estadística Aplicada a\\\\[2mm] la Economía}\n"
"EU- Ekonomiari Aplikatutako\\\\[2mm] Estatistika}\n"
"\\examname{\n"
"EN- }\n"
"ES- }\n"
"EU- 1. Partziala, 2015eko martxoak 6}\n"
"\\NumberOfVersions{1}                       % Número de modelos distintos\n"
"% \\iflanguage{spanish}{\\decimalcomma}{}    % ...o coma decimal. Sólo\n"
"ES-\\iflanguage{spanish}{\\decimalpoint}{}   % puede emplearse con español\n"
"% EU-\\iflanguage{basque}{\\decimalpoint}{}  % o euskera activados.\n"
"\\NoRearrange\n"
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
"\\usepackage{Sweave}\n"
"\\begin{document}\n"
"\\begin{frontmatter}\n"
"\\setcounter{page}{0}\n"
"\\thispagestyle{empty}\n"
"\\begin{center}\n"
"\\membrete\n"
"\\end{center}\n"
"\\vfill\n"
"\\begin{center}\n"
"\\textbf{\\large\n"
"EN- DIRECTIONS}\n"
"ES- INSTRUCCIONES}\n"
"EU- JARRAIBIDEAK}\n"
"\\end{center}\n"
"\\begin{enumerate}\n"
"\\item\n"
"EN- Correctly answered questions give one point.\n"
"EN- There is only one correct answer to each question.\n"
"EN- Questions not correctly answered carry a penalty of $-0.20$ points,\n"
"EN- so it is better to leave a question  unanswered rather than giving\n"
"EN- a wrong answer.\n"
"%\n"
"ES- Respuestas correctas otorgan un punto.\n"
"ES- Hay una única respuesta correcta para cada pregunta.\n"
"ES- Las respuestas incorrectas penalizan con  $-0.20$ puntos.\n"
"ES- Es mejor dejar una pregunta sin contestar que dar una respuesta incorrecta.\n"
"%\n"
"EU- Erantzun zuzenek puntu bat balio dute.\n"
"EU- Galdera bakoitzean erantzun zuzen bakarra dago.\n"
"EU- Okerreko erantzunek $-0.20$ puntuko penalizazioa dute,\n"
"EU- beraz hobe da ez erantzutea, oker erantzutea baino.\n"
"\\item\n"
"EN- Our goal is to gauge your understanding and command of concepts\n"
"EN- learned during the course, not your visual sharpness. It is\n"
"EN- a fact, though, that in a multiple choice exam great attention\n"
"EN- has to be paid to the details. It is quite common that knowledgeable  students\n"
"EN- waste their chances of a good grade because they\n"
"EN- do not pay sufficient attention to the precise wording of\n"
"EN- questions.\n"
"%\n"
"ES- Nuestro objetivo es evaluar tu comprensión y dominio de conceptos\n"
"ES- aprendidos en el curso, no tu agudeza visual. Sin embargo, en un\n"
"ES- examen de elección múltiple hay que prestar mucha atención a los detalles.\n"
"ES- Es frecuente que estudiantes bien preparados echen a perder una buena nota\n"
"ES- por prestar insuficiente atención a los enunciados.\n"
"%\n"
"EU- Gure helburua da baloratzea noraino ulertzen eta menperatzen diren ikasitako\n"
"EU- kontzeptuak, gure helburua ez da baloratzea zure zorroztasun bisuala. Hala ere,\n"
"EU- aukera anitzeko azterketa batean inportantea da kasu egitea detaileei.\n"
"EU- Horregatik funtsezkoa da, emaitza ona lortzeko, galderen enuntziatuak\n"
"EU- atentzio handiz irakurtzea.\n"
"\\begin{center}\n"
"\\large\n"
"EN- \\fbox{\\textbf{Please, do yourself a favour and read carefully before you answer!}}\n"
"ES- \\fbox{\\textbf{¡Por favor, lee con mucha atención antes de contestar!}}\n"
"EU- \\fbox{\\textbf{Mesedez, erantzun baino lehen, irakurri galderak arretaz!}}\n"
"\\normalsize\n"
"\\end{center}\n"
"\\item\n"
"EN- It will probably help you to discard first answers that are clearly inadequate.\n"
"ES- Probablemente te ayudará descartar primero respuestas claramente inadecuadas.\n"
"EU- Argi eta garbi okerrak diren erantzunak lehenengo baztertzea lagungarria izango zaizu.\n"
"\\item\n"
"EN- Students scoring\n"
"ES- Estudiantes con\n"
"EU- Azterketa honetan\n"
" 19.6\n"
"EN- or more points in this exam get full points towards their final grade.\n"
"ES- o más puntos en este examen obtienen el total de puntos para su nota final.\n"
"EU- puntu edo gehiago lortzen badira azterketaren puntu guztiak lortzen dira.\n"
"\\item\n"
"EN- The time scheduled for this exam is 1:20h.\n"
"ES- El tiempo previsto para este examen es 1:20h.\n"
"EU- Azterketa honen iraupena  1:20 da.\n"
"\\end{enumerate}\n"
"\\vfill\n"
"EN- \\hfill\\textbf{Do not turn the page until advised to do so!}\\par\n"
"EN- \\hfill\\textbf{Mark the type of your exam on the orange sheet NOW!}\n"
"%\n"
"ES- \\hfill\\textbf{No pases la página hasta que se indique.}\\par\n"
"ES- \\hfill\\textbf{Marca el tipo de examen en tu hoja de codificación ¡AHORA!}\n"
"%\n"
"EU- \\hfill\\textbf{Ez zabaldu azterketa abisua eman arte!}\\par\n"
"EU- \\hfill\\textbf{Markatu ORAIN zein den zure azterketa-mota erantzunen orrialdean!!}\n"
"\\par\n"
"\\end{frontmatter}\n"
"%\n"
"%  Algunas abreviaturas cuyo lugar natural es EA3examen.sty, pero que de momento\n"
"%  están aquí\n"
"%\n"
"\\def\\oh{\\frac{1}{2}}                  % Un medio\n"
"\\def\\Y{                                % Conjunción copulativa 'y'\n"
"EN- and                                  % en los tres idiomas\n"
"ES- y\n"
"EU- eta\n"
"}\n"
"\\def\\af{\n"
"EN- All other answers are false.}\n"
"ES- Las demás respuestas son falsas.}\n"
"EU- Beste erantzunak faltsuak dira.}\n"
"\\def\\Sesgo{\n"
"EN- \\textrm{Bias}}\n"
"ES- \\textrm{Sesgo}}\n"
"EU- \\textrm{Alborapena}}\n"
"\\def\\Var{\n"
"EN- \\textrm{Var}}\n"
"ES- \\textrm{Var}}\n"
"EU- \\textrm{Bar}}\n"
"\\def\\anylim#1{\n"
"    \\buildrel#1\\over\\longrightarrow}\n"
"\\def\\plim{\\anylim{p}}                    % convergencia en distribución\n"
"\\def\\dlim{\\anylim{d}}                    % convergencia en probabilidad\n"
"\\def\\cslim{                               % convergencia casi segura\n"
"ES- \\anylim{c.s.}\n"
"EN- \\anylim{a.s.}\n"
"EU-\n"
"}\n"
"\\hyphenation{ge-ne-ra-triz}\n"
"\\begin{multiplechoice}[title={\\eleccionmultiple},resetcounter=yes]\n"
"\\begin{question}\n"
"EN- The capital city of Spain is:\n"
"ES- La capital de España es:\n"
"EU- Espainiako hiriburua da:\n"
"    \\choice{Paris.}\n"
"    \\choice{Pekín.}\n"
"    \\choice[!]{Madrid.}\n"
"    \\choice{Kuala Lumpur.}\n"
"\\end{question}\n"
"\\end{multiplechoice}\n"
"\\end{document}\n"
)


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
