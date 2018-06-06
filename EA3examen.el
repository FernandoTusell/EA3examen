;; Idiomas nuevos pueden a�adirse aqu� con sus correspondientes prefijos,
;; a�adiendo en su caso un define-key para el submen�

(setq es '("ES- " "Espa�ol"))
(setq eu '("EU- " "Euskera"))
(setq en '("EN- " "Ingl�s"))
(setq fr '("FR- " "Franc�s"))
(setq languages '(es eu en))

(setq buffer-file-coding-system 'latin-1)

;; (let (value)
;; (setq prefijos (dolist (element (reverse languages) value)
;;               (setq value (cons (car (eval element)) value))))
;; )

(setq prefijos (mapcar (lambda(arg) (car (eval arg))) languages))

(defun babeliza (prefijos)
  "Babeliza l�nea o regi�n.
Toma una l�nea o regi�n y a�ade a cada una de sus l�neas un prefijo
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
  `(menu-item "Franc�s" conmuta-idioma-fr
	      :button (:toggle . (memq 'fr languages))
	      :help "A�ade/elimina idioma franc�s")
  )

(define-key submenua [q]
  `(menu-item "Ingl�s" conmuta-idioma-en
	      :button (:toggle . (memq 'en languages))
	      :help "A�ade/elimina idioma ingl�s")
  )

(define-key submenua [p]
  `(menu-item "Euskera" conmuta-idioma-eu
	      :button (:toggle . (memq 'eu languages))
	      :help "A�ade/elimina idioma euskera")
  )

(define-key submenua [m]
  `(menu-item "Espa�ol" conmuta-idioma-es
	      :button (:toggle . (memq 'es languages))
	      :help "A�ade/elimina idioma espa�ol")
  )

(define-key menu-x [f8]
  `(menu-item "Crear versiones" crear-versiones-proxy
	      :help "Crea versiones .tex de todos los idomas selecccionados")
  )

(define-key menu-x [j] '(menu-item "--"))
(define-key menu-x [hg]
  `(menu-item ".Rnw -> .tex" Rnw2TeX
	      :help "Procesa .Rnw para generar .tex")
  )

(define-key menu-x [f7]
  `(menu-item "Babeliza" babeliza-proxy
	      :help "Replica regi�n o l�nea en todos los idiomas")
  )

(define-key menu-x [f6]
  `(menu-item "Elimina l�nea" kill-current-line
	      :help "Elimina l�nea")
  )

(define-key menu-x [f5]
  `(menu-item "Duplica l�nea/regi�n"  duplicate-line-or-region
	      :help "Duplica regi�n o l�nea sin babelizar")
  )

(define-key menu-x [g] '(menu-item "--"))
 (defvar submenu (make-sparse-keymap))
(define-key menu-x [d] (cons "Crear..." submenu))

 (define-key submenu [f4]
   `(menu-item "Cuesti�n" nueva-pregunta
	       :help "Crea una nueva pregunta vac�a.")
   )

(define-key submenu [gg]
  `(menu-item "Bloque"
	      (lambda ()
		(interactive)
		(nuevo-bloque)
		(nueva-pregunta))
  :help "Crea un nuevo bloque con una pregunta vac�a.")
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
	      :help "Crea nueva plantilla de examen. Usar s�lo en buffer limpio")
  )

;Define el bot�n "EA3" como contenedor de los dem�s submen�s
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
