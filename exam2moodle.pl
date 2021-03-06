#!/usr/bin/perl
# Proceso de ficheros examdesign + EA3 --> Moodle XML
# Primera versi�n 2008, ampliado durante el confinamiento 2020.
# Versi�n de 2021 incorpora bloques v�a cloze
#
# (C) F. Tusell, 2021. Copia y modifica a tu antojo.

#
# Las cuestiones de Cloze no se rotulan, por tanto necesitamos hacerlo
# nosotros con las etiquetas que siguen:

@lab = ("&nbsp (a) &nbsp ", "&nbsp (b) &nbsp  ", "&nbsp (c)  &nbsp ", "&nbsp (d)  &nbsp ", "&nbsp (e) &nbsp  ") ;


sub proceso_cuestiones_bloque {
    if ($estado eq "PreambuloBloque") {
	$enunciado = "<P>" ;
	  while(<IN>) {
	      if($_ !~ /.*\\begin\{question.*$/) {
		  $enunciado = $enunciado.$_ ;
	      } else {
		  $estado = "FinalizadoPreambulo" ;
		  last ;
	      }
	  }				       #  Hemos llegado al final del enunciado
					       #  del pre�mbulo de bloque; lo imprimimos
	$enunciado =~ s/\$/ \$\$/g ;
	print OUT $enunciado, "</P>\n" ;
    }
      if ($_ =~ /^\s*\\begin\{question\}.*$/ ) {  #  Si es una l�nea de comienzo de pregunta,
	  $enunciado = "<P>" ;                   #  inicializa un contenedor de enunciado.
	  $estado = "InteriorPregunta" ;
	  while(<IN>) {
	      if($_ !~ /\s*\\choice.*$/) {
		  $enunciado = $enunciado.$_ ;
	      } else {
		  last ;
	      }
	  }				       #  Hemos llegado al final del enunciado de
					       #  una subpregunta; lo imprimimos
	  $enunciado =~ s/\$/ \$\$/g ;
	  print OUT $enunciado, "\n" ;
	  print OUT "{1:MULTICHOICE_V:" ;
	  $ilab = 0 ;                          #  Ordinal de etiqueta a poner a la siguiente
      }                                        #  respuesta

      if ($_ =~ /\s*\\choice\[!\]\{(.*)\}$/)  {
	  $linea = $1 ;
	  $linea =~ s/\$/\$\$/g ;
	  $linea =~ s/\}/\\\}/g ;
	  if ($ilab eq 0) {
	      print OUT "=".$lab[$ilab].$linea ;
	  } else {
	      print OUT "~=".$lab[$ilab].$linea ;
	  }
	  $ilab++ ;
      }
      elsif ($_ =~ /.*\\choice\[!\]\{(.*)$/)  {
	  $linea = $1 ;
	  $linea =~ s/\$/\$\$/g ;
	  $linea =~ s/\}/\\\}/g ;
	  if ($ilab eq 0) {
	      print OUT "=".$lab[$ilab].$linea ;
	  } else {
	      print OUT "~=".$lab[$ilab].$linea ;
	  }
	  $ilab++ ;
    }
     elsif ($_ =~ /.*\\choice\{(.*)\}$/) {
	 $linea = $1 ;
	 $linea =~ s/\$/\$\$/g ;
	 $linea =~ s/\}/\\\}/g ;
	 if ($ilab eq 0) {
	     print OUT $lab[$ilab].$linea ;
	 } else {
	     print OUT "~".$lab[$ilab].$linea ;
	 }
	 $ilab++ ;
      }
      elsif ($_ =~ /.*\\choice\{(.*)\s*$/) {
	  $linea = $1 ;
	  $linea =~ s/\$/\$\$/g ;
	  $linea =~ s/\}/\\\}/g ;
	  if ($ilab eq 0) {
	     print OUT $lab[$ilab].$linea ;
	  } else {
	     print OUT "~".$lab[$ilab].$linea ;
	  }
	  $ilab++ ;
      }
      elsif ($_ =~ /^\s*\}\s*$/) {
	  # no hacemos nada
      }
      elsif ($_ =~ /\s*\\end\{question\}.*$/ ) {
	  print OUT  "}\n" ;
	  $estado = "FueraPregunta" ;
	  $ilab = 0 ;
      }
      else {
	  $linea = $_ ;
	  if ($linea =~ /^\s*$/)  {
	      $noblanca = TRUE ;
	  }

      if ($estado eq "InteriorPregunta") {
	  if ($noblanca) {
	      $linea =~ s/\$/\$\$/g ;
	      $linea =~ s/\}/\\\}/g ;
	      print OUT $linea ;
	  }
	  }
      # if ($estado eq "PreambuloBloque") {
      #		  if ($noblanca) {
      #		      $linea =~ s/\$/\$\$/g ;
      #		      print OUT $linea ;
      #		  }
      #		  }
      }
}

sub proceso_cuestiones_sueltas {
  if ($_ =~ /^%%%cat: (.*) %%%$/ ) {       # Categor�a de la pregunta.
	  $linea = $1 ;
	  chomp $linea ;
	  print OUT "<question type=\"category\"><category><text>\$course\$/" ;
	  print OUT $linea ;
	  print OUT "</text></category></question> ","\n"
      }
      if ($_ =~ /^\s*\\begin\{question\}.*$/ ) {  #  Si es una l�nea de comienzo de pregunta,
	  $enunciado = " " ;                   #  inicializa un contenedor de enunciado.
	  $estado = "InteriorPregunta" ;
	  while(<IN>) {
	      if ($_ !~ /.*\\choice.*$/) {     #  Acumula todas las l�neas subsiguientes
		  chop ;                       #  hasta el primer \choice en el enunciado.
		  $enunciado = $enunciado.$_ ;
	      }
	      else {
		  last ;
	      }
	  }
					       #  Hemos llegado al final del enunciado;
					       #  lo imprimimos

	  print OUT "<question type=\"multichoice\">", "\n" ;
	  $numpreg = $numpreg + 1 ;
	  print OUT "<name><text>".$numpreg."</text></name>", "\n" ;
	  print OUT "<questiontext format=\"html\">", "\n" ;
	  $enunciado = "<text>".$enunciado."</text>" ;
	  $enunciado =~ s/\$/ \$\$/g ;
	  print OUT $enunciado, "\n" ;
	  print OUT "</questiontext>", "\n" ;
	  print OUT "<defaultgrade>1</defaultgrade>\n<penalty>0.0</penalty>", "\n" ;
      }

      if ($_ =~ /.*\\choice\[!\]\{(.*)\s*$/)  {
	  $linea = $1 ;
	  $linea =~ s/\$/\$\$/g ;
	  $linea = "<answer fraction=\"100\" format=\"html\">\n<text>".$linea ;
      }
      elsif ($_ =~ /.*\\choice\{(.*)\s*$/) {
	  $linea = $1 ;
	  # $linea =~ s/\$/\$\$/g ;
	  $linea = "<answer fraction=\"0\" format=\"html\">\n<text>".$linea ;
      }
      elsif ($_ =~ /^\s*\}\s*$/) {
	  $linea = "</text>\n</answer>" ;
      }
      elsif ($_ =~ /\s*\\end\{question\}.*$/ ) {
	  $linea = "</question>";
	  print OUT $linea, "\n\n" ;
	  $estado = "FueraPregunta" ;
      }
      else {
	  $linea = $_ ;
	  if ($linea =~ /^\s*$/)  {
	      $noblanca = TRUE ;
	  }
      }
      if ($estado eq "InteriorPregunta") {
	  if ($noblanca) {
	      $linea =~ s/\$/\$\$/g ;
	      print OUT $linea ;
	  }
      }
}


if (!@ARGV) {                                  # Leer todos los nombres de fichero pasados
  @ARGV = <STDIN> ;                            # como argumento.
  chop(@ARGV);
  }

for (@ARGV) {
  $entrada = $_ ;                              # Para cada uno de esos nombres...
  ($idioma,$base,$sufijo)  = split(/\./,$entrada) ;
  $salida  = $idioma.".".$base.".xml" ;
  open(IN,$entrada) ;                          # abrir el fichero de entrada,
  open(OUT,">$salida") ;                       # el fichero de salida,
					       # e iniciar el proceso de conversi�n.

  print OUT "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "\n" ;
  print OUT "<quiz>", "\n" ;
  $numpreg = 0 ;
  $segmento = "Sueltas" ;
  while(<IN>) {
      if ($_ =~ /^\s*\\begin\{block\}.*$/ ) {     # Comprobar si entramos en un bloque...
	  $segmento = "Bloque" ;
	  print OUT "<question type=\"cloze\">\n" ;
	  print OUT "<name><text>".$numpreg."</text></name>\n" ;
	  print OUT "<questiontext format=\"html\">\n" ;
	  print OUT "<text><![CDATA[<p><BR/>" ;
	  $estado = "PreambuloBloque" ;
      } elsif ($_ =~ /^\s*\\end\{block\}.*$/ ) {  # ...o si salimos de �l
	  $segmento = "Sueltas" ;
	  print OUT "]]></text></questiontext>\n<defaultgrade>1</defaultgrade>\n" ;
	  print OUT "</question>\n\n" ;
      }
      if ($segmento eq "Bloque" ) {
	&proceso_cuestiones_bloque ;
      } else {
	&proceso_cuestiones_sueltas ;
      }
  }
  print OUT "</quiz>", "\n" ;
  close(OUT) ;
}
