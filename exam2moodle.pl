#!/usr/bin/perl
# Proceso de ficheros examdesign + EA3 --> Moodle XML
# (C) F. Tusell, Mayo 2008. Copia y modifica a tu antojo.

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
					       # e iniciar el proceso de conversión.

  print OUT "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "\n" ;
  print OUT "<quiz>", "\n" ;
  $numpreg = 0 ;
  while(<IN>) {
      if ($_ =~ /^\\begin\{question\}.*$/ ) {  #  Si es una línea de comienzo de pregunta,
	  $enunciado = " " ;                   #  inicializa un contenedor de enunciado.
	  $estado = "InteriorPregunta" ;
	  while(<IN>) {
	      if ($_ !~ /.*\\choice.*$/) {     #  Acumula todas las líneas subsiguientes
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
	  $linea =~ s/\$/\$\$/g ;
	  $linea = "<answer fraction=\"0\" format=\"html\">\n<text>".$linea ;
      }
      elsif ($_ =~ /^\s*\}\s*$/) {
	  $linea = "</text>\n</answer>" ;
      }
      elsif ($_ =~ /.*\\end\{question\}.*$/ ) {
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
	      print OUT $linea, "\n" ;
	  }
      }

  }
  print OUT "</quiz>", "\n" ;
  close(OUT) ;
}
