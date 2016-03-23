#!/bin/bash
#
#  Procesar ficheros multilingues eliminando las líneas en euskera y
#  castellano, y reteniendo las comunes y las de inglés.
#
grep -v "^ES-" $1 | grep -v "^EU-" | sed s/^EN-// > EN.$1
grep -v "^EN-" $1 | grep -v "^EU-" | sed s/^ES-// > ES.$1
grep -v "^ES-" $1 | grep -v "^EN-" | sed s/^EU-// > EU.$1
