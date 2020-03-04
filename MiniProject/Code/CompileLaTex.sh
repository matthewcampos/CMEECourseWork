#!/bin/bash
pdflatex $1.tex
pdflatex $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex
evince $1.pdf &

## Cleanup
rm *~
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc
rm -f texput.log
mkdir -p compiled && mv $1.* compiled/
find ./compiled -not -name $1.tex -not -name $1.pdf -type f -delete
mv ./compiled/* . && rmdir compiled
mv $1.pdf ../Results/
