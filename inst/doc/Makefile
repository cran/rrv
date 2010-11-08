makeall:
	make makedev
	make clean

makedev:
	make rrv.pdf

rrv.pdf: rrv.tex maia2.sty
	pdflatex rrv.tex

rrv.tex: rrv.rnw
	R CMD Sweave rrv.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*


