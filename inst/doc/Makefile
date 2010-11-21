bldcmd=R CMD Sweave

default:
	make draft
	make clean

enhanced:
	make draft bldcmd=makesweave

draft:
	make rrv1.pdf
	make rrv2.pdf

rrv1.pdf: rrv1.tex v1s3.tex maia2.sty
	pdflatex rrv1.tex

rrv2.pdf: rrv2.tex maia2.sty
	pdflatex rrv2.tex

rrv1.tex: rrv1.rnw
	$(bldcmd) rrv1.rnw

rrv2.tex: rrv2.rnw
	$(bldcmd) rrv2.rnw

v1s3.tex: ext/v1s3.rnw
	$(bldcmd) ext/v1s3.rnw

init:
	rm -f /tmp/makesweavepipe
	mkfifo /tmp/makesweavepipe
	echo -e "0\n" > /tmp/makesweavestatus
	makesweave -i &

clean:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f tmp*
	rm -f Rplots*

