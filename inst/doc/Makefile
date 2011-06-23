default:
	make -f make-ext rrv1.pdf
	make -f make-ext rrv2.pdf
	make clean

enhanced:
	make -f make-ext rrv1.pdf bldcmd=makesweave
	make -f make-ext rrv2.pdf bldcmd=makesweave

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

