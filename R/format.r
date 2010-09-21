money.format = function (x, ndec=0, cs=NULL, sep=c (".", ","), br=TRUE, fill=TRUE)
{	v = is.finite (x)
	nv = length (v)
	ok = all (v)
	if (!ok) x = x [v]
	n = length (x)
	if (n > 0)
	{	pos = (x >= 0)
		x = abs (x)
		fstr = paste ("%.", ndec, "f", sep="")
		x = strsplit (sprintf (fstr, x), "\\.")
		y = character (n)
		for (i in 1:n)
		{	z = x [[i]][1]
			nc = nchar (z)
			ngr = ceiling (nc / 3)
			gr1 = (1:ngr) * 3
			gr2 = gr1 - 2
			gr1 [ngr] = nc
			gr1 = rev (nc - gr1 + 1)
			gr2 = rev (nc - gr2 + 1)
			y [i] = substring (z, gr1 [1], gr2 [1])
			if (ngr > 1) for (j in 2:ngr)
				y [i] = paste (y [i], substring (z, gr1 [j], gr2 [j]), sep=sep [2])
			
		}
		if (ndec > 0)
			for (i in 1:n) y [i] = paste (y [i], x [[i]][2], sep=sep [1])
		if (!is.null (cs) ) y = paste (cs, y, sep="")
		if (br)
		{	y [!pos] = paste ("(", y [!pos], ")", sep="")
			if (fill) y [pos] = paste (y [pos], " ", sep="")
		}
		y [!pos] = paste ("-", y [!pos], sep="")
		if (fill)
		{	nc = nchar (y)
			nmax = max (nc)
			ndiff = nmax - nc
			for (i in 1:n)
				y [i] = paste (paste (rep (" ", ndiff [i]), collapse=""), y [i], sep="")
		}
		if (ok) y
		else
		{	z = rep (NA, nv)
			z [which (v)] = y
			z
		}
	}
	else as.character (rep (NA, nv) )
}



