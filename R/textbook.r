#output incorrect, 2-source subsets
#ranges may not be correct
.textbookplot = function (x)
{	x = as.matrix (x)
	f_sd = cpr_sd (x)
	f_expected = cpr_expected (x)
	nv = ncol (x)
	names = colnames (x)
	if (nv == 2)
	{	p1 = .textbook.point (f_sd, f_expected, c (1, 0) )
		p2 = .textbook.point (f_sd, f_expected, c (0, 1) )
		p = rbind (p1, p2)
		x = p [,1]
		y = p [,2]
		xlim = range (x)
		xlim = xlim + 0.25 * c (-1, 1) * diff (xlim)
		plot (x, y, xlim=xlim, pch=16, cex=1.5, col="grey", xlab="sd", ylab="expected")
		.textbook.lines (f_sd, f_expected, c (1, 0), c (0, 1) )
		text (x, y, names)
	}
	else if (nv == 3)
	{	p1 = .textbook.point (f_sd, f_expected, c (1, 0, 0) )
		p2 = .textbook.point (f_sd, f_expected, c (0, 1, 0) )
		p3 = .textbook.point (f_sd, f_expected, c (0, 0, 1) )
		p = rbind (p1, p2, p3)
		x = p [,1]
		y = p [,2]
		xlim = range (x)
		xlim = xlim + 0.25 * c (-1, 1) * diff (xlim)
		plot (x, y, xlim=xlim, pch=16, cex=1.5, col="grey", xlab="sd", ylab="expected")
		.textbook.lines (f_sd, f_expected, c (1, 0, 0), c (0, 1, 0) )
		.textbook.lines (f_sd, f_expected, c (1, 0, 0), c (0, 0, 1) )
		.textbook.lines (f_sd, f_expected, c (0, 1, 0), c (0, 0, 1) )
		text (x, y, names)
	
	}
	else if (nv == 4)
	{	p1 = .textbook.point (f_sd, f_expected, c (1, 0, 0, 0) )
		p2 = .textbook.point (f_sd, f_expected, c (0, 1, 0, 0) )
		p3 = .textbook.point (f_sd, f_expected, c (0, 0, 1, 0) )
		p4 = .textbook.point (f_sd, f_expected, c (0, 0, 0, 1) )
		p = rbind (p1, p2, p3, p4)
		x = p [,1]
		y = p [,2]
		xlim = range (x)
		xlim = xlim + 0.25 * c (-1, 1) * diff (xlim)
		plot (x, y, xlim=xlim, pch=16, cex=1.5, col="grey", xlab="sd", ylab="expected")
		.textbook.lines (f_sd, f_expected, c (1, 0, 0, 0), c (0, 1, 0, 0) )
		.textbook.lines (f_sd, f_expected, c (1, 0, 0, 0), c (0, 0, 1, 0) )
		.textbook.lines (f_sd, f_expected, c (1, 0, 0, 0), c (0, 0, 0, 1) )
		.textbook.lines (f_sd, f_expected, c (0, 1, 0, 0), c (0, 0, 1, 0) )
		.textbook.lines (f_sd, f_expected, c (0, 1, 0, 0), c (0, 0, 0, 1) )
		.textbook.lines (f_sd, f_expected, c (0, 0, 1, 0), c (0, 0, 0, 1) )
		text (x, y, names)
	}
	else
		stop ("textbookplot supports 2 to 4 variables")
}

.textbook.lines = function (fx, fy, wa, wb)
{	w = NULL
	for (j in 1:length (wa) )
	{	wj = seq (wa [j], wb [j], length = 20)
		w = cbind (w, wj)
	}
	x = y = numeric (20)
	for (i in 1:20)
	{	x [i] = fx (w [i,])
		y [i] = fy (w [i,])
	}
	lines (x, y)
}

.textbook.point = function (fx, fy, w)
{	x = fx (w)
	y = fy (w)
	c (x, y)
}



