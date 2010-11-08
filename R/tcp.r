#temp call to thm
#todo: implement properly...
tcp = function (f, ...) thm (f, ...)

#note: points b and c wrong order
thm = function (f, n=17, ...)
{	tg = .triangulargrid (f, n, TRUE)
	labs = f$names
	cols = heat.colors (500)
	.triangleplot (TRUE, FALSE, labs [1], labs [2], labs [3])
	for (i in 1:(n - 1) )
	{	px = c (tg$mx [i, 1], tg$mx [i + 1, 1], tg$mx [i, 2])
		py = c (tg$my [i, 1], tg$my [i + 1, 1], tg$my [i, 2])
		pf = c (tg$mf [i, 1], tg$mf [i + 1, 1], tg$mf [i, 2])
		.thmp (px, py, pf, cols)
		if (i < n - 1)
			for (j in 2:(n - i) )
			{	px = c (px [2:3], tg$mx [i + 1, j])
				py = c (py [2:3], tg$my [i + 1, j])
				pf = c (pf [2:3], tg$mf [i + 1, j])
				.thmp (px, py, pf, cols)
				px = c (px [2:3], tg$mx [i, j + 1])
				py = c (py [2:3], tg$my [i, j + 1])
				pf = c (pf [2:3], tg$mf [i, j + 1])
				.thmp (px, py, pf, cols)
			}
	}
	.triangleplot (FALSE, TRUE)
}

#todo: equal sides/angles, labels...
.triangleplot = function (new=TRUE, border=TRUE, alab=NA, blab=NA, clab=NA, ...)
{	if (new)
	{	plot.new ()
		plot.window (c (-1, 1), c (0, 1) )
	}
	if (border)
	{	x = c (0, 1, -1, 0)
		y = c (1, 0, 0, 1)
		lines (x, y, ...)
	}
	if (!is.na (alab) ) text (0.25, 0.85, alab, adj=c (0, 0.5) )
	if (!is.na (blab) ) text (-1, 0.35, blab, adj=c (0, 0.5) )
	if (!is.na (clab) ) text (1, 0.35, clab, adj=c (1, 0.5) )
}

.thmp = function (x, y, v, cols)
{	v = mean (v)
	col = cols [round (v * length (cols) ) ]
	polygon (x, y, border=NA, col=col)
}

.triangulargrid = function (f, n, scale=FALSE)
{	u = seq (0, 1, length=n)
	mx = my = mf = matrix (numeric (), nr=n, nc=n)
	for (i in 1:n)
	{	a = u [i]
		for (j in 1:(n - i + 1) )
		{	b = u [j]
			xy = .ab2xy (a, b)
			mx [i, j] = xy [1]
			my [i, j] = xy [2]
			mf [i, j] = f (c (a, b, 1 - a - b) )
		}
	}
	if (scale)
	{	fmin = min (mf, na.rm=TRUE)
		fmax = max (mf, na.rm=TRUE)
		if (fmin == fmax) mf [] = 0.05
		else mf = 0.9 * (mf - fmin) / (fmax - fmin) + 0.05
	}
	LIST (n, mx, my, mf, u)
}

.ab2xy = function (a, b)
{	y = a
	#todo: simplify...
	x = (a - 1) + (1 - a - b) * 2
	c (x, y)
}




