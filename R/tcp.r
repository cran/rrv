thm = function (f, n=25, ...)
{	tg = .triangulargrid (f, n, TRUE)
	labs = f$names
	cols = .thmcol (500)
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

tcp = function (f, ...)
{	labs = f$names
	.triangleplot (TRUE, FALSE, labs [1], labs [2], labs [3])
	#lines (f)
	.triangleplot (FALSE, TRUE)
}

#lines.pr = function (f, ...) {}
#lines.expectedpr = function (f, ...) {}
#lines.variancepr = function (f, ...) {}

#todo: equal sides/angles, labels...
.triangleplot = function (new=TRUE, border=TRUE, alab=NA, blab=NA, clab=NA, ...)
{	if (new)
	{	p0 = par (mar=rep (0.25, 4) )
		plot.new ()
		plot.window (c (-1, 1), c (0, 1) )
		#par (p0)
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
	implant (LIST (), n, mx, my, mf, u)
}

.thmp = function (x, y, v, cols)
{	v = mean (v)
	col = cols [round (v * length (cols) ) ]
	polygon (x, y, border=NA, col=col)
}

.thmcol = function (n)
{	#ca = c (0.1, 0.3, 0.5)
	ca = c (0.2, 0.3, 0.1)
	cb = c (0, 0.8, 0.05)
	cc = c (0.4, 0.9, 0)
	cd = c (0.85, 0.9, 0.85)
	y = character (n)
	for (i in 1:n)
	{	u = (i - 1) / (n - 1)
		cv = NULL
		if (u < 1/3)
		{	p = 3 * u
			cv = (1 - p) * ca + p * cb
		}
		else if (u < 2/3)
		{	p = 3 * (u - 1/3)
			cv = (1 - p) * cb + p * cc
		}
		else
		{	p = 3 * (u - 2/3)
			cv = (1 - p) * cc + p * cd
		}
		cv [cv < 0] = 0
		cv [cv > 1] = 1
		y [i] = rgb (cv [1], cv [2], cv [3])
	}
	y
}

.ab2xy = function (a, b)
{	y = a
	x = (a - 1) + (1 - a - b) * 2
	c (x, y)
}

