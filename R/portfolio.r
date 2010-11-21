portfolio = function (x)
{	f = FUNCTION (function (w) rprv (x, w), x=rrv (x) )
	extend (f, "portfolio")
}

portfolio.names = function (f) .names (f)
.names = function (f) colnames (f$x)

.xpr = function (f, sc, names) extend (f, c (sc, "pr"), names)

#todo: all pr functions, need to be vectorised for w
#todo: update plotting functions to reflect this
expectedpr = function (f)
{	x = apply (f$x, 2, mean)
	g = FUNCTION (function (w) sum (w * x), x)
	.xpr (g, "expectedpr", .names (f) )
}

#inefficient?
variancepr = function (f)
{	g = FUNCTION (function (w) sum (outer (w, w) * x), x=cov (f$x) )
	.xpr (g, "variancepr", .names (f) )
}

#todo: implement inverse continuous (u)ecdf function
quantilepr = function (f, p)
{	g = FUNCTION (function (w) quantile (.rprv (x, w), p, names=FALSE), x=f$x, p)
	.xpr (g, "quantilepr", .names (f) )
}

sdpr = function (f)
{	g = FUNCTION (function (w) sqrt (sum (outer (w, w) * x) ), x=cov (f$x) )
	.xpr (g, "sdpr", .names (f) )
}

medianpr = function (f)
{	g = FUNCTION (function (w) median (.rprv (x, w) ), x=f$x)
	.xpr (g, "medianpr", .names (f) )
}

#note, f$names used to compute n
plot.pr = function (f, ...)
{	n = length (f$names)
	if (n == 2) .plot.pr2 (f, ...)
	else if (n == 3) .plot.pr3 (f, ...)
	else stop ("can only plot 2 or 3 variable case")
	invisible (NULL)
}

.plot.pr2 = function (f, ylab=class (f) [1], ..., n=25)
{	w = seq (0, 1, length=n)
	r = numeric (n)
	for (i in 1:n) r [i] = f (c (1 - w [i], w [i]) )
	plot (w, r, type="l", xaxt="n", ylab=ylab, ...)
	axis (1, c (0, 1), f$names)
	i = c (1, n)
	points (w [i], r [i], pch=16, cex=1.5)
}

.plot.pr3 = function (f, heat=TRUE, contour=TRUE, ..., n=30)
{	if (heat)
	{	thm (f, n, ...)
		#if (contour) lines (f)
	}
	else if (contour)
		tcp (f, ...)
}



