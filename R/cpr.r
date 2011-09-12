#conditional portfolio return
.cpr = function (f, x)
	extend (FUNCTION (f), "cpr", nv=ncol (x), names=colnames (x) )

#conditional-parameterised portfolio return
.cppr = function (f, x)
	extend (.cpr (f, x), "cppr")

#conditional portfolio return (rprv-valued)
cpr_rrv = function (x)
{	f.seed = function (w) rprv (.$x, w) 
	f = .cpr (f.seed, x)
	extend (f, "cpr_rrv", x=rrv (x) )
}

#todo: all pr functions, need to be vectorised for w
#todo: update plotting functions to reflect this
cpr_expected = function (x)
{	f.seed = function (w) sum (w * .$x.mean)
	f = .cppr (f.seed, x)
	x.mean = apply (x, 2, mean)
	extend (f, "cpr_expected", x.mean)
}

cpr_variance = function (x)
{	f.seed = function (w) sum (outer (w, w) * .$x.cov)
	f = .cppr (f.seed, x)
	x.cov = cov (x)
	extend (f, "cpr_variance", x.cov)
}

cpr_quantile = function (p, x)
{	f.seed = function (w) ecdfinv (.rprv (.$x, w), .$p)
	f = .cppr (f.seed, x)
	extend (f, "cpr_quantile", p, x)
}

#if duplicates, may not be same as cpr_quantile
cpr_median = function (x)
{	f.seed = function (w) median (.rprv (.$x, w) )
	f = .cppr (f.seed, x)
	extend (f, "cpr_median")
}

cpr_sd = function (x)
{	f.seed = function (w) sqrt (sum (outer (w, w) * .$x.cov) )
	f = .cppr (f.seed, x)
	x.cov = cov (x)
	extend (f, "cpr_sd", x.cov)
}

portfolio.names = function (f) .names (f)
.names = function (f) colnames (f$x)

plot.cppr = function (x, ...) s3x_plot (f=x, ...)

s3x_plot.cppr = function (f, ...)
{	if (f$nv == 2) .plot.pr2 (f, ...)
	else if (f$nv == 3) .plot.pr3 (f, ...)
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


