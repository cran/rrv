rrv = function (x)
{	if (!is.matrix (x) )
		x = as.matrix (cbind (x) )
	if (is.null (colnames (x) ) )
		colnames (x) = as.character (1:ncol (x) )
	class (x) = "rrv"
	x
}

rprv = function (x, w=rep (1, ncol (x) ) )
{	y = .rprv (x, w)
	colnames (y) = "x"
	extend (rrv (y), "rprv")
}

print.rrv = function (x, ...) print.default (x, ...)

plot.rrv = function (x, ...)
{	if (ncol (x) <= 2)
		plot (mecdf (unclass (x) ), ...)
	else
		warning ("couldn't plot rrv")
}

samp.rrv = function (x, n=3, m=n)
	samp.matrix (x, n, m)

.rprv = function (x, w) x %*% w


