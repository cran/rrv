rrv = function (x)
{	if (!is.matrix (x) )
		x = as.matrix (cbind (x) )
	if (is.null (colnames (x) ) )
		colnames (x) = as.character (1:ncol (x) )
	structure (x, class=c ("rrv", "matrix") )
}

rprv = function (x, w)
{	y = .rprv (x, w)
	extend (rrv (y), "rprv")
}

print.rrv = function (x, ...) print.default (x, ...)

plot.rrv = function (x, ...)
{	if (ncol (x) <= 2)
		plot (mecdf (x), ...)
	else
		warning ("couldn't plot rrv")
}

.rprv = function (x, w) x %*% w

