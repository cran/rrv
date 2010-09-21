rrv = function (x, ...)
	extend (mecdf (x, ...), "rrv")

rprv = function (x, w, ...)
{	if (is.matrix (x) )
	{	if (missing (w) )
		{	n = ncol (x)
			w = rep (1 / n, n)
		}
		x = x %*% w
	}
	extend (rrv (x, ...), "rprv")
}

print.rrv = function (x, ...) cat ("rrv object\n")
print.rprv = function (x, ...) cat ("rprv object\n")
