.trianglew = function (n)
{	n = n + 1
	m = matrix (numeric (), nr=n * (n + 1) / 2, nc=3)
	enumf = 1:n
	enumr = n:1
	ws = (enumf - 1) / (n - 1)
	r = 1
	for (i in enumf)
		for (j in 1:enumr [i])
		{	m [r, 1] = ws [i]
			m [r, 2] = ws [j]
			m [r, 3] = 1 - ws [i] - ws [j]
			r = r + 1
		}
	m
}

.subtriangles = function (n)
{	m = matrix (numeric (), nr=n * (n + 1) / 2, nc=3)
	enumf = 1:n
	enumr = n:1
	r = q = 1
	for (i in enumf)
	{	for (j in 1:enumr [i])
		{	m [r, 1] = q
			m [r, 2] = q + enumr [i] + 1
			m [r, 3] = q + 1
			r = r + 1
			q = q + 1
		}
		q = q + 1
	}
	m
}

.plot.test = function (n)
{	nt = n * (n + 1) / 2
	subs = .subtriangles (n)
	ws = .trianglew (n)
	plot (0:1, 0:1, pch=NA)
	for (i in 1:nt)
	{	p1 = subs [i, 1]
		p2 = subs [i, 2]
		p3 = subs [i, 3]
		x1 = ws [p1, 1]
		y1 = ws [p1, 2]
		x2 = ws [p2, 1]
		y2 = ws [p2, 2]
		x3 = ws [p3, 1]
		y3 = ws [p3, 2]	
		#lines (c (x1, x2, x3, x1), c (y1, y2, y3, y1) )
		polygon (c (x1, x2, x3, x1), c (y1, y2, y3, y1), col="purple")
	}
}

.plot.test (4)
