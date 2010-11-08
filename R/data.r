markowitz1959data = function ()
{	path = paste (.find.package ("rrv"), "/data/markowitz.csv", sep="")
	read.csv (path, check.names=FALSE)
}

