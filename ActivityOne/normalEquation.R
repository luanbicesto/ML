normalEquation<-function()
{
	chunkRead = 417340
	nParams = 91

	data = read.table("trainSet.txt", sep = "," , header = F , nrows = chunkRead)
	Y = data[,1]
	X = data[,c(2:nParams)]
	fit = lm(Y ~ ., data = X)
	summary(fit)

	write.csv(coefficients(fit),"ne.csv")
}

normalEquation()
