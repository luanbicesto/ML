gradientDiscent<-function()
{
	# inicialize variables
	nParams = 91
	theta = matrix(c(0), nrow=nParams)
	learningRate = 0.00000001
	#numIters = 50
	chunkRead = 1000
	precision = 10 ^ -5
	
	data = read.table("trainSet.txt", sep = "," , header = F , nrows = chunkRead, skip = 0)
	Y = data[,1]
	X = data[,c(2:nParams)]
	X = cbind(1, data.matrix(X))

	costFuncNew = 1
	costFuncOld = 0.001

	#for (k in 1:numIters){
	while(abs(costFuncNew - costFuncOld)/costFuncNew > precision){
		costFuncOld = costFuncNew
		error = (X %*% theta - matrix(Y))
		costFuncNew = sum(error[,1] ^ 2)
		costFuncNew = costFuncNew / (2*length(Y))
		print(costFuncNew)
		thetaAnt = theta
		fator = t(X) %*% error / length(Y)
		theta = theta - learningRate * fator
	}

	write.csv(thetaAnt,"thetaGd.csv")
}

gradientDiscent()

