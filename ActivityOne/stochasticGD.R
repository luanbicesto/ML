stochasticGD<-function()
{
	# inicialize variables
	nParams = 91
	theta = matrix(c(1,1), nrow=nParams)
	learningRate = 0.00000001
	numIters = 20
	chunkRead = 1000

	data = read.table("trainSet.txt", sep = "," , header = F , nrows = chunkRead, skip = 0)
	Y = data[,1]
	X = data[,c(2:nParams)]

	for (k in 1:numIters) { # TODO: alter stop condition
		print(k)
		cfNew = 0

		for(i in 1:length(Y)){
			Xi = X[i,]
			Xi = cbind(1, data.matrix(Xi))
			hypoteseValue = Xi %*% theta
			thetaAnt = theta

			error = hypoteseValue - Y[i]
			fator = learningRate * error
			cfNew = cfNew + (error ^ 2)
			
			#foreach parameter that has to be found			
			for (j in 1:nParams){
				theta[j, 1] = theta[j, 1] - (fator * Xi[j])
			}
		}
		cfNew = cfNew / (2*length(Y))
		print(cfNew)
	}

	write.csv(thetaAnt,"theta.csv")
}

stochasticGD()

