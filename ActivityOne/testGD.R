testGD<-function()
{
	rows = 1000
	results = matrix(ncol = 3, nrow = 0)
	error = 0
	lineError = 0

	#read parameters values
	thetas = read.csv("ne.csv",header=F)
	thetas = thetas[,2]
	thetas = thetas[2:92]
	thetas = data.matrix(thetas)
	thetas = as.matrix(sapply(thetas, as.numeric))

	#read input
	input = read.table("trainSet.txt", sep = "," , header = F , nrows = rows)
	target = input[,1] 
	input = input[,2:91]

	for(i in 1:rows){
		line = input[i,]
		line = data.matrix(line)
		line = cbind(1, line)

		predictedYear = line %*% thetas
		#lineError = round((target[i] - predictedYear) ^ 2, 2)
		lineError = (target[i] - predictedYear) ^ 2
		error = error + lineError
		results = rbind(results, c(target[i], predictedYear, round(abs(target[i] - predictedYear), 2)))
	}

	soma = sum(results[,3] ^ 2)
	soma = soma / (2*rows)
	print(soma)
	print(error / (2*rows))
}

testGD()
